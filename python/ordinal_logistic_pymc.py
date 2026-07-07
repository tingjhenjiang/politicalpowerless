#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Weighted ordinal (cumulative-logit) regression on GPU, replacing R survey::svyolr
/ ordinal::clm which hit out-of-memory and long compute on the responsiveness data.

Pipeline
--------
1. R side: analyse_ordinallogistic_instance$export_model_data_to_parquet() writes
   one parquet file per imputation (overall_nonagenda_df filtered to newimp==m)
   plus modelvars metadata json (see 14_analyse_ordinallogistic.R).
2. This script fits, for each imputation file, a weighted proportional-odds model
       P(respondopinion <= k | x) = logistic(cutpoint_k - x @ beta)
   with observation weights myown_wr entering as a weighted pseudo-log-likelihood
   (pm.Potential adds sum_i w_i * logp_i, the same estimator as svyolr).
3. Posterior means/sds per imputation are pooled across imputations with Rubin's
   rules and written to a csv.

Estimation backends (--method):
  advi      ADVI with minibatching (default; scales to millions of rows on a
            4GB GPU since only a minibatch is resident per step)
  nuts      full NUTS via numpyro/jax on GPU (accurate; needs the whole design
            matrix on GPU -- fine up to a few hundred thousand rows)

Usage:
  python ordinal_logistic_pymc.py --datadir /path/to/parquetdir --outfile results.csv \
      [--method advi] [--draws 1000] [--batchsize 4096] [--advi-steps 50000]

The parquet directory must contain files named ordinal_model_data_imp<M>.parquet
and a modelvars.json with fields: outcome, weight, conti_vars, catg_vars.
"""
import argparse
import glob
import json
import os
import re
import sys

import numpy as np
import pandas as pd


def parse_args():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--datadir", required=True, help="directory containing ordinal_model_data_imp<M>.parquet and modelvars.json")
    p.add_argument("--outfile", required=True, help="pooled results csv path")
    p.add_argument("--periemp-outfile", default=None, help="optional csv of per-imputation estimates")
    p.add_argument("--method", choices=["advi", "nuts"], default="advi")
    p.add_argument("--draws", type=int, default=1000, help="posterior draws (per chain for nuts)")
    p.add_argument("--tune", type=int, default=1000, help="nuts tuning steps")
    p.add_argument("--chains", type=int, default=2)
    p.add_argument("--advi-steps", type=int, default=50000)
    p.add_argument("--batchsize", type=int, default=4096, help="advi minibatch size")
    p.add_argument("--seed", type=int, default=20260707)
    p.add_argument("--imps", default=None, help="comma separated imputation numbers to run (default: all found)")
    return p.parse_args()


def load_metadata(datadir):
    with open(os.path.join(datadir, "modelvars.json"), "r", encoding="utf-8") as fh:
        meta = json.load(fh)
    for key in ("outcome", "weight", "conti_vars", "catg_vars"):
        if key not in meta:
            raise KeyError(f"modelvars.json missing field: {key}")
    # jsonlite auto_unbox collapses length-1 arrays to scalars
    for key in ("conti_vars", "catg_vars"):
        if isinstance(meta[key], str):
            meta[key] = [meta[key]]
    return meta


def build_design(df, meta):
    """Build outcome codes, design matrix (treatment coding, first level as
    reference -- mirrors R model.matrix defaults), and normalized weights."""
    outcome = meta["outcome"]
    weightcol = meta["weight"]
    conti = [v for v in meta["conti_vars"] if v in df.columns]
    catg = [v for v in meta["catg_vars"] if v in df.columns]
    missing = [v for v in meta["conti_vars"] + meta["catg_vars"] if v not in df.columns]
    if missing:
        print(f"  WARNING: variables not in data, skipped: {missing}", file=sys.stderr)

    d = df[[outcome, weightcol] + conti + catg].dropna()
    # ordered outcome -> integer codes 0..K-1 preserving category order
    yc = d[outcome]
    if not yc.dtype.name.startswith("category"):
        yc = yc.astype("category")
    y = yc.cat.codes.to_numpy().astype("int32")
    if (y < 0).any():
        raise ValueError("outcome contains unmapped categories")
    K = len(yc.cat.categories)

    Xnum = d[conti].astype("float64")
    dummies = pd.get_dummies(d[catg].astype("category"), drop_first=True, dtype="float64") if catg else pd.DataFrame(index=d.index)
    X = pd.concat([Xnum, dummies], axis=1)
    w = d[weightcol].to_numpy().astype("float64")
    # normalize weights to mean 1 so the pseudo-likelihood carries the actual
    # sample-size amount of information (standard practice for weighted PL)
    w = w / w.mean()
    return y, K, X, w, list(yc.cat.categories)


def fit_one_imputation(y, K, X, w, args):
    import pymc as pm
    import pytensor.tensor as pt

    Xv = X.to_numpy()
    n, p = Xv.shape
    coords = {"coef": list(X.columns), "cut": np.arange(K - 1)}

    if args.method == "advi":
        with pm.Model(coords=coords) as model:
            batch_y, batch_X, batch_w = pm.Minibatch(y, Xv, w, batch_size=args.batchsize)
            beta = pm.Normal("beta", 0.0, 2.5, dims="coef")
            cuts = pm.Normal(
                "cutpoints", mu=0.0, sigma=5.0, dims="cut",
                transform=pm.distributions.transforms.ordered,
                initval=np.linspace(-1, 1, K - 1),
            )
            eta = pt.dot(batch_X, beta)
            dist = pm.OrderedLogistic.dist(eta=eta, cutpoints=cuts)
            logp = pm.logp(dist, batch_y)
            # weighted pseudo-log-likelihood, rescaled from minibatch to full data
            pm.Potential("wloglik", (n / args.batchsize) * pt.sum(batch_w * logp))
            approx = pm.fit(n=args.advi_steps, method="advi", random_seed=args.seed,
                            progressbar=False)
            idata = approx.sample(draws=args.draws, random_seed=args.seed)
    else:
        with pm.Model(coords=coords) as model:
            beta = pm.Normal("beta", 0.0, 2.5, dims="coef")
            cuts = pm.Normal(
                "cutpoints", mu=0.0, sigma=5.0, dims="cut",
                transform=pm.distributions.transforms.ordered,
                initval=np.linspace(-1, 1, K - 1),
            )
            eta = pt.dot(pm.Data("X", Xv), beta)
            dist = pm.OrderedLogistic.dist(eta=eta, cutpoints=cuts)
            logp = pm.logp(dist, pm.Data("y", y))
            pm.Potential("wloglik", pt.sum(pm.Data("w", w) * logp))
            idata = pm.sample(
                draws=args.draws, tune=args.tune, chains=args.chains,
                nuts_sampler="numpyro", random_seed=args.seed, progressbar=False,
            )

    post = idata.posterior
    rows = []
    for name, dim in (("beta", "coef"), ("cutpoints", "cut")):
        da = post[name]
        mean = da.mean(dim=("chain", "draw")).to_numpy()
        sd = da.std(dim=("chain", "draw"), ddof=1).to_numpy()
        labels = [str(v) for v in da.coords[dim].to_numpy()]
        for lab, m_, s_ in zip(labels, mean, sd):
            rows.append({"term": f"{name}[{lab}]", "estimate": m_, "se": s_})
    return pd.DataFrame(rows)


def rubin_pool(per_imp):
    """per_imp: list of DataFrames with columns term/estimate/se."""
    allq = pd.concat([d.set_index("term")["estimate"].rename(i) for i, d in enumerate(per_imp)], axis=1)
    allu = pd.concat([(d.set_index("term")["se"] ** 2).rename(i) for i, d in enumerate(per_imp)], axis=1)
    m = allq.shape[1]
    qbar = allq.mean(axis=1)
    ubar = allu.mean(axis=1)
    if m == 1:
        # single imputation: no between-imputation variance component
        b = pd.Series(0.0, index=qbar.index)
    else:
        b = allq.var(axis=1, ddof=1)
    t = ubar + (1 + 1 / m) * b
    se = np.sqrt(t)
    # Rubin (1987) degrees of freedom
    lam = ((1 + 1 / m) * b / t).clip(lower=1e-12)
    df = (m - 1) / lam ** 2 if m > 1 else pd.Series(np.inf, index=qbar.index)
    from scipy import stats
    zval = qbar / se
    pval = 2 * stats.t.sf(np.abs(zval), df)
    lo = qbar - stats.t.ppf(0.975, df) * se
    hi = qbar + stats.t.ppf(0.975, df) * se
    return pd.DataFrame({
        "term": qbar.index, "estimate": qbar.to_numpy(), "se": se.to_numpy(),
        "statistic": zval.to_numpy(), "df": df.to_numpy(), "p.value": pval,
        "conf.low": lo.to_numpy(), "conf.high": hi.to_numpy(), "fmi": lam.to_numpy(),
    })


def main():
    args = parse_args()
    meta = load_metadata(args.datadir)
    files = sorted(glob.glob(os.path.join(args.datadir, "ordinal_model_data_imp*.parquet")))
    if args.imps:
        wanted = {int(s) for s in args.imps.split(",")}
        files = [f for f in files if int(re.search(r"imp(\d+)\.parquet$", f).group(1)) in wanted]
    if not files:
        raise SystemExit(f"no ordinal_model_data_imp*.parquet found in {args.datadir}")
    print(f"found {len(files)} imputation files; method={args.method}")

    per_imp = []
    for f in files:
        m = re.search(r"imp(\d+)\.parquet$", f).group(1)
        print(f"== imputation {m}: {f}")
        df = pd.read_parquet(f)
        y, K, X, w, cats = build_design(df, meta)
        print(f"   n={len(y)}, K={K} outcome levels {cats}, p={X.shape[1]} design columns")
        res = fit_one_imputation(y, K, X, w, args)
        res["imp"] = int(m)
        per_imp.append(res)

    if args.periemp_outfile:
        pd.concat(per_imp).to_csv(args.periemp_outfile, index=False)
        print(f"per-imputation estimates -> {args.periemp_outfile}")

    pooled = rubin_pool([d.drop(columns="imp") for d in per_imp])
    pooled.to_csv(args.outfile, index=False)
    print(f"pooled results -> {args.outfile}")
    print(pooled.to_string(index=False, max_rows=40))


if __name__ == "__main__":
    main()
