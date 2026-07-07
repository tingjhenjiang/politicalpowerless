# Python/GPU 分析程式

R端記憶體不足（OOM）或運算過久的模型，改以Python+GPU估計。R前處理資料以parquet交換。

## ordinal_logistic_pymc.py — 加權ordinal logistic（取代survey::svyolr）

回應變數 `respondopinion`（cumulative logit / proportional odds），抽樣權重 `myown_wr`
以weighted pseudo-log-likelihood進入模型（與svyolr同一估計量；權重正規化為平均1）。

### 1. R端匯出資料

```r
# preprocess.R 環境下
overall_nonagenda_df <- merge_all_instance$get_overall_nonagenda_df(size="tiny")
analyse_ordinallogistic_instance$export_model_data_to_parquet(overall_nonagenda_df)
# 輸出至 <save_dir>/python_ordinal/ordinal_model_data_imp<M>.parquet + modelvars.json
```

### 2. Python端估計（conda env: r_py_env）

```bash
# ADVI＋minibatch（預設；大資料、4GB GPU也可跑）
/root/miniconda3/envs/r_py_env/bin/python ordinal_logistic_pymc.py \
  --datadir <save_dir>/python_ordinal --outfile ordinal_pooled.csv \
  --method advi --advi-steps 50000 --batchsize 4096

# NUTS（numpyro/JAX on GPU；較準，需資料可整批放進GPU）
/root/miniconda3/envs/r_py_env/bin/python ordinal_logistic_pymc.py \
  --datadir <save_dir>/python_ordinal --outfile ordinal_pooled.csv \
  --method nuts --draws 1000 --tune 1000 --chains 2
```

各插補樣本分開估計後以Rubin's rules合併（estimate/se/df/p/CI/fmi）輸出csv。
`--periemp-outfile` 可另存各插補估計值。

### 驗證

以模擬資料（N=20000, 4 levels, 已知係數）比對：
- NUTS/GPU與ADVI估計皆與真值及加權`MASS::polr`一致（誤差 < 0.03）
- 環境需求：pymc、jax/jaxlib（conda-forge CUDA版）、numpyro、pyarrow、pandas、scipy；
  numpy須<2.5（numba相容性）
