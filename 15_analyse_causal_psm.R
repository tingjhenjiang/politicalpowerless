source(file = "13_preprocessing_merge_all_datasets.R")
# 因果分析：傾向分數方法 --------------------------------
# 延伸本研究原有之相關性分析（政治參與／社經地位與立委回應），以傾向分數方法
# 估計因果效果（政治平等／political powerlessness議題）：
#  - 二元處理（treatment）：propensity score matching（MatchIt；Ho, Imai, King & Stuart 2011）
#  - 連續處理：generalized propensity score／IPW for continuous treatment
#    （WeightIt；stabilized weights，Robins, Hernan & Brumback 2000；Hirano & Imbens 2004）
#  - 平衡診斷：cobalt::bal.tab／love.plot
# 抽樣權重myown_wr以s.weights納入傾向分數估計（DuGoff, Schuler & Stuart 2014），
# 結果模型使用「傾向分數權重×抽樣權重」之總權重
# 參考書目程式：propensity_score/（Leite 2017, Practical Propensity Score Methods Using R）
# 草稿：見(draft)10_psscore_matching.R、(draft)11_test_survey_and_propensityscore.R
analyse_causal_psm_class <- R6::R6Class("analyse_causal_psm", inherit=merge_all_datasets_class, public = list(
  causal_results_dir = NULL,
  causal_confounders = NULL,
  initialize = function(dataset_in_scriptsfile_directory="/mnt", filespath="/mnt", dataset_file_directory="/mnt", debug_func_mode=TRUE) {
    super$initialize(dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory, filespath=filespath, dataset_file_directory=dataset_file_directory, debug_func_mode=debug_func_mode)
    self$causal_results_dir <- file.path(save_dataset_in_scriptsfile_directory, "analyse_res", "causal")
    # 預設干擾變數（confounders）：處理前（pre-treatment）人口學變數；
    # 分析政治參與之因果效果時應另加入myown_factoredses_overallscaled（SES為參與之前置因）
    self$causal_confounders <- c("myown_sex","myown_age_overallscaled","myown_selfid","myown_marriage","myown_religion","myown_areakind","SURVEY")
  },
  build_causal_formula = function(treatment, confounders=self$causal_confounders, extra_confounders=c()) {
    unique(c(confounders, extra_confounders)) %>%
      paste0(collapse="+") %>%
      paste0(treatment,"~",.) %>%
      as.formula()
  },
  # 連續處理變數依加權中位數二分（供PSM用；連續處理本身建議直接用GPS）
  binarize_treatment_by_median = function(df, treatment, weightvar="myown_wr") {
    med<-matrixStats::weightedMedian(df[[treatment]], w=df[[weightvar]], na.rm=TRUE)
    as.integer(df[[treatment]] > med)
  },
  # * 二元處理：propensity score matching ----------------
  # 每個插補樣本各自配對與估計；estimand預設ATT
  run_psm_binary = function(df, treatment, outcome="respondopinion", confounders=self$causal_confounders,
                            extra_confounders=c(), matchmethod="nearest", distance="glm", estimand="ATT",
                            caliper=NULL, ratio=1, imps=1:6, impvar="newimp", weightvar="myown_wr", save=TRUE) {
    psformula<-self$build_causal_formula(treatment, confounders, extra_confounders)
    res<-custom_parallel_lapply(imps, function(m, ...) {
      d<-dplyr::filter(df, .data[[impvar]]==!!m) %>%
        dplyr::select(tidyselect::any_of(unique(c(outcome, treatment, confounders, extra_confounders, weightvar)))) %>%
        stats::na.omit() %>% droplevels()
      matchargs<-list(formula=psformula, data=d, method=matchmethod, distance=distance,
                      estimand=estimand, ratio=ratio, s.weights=as.formula(paste0("~",weightvar)))
      if (!is.null(caliper)) matchargs$caliper<-caliper
      mobj<-do.call(MatchIt::matchit, matchargs)
      baltab<-cobalt::bal.tab(mobj, un=TRUE, stats=c("mean.diffs","ks.statistics"))
      md<-MatchIt::match.data(mobj)
      # match.data之weights已含matching weight與s.weights之乘積（MatchIt文件）
      outcomeformula<-as.formula(paste0(outcome,"~",treatment))
      outmod<-MASS::polr(outcomeformula, data=md, weights=md$weights, Hess=TRUE)
      list(matchit=mobj, balance=baltab, outcome_model=outmod, n_matched=nrow(md))
    }, df=df, psformula=psformula, method=parallel_method)
    res %<>% magrittr::set_names(paste0("imp",imps))
    if (save==TRUE) {
      dir.create(self$causal_results_dir, showWarnings=FALSE, recursive=TRUE)
      psm_binary_results<-res
      save(psm_binary_results, file=file.path(self$causal_results_dir, paste0("psm_binary_",treatment,"_on_",outcome,".RData")))
    }
    res
  },
  # * 連續處理：generalized propensity score（IPW for continuous treatment）----------------
  # WeightIt method="glm"對連續處理以條件密度（預設常態；density可指定"dt_2"、"kernel"等）
  # 建構inverse probability weights，並自動以無條件密度為分子穩定化
  # （stabilized weights，Robins et al. 2000），等同GPS加權法；
  # 使用者提示：處理有時為連續變數（如myown_factoredparticip_overallscaled），即用此法
  run_gps_continuous = function(df, treatment="myown_factoredparticip_overallscaled", outcome="respondopinion",
                                confounders=self$causal_confounders, extra_confounders=c(),
                                density=NULL, trim_at=0.99, imps=1:6, impvar="newimp",
                                weightvar="myown_wr", treatment_form="linear", save=TRUE) {
    psformula<-self$build_causal_formula(treatment, confounders, extra_confounders)
    res<-custom_parallel_lapply(imps, function(m, ...) {
      d<-dplyr::filter(df, .data[[impvar]]==!!m) %>%
        dplyr::select(tidyselect::any_of(unique(c(outcome, treatment, confounders, extra_confounders, weightvar)))) %>%
        stats::na.omit() %>% droplevels()
      weightitargs<-list(formula=psformula, data=d, method="glm", s.weights=weightvar)
      if (!is.null(density)) weightitargs$density<-density
      wobj<-do.call(WeightIt::weightit, weightitargs)
      # 極端權重截尾（trimming）以穩定估計
      if (!is.null(trim_at)) wobj<-WeightIt::trim(wobj, at=trim_at)
      baltab<-cobalt::bal.tab(wobj, un=TRUE, stats=c("correlations"))
      # WeightIt回傳之weights不含抽樣權重，結果模型需與抽樣權重相乘
      totalw<-wobj$weights * d[[weightvar]]
      outcomeformula<-switch(treatment_form,
        "linear"=as.formula(paste0(outcome,"~",treatment)),
        "quadratic"=as.formula(paste0(outcome,"~",treatment,"+I(",treatment,"^2)")),
        as.formula(paste0(outcome,"~",treatment_form))
      )
      outmod<-MASS::polr(outcomeformula, data=d, weights=totalw, Hess=TRUE)
      list(weightit=wobj, balance=baltab, outcome_model=outmod,
           effective_n=sum(totalw)^2/sum(totalw^2))
    }, df=df, psformula=psformula, method=parallel_method)
    res %<>% magrittr::set_names(paste0("imp",imps))
    if (save==TRUE) {
      dir.create(self$causal_results_dir, showWarnings=FALSE, recursive=TRUE)
      gps_continuous_results<-res
      save(gps_continuous_results, file=file.path(self$causal_results_dir, paste0("gps_",treatment,"_on_",outcome,".RData")))
    }
    res
  },
  # * 平衡診斷輸出 ----------------
  inspect_causal_balance = function(res) {
    for (nm in names(res)) {
      message(paste("== balance:",nm))
      print(res[[nm]]$balance)
    }
    invisible(res)
  },
  plot_causal_balance = function(res, saveprefix=NULL) {
    plots<-lapply(names(res), function(nm) {
      p<-cobalt::love.plot(res[[nm]][[1]], stars="raw") + ggplot2::ggtitle(nm)
      if (!is.null(saveprefix)) {
        targetsavefilename<-here::here(paste0("plot/causal/",saveprefix,"_",nm,".png"))
        dir.create(dirname(targetsavefilename), showWarnings=FALSE, recursive=TRUE)
        ggplot2::ggsave(filename=targetsavefilename, plot=p)
      }
      p
    })
    plots
  },
  # * 跨插補樣本pooling（Rubin's rules）----------------
  pool_causal_models = function(res, writecsv_path=NULL) {
    outmods<-lapply(res, magrittr::extract2, "outcome_model") %>% unname()
    #註：polr之coef()不含門檻參數zeta而vcov()含之，直接餵mitools::MIcombine會維度不合，
    #故以完整參數向量c(coef,zeta)與vcov自組MIcombine輸入（其餘同shared_functions.R之micombineresult）
    fullcoefs<-lapply(outmods, function(m) c(stats::coef(m), m$zeta))
    vcovs<-lapply(outmods, stats::vcov)
    poolresult1<-mitools::MIcombine(results=fullcoefs, variances=vcovs) %>%
      mitools:::summary.MIresult() %>%
      dplyr::select(missInfo)
    poolresult2<-mice::pool(outmods) %>%
      mice:::summary.mipo(conf.int=TRUE)
    poolresult<-dplyr::bind_cols(poolresult1,poolresult2)
    if (!is.null(writecsv_path)) write.csv(poolresult, file=writecsv_path, row.names=FALSE)
    poolresult
  },
  # * 劑量反應（dose-response）曲線：GPS加權下不同處理水準之預測結果機率 ----------------
  compute_dose_response = function(res, treatment, treat_quantiles=seq(0.1,0.9,0.2)) {
    lapply(names(res), function(nm) {
      outmod<-res[[nm]]$outcome_model
      dat<-outmod$model
      tq<-quantile(dat[[treatment]], probs=treat_quantiles, na.rm=TRUE)
      newdata<-data.frame(tq) %>% magrittr::set_names(treatment)
      preds<-predict(outmod, newdata=newdata, type="probs")
      data.frame(imp=nm, treatment_value=tq, treat_quantile=treat_quantiles, preds, check.names=FALSE)
    }) %>% plyr::rbind.fill()
  }
))

if (FALSE) { #使用範例（僅保留參考用）
  causal_instance <- analyse_causal_psm_class$new(
    dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
    filespath=filespath,
    dataset_file_directory=dataset_file_directory,
    debug_func_mode=FALSE
  )
  overall_nonagenda_df <- causal_instance$get_overall_nonagenda_df(size="tiny")
  # 連續處理：政治參與對立委回應之因果效果（GPS/IPW）
  gps_res <- causal_instance$run_gps_continuous(
    overall_nonagenda_df,
    treatment="myown_factoredparticip_overallscaled",
    outcome="respondopinion",
    extra_confounders=c("myown_factoredses_overallscaled") #SES為參與之前置因，須控制
  )
  causal_instance$inspect_causal_balance(gps_res)
  pooled <- causal_instance$pool_causal_models(gps_res, writecsv_path="causal_gps_particip.csv")
  doseresp <- causal_instance$compute_dose_response(gps_res, treatment="myown_factoredparticip_overallscaled")
  # 連續處理：SES對立委回應
  gps_ses_res <- causal_instance$run_gps_continuous(
    overall_nonagenda_df,
    treatment="myown_factoredses_overallscaled",
    outcome="respondopinion"
  )
  # 二元處理範例：高低參與（加權中位數二分）之PSM
  # 註：中位數二分後treated與control數量相近，1:1不放回配對donor pool小，
  # 建議搭配caliper丟棄距離過遠之配對以確保平衡
  overall_nonagenda_df$particip_high <- causal_instance$binarize_treatment_by_median(
    overall_nonagenda_df, "myown_factoredparticip_overallscaled")
  psm_res <- causal_instance$run_psm_binary(
    overall_nonagenda_df,
    treatment="particip_high",
    outcome="respondopinion",
    caliper=0.1,
    extra_confounders=c("myown_factoredses_overallscaled")
  )
  causal_instance$inspect_causal_balance(psm_res)
  psm_pooled <- causal_instance$pool_causal_models(psm_res)
}
