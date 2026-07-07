source(file = "13_preprocessing_merge_all_datasets.R")
# modeling on political participation --------------------------------
#Handle Missing Values with brms
#https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html
#DEM 7473 - Bayesian Regression using the INLA Approach
#https://rpubs.com/corey_sparks/431920
#DEM 7473 - Week 7: Bayesian modeling part 1
#https://rpubs.com/corey_sparks/431913
#DEM 7473 - Week 5: Hierarchical Models - Cross level interactions & Contextual Effects
#https://rpubs.com/corey_sparks/424927
#DEM 7473 - Week 3: Basic Hierarchical Models - Random Intercepts and Slopes
#https://rpubs.com/corey_sparks/420770
#DEM 7283 Example 10 - Survey Information and Small Area Estimation
#https://rpubs.com/corey_sparks/484730
#Example of using survey design weights Bayesian regression models for survey data
#https://rpubs.com/corey_sparks/157901
#HLM! 想聽不懂,很難!
#https://www.slideshare.net/beckett53/hlm-20140929
#第 60 章 隨機截距模型中加入共變量 random intercept model with covariates
#https://wangcc.me/LSHTMlearningnote/%E9%9A%A8%E6%A9%9F%E6%88%AA%E8%B7%9D%E6%A8%A1%E5%9E%8B%E4%B8%AD%E5%8A%A0%E5%85%A5%E5%85%B1%E8%AE%8A%E9%87%8F-random-intercept-model-with-covariates.html
#https://bookdown.org/wangminjie/R4SS/
#R-Sessions 16: Multilevel Model Specification (lme4)
#http://www.rensenieuwenhuis.nl/r-sessions-16-multilevel-model-specification-lme4/
#Bayesian mixed effects (aka multi-level) ordinal regression models with brms
#https://kevinstadler.github.io/blog/bayesian-ordinal-regression-with-random-effects-using-brms/
#Building a Multilevel Model in BRMS Tutorial: Popularity Data
#https://www.rensvandeschoot.com/tutorials/brms-started/
#Advanced Bayesian Multilevel Modeling with the R Package brms
#https://cran.r-project.org/web/packages/brms/vignettes/brms_multilevel.pdf
# 58 for dependent data
#https://wangcc.me/LSHTMlearningnote/Hierarchical.html
#Advanced Bayesian Multilevel Modelingwith the R Package brms (syntax)
#https://arxiv.org/pdf/1705.11123.pdf
#fixed effect v. random effect
#https://zhuanlan.zhihu.com/p/60528092
#ploting analysing result
#https://biol609.github.io/lectures/23c_brms_prediction.html#243_assessing_fit
#easy bayes
#https://m-clark.github.io/easy-bayes/posterior-predictive-checks.html
#calc p-value hypothesis testing
#https://www.rensvandeschoot.com/tutorials/brms-started/
#https://bookdown.org/content/4253/introducing-the-multilevel-model-for-change.html
#https://m-clark.github.io/mixed-models-with-R/
#svylme
#https://rdrr.io/github/tslumley/svylme/
#multiple imputation https://cran.r-project.org/web/packages/merTools/vignettes/imputation.html
#time to use random effect https://ah.nccu.edu.tw/bitstream/140.119/37427/8/503008.pdf
#lmerTest
#merTools
#sjstats
#data transformation
#https://molecular-service-science.com/2012/02/18/data-transformation/
#r-glmmadaptive
#nests: id
analyse_pp_class <- R6::R6Class("analyse_pp", inherit=merge_all_datasets_class, public = list(
  ppmodels_file = NULL,
  initialize = function(dataset_in_scriptsfile_directory="/mnt", filespath="/mnt", dataset_file_directory="/mnt", debug_func_mode=TRUE) {
    super$initialize(dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory, filespath=filespath, dataset_file_directory=dataset_file_directory, debug_func_mode=debug_func_mode)
    self$ppmodels_file<-file.path(save_dataset_in_scriptsfile_directory,"analyse_res","ppmodels.RData")
  },
  # prepare data: matching different surveys and centering ----------------------------
  get_merged_acrossed_surveys_list = function(minuspolicy=TRUE) {
    survey_data_imputed <- self$get_survey_data_imputed_stage(stage="mirt_lca_clustering_idealpoints")
    ret_merged_for_idealpoint_and_pp_df_list(survey_data_imputed, dataset_in_scriptsfile_directory, minuspolicy=minuspolicy)
  },
  get_all_ppmodels = function() {
    load_env <- new.env()
    load(file=self$ppmodels_file, envir=load_env, verbose=TRUE)
    load_env$all_ppmodels
  },
  # all possible models ----------------------------
  get_ppmodel_args = function(all_ppmodels=self$get_all_ppmodels(), needimps=1:6) {
    data.frame("formula"=c(
      #deduct myown_marriage myown_religion myown_areakind
      "myown_factoredparticip_ordinal~1+SURVEY+cluster_kamila+(1|cluster_kamila)+myown_factoredses_overallscaled+myown_sex+myown_selfid+myown_religion+myown_age_overallscaled+myown_areakind+(1|myown_areakind/admindistrict/adminvillage)+(1|admincity)"
      #歷次嘗試過的其他模型設定請見git歷史版本，例如：
      #"myown_factoredparticip_ordinal~1+(1|myown_areakind/admincity/admindistrict/adminvillage)+(1|cluster_kamila)+(1|SURVEY)"
      #"myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+myown_selfid+myown_areakind+cluster_kamila+myown_religion+(1|adminvillage)"
      #"myown_factoredparticip~myown_factoredses_overallscaled+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+myown_marriage+myown_sex+myown_selfid+cluster_kamila+myown_areakind+SURVEY+cluster_kamila*SURVEY+1+(1|cluster_kamila)"
      #"myown_factoredparticip~(1|adminvillage/admindistrict/admincity/myown_areakind/cluster_kamila/SURVEY)"
    ), stringsAsFactors=FALSE) %>%
      cbind(., needimp = rep(needimps, each = nrow(.)), stringsAsFactors=FALSE) %>%
      dplyr::mutate(storekey=paste0(formula,needimp)) %>%
      dplyr::filter(!(formula %in% !!names(all_ppmodels)))
  },
  # 以ordinal::clmm擬合政治參與的cumulative link mixed models
  run_ppmodels = function(merged_acrossed_surveys_list=self$get_merged_acrossed_surveys_list(), ppmodel_args=self$get_ppmodel_args(), savemodelfilename="pp_efficient_full", save=TRUE) {
    ppmodels<-custom_apply_thr_argdf(ppmodel_args, "storekey", function(fikey, loopargdf, datadf, ...) {
      needrow<-dplyr::filter(loopargdf, storekey==!!fikey)
      f<-needrow$formula %>% as.formula()
      needimp<-needrow$needimp
      # lme4::glmer(formula=., data={
      #     dplyr::mutate_at(datadf, "myown_factoredparticip", ~myown_factoredparticip-min(myown_factoredparticip)+1 )
      # }, family = Gamma) %>% #, family = poisson(link = "log")
      #robustlmm::rlmerRcpp(formula=., data=datadf) %>%
      #lme4::lmer(formula=., data=datadf) %>%
      #lme4::bootMer(x=., data=datadf) %>%
      retmodel<-datadf[[needimp]] %>%
        {list(
          formula=as.formula(needrow$formula),
          data=., weights=.$myown_wr, Hess=TRUE, model = TRUE, link = "logit",
          threshold = "flexible"
        )} %>%
        do.call(ordinal::clmm, args=.) %>%
        try()
        #c("flexible", "symmetric", "symmetric2", "equidistant")
      return(retmodel)
    }, datadf=merged_acrossed_surveys_list)
    if (save==TRUE) {
      try({
        all_ppmodels<-self$get_all_ppmodels()
        if (length(all_ppmodels)==0 | identical(all_ppmodels, list(a=1))) {
          all_ppmodels<-ppmodels
        } else {
          all_ppmodels<-rlist::list.merge(all_ppmodels,ppmodels)
        }
        try(save(all_ppmodels,file=self$ppmodels_file))
      })
      try(save(ppmodels, file=file.path(save_dataset_in_scriptsfile_directory,"analyse_res",paste0(savemodelfilename,".RData"))))
    }
    ppmodels
  },
  inspect_ppmodels = function(all_ppmodels=self$get_all_ppmodels()) {
    lapply(all_ppmodels, function(X) {try(ordinal:::summary.clmm(X))})
  },
  # 合併多重插補clmm模型結果（Rubin's rules）
  pool_ppmodels = function(modeltype="", outputcsv="TMP.csv") {
    #modeltype例如"(very_precious_efficient_full)"、"(very_precious_efficient_without_marriage)"等
    load_env <- new.env()
    load(file=file.path(save_dataset_in_scriptsfile_directory, "analyse_res", paste0("ppmodels",modeltype,".RData")), envir=load_env, verbose=TRUE)
    all_ppmodels <- load_env$all_ppmodels
    #pooling https://rdrr.io/github/DaanNieboer/ordinalimputation/api/
    t<-mice::as.mira(all_ppmodels)
    pv<-mice::pool(t)
    pooledppres<-pooling.clmm(t$analyses)
    summarytable<-cbind(summary(pv), pooledppres$fixed_effects)
    print(summarytable)
    if (!is.null(outputcsv)) {
      write.csv(summarytable, outputcsv)
    }
    print(pooledppres$random_dist)
    list("summarytable"=summarytable, "pooledppres"=pooledppres)
  }
))

# inspecting data distributions（僅保留參考用） ------------
if ({plotting_to_inspect_distribution<-FALSE;plotting_to_inspect_distribution}) {
  merged_acrossed_surveys_overall<-dplyr::bind_rows(merged_acrossed_surveys_list)
  plotsvykey<-"2016citizen"
  plotsvykey<-"2010overall"
  dplyr::filter(merged_acrossed_surveys_overall, .imp==1) %>%
    custom_plot("myown_factoredparticip_scaled","myown_wr")
  merged_acrossed_surveys_overall %>%
  {
    custom_plot(., "myown_factoredparticip","myown_wr") %>% print()
    custom_plot(., "myown_factoredparticip_scaled","myown_wr") %>% print()
  }
  t<-bestNormalize::bestNormalize(merged_acrossed_surveys_overall$myown_factoredparticip)
  adopting_transformation_method<-try(lapply(merged_acrossed_surveys_list_with_normality, function(X) {X[[2]]$chosen_transform}))
}

if (FALSE) { #模型殘差診斷（僅保留參考用）
  lapply(ppmodels, lme4:::summary.merMod, signif.stars=TRUE)
  lapply(ppmodels, summary, signif.stars=TRUE)
  lapply(ppmodels, function(X) {sum(lme4:::residuals.merMod(X))} )
  lapply(ppmodels, function(X) {hist(lme4:::residuals.merMod(X), breaks = 100)} )
  lapply(ppmodels, function(X) {shapiro.test(lme4:::residuals.merMod(X))} )
  lapply(ppmodels, robustlmm:::summary.rlmerMod)
  lapply(ppmodels, function(X) { sum(robustlmm:::residuals.rlmerMod(X))  } )
  lapply(ppmodels, function(X) { hist(robustlmm:::residuals.rlmerMod(X), breaks = 100)  } )
  lapply(ppmodels, function(X) { shapiro.test(robustlmm:::residuals.rlmerMod(X))  } )
}

if ({bayesian<-FALSE;bayesian}) { #brms貝氏多層次模型嘗試（僅保留參考用）
  #ses efficacy marriage age sex selfid psu ssu areakind clusterkamila SURVEY
  pp_to_cluster_nullmod <-
    brms::brm(#
      brms::bf(myown_factoredparticip_scaled|weights(myown_wr)~(1|adminvillage/admindistrict/admincity/myown_areakind/cluster_kamila/SURVEY) ),
      prior=prior1,
      family=brms::dskew_normal(
        x,
        mu = 0,
        sigma = 1,
        alpha = -2,
        xi = NULL,
        omega = NULL,
        log = FALSE
      ), #brms::student,
      data = merged_acrossed_surveys_list[[1]],
      chains=4,
      cores=parallel::detectCores(),
      iter = 2000,
      sample_prior = TRUE
      #,file = here::here("data/policyidealpoint_cos_similarity_to_median_to_kamila-robust")
    )
}

if (FALSE) { #brms prior設定嘗試（僅保留參考用）
  brms::get_prior(formula=brms::bf(myown_factoredparticip_scaled|weights(myown_wr)~(1|adminvillage/admindistrict/admincity/myown_areakind/cluster_kamila/SURVEY)),data = merged_acrossed_surveys_list[[1]])
  brms::get_prior(formula=brms::bf(myown_factoredparticip_scaled|weights(myown_wr)~((myown_factoredses_overallscaled+myown_marriage+myown_age_overallscaled+myown_age*myown_age+myown_sex+myown_selfid+myown_areakind+cluster_kamila+cluster_kamila*SURVEY)|SURVEY) +(1|cluster_kamila/SURVEY) ),data = merged_acrossed_surveys_list[[1]])

  prior1 <- c(brms::set_prior("normal(-10,100)", class = "b", coef = "extrav"),
              brms::set_prior("normal(10,100)", class = "b", coef = "extrav:texp"),
              brms::set_prior("normal(-5,100)", class = "b", coef = "sex"),
              brms::set_prior("normal(-5,100)", class = "b", coef = "texp"),
              brms::set_prior("normal(10,100)", class = "b", coef = "intercept" ))

  pp_to_cluster_mod1 <-
    brms::brm(#
      brms::bf(myown_factoredparticip_scaled|weights(myown_wr)~(myown_factoredses_overallscaled+myown_marriage+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+myown_sex+myown_selfid+myown_areakind+cluster_kamila+cluster_kamila*SURVEY)|SURVEY+(1|cluster_kamila/SURVEY) ),
      prior=prior1,
      family=brms::dskew_normal(
        x,
        mu = 0,
        sigma = 1,
        alpha = -2,
        xi = NULL,
        omega = NULL,
        log = FALSE
      ), #brms::student,
      data = merged_acrossed_surveys_list[[1]],
      chains=4,
      cores=parallel::detectCores(),
      iter = 2000,
      sample_prior = TRUE
    )

  brms:::summary.brmsfit(cossim_to_cluster_mod_robusts)
  brms::pp_check(cossim_to_cluster_mod_robusts)
  brms::pp_check(cossim_to_cluster_mod_robusts, group="cluster_kamila")

  #load(file=paste0(dataset_in_scriptsfile_directory,"brms/test_cossim_to_cluster_mod_robusts.RData"), verbose=TRUE)
  save(pp_to_cluster_nullmod, file=paste0(dataset_in_scriptsfile_directory,"brms/pp_to_cluster_nullmod.RData"))
}

if (FALSE) { #模型比較（僅保留參考用）
  get_vcov(t$analyses[[1]])
  pp_fullmod<-ppmodels[[1]]
  pp_noagemod<-ppmodels[[1]]
  pp_noareakindmod<-ppmodels[[1]]
  pp_noreligionmod<-ppmodels[[1]]
  MuMIn::model.sel(pp_fullmod,pp_noagemod,pp_noareakindmod,pp_noreligionmod)
}

#pick parameters
#https://easystats.github.io/parameters/index.html
#parameters::model_parameters(model)
