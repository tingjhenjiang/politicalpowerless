source(file = "13_preprocessing_merge_all_datasets.R")
# modeling on responsiveness（立委回應民意） --------------------------------
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
#the ordinal package, via the clmm and clmm2 functions (clmm = Cumulative Link Mixed Model)
#nests: id
#Hausman test
#https://bookdown.org/tpemartin/econometric_analysis/r-for-panel-data.html#hausman-1
#linear growth model
#https://quantdev.ssri.psu.edu/sites/qdev/files/GCM_Chp3_Tutorial_2.html
#Using R and lme/lmer to fit different two- and three-level longitudinal models
#https://rpsychologist.com/r-guide-longitudinal-lme-lmer
analyse_responsiveness_class <- R6::R6Class("analyse_responsiveness", inherit=merge_all_datasets_class, public = list(
  respondmodels_file = NULL,
  #調查後半年內表決的法案（7-6-0-15-12為第7屆half yr界線、9-2-1-2-58為第9屆half yr界線）
  halfyr_billids = NULL,
  initialize = function(dataset_in_scriptsfile_directory="/mnt", filespath="/mnt", dataset_file_directory="/mnt", debug_func_mode=TRUE) {
    super$initialize(dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory, filespath=filespath, dataset_file_directory=dataset_file_directory, debug_func_mode=debug_func_mode)
    self$respondmodels_file<-file.path(save_dataset_in_scriptsfile_directory,"analyse_res","respondmodels.RData")
    self$halfyr_billids<-c("7-6-0-14-3","7-6-0-14-9","7-6-0-14-24","7-6-0-14-27",
                           "7-6-0-15-8","7-6-0-15-11","7-6-0-15-12",
                           "9-2-0-13-1","9-2-0-13-2","9-2-0-13-4","9-2-0-13-5","9-2-0-13-6","9-2-0-13-7","9-2-0-13-8","9-2-0-13-9","9-2-0-16-12","9-2-0-16-15","9-2-0-16-16","9-2-0-16-53","9-2-0-16-57","9-2-0-16-58","9-2-0-16-62","9-2-0-16-64","9-2-0-16-65","9-2-0-16-66","9-2-0-16-67","9-2-0-16-68","9-2-0-16-70","9-2-0-16-72","9-2-0-16-80","9-2-0-16-96","9-2-0-16-98","9-2-0-17-34","9-2-0-17-35","9-2-0-17-36","9-2-0-17-37","9-2-0-17-38","9-2-0-17-39","9-2-0-17-41","9-2-0-17-61","9-2-1-1-2","9-2-1-1-4","9-2-1-1-5","9-2-1-2-1","9-2-1-2-2","9-2-1-2-3","9-2-1-2-4","9-2-1-2-5","9-2-1-2-14","9-2-1-2-18","9-2-1-2-42","9-2-1-2-47","9-2-1-2-53","9-2-1-2-55","9-2-1-2-58")
  },
  get_all_respondmodels_keys = function() {
    load_env <- new.env()
    tryres<-try(load(file=self$respondmodels_file, envir=load_env, verbose=TRUE))
    all_respondmodels_keys<-try(names(load_env$all_respondmodels))
    if (is(all_respondmodels_keys,'try-error')) c() else all_respondmodels_keys
  },
  # * ordinal part：論文採用的cumulative link mixed model -------------------
  get_respondmodel_args = function(all_respondmodels_keys=self$get_all_respondmodels_keys(), newimps=1:6) {
    data.frame("formula"=c(
      #basemod
      #"respondopinion~1+days_diff_survey_bill_overallscaled*myown_factoredparticip_overallscaled+days_diff_survey_bill_overallscaled*myown_factoredses_overallscaled+days_diff_survey_bill_overallscaled*cluster_kamila+days_diff_survey_bill_overallscaled*similarity_distance_overallscaled+days_diff_survey_bill_overallscaled*myown_sex+days_diff_survey_bill_overallscaled*myown_selfid+similarity_distance_overallscaled*myown_factoredparticip_overallscaled+(days_diff_survey_bill_overallscaled|admindistrict/id_wth_survey)+(1|billid_myown)+issuefield+seniority_overallscaled+(1|partyGroup/legislator_name)+party_pressure_overallscaled+partysize+SURVEY"#,
      #simplemod
      "respondopinion~1+days_diff_survey_bill_overallscaled*myown_factoredparticip_overallscaled+days_diff_survey_bill_overallscaled*myown_factoredses_overallscaled+days_diff_survey_bill_overallscaled*cluster_kamila+(days_diff_survey_bill_overallscaled|admindistrict/id_wth_survey)+(1|billid_myown)+issuefield+(1|partyGroup/legislator_name)+party_pressure_overallscaled+SURVEY"#,
      #nullmod
      #"respondopinion~1+(1|billid_myown)+(1|partyGroup/legislator_name)+(1|adminvillage/id_wth_survey)+SURVEY"
      #simp3mod
      #"respondopinion~1+days_diff_survey_bill_overallscaled*myown_factoredparticip_overallscaled+days_diff_survey_bill_overallscaled*myown_factoredses_overallscaled+(days_diff_survey_bill_overallscaled|admindistrict/id_wth_survey)+(1|billid_myown)+(1|partyGroup/legislator_name)+party_pressure_overallscaled+SURVEY"#,
      #歷次嘗試過的其他模型設定（含隨機斜率完整版）請見git歷史版本
    ),stringsAsFactors=FALSE) %>%
      cbind(., newimp = rep(newimps, each = nrow(.) )) %>%
      #cbind(., withindays = rep(c(1095,80), each = nrow(.) )) %>% #183
      dplyr::mutate(storekey=paste0("2ndcorrected_obs_simpmod_3levels_toadmindistrict_noadminvillage_imp",newimp)) %>%
      dplyr::mutate(file = paste0(self$respondmodels_file,storekey,".RData")) %>%
      dplyr::filter(!(formula %in% !!all_respondmodels_keys))
  },
  run_respond_clmm_models = function(overall_nonagenda_df, respondmodel_args_needed=self$get_respondmodel_args(), save=TRUE) {
    halfyr_billids<-self$halfyr_billids
    respondmodels<-custom_apply_thr_argdf(respondmodel_args_needed, "storekey", function(fikey, loopargdf, datadf, ...) {
      argrow<-dplyr::filter(loopargdf, storekey==!!fikey)
      #reason for 3823: older bills 許添財 賴清德
      respondmodel<-dplyr::select(datadf, -tidyselect::ends_with("NA")) %>%
        dplyr::filter( billid_myown %in% !!halfyr_billids,
                       newimp==!!argrow$newimp) %>%
        droplevels() %>%
        {list(formula=as.formula(argrow$formula), data=., weights=magrittr::use_series(., "myown_wr"),
              control=ordinal::clmm.control(maxIter=2000, maxLineIter=2000, innerCtrl = "noWarn"),
              Hess=TRUE, model=TRUE, link="logit", threshold="flexible")} %>%
        do.call(ordinal::clmm, args=.) %>%
        try()
      return(respondmodel)
    }, respondmodel_args_needed=respondmodel_args_needed, datadf=overall_nonagenda_df, halfyr_billids=halfyr_billids)
    if (save==TRUE) {
      message(respondmodel_args_needed$file[1])
      save(respondmodels,file=respondmodel_args_needed$file[1])
    }
    respondmodels
  },
  merge_respondmodels_into_all = function(respondmodels) {
    load_env <- new.env()
    load(file=self$respondmodels_file, envir=load_env, verbose=TRUE)
    all_respondmodels <- load_env$all_respondmodels
    if (length(all_respondmodels)==0 | identical(all_respondmodels, list(a=1))) {
      all_respondmodels<-respondmodels
    } else {
      all_respondmodels<-rlist::list.merge(all_respondmodels,respondmodels)
    }
    save(all_respondmodels,file=self$respondmodels_file)
    all_respondmodels
  },
  # * glmlasso part -------------------
  #https://rdrr.io/cran/glmmLasso/src/demo/glmmLasso-soccer.r
  run_respond_glmmlasso_models = function(overall_nonagenda_df, lambdas=seq(500,0,by=-10)) {
    respondmodels_file<-self$respondmodels_file
    respondmodel_args<-data.frame("rnd"=c(
      "+(days_diff_survey_bill_overallscaled|admindistrict/id_wth_survey)+(1|billid_myown)+(1|admindistrict/id_wth_survey)+(1|partyGroup/legislator_name)"
    ),stringsAsFactors=FALSE) %>%
      cbind(., lambda = rep(lambdas, each = nrow(.) )) %>%
      dplyr::mutate(storekey=paste0("2ndcondense_timevarying_lasso", lambda)) %>%
      dplyr::mutate(file = paste0(respondmodels_file,storekey,".RData"))
    rundatadf<-dplyr::select(overall_nonagenda_df, -tidyselect::ends_with("NA")) %>%
      dplyr::filter( billid_myown %in% !!self$halfyr_billids ) %>%
      droplevels()
    respondmodels<-custom_apply_thr_argdf(respondmodel_args, "storekey", function(fikey, loopargdf, datadf, ...) {
      argrow<-dplyr::filter(loopargdf, storekey==!!fikey)
      respglmmlassos <- glmmLasso::glmmLasso(
        fix=respondopinion~1+days_diff_survey_bill_overallscaled+myown_factoredparticip_overallscaled+similarity_distance_overallscaled+myown_factoredses_overallscaled+as.factor(cluster_kamila)+as.factor(myown_sex)+as.factor(myown_selfid)+as.factor(issuefield)+seniority_overallscaled+party_pressure_overallscaled+as.factor(partysize)+as.factor(SURVEY),
        rnd=list(admindistrict=~1+days_diff_survey_bill_overallscaled,id_wth_survey=~1+days_diff_survey_bill_overallscaled,billid_myown=~1,legislator_name=~1),
        family=glmmLasso::cumulative(), data =datadf, lambda=argrow$lambda,
        switch.NR=TRUE, control=list(print.iter=TRUE)
      ) %>%
        try() %>%
        list() %>%
        magrittr::set_names(paste0("respglmmlasso", argrow$lambda))
      tryn<-1
      while (TRUE) {
        loadsavestatus<-try({
          load(file=argrow$file, verbose=TRUE)
          all_respondmodels<- respglmmlassos %>%
            list() %>%
            magrittr::set_names(argrow$storekey) %>%
            rlist::list.merge(all_respondmodels, .)
          save(all_respondmodels, file=argrow$file)
        })
        tryn<-tryn+1
        if(!is(loadsavestatus, 'try-error') | tryn>10) break
      }
      respglmmlassos
    }, datadf=rundatadf, mc.cores=1)
    respondmodels
  },
  # * MuMIn part -------------------
  run_mumin_dredge = function(modelfilename="respondmodels.corrected_obs_fixhalfyrbill_simple1_rndintconly.RData") {
    load_env <- new.env()
    load(file=file.path(save_dataset_in_scriptsfile_directory,"analyse_res",modelfilename), envir=load_env, verbose=TRUE)
    dredgeres<-MuMIn::dredge(load_env$respondmodels)
    save(dredgeres, file=file.path( save_dataset_in_scriptsfile_directory,"analyse_res","respondmodels.dredgeres.RData"  ))
    dredgeres
    # * model selection part -------------------
    ##https://sites.google.com/site/rforfishandwildlifegrads/home/mumin_usage_examples
  },
  # * reporting part：合併多重插補模型並輸出（Rubin's rules） -------------------
  pool_respondmodels = function(modimps=1:6, storekey_prefix="respondmodels.RData2ndcorrected_obs_simpmod_3levels_toadmindistrict_noadminvillage_imp", outputcsv="TMP.csv") {
    respmods<-list()
    for (modimp in modimps) {
      try({
        load_env <- new.env()
        load(file=file.path(save_dataset_in_scriptsfile_directory,"analyse_res",paste0(storekey_prefix,modimp,".RData" )), envir=load_env, verbose=TRUE)
        respmods[[as.character(modimp)]]<-load_env$respondmodels
      })
    }
    needrespmods<-respmods#[c(1,3,4,5,6)]
    t<-mice::as.mira(needrespmods)
    pv<-mice::pool(t)
    pooledppres<-pooling.clmm(t$analyses)
    summarytable<-cbind(summary(pv), pooledppres$fixed_effects)
    print(summarytable)
    if (!is.null(outputcsv)) {
      write.csv(summarytable, outputcsv)
    }
    print(pooledppres$random_dist)
    lapply(respmods, function(X) {print(X$info$AIC)})
    r_effect<-lapply(needrespmods, magrittr::extract2, "ST") %>%
      {lapply(names(.[[1]]), function(key, list_of_reffect_result) {
        lapply(list_of_reffect_result, function(Y,key) {magrittr::extract2(Y,key)}, key=key) %>%
          {purrr::reduce(.,magrittr::add)/length(list_of_reffect_result)} %>%
          as.data.frame()
      }, list_of_reffect_result=.)} %>%
      {magrittr::set_names(.,names(needrespmods[[1]]$ST))} %>%
      dplyr::bind_rows()
    list("summarytable"=summarytable, "pooledppres"=pooledppres, "random_effects"=r_effect)
  }
))

# * brms part（僅保留參考用，含informative priors設定） -------------------
if (FALSE) {
  #要把屆次加入群
  modelformula<-"respondopinion | weights(myown_wr)~1+days_diff_survey_bill_overallscaled*myown_factoredparticip_overallscaled+days_diff_survey_bill_overallscaled*similarity_distance_overallscaled+days_diff_survey_bill_overallscaled*myown_factoredses_overallscaled+days_diff_survey_bill_overallscaled*cluster_kamila+days_diff_survey_bill_overallscaled*myown_sex+days_diff_survey_bill_overallscaled*myown_selfid+(1+days_diff_survey_bill_overallscaled||admindistrict/id_wth_survey)+(1|billid_myown)+issuefield+myown_factoredses_overallscaled+myown_sex+myown_selfid+similarity_distance_overallscaled*myown_factoredparticip_overallscaled+elec_dist_type+seniority_overallscaled+cluster_kamila+(1|partyGroup/legislator_name)+party_pressure_overallscaled+partysize+SURVEY" %>%
    brms::brmsformula()
  #tprior<-brms::get_prior(formula=modelformula,data = overall_nonagenda_df[sample(nrow(overall_nonagenda_df), 100000), ])
  prior1 <- c(brms::set_prior("normal(0.5085828514,0.1)", class = "b", coef = "cluster_kamila.C"),
              brms::set_prior("normal(0.3333249552,0.1)", class = "b", coef = "cluster_kamila.L"),
              brms::set_prior("normal(-0.4338196234,0.1)", class = "b", coef = "cluster_kamila.Q"),
              brms::set_prior("normal(-0.3058793592,0.1)", class = "b", coef = "cluster_kamilaE4"),
              brms::set_prior("normal(1.4566205110,0.1)", class = "b", coef = "cluster_kamilaE5"),
              brms::set_prior("normal(-0.0569702140,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled"),
              brms::set_prior("normal(0.2178754552,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:cluster_kamila.C"),
              brms::set_prior("normal(-0.2320120817,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:cluster_kamila.L"),
              brms::set_prior("normal(0.3002668657,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:cluster_kamila.Q"),
              brms::set_prior("normal(-0.1600304458,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:cluster_kamilaE4"),
              brms::set_prior("normal(0.8573951154,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:cluster_kamilaE5"),
              brms::set_prior("normal(0.1403038135,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:myown_factoredparticip_overallscaled"),
              brms::set_prior("normal(0.5074425821,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:myown_factoredses_overallscaled"),
              brms::set_prior("normal(1.3914550996,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:myown_selfidaboriginal"),
              brms::set_prior("normal(-1.0652859391,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:myown_selfidforeignstate"),
              brms::set_prior("normal(0.4531094029,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:myown_selfidhakka"),
              brms::set_prior("normal(0.01,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:myown_selfidnewresid"),
              brms::set_prior("normal(-0.0586894384,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:myown_sexfemale"),
              brms::set_prior("normal(0.1450243475,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:similarity_distance_overallscaled"),
              brms::set_prior("normal(-0.0228590321,0.1)", class = "b", coef = "elec_dist_typepartylist"),
              brms::set_prior("normal(0.0048392311,0.1)", class = "b", coef = "issuefieldeco"),
              brms::set_prior("normal(0.4706332133,0.1)", class = "b", coef = "issuefieldesc"),
              brms::set_prior("normal(-0.3639829927,0.1)", class = "b", coef = "issuefieldinteraff"),
              brms::set_prior("normal(0.2403056082,0.1)", class = "b", coef = "myown_factoredparticip_overallscaled"),
              brms::set_prior("normal(-0.0002353101,0.1)", class = "b", coef = "myown_factoredparticip_overallscaled:similarity_distance_overallscaled"),
              brms::set_prior("normal(0.005,0.1)", class = "b", coef = "myown_factoredses_overallscaled"),
              brms::set_prior("normal(-0.005,0.1)", class = "b", coef = "myown_selfidaboriginal"),
              brms::set_prior("normal(0.005,0.1)", class = "b", coef = "myown_selfidforeignstate"),
              brms::set_prior("normal(0.8124392212,0.1)", class = "b", coef = "myown_selfidhakka"),
              brms::set_prior("normal(-1.8443427853,0.1)", class = "b", coef = "myown_selfidnewresid"),
              brms::set_prior("normal(-0.0535753347,0.1)", class = "b", coef = "myown_sexfemale"),
              brms::set_prior("normal(0.0614349248,0.1)", class = "b", coef = "party_pressure_overallscaled"),
              brms::set_prior("normal(-0.1308563682,0.1)", class = "b", coef = "partysizesmall"),
              brms::set_prior("normal(-0.0114244127,0.1)", class = "b", coef = "seniority_overallscaled"),
              brms::set_prior("normal(-0.2567981031,0.1)", class = "b", coef = "similarity_distance_overallscaled"),
              brms::set_prior("normal(-0.0373555187,0.1)", class = "b", coef = "SURVEY2016citizen"))
  respondmodels <- dplyr::select(overall_nonagenda_df, -tidyselect::ends_with("NA")) %>%
    dplyr::filter(billid_myown %in% !!c("7-6-0-14-3","7-6-0-14-9","7-6-0-14-24","7-6-0-14-27","9-2-0-13-1","9-2-0-13-2","9-2-0-13-4","9-2-0-13-5","9-2-0-13-6","9-2-0-13-7","9-2-0-13-8","9-2-0-13-9","9-2-0-16-12","9-2-0-16-15","9-2-0-16-16","9-2-0-16-53","9-2-0-16-57","9-2-0-16-58","9-2-0-16-62","9-2-0-16-64","9-2-0-16-65","9-2-0-16-66","9-2-0-16-67","9-2-0-16-68","9-2-0-16-70","9-2-0-16-72","9-2-0-16-80","9-2-0-16-96","9-2-0-16-98")  ) %>%
    droplevels() %>%
    brms::brm(modelformula, data = ., family = brms::cumulative(link = "logit"),
              prior=prior1, chains = 2, cores = parallel::detectCores(), iter = 1500) %>%
    try() %>%
    list() %>%
    magrittr::set_names("brms_responsive")
  #brms:::summary.brmsfit(brmmodelonrespondopinion)
  #brms:::plot.brmsfit(brmmodelonrespondopinion, ask = FALSE)
  #brms::WAIC(brmmodelonrespondopinion)
  #save(brmmodelonrespondopinion, file=paste0(dataset_in_scriptsfile_directory, "brmmodelonrespondopinion.RData"))
}

# * model comparison reporting（僅保留參考用） -------------------
if (FALSE) {
  load(file=paste0(save_dataset_in_scriptsfile_directory,"analyse_res/resp_fixed_bills(mini).RData" ), verbose=TRUE)
  #big but corrupted
  #load(file=paste0(save_dataset_in_scriptsfile_directory,"analyse_res/respondmodels.fixs1yrbill_rndintconly.RData" ), verbose=TRUE)
  #load(file=paste0(save_dataset_in_scriptsfile_directory,"analyse_res/resp_fixed_bills_to_halfyr.RData" ), verbose=TRUE)
  load(file=paste0(save_dataset_in_scriptsfile_directory,"analyse_res/respondmodels.corrected_obs_fixedhalfyrbill_fullmod_rndintconly.RData" ), verbose=TRUE)
  fullmodel<-respondmodels
  load(file=paste0(save_dataset_in_scriptsfile_directory,"analyse_res/respondmodels.corrected_obs_fixshalfyrbill_simple1_rndintconly.RData" ), verbose=TRUE)
  simple1mod<-respondmodels
  #load(file=paste0(save_dataset_in_scriptsfile_directory,"analyse_res/respondmodels.corrected_obs_fixshalfyrbill_simple2_nokamila_rndintconly.RData" ), verbose=TRUE)
  simple2mod<-respondmodels
  load(file=paste0(save_dataset_in_scriptsfile_directory,"analyse_res/respondmodels.corrected_obs_fixshalfyrbill_simple3_noissuefield_rndintconly.RData" ), verbose=TRUE)
  simple3mod<-respondmodels
  load(file=paste0(save_dataset_in_scriptsfile_directory,"analyse_res/respondmodels.corrected_obs_fixshalfyrbill_simple4_noissuefieldkamila_rndintconly.RData" ), verbose=TRUE)
  simple4mod<-respondmodels
  MuMIn::model.sel(fullmodel,simple1mod,simple3mod,simple4mod)
  t<-summary(respondmodels)
  write.csv(cbind(t$coefficients,confint(respondmodels)), "TMP.csv")
  t$info
  coeft<-coef(summary(respondmodels))
}

#法案清單備註（half yr界線）：
# 7-6-0-15-12 half yr to here（第7屆）
# 9-2-1-2-58 half yr to here（第9屆）
#第7屆其後: 7-7-0-17-3 7-7-0-17-4 7-7-0-17-22 7-7-0-17-27 7-7-0-17-29 7-7-0-17-30
#第9屆其後: 9-3-0-10-1 9-3-0-15-1 9-3-1-2-1 ~ 9-3-1-3-28 9-3-3-2-629 ~ 9-3-3-2-632
