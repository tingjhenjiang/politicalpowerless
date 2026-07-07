source(file = "06_preprocessing_imputation.R")
# 第五部份：factor analysis 與 IRT latent variables -------------------------------------------
# MCMCmixfactanal
fa_and_irt_process_class <- R6::R6Class("fa_and_irt_process", inherit=survey_imputation_class, public = list(
  miced_survey_9_ubuntu_filepath = NULL,
  survey_question_category_df = NULL,
  need_ses_var_assigned = NULL,
  needsesmod = NULL,
  need_efficacy_recode_var_detail_surveyid = NULL,
  need_particip_var = NULL,
  initialize = function(dataset_in_scriptsfile_directory="/mnt", filespath="/mnt", dataset_file_directory="/mnt", debug_func_mode=TRUE) {
    super$initialize(dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory, filespath=filespath, dataset_file_directory=dataset_file_directory, debug_func_mode=debug_func_mode)
    self$miced_survey_9_ubuntu_filepath <- file.path(dataset_file_directory, "rdata", "miced_survey_9_Ubuntu18.04.3LTSdf.RData")
    self$survey_question_category_df <- custom_ret_survey_book_file(dataset_file_directory=dataset_file_directory, subject="pp")
    # 第五-1部份：IRT latent variables 將職業社經地位、家庭收入、教育程度萃取成為階級
    self$need_ses_var_assigned <- c("myown_eduyr","myown_ses","myown_income","myown_family_income")
    self$needsesmod <- 'myown_factoredses=~myown_eduyr+myown_ses+myown_income+myown_family_income'
    # 第五-2部份：IRT latent variables 政治效能感
    self$need_efficacy_recode_var_detail_surveyid <- list(
      "2004citizen@v47"=c(1,2,3,4,5),
      "2004citizen@v48"=c(1,2,3,4,5),
      "2004citizen@v49"=c(5,4,3,2,1),
      "2004citizen@v50"=c(1,2,3,4,5),
      "2004citizen@v51"=c(4,3,2,1),
      "2004citizen@v52"=c(4,3,2,1),
      "2010env@v21a"=c(1,2,3,4,5),
      "2010env@v21b"=c(1,2,3,4,5),
      "2010env@v70a"=c(1,2,3,4,5),
      "2010env@v70c"=c(5,4,3,2,1),
      "2010env@v70d"=c(5,4,3,2,1),
      "2010env@v70e"=c(1,2,3,4,5),
      "2010env@v78"=c(4,3,2,1),
      "2010env@v79"=c(4,3,2,1),
      "2010env@v26a"=c(1,2,3,4,5),
      "2010env@v26d"=c(1,2,3,4,5),
      "2010env@v26f"=c(1,2,3,4,5),
      "2010overall@v67d"=c(6,5,4,3,2,1),
      "2010overall@v67e"=c(6,5,4,3,2,1),
      "2010overall@v67f"=c(1,2,3,4,5,6),
      "2010overall@v67g"=c(1,2,3,4,5,6),
      "2010overall@v67h"=c(6,5,4,3,2,1),
      "2010overall@v67i"=c(6,5,4,3,2,1),
      "2016citizen@d16a"=c(1,2,3,4,5),
      "2016citizen@d16b"=c(5,4,3,2,1),
      "2016citizen@d16c"=c(5,4,3,2,1),
      "2016citizen@d16d"=c(1,2,3,4,5)
    )
    # 第五-3部份：IRT latent variables 政治參與；用item respond抓出隱藏變數「政治參與程度」
    # https://www.researchgate.net/post/How_to_conduct_item_analysis_with_a_likert_scale_questionaire
    # mirt help: https://github.com/philchalmers/mirt/wiki
    # http://moodle.ncku.edu.tw/pluginfile.php/977679/mod_resource/content/1/item_response_theory.pdf
    #2004citizen: v28 v29 v30 v31 v32 v33 v34 v35 v36 v37 v38 v39 v40 v59
    #2016citizen-fit2-z1: b4 h2a h2b h2c h2d h2e h2f h2g h2h h3a h3b h3c h4
    #2010overall-fit2: v79a v79b v79c v79d
    #2010env-fit1: v34 v35a v35b v35c ( v33f v75 v76 v77-為了環保而刻意不買某些產品,常不常參與社區的環保工作,常不常反應社區中容易造成天災危險的情況,常不常反應社區中造成環境污染的情況)
    self$need_particip_var <- list(
      "2004citizen"=c("v28","v29","v30","v31","v32","v33","v34","v35","v36","v37","v38","v39","v40"), #,"v59",v88 v90
      "2010env"=c("v34","v35a","v35b","v35c"), #投票"v104",
      "2010overall"=c("v79a","v79b","v79c","v79d") %>%
        c("v82a","v82b","v82c","v82d","v83","v85"), #可能要補間接參與v82a,v82b,v82c,v82d;v83,v85 投票
      "2016citizen"=c("h2a","h2b","h2c","h2d","h2e","h2f","h2g","h2h","h3a","h3b","h3c") %>%#h4 投票; 可能要補間接參與h1_01,h1_02,h1_03,h1_04,h1_05,h1_06,h1_07,h1_08,h1_09,h1_10,h1_11,h1_12,h1_13,h1_14,h1_15
        c("h4r","h1_01","h1_02","h1_03","h1_04","h1_05","h1_06","h1_07","h1_08","h1_09","h1_10","h1_11","h1_12","h1_13","h1_14","h1_15") %>%
        c("b4") #談論社會上發生的事情
    )
  },
  # miced_survey_2surveysonly系列各階段暫存檔路徑；stage=""代表06階段的原始填補檔
  miced_2surveys_stage_filepath = function(stage="") {
    if (stage=="") {
      self$miced_survey_2surveysonly_filepath
    } else {
      file.path(save_dataset_in_scriptsfile_directory, paste0("miced_survey_2surveysonly_", stage, ".RData"))
    }
  },
  get_survey_data_imputed_stage = function(stage="") {
    load_env <- new.env()
    load(file=self$miced_2surveys_stage_filepath(stage=stage), envir=load_env, verbose=TRUE)
    load_env$survey_data_imputed
  },
  save_survey_data_imputed_stage = function(survey_data_imputed, stages=c("mirt","mirt_lca","mirt_lca_clustering","mirt_lca_clustering_idealpoints")) {
    #save(survey_data_imputed, file=paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt.RData"))
    #save(survey_data_imputed, file=paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt_lca_clustering.RData"))
    for (stage in stages) {
      save(survey_data_imputed, file=self$miced_2surveys_stage_filepath(stage=stage))
    }
  },
  lavaan_to_factor = function(X,imps,returnv="df",need_ses_var_assigned=self$need_ses_var_assigned, needsesmod=self$needsesmod) {
    #need_ses_var_assigned %<>% extract2(X$SURVEY[1]) %>%
    #  intersect(names(X))
    message("need ses var is ", X$SURVEY[1], " ", need_ses_var_assigned)
    for (checkvar in need_ses_var_assigned) {
      message("nrows of empty ", checkvar, " are ", sum(is.na(X$checkvar)) )
    }
    fittingmodels<-list()
    for (impn in imps) {
      storekey<-paste0(X$SURVEY[1],"_imp",impn)
      dat<-data.frame(X[X$.imp==impn,c("myown_wr",need_ses_var_assigned)]) %>%
        dplyr::mutate_at(need_ses_var_assigned, ~as.numeric(scale(.)))
      lavaan.fit<-lavaan::cfa(needsesmod, data = dat, sampling.weights="myown_wr")
      fittingmodels[[storekey]]<-lavaan.fit
      #summary(lavaan.fit)
      #df_svydesign <- svydesign(ids = ~1, data=dat, weights = X[X$.imp==impn,]$myown_wr)
      #survey.fit <- lavaan.survey(lavaan.fit=lavaan.fit, survey.design=df_svydesign)
      #summary(survey.fit)
      predicted_ses<-lavaan::lavPredict(lavaan.fit)[,1]
      X$myown_factoredses[X$.imp==impn]<- predicted_ses
      X$myown_factoredses_scaled[X$.imp==impn]<- predicted_ses %>%
        {as.numeric(scale(.))}
      X$myown_factoredses_mean[X$.imp==impn]<- mean(predicted_ses)
      #https://jiaxiangli.netlify.com/2018/10/21/factor-analysis/
      if ({oldfactorses<-FALSE;oldfactorses}) {
        df_svydesign <- X[X$.imp==impn,need_ses_var_assigned] %>%
          svydesign(ids = ~1, data = ., weights = X[X$.imp==impn,]$myown_wr)
        needformula <- as.formula(paste0("~",need_ses_var_assigned,collapse = "+"))
        svy.efa.results<-svyfactanal(formula=needformula,
                                     #x=X[X$.imp==impn,need_ses_var_assigned],
                                     design=df_svydesign, factors=1,
                                     n="sample", #n = c("none", "sample", "degf","effective", "min.effective"),
                                     rotation="promax"
                                     #,scores="Bartlett"
        )
        efa.results<-factanal(
          X[X$.imp==impn,need_ses_var_assigned],
          data=X[X$.imp==impn,need_ses_var_assigned],
          factors=1,
          rotation="promax",
          scores=c("regression")
        )
        X$myown_factoredses[X$.imp==impn]<-efa.results$scores[,1]
      }
    }
    if (returnv=="model") {
      return(fittingmodels)
    } else {
      return(X)
    }
  },
  compute_factoredses = function(survey_data_imputed, imps=imputation_sample_i_s) {
    lapply(survey_data_imputed, self$lavaan_to_factor, imps=imps)
  },
  get_lavaan_cfa_ses_models = function(survey_data_imputed, imps=imputation_sample_i_s) {
    lapply(survey_data_imputed, self$lavaan_to_factor, imps=imps, returnv="model")
  },
  inspect_lavaan_survey_ses_models = function(survey_data_imputed, lavaan_cfa_ses_models) {
    for (key in names(lavaan_cfa_ses_models)) {
      message(paste("now in",key))
      build_svydata<-survey_data_imputed[[key]] %>%
        dplyr::mutate_at(self$need_ses_var_assigned, ~as.numeric(scale(.)))
      mice.imp2<-split(build_svydata, build_svydata$.imp) %>%
        mitools::imputationList()
      svy.df_imp<-survey::svydesign(ids=~1, weights=~myown_wr,data=mice.imp2)
      lavaan_fit_ses.model<-lavaan::cfa(self$needsesmod) #,do.fit = FALSE
      out3<-lavaan.survey::lavaan.survey(lavaan_fit_ses.model,survey.design=svy.df_imp)
      #lavaan:::summary(lavaan_cfa_ses_models[[key]][[1]]) %>% print
      lavaan::summary(out3)
      lavaan::fitMeasures(out3, "chisq") %>% print()
      if (readline("continue?")=="N") break
    }
    #install.packages("psy")
    #library(psy)
    #psy::scree.plot(fa.class$correlation)
  },
  compute_factoredefficacy = function(survey_data_imputed, imps=imputation_sample_i_s) {
    lapply(survey_data_imputed,function(X,need_efficacy_recode_var_detail_surveyid,imps) {
      needlist <- rlist::list.match(need_efficacy_recode_var_detail_surveyid, X$SURVEY[1])
      needvars <- names(needlist) %>%
        sapply(customgsub,paste0(X$SURVEY[1],"@"),"") %>%
        unname()
      needlist %<>% magrittr::set_names(needvars)
      irt_target_d <- X[,c(".imp","myown_wr",needvars)] #原版未選入myown_wr，會使survey.weights取欄失敗，已修正
      for (needvar in needvars) {
        #needvar<-"v49"
        #ori: [2] 同意 [4] 不同意 [4] 不同意 [2] 同意 [4] 不同意 [1] 非常同意
        #should be: 422425
        #needvar<-"v47"
        #ori: [4] 不同意 [4] 不同意 [2] 同意 [2] 同意 [3] 既不同意也不反對 [2] 同意
        #should be: 442232
        irt_target_d[,needvar] %<>% factor(.,levels(.)[extract2(needlist,needvar)]) %>%
          seq(from=1,to=length(extract2(needlist,needvar)))[.]
      }
      for (imp in imps) {
        estimatemodel<-mirt::mirt(
          data=irt_target_d[irt_target_d$.imp==imp,needvars],
          model=1,
          itemtype = "graded",
          technical = list("NCYCLES"=40000),
          survey.weights = irt_target_d[irt_target_d$.imp==imp,c("myown_wr")])
        poliefficacy<-mirt::fscores(estimatemodel,method="EAP",rotate="varimax") %>%
          as.data.frame() %>%
          set_colnames(c("myown_factoredefficacy"))
        avgpoliefficacy<-mean(poliefficacy$myown_factoredefficacy)
        X[irt_target_d$.imp==imp,c("myown_factoredefficacy")]<-poliefficacy$myown_factoredefficacy
        X[irt_target_d$.imp==imp,c("myown_factoredefficacy_scaled")]<-poliefficacy$myown_factoredefficacy %>% #bind_cols(X,poliefficacy)
          {as.numeric(scale(.))}
        X[irt_target_d$.imp==imp,c("myown_factoredefficacy_mean")]<-avgpoliefficacy
      }
      return(X)
    },
    need_efficacy_recode_var_detail_surveyid=self$need_efficacy_recode_var_detail_surveyid,
    imps=imps)
  },
  # 把政治參與變數依標籤重新排序（越參與數字越大，比較好解釋）
  reorder_particip_vars = function(survey_data_imputed) {
    lapply(survey_data_imputed,function(X,need_particip_var_assigned) {
      need_particip_var_assigned %<>% magrittr::extract2(X$SURVEY[1]) %>%
        base::intersect(names(X))
      customreordercatbylabelname<-function(X,desc=FALSE) {
        forcats::fct_reorder(X,as.character(X),.fun=unique,.desc=desc) %>%
          return()
      }
      X <- dplyr::mutate_at(X,need_particip_var_assigned, customreordercatbylabelname, desc=TRUE)
      #forcats::fct_reorder(f,sort(levels(f),decreasing=FALSE))
      #forcats::fct_reorder(f,sort(levels(f),decreasing=TRUE))
      return(X)
    },need_particip_var_assigned=self$need_particip_var)
  },
  # 第五-3-1部份：parametric IRT non-Rasch models - GRM Model
  # mirt::mirt by 'graded'
  # ltm:grm
  mirt_to_model_particip = function(X,need_particip_var_assigned,imps,returnv="both",survey_question_category_df,method=parallel_method,...) {
    needparticip_surveyi<-X$SURVEY[1]
    need_detailed_particip_var<-magrittr::extract2(need_particip_var_assigned,needparticip_surveyi)
    base::setdiff(magrittr::extract2(need_particip_var_assigned,needparticip_surveyi),names(X)) #"h4r" ???
    irt_target_d<-X[,c(".imp","myown_wr",need_detailed_particip_var)] %>% #原版未選入myown_wr，會使survey.weights取欄失敗，已修正
      dplyr::mutate_at(.vars=need_detailed_particip_var, .funs=function(f) {
        #return(as.numeric(levels(f))[f])
        (seq(from=1,to=length(f)))[f] %>% return()
      })
    corresponding_item_type<-data.frame("ID"=need_detailed_particip_var) %>%
      dplyr::left_join(survey_question_category_df[[needparticip_surveyi]]) %>%
      magrittr::use_series("itemtype")
    store_keys_df<-data.frame("store_key"=paste0(needparticip_surveyi, "_imp", imps),
                              "imp"=imps, stringsAsFactors =FALSE)
    estimatemodels<-custom_parallel_lapply(store_keys_df$imp, function(imp,...) {
      message(paste("now in imp",imp))
      mirtargs<-list(
        data=irt_target_d[irt_target_d$.imp==imp,need_detailed_particip_var],
        model=1,
        itemtype = corresponding_item_type,
        technical = list("NCYCLES"=500),
        survey.weights = irt_target_d[irt_target_d$.imp==imp,c("myown_wr")],
        dentype= "Davidian-6", #"EHW"
        SE=FALSE
      )
      mirtargs2<-mirtargs
      mirtargs2[["itemtype"]]<-"graded"
      estimatemodel<-try(do.call(mirt::mirt, mirtargs))
      if (is(estimatemodel, 'try-error')) {
        estimatemodel<-try(do.call(mirt::mirt, mirtargs2))
      }
      return(estimatemodel)
    }, irt_target_d=irt_target_d, need_detailed_particip_var=need_detailed_particip_var, corresponding_item_type=corresponding_item_type,
    method=method) %>%
      magrittr::set_names(store_keys_df$store_key)
    for (storekey in store_keys_df$store_key) {
      needimp<-dplyr::filter(store_keys_df, store_key==!!storekey) %>% magrittr::use_series("imp") %>% as.integer()
      estimatemodel<-estimatemodels[[storekey]]
      poliparticipt<-mirt::fscores(estimatemodel,method="EAP",rotate = "varimax") %>%
        as.data.frame() %>%
        magrittr::set_colnames(c("myown_factoredparticip"))
      meanpp<-mean(poliparticipt$myown_factoredparticip)
      X[X$.imp==needimp,c("myown_factoredparticip")]<-poliparticipt$myown_factoredparticip
      X[X$.imp==needimp,c("myown_factoredparticip_scaled")]<-poliparticipt$myown_factoredparticip %>% #bind_cols(X,poliparticipt)
        {as.numeric(scale(.))}
      X[X$.imp==needimp,c("myown_factoredparticip_mean")]<-meanpp
    }
    #View(X[,c(need_detailed_particip_var,"myown_factoredparticip")])
    if (returnv=="model") {
      return(estimatemodels)
    } else if (returnv=="dataframe") {
      return(X)
    } else {
      return(list(X,estimatemodels))
    }
  },
  compute_factoredparticip = function(survey_data_imputed, imps=imputation_sample_i_s) {
    for (surveytitle in names(survey_data_imputed)) {
      survey_data_imputed[[surveytitle]]$myown_factoredparticip<-NULL
      survey_data_imputed[[surveytitle]]$myown_factoredparticip_scaled<-NULL
      survey_data_imputed[[surveytitle]]$myown_factoredparticip_mean<-NULL
    }
    survey_data_imputed_mirt_models_and_df <- lapply(survey_data_imputed, self$mirt_to_model_particip, need_particip_var_assigned=self$need_particip_var, survey_question_category_df=self$survey_question_category_df,
                                                     imps=imps, method=parallel_method)
    survey_data_imputed<-lapply(survey_data_imputed_mirt_models_and_df, magrittr::extract2, 1) %>%
      magrittr::set_names(names(survey_data_imputed_mirt_models_and_df))
    mirt_partcip_models<-lapply(survey_data_imputed_mirt_models_and_df, magrittr::extract2, 2) %>%
      magrittr::set_names(names(survey_data_imputed_mirt_models_and_df))
    list("survey_data_imputed"=survey_data_imputed, "mirt_partcip_models"=mirt_partcip_models)
  },
  inspect_mirt_particip_models = function(mirt_partcip_models) {
    #https://github.com/datacamp/tidymirt
    t<-lapply(mirt_partcip_models$`2010overall`,mirt:::summary)
    t<-lapply(mirt_partcip_models$`2016citizen`,mirt:::summary)
    t<-lapply(mirt_partcip_models$`2010overall`,tidymirt:::glance.SingleGroupClass)
    t<-lapply(mirt_partcip_models$`2016citizen`,tidymirt:::glance.SingleGroupClass)
    t
  },
  #output factor loadings
  output_factor_loadings = function(mirt_partcip_models) {
    for (surveytitle in names(mirt_partcip_models)) {
      for (mirtmodelkey in names(mirt_partcip_models[[surveytitle]])) {
        coefdf <- custom_mirt_coef_to_df(mirt_partcip_models[[surveytitle]][[mirtmodelkey]]) %>%
          dplyr::rename(item=rowvar) %>%
          dplyr::arrange(item) #,par_type,variable %>%  dplyr::filter(variable=="par")
        View(coefdf)
        write.csv(coefdf, "TMP.csv")
        mirt:::summary(mirt_partcip_models[[surveytitle]][[mirtmodelkey]], rotate = "oblimin") %>% print()
        if (readline(paste("now in",mirtmodelkey, "continue?"))=="N") break
      }
    }
  },
  #visualize and inspection
  visualize_factoredparticip = function(survey_data_imputed) {
    for (survey_title in names(survey_data_imputed)) {
      c("myown_factoredparticip",self$need_particip_var[[survey_title]]) %>%
        survey_data_imputed[[survey_title]][,.] %>%
        View()
      if (readline("continue?")=="N") break
      if (FALSE) {
        dplyr::select(survey_data_imputed$`2010overall`, !!self$need_particip_var[["2010overall"]], myown_factoredparticip) %>%
          View()
        survey_data_imputed$`2010overall` %>%
          #dplyr::filter(.imp==1) %>%
          custom_plot("myown_factoredparticip", "myown_wr")
      }
    }
  }
))

# other possible control vars ----------------

#trust
# 2016citizen@c15
# 2016citizen@c16a
# 2016citizen@c16b
# 2016citizen@c16c
# 2016citizen@d16e
# 2016citizen@d16f
# 2016citizen@d18a
# 2016citizen@d18b
# 2016citizen@d20
# 2016citizen@d21
# 2016citizen@d22
# 2016citizen@d23
# 2016citizen@f7

#unsatisfication
# 2016citizen@e1
# 2016citizen@e2a
# 2016citizen@e2b
# 2016citizen@e2c
# 2016citizen@e2d
# 2016citizen@e2e
# 2016citizen@e2f
# 2016citizen@e2g
# 2016citizen@e2h
# 2016citizen@e2i
# 2016citizen@f2
# 2016citizen@f6

#deprivation
# 2016citizen@c8r
# 2016citizen@c9r

#social relation
# 2016citizen@b1
# 2016citizen@b2

# political information
# 2016citizen@myown_online_time

#political knowledge
# 2016citizen@g6a
# 2016citizen@g6b
# 2016citizen@g6c
# 2016citizen@g6d

# old method and skip（僅保留參考用，不會執行）####################

if ({using_ltm_package <- FALSE;using_ltm_package}) {
  survey_data_with_particip <- lapply(survey_data_test,function(X,need_particip_var_assigned) {
    #for testing purpose
    X<-survey_data_test[[1]]
    need_particip_var_assigned<-need_particip_var

    need_particip_var_assigned %<>% extract2(X$SURVEY[1]) %>%
      intersect(names(X))
    fit1 <- ltm::grm(X[,need_particip_var_assigned], constrained = TRUE, start.val = "random")
    fit2 <- ltm::grm(X[,need_particip_var_assigned], na.action = na.omit, start.val = "random")
    fit_testresult<-anova(fit1, fit2)
    if ((fit_testresult$p.value<=0.05) & (fit_testresult$L0 < fit_testresult$L1) ) {
      fit<-fit2
    } else {
      fit<-fit1
    }
    margins(fit)
    summary(fit)
    coef(fit)
    #if (fit_testresult$aic0>fit_testresult$aic1 & fit_testresult$bic0>fit_testresult$bic1) {
    #  fit<-fit2
    #} else {
    #  fit<-fit1
    #}
    X %<>% left_join(
      fit %>%
        factor.scores() %>%
        use_series("score.dat") %>%
        dplyr::select(-contains("Exp"),-contains("Obs")) %>%
        rename(myown_factored_partcip=z1,myown_factored_partcip.se=se.z1)
    )
    X$myown_factored_partcip %<>% scale() %>% as.numeric()
    X
  },need_particip_var)

  information(fit, c(-4, 4))
  sapply(1:length(participation_var[[itrn]]),function (X) information(fit, c(-4, 4), items = c(X)) )
  #characteristic curve for each item
  plot(fit, lwd = 0.8, cex = 0.8, legend = TRUE, cx = "left", xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
  #information curve
  plot(fit, type = "IIC", lwd = 0.8, cex = 0.5, legend = TRUE, cx = "topleft",xlab = "Latent Trait", cex.main = 0.8, cex.lab = 1, cex.axis = 1)
  #test information curve
  plot(fit, type = "IIC", items = 0, lwd = 2, xlab = "Latent Trait",cex.main = 1, cex.lab = 1, cex.axis = 1)
  info1 <- information(fit, c(-4, 0))
  info2 <- information(fit, c(0, 4))
  text(-2.5, 8, labels = paste("Information in (-4, 0):",paste(round(100 * info1$PropRange, 1), "%", sep = ""),"\n\nInformation in (0, 4):",paste(round(100 * info2$PropRange, 1), "%", sep = "")), cex = 0.7)
  par(mfrow = c(1, 1)) #configure how many figures would show in row and column
  #characteristic curve overall in different category
  #plot(fit, category = 1, lwd = 0.8, cex = 0.8, legend = TRUE, cx = -0.8,cy = 0.85, xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8,cex.axis = 0.8)
  for (ctg in 1:4) {
    plot(fit, category = ctg, lwd = 0.8, cex = 0.8, annot = TRUE,
         xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8,
         cex.axis = 0.8)
    Sys.sleep(2)
  }
}
# 第五-3-2部份：non-parametric IRT Mokken scale analysis Model ####################
#################### mokken, Mokken Scale Analysis in R
#################### read: https://www.jstatsoft.org/article/view/v020i11/v20i11.pdf

if ({using_IRT_Mokken <- FALSE;using_IRT_Mokken}) {
  mokken::coefH(as.data.frame(X[,need_particip_var_assigned]))
  checkmokkenresult<-mokken::check.monotonicity(as.data.frame(X[,need_particip_var_assigned]))
  summary(checkmokkenresult)
  plot(checkmokkenresult)
  scale.checkmokkenresult <- mokken::aisp(as.data.frame(X[,need_particip_var_assigned]))
}
# 第五-3-3部份：parametric IRT Rasch models - Partial Credit Model ####################
# mirt::Rasch
# eRm::PCM
# 第五-3-4部份：parametric IRT Rasch models - Rating Scale Model ####################
# eRm::RSM
# mirt:mirt
# 'grsm' and 'grsmIRT' - graded ratings scale model in the slope-interceptand classical IRT parameterization.
# 'grsmIRT'is restricted to unidimensional models (Muraki, 1992)

if ({usinggrsm <- FALSE;usinggrsm}) {
  rst_mirt1 <- mirt::mirt(data = X[,need_particip_var_assigned], model = 1, verbose = T, itemtype= "grsmIRT")
  coef(rst_mirt1)
  for (itemplotn in 1:length(need_particip_var_assigned)) {
    mirt::itemplot(rst_mirt1, itemplotn)
    Sys.sleep(1)
  }
  summary(rst_mirt1)
  residuals(rst_mirt1)
  mirt::fscores(rst_mirt1,method = "EAP") %>% View()
}
# 第五-3-5部份：parametric IRT non-Rasch models - Generalized Partial Credit Model - Polytomous IRT ####################
#################### Finch, W. Holmes＆French, Brian F. (2015). Latent Variable Modeling with R. Florence: Taylor and Francis
## ltm::gpcm
## mirt::mirt by gpcmIRT
## 2016 not fit: gpcm, rasch 1PL all not fit;

if ({usinggpcm <- FALSE;usinggpcm}) {
  gpcmconstraint<-"gpcm" #c("gpcm", "1PL", "rasch")
  X.gpcm<-ltm::gpcm(X[,need_particip_var_assigned],constraint=gpcmconstraint,start.val="random")
  summary(X.gpcm)
  plot(survey_data.gpcm, lwd = 0.8, cex = 0.8, legend = TRUE, cx = "left", xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
  plot(survey_data.gpcm,type=c("IIC"), lwd = 0.8, cex = 0.8, legend = TRUE, cx = "left", xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
  ltm::GoF.gpcm(X.gpcm)
}
#margins(fit1)
