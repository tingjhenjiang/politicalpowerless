source(file = "10_preprocessing_get_similarities.R")
# 第十部份：以IRT測量受訪者政策理想點（ideal points）與到中位選民的相似度 --------------------------------
measure_idealpoints_class <- R6::R6Class("measure_idealpoints", inherit=similarities_class, public = list(
  survey_keys = NULL,
  survey_question_category_df_idealpoint = NULL,
  bills_answer_to_bill_bills_billcontent_rdata_filepath = NULL,
  survey_parallelfa_n_factors_file = NULL,
  random_polychor_survey_parallelfa_n_factors_file = NULL,
  survey_idealpoints_mirt_models_file = NULL,
  survey_idealpoints_mirt_models_backup_file = NULL,
  mirtfscores_similarity_scoresdf_file = NULL,
  mirtfscores_similarity_scoresdf_backupfile = NULL,
  policy_idealpoint_colname_header = NULL,
  survey_with_idealpoint_name = NULL,
  initialize = function(dataset_in_scriptsfile_directory="/mnt", filespath="/mnt", dataset_file_directory="/mnt", debug_func_mode=TRUE) {
    super$initialize(dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory, filespath=filespath, dataset_file_directory=dataset_file_directory, debug_func_mode=debug_func_mode)
    self$survey_keys <- c("2010overall","2016citizen")
    self$survey_question_category_df_idealpoint <- custom_ret_survey_book_file(dataset_file_directory=dataset_file_directory)
    self$bills_answer_to_bill_bills_billcontent_rdata_filepath <- file.path(dataset_in_scriptsfile_directory, "bills_answer_to_bill_bills_billcontent.RData")
    self$survey_parallelfa_n_factors_file <- file.path(dataset_in_scriptsfile_directory, "survey_parallelfa_n_factors.RData")
    self$random_polychor_survey_parallelfa_n_factors_file <- file.path(dataset_in_scriptsfile_directory, "random.polychor.survey_parallelfa_n_factors.RData")
    self$survey_idealpoints_mirt_models_file <- file.path(save_dataset_in_scriptsfile_directory, "survey_idealpoints_mirt_models.RData")
    #survey_idealpoints_mirt_models_file <- paste0(save_dataset_in_scriptsfile_directory, "survey_idealpoints_mirt_models_Davidian.RData")
    self$survey_idealpoints_mirt_models_backup_file <- file.path(save_dataset_in_scriptsfile_directory, "survey_idealpoints_mirt_models_backup.RData")
    self$mirtfscores_similarity_scoresdf_file <- file.path(save_dataset_in_scriptsfile_directory, "mirtfscores_similarity_scoresdf.RData")
    self$mirtfscores_similarity_scoresdf_backupfile <- file.path(save_dataset_in_scriptsfile_directory, "mirtfscores_similarity_scoresdf_backupfile.RData")
    self$policy_idealpoint_colname_header<-"policyidealpoint"
    self$survey_with_idealpoint_name<-self$miced_2surveys_stage_filepath(stage="mirt_lca_clustering_idealpoints")
  },
  # bills_answer_to_bill
  # bills_billcontent
  # bills_billcontent_with_relatedq
  # term_related_q
  # bills_billid_to_relatedq_pairs
  get_bills_env = function() {
    load_env <- new.env()
    load(file=self$bills_answer_to_bill_bills_billcontent_rdata_filepath, envir=load_env, verbose=TRUE)
    load_env
  },
  reduce_levels_from_ten_to_seven = function(X) {
    if (length(unique(X)) %in% c(10,11)) {
      newlist<- list(0,1,2,2,3,3,3,4,4,5,6) %>%
        magrittr::set_names(1:11)
      X<-dplyr::recode(X, !!!newlist)
    }
    return(X)
  },
  custom_eucli_similarity = function(x, y, method="euclidean", ...) {
    matrix(as.numeric(x),nrow=1) %>%
      rbind(as.numeric(y)) %>%
      dist(method=method) %>%
      as.numeric() %>%
      return()
  },
  scale_num = function(X) {
    scale(X) %>% as.numeric() %>% return()
  },
  # fa parallel探測因素數目 --------------------------------
  get_survey_parallelfa_arguments_df = function(imps=imputation_sample_i_s) {
    data.frame("survey"=self$survey_keys) %>%
      cbind(., imp = rep(imps, each = nrow(.))) %>%
      dplyr::left_join(data.frame(
        survey=c("2004citizen","2004citizen","2004citizen","2010env","2010env","2010env","2010overall","2010overall","2010overall","2016citizen"),
        term=c(5,6,"5&6",7,8,"7&8",7,8,"7&8",9)
      )) %>%
      cbind(., fm = rep(c("minres", "ml", "wls", "pa"), each = nrow(.))) %>%
      cbind(., needvars = rep(c("limited", "unlimited"), each = nrow(.))) %>%
      dplyr::mutate(store_key=paste0(survey, "_imp", imp, "_term", term, "_fm", fm, "_needvars", needvars)) %>%
      dplyr::arrange(needvars, survey, imp) %>%
      dplyr::filter(needvars=="unlimited", survey %in% c("2010overall","2016citizen"), term %in% c(7,9))
  },
  run_parallelfa_n_factors = function(survey_data_imputed, survey_parallelfa_arguments_df=self$get_survey_parallelfa_arguments_df(), term_related_q=self$get_bills_env()$term_related_q) {
    survey_question_category_df<-self$survey_question_category_df_idealpoint
    survey_parallelfa_n_factors_file<-self$survey_parallelfa_n_factors_file
    reduce_levels_from_ten_to_seven<-self$reduce_levels_from_ten_to_seven
    survey_parallelfa_n_factors <- survey_parallelfa_arguments_df$store_key %>%
      magrittr::set_names(custom_parallel_lapply(., function(fikey, ...) {
        fi<-which(survey_parallelfa_arguments_df$store_key==fikey)
        message(paste("now in", survey_parallelfa_arguments_df[fi,"store_key"],"and fi is",fi))
        argument_imp<-survey_parallelfa_arguments_df$imp[fi]
        parallel_fa_method_fm<-as.character(survey_parallelfa_arguments_df$fm[fi])
        surveyvars<-survey_question_category_df[[survey_parallelfa_arguments_df$survey[fi]]] %>%
          dplyr::filter(SURVEY==!!survey_parallelfa_arguments_df$survey[fi]) %>%
          magrittr::use_series("ID")
        ordinalsurveyvars<-dplyr::filter(survey_question_category_df[[survey_parallelfa_arguments_df$survey[fi]]], MEASUREMENT=="ordinal") %>%
          magrittr::use_series("ID") %>%
          c("myown_indp_atti")
        targetsurveydf<-as.character(survey_parallelfa_arguments_df$survey[fi]) %>%
          magrittr::extract2(survey_data_imputed, .) %>%
          {.[.$.imp==argument_imp, surveyvars]} %>%
          dplyr::mutate_all(unclass)
        notemptyvars<-sapply(targetsurveydf, is.na) %>% colSums() %>% .[.==0] %>% names()
        fewerthan8catgsvars<-sapply(targetsurveydf, function(X) {
          length(unique(X))
        }) %>% .[.<=8] %>% names()
        largerthan8catgsvars<-dplyr::setdiff(names(targetsurveydf), fewerthan8catgsvars)
        targetsurveydf <- dplyr::mutate_at(targetsurveydf, .vars=largerthan8catgsvars, .funs=~reduce_levels_from_ten_to_seven(.))
        finalneedvars<-notemptyvars
        #finalneedvars<-dplyr::intersect(notemptyvars, fewerthan8catgsvars)
        if (as.character(survey_parallelfa_arguments_df$needvars[fi])=="limited") {
          argdf_term <- as.character(survey_parallelfa_arguments_df$term[fi])
          if (grepl(pattern="&", x=argdf_term) ) {
            argdf_term <- unlist(stringr::str_split(argdf_term, pattern="&"))
          }
          extractedneedvars <- magrittr::extract(term_related_q, argdf_term) %>%
            unlist() %>%
            unique()
          finalneedvars <- extractedneedvars %>%
            gsub(pattern=paste0(as.character(survey_parallelfa_arguments_df$survey[fi]),"@"),"",.) %>%
            dplyr::intersect(finalneedvars) %>%
            unique()
        }
        targetsurveydf<-targetsurveydf[,finalneedvars]
        res_n_factors <- try({
          #https://rmc.ehe.osu.edu/files/2018/08/Parallel-AnalysisOct2017.pdf
          psych::fa.parallel(targetsurveydf, fm=parallel_fa_method_fm, main="Parallel Analysis Scree Plots", cor="poly")
          #random.polychor.pa::random.polychor.pa(nrep=50, data.matrix = targetsurveydf, q.eigen=.99)
        })
        res_n_factors <- hullEFA(X=dplyr::mutate_all(targetsurveydf, as.numeric),
                                 extr="ML", index_hull="CAF")
        res_n_factors <- try({ dplyr::mutate_all(targetsurveydf, as.numeric) %>%
            EFA.MRFA::parallelMRFA(corr="Polychoric")})
        if(is(res_n_factors, 'try-error')) {
          message(paste0("error at ",survey_parallelfa_arguments_df[fi,"store_key"]))
        }
        while (TRUE) {
          tryloadsaveresult<-try({
            load(file=survey_parallelfa_n_factors_file, verbose=TRUE)
            survey_parallelfa_n_factors[[as.character(survey_parallelfa_arguments_df$store_key[fi])]]<-res_n_factors
            save(survey_parallelfa_n_factors, file=survey_parallelfa_n_factors_file)
          })
          if(!is(tryloadsaveresult, 'try-error')) {
            break
          }
        }
        return(res_n_factors)
      }, survey_question_category_df=survey_question_category_df,
      survey_data_imputed=survey_data_imputed,
      survey_parallelfa_arguments_df=survey_parallelfa_arguments_df,
      term_related_q=term_related_q,
      survey_parallelfa_n_factors_file=survey_parallelfa_n_factors_file,
      reduce_levels_from_ten_to_seven=reduce_levels_from_ten_to_seven,
      method=parallel_method, mc.cores=1), .)
    save(survey_parallelfa_n_factors, file=survey_parallelfa_n_factors_file)
    ##save(random.polychor.survey_parallelfa_n_factors, file=self$random_polychor_survey_parallelfa_n_factors_file)
    survey_parallelfa_n_factors
  },
  get_survey_parallelfa_arguments_df_with_nfact = function(survey_parallelfa_arguments_df=self$get_survey_parallelfa_arguments_df()) {
    load_env <- new.env()
    load(file=self$survey_parallelfa_n_factors_file, envir=load_env, verbose=TRUE)
    survey_parallelfa_n_factors <- load_env$survey_parallelfa_n_factors
    lapply(names(survey_parallelfa_n_factors), function(store_key_of_survey_parallelfa_n_factors_args,...) {
      obj_parallel_analysis_result<-survey_parallelfa_n_factors[[store_key_of_survey_parallelfa_n_factors_args]]
      data.frame(store_key=store_key_of_survey_parallelfa_n_factors_args,
                 nfact=tryCatch(obj_parallel_analysis_result$nfact, error=function(msg) {return(NA)}),
                 ncomp=tryCatch(obj_parallel_analysis_result$ncomp, error=function(msg) {return(NA)})
      )
    },survey_parallelfa_n_factors=survey_parallelfa_n_factors) %>%
      dplyr::bind_rows() %>%
      dplyr::left_join(survey_parallelfa_arguments_df,.)
  },
  get_distincted_survey_parallelfa_arguments_df = function(survey_parallelfa_arguments_df=self$get_survey_parallelfa_arguments_df_with_nfact(), imps=imputation_sample_i_s) {
    distincted_survey_parallelfa_arguments_df<-survey_parallelfa_arguments_df %>%
      dplyr::distinct(survey, imp, term, needvars, nfact, ncomp) %>%
      dplyr::arrange(survey, term, imp, needvars, ncomp) %>%
      reshape2::melt(., id.vars=c("survey","imp","term","needvars")) %>%
      dplyr::rename(ncompnfact=value, reduct_type=variable) %>%
      dplyr::filter(!is.na(ncompnfact)) %>%
      dplyr::distinct(survey, term, imp, reduct_type, ncompnfact)
      #dplyr::filter(needvars=="limited")  %>%
      #dplyr::filter(!(term %in% c(5,6,7,8))) %>%
    distincted_survey_parallelfa_arguments_df %<>% dplyr::bind_rows(
      data.frame("survey"="2016citizen", "term"="9") %>%
        cbind(., imp = rep(imps, each = nrow(.))) %>%
        cbind(., ncompnfact = rep(c(12,21:23), each = nrow(.))) %>%
        cbind(., reduct_type = rep(c("nfact"), each = nrow(.)))
    ) %>%
      dplyr::bind_rows(
        data.frame("survey"="2010overall", "term"="7") %>%
          cbind(., imp = rep(imps, each = nrow(.))) %>%
          cbind(., ncompnfact = rep(5:8, each = nrow(.))) %>%
          cbind(., reduct_type = rep(c("nfact"), each = nrow(.)))
      ) %>%
      dplyr::distinct_all()
    distincted_survey_parallelfa_arguments_df
  },
  # explore data distributions on issues --------------------------------
  explore_issue_distributions = function(survey_data_imputed) {
    for (surveytitle in self$survey_keys) {
      needqsdf <- magrittr::extract2(self$survey_question_category_df_idealpoint,surveytitle)
      needqs <- needqsdf$ID
      needdf <- magrittr::extract2(survey_data_imputed,surveytitle)
      for (needq in needqs) {
        question_detail<-dplyr::filter(needqsdf,ID==!!needq) %>% magrittr::use_series("QUESTION")
        custom_plot(needdf, fvar=needq, weightvar="myown_wr", usingsurveypkg=FALSE) %>% print()
        if (readline(paste("now in",surveytitle,needq,question_detail,"continue?"))=="N") break
      }
    }
  },
  # exploratory IRT 探測問卷因素結構 --------------------------------
  get_distincted_runonly = function(distincted_survey_parallelfa_arguments_df=self$get_distincted_survey_parallelfa_arguments_df(), avoid_run_duplicated_models=FALSE) {
    distincted_survey_parallelfa_arguments_df_runonly<- distincted_survey_parallelfa_arguments_df %>%
      dplyr::distinct(survey, imp, term, ncompnfact) %>%
      dplyr::mutate(runmirt_store_key=paste0(survey,"_imp",imp,"_ncompnfact",ncompnfact)) %>%
      dplyr::filter(ncompnfact %in% c(6,12)) %>%
      dplyr::arrange(survey, imp, ncompnfact)
    if (avoid_run_duplicated_models==TRUE) {
      load_env <- new.env()
      load(file=self$survey_idealpoints_mirt_models_file, envir=load_env, verbose=TRUE)
      processed_idealpoint_mirt_keys<-sapply(load_env$survey_idealpoints_mirt_models, class) %>%
        .[.=="SingleGroupClass"] %>%
        names()
      distincted_survey_parallelfa_arguments_df_runonly<-dplyr::filter(distincted_survey_parallelfa_arguments_df_runonly, !(runmirt_store_key %in% !!processed_idealpoint_mirt_keys) )
    }
    distincted_survey_parallelfa_arguments_df_runonly
  },
  run_idealpoint_mirt_models = function(survey_data_imputed, distincted_survey_parallelfa_arguments_df_runonly=self$get_distincted_runonly(), term_related_q=self$get_bills_env()$term_related_q, runkeys_range=NULL) {
    survey_question_category_df<-self$survey_question_category_df_idealpoint
    survey_idealpoints_mirt_models_file<-self$survey_idealpoints_mirt_models_file
    load_env <- new.env()
    load(file=survey_idealpoints_mirt_models_file, envir=load_env, verbose=TRUE)
    survey_idealpoints_mirt_models <- load_env$survey_idealpoints_mirt_models
    runkeys<-distincted_survey_parallelfa_arguments_df_runonly$runmirt_store_key
    if (!is.null(runkeys_range)) {
      runkeys<-runkeys[runkeys_range] #[1:24] #[45:25]
    }
    survey_idealpoints_mirt_models<-runkeys %>%
      magrittr::set_names(custom_parallel_lapply(., function(fikey, ...) {
        message(paste("now in",fikey))
        needrow<-dplyr::filter(distincted_survey_parallelfa_arguments_df_runonly, runmirt_store_key==!!fikey)
        argdf_term <- as.character(needrow$term)
        if (grepl(pattern="&", x=argdf_term) ) {
          argdf_term <- unlist(stringr::str_split(argdf_term, pattern="&"))
        }
        if (FALSE) {
          extractedneedvars <- magrittr::extract(term_related_q, argdf_term) %>%
            unlist() %>%
            unique() %>%
            sort()
          extractedneedvars_without_survey <- gsub(pattern=paste0(as.character(needrow$survey),"@"), replacement="", x=extractedneedvars)
        } else {
          extractedneedvars <- magrittr::extract2(survey_question_category_df, needrow$survey) %>%
            magrittr::use_series("SURVEYCOMPLETEID")
          extractedneedvars_without_survey <- magrittr::extract2(survey_question_category_df, needrow$survey) %>%
            magrittr::use_series("ID")
        }
        to_explor_IRT_itemtypes<-data.frame(SURVEYCOMPLETEID=extractedneedvars, ID=extractedneedvars_without_survey, SURVEY=needrow$survey) %>%
          dplyr::left_join(survey_question_category_df[[needrow$survey]]) %>%
          mutate_cond(grepl(pattern="myown_indp_atti", x=SURVEYCOMPLETEID), itemtype="graded")
        needsurveydatadf <- survey_data_imputed[[as.character(needrow$survey)]] %>%
          {.[.$.imp==needrow$imp,]}
        to_explor_IRT_data<-needsurveydatadf[,to_explor_IRT_itemtypes$ID] %>%
          dplyr::mutate_all(unclass)
        mirtmethod<-if (as.integer(needrow$ncomp)>=3) "QMCEM" else "EM"
        load(file=survey_idealpoints_mirt_models_file, verbose=TRUE)
        parsv <- try({
          mirt::mod2values(survey_idealpoints_mirt_models[[fikey]])
        })
        if(is(parsv, 'try-error')) { # | fikey %in% c("2016citizen_imp22_ncompnfact12","2016citizen_imp23_ncompnfact12","2016citizen_imp24_ncompnfact12")
          parsv<-NULL
        }
        library(mirt)
        mirtCluster(parallel::detectCores())
        explor_mirt_model<-mirt(
          to_explor_IRT_data,
          model=as.integer(needrow$ncomp),
          itemtype=to_explor_IRT_itemtypes$itemtype,
          technical=list(NCYCLES=3000,MAXQUAD=40000),
          survey.weights = needsurveydatadf[,c("myown_wr")],
          dentype = "Gaussian", #"", empiricalhist
          method=mirtmethod,
          #SE=TRUE,
          #SE.type="sandwich",
          pars=parsv
        )
        tryn<-1
        while (TRUE) {
          tryloadsaveresult<-try({
            load(file=survey_idealpoints_mirt_models_file, verbose=TRUE)
            survey_idealpoints_mirt_models[[fikey]]<-explor_mirt_model
            save(survey_idealpoints_mirt_models, file=survey_idealpoints_mirt_models_file)
          })
          tryn<-tryn+1
          if(!is(tryloadsaveresult, 'try-error') | tryn>11) {
            break
          }
        }
        return(explor_mirt_model)
      }, survey_question_category_df=survey_question_category_df,
      survey_data_imputed=survey_data_imputed,
      term_related_q=term_related_q,
      distincted_survey_parallelfa_arguments_df_runonly=distincted_survey_parallelfa_arguments_df_runonly,
      survey_idealpoints_mirt_models_file=survey_idealpoints_mirt_models_file,
      method=parallel_method, mc.cores=1 #1
      ), .)
    save(survey_idealpoints_mirt_models, file=self$survey_idealpoints_mirt_models_backup_file)
    survey_idealpoints_mirt_models
  },
  # merge models -----------
  merge_idealpoint_mirt_models = function(extra_model_files=c()) {
    load_env <- new.env()
    load(file=self$survey_idealpoints_mirt_models_file, envir=load_env, verbose=TRUE)
    t<-load_env$survey_idealpoints_mirt_models
    load(file=self$survey_idealpoints_mirt_models_backup_file, envir=load_env, verbose=TRUE)
    t2<-load_env$survey_idealpoints_mirt_models
    modellists<-list(t,t2)
    for (extra_model_file in extra_model_files) {
      #例如paste0(save_dataset_in_scriptsfile_directory,"survey_idealpoints_mirt_models (assume_normal_6and12only).RData")
      load(file=extra_model_file, envir=load_env, verbose=TRUE)
      modellists[[length(modellists)+1]]<-load_env$survey_idealpoints_mirt_models
    }
    survey_idealpoints_mirt_models<-do.call(rlist::list.merge, modellists)
    save(survey_idealpoints_mirt_models, file=self$survey_idealpoints_mirt_models_file)
    survey_idealpoints_mirt_models
  },
  get_survey_idealpoints_mirt_models = function() {
    load_env <- new.env()
    load(file=self$survey_idealpoints_mirt_models_file, envir=load_env, verbose=TRUE)
    load_env$survey_idealpoints_mirt_models
  },
  # Checking factor scores and factor structure and goodness of fit --------------------------------
  get_complete_inf_mirt_models = function(survey_idealpoints_mirt_models=self$get_survey_idealpoints_mirt_models(), distincted_survey_parallelfa_arguments_df_runonly=self$get_distincted_runonly()) {
    lapply(survey_idealpoints_mirt_models,tidymirt:::glance.SingleGroupClass) %>%
      dplyr::bind_rows() %>%
      data.frame(runmirt_store_key=names(survey_idealpoints_mirt_models)) %>%
      dplyr::left_join(distincted_survey_parallelfa_arguments_df_runonly, .) %>%
      dplyr::arrange(survey, SABIC, BIC, AIC, imp)
    #write.csv(complete_inf_mirt_models, "TMP.csv")
    #mirtmodelinfindicators<-c("AIC","AICc","SABIC","HQ","BIC")
  },
  output_idealpoint_mirt_coefs = function(survey_idealpoints_mirt_models=self$get_survey_idealpoints_mirt_models(), distincted_survey_parallelfa_arguments_df_runonly=self$get_distincted_runonly(), outputcsv="TMP.csv") {
    needimps<-custom_ret_appro_kamila_clustering_parameters()
    need_survey_idealpoints_mirt_models_keys<-dplyr::semi_join(distincted_survey_parallelfa_arguments_df_runonly,needimps) %>%
      magrittr::use_series("runmirt_store_key")
    mirtsurveyresults<-list()
    for (mirt_model_on_survey_key in need_survey_idealpoints_mirt_models_keys) {
      mirtsurveyresult<-custom_mirt_coef_to_df(survey_idealpoints_mirt_models[[mirt_model_on_survey_key]], printSE = TRUE) %>%
        data.frame(key=mirt_model_on_survey_key, .)
      mirtsurveyresults[[mirt_model_on_survey_key]]<-mirtsurveyresult
      if (FALSE) {
        mirt:::summary(survey_idealpoints_mirt_models[[mirt_model_on_survey_key]], rotate="none") %>% print()
        readline(paste("now in",mirt_model_on_survey_key,"unrotated, continue?"))
        mirt:::summary(survey_idealpoints_mirt_models[[mirt_model_on_survey_key]], rotate="varimax") %>% print()
        #mirt::itemfit(survey_idealpoints_mirt_models[[mirt_model_on_survey_key]], QMC=TRUE) %>% print()
        readline(paste("now in",mirt_model_on_survey_key,"rotated, continue?"))
      }
    }
    plyr::rbind.fill(mirtsurveyresults) %>%
      write.csv(file=outputcsv)
  },
  # median calculation: similarity to median --------------------------------
  compute_mirtfscores_similarity = function(survey_data_imputed, survey_idealpoints_mirt_models=self$get_survey_idealpoints_mirt_models(), distincted_survey_parallelfa_arguments_df_runonly=self$get_distincted_runonly(), thismc.cores=24, save=TRUE) {
    policy_idealpoint_colname_header<-self$policy_idealpoint_colname_header
    mirtfscores_similarity_scoresdf_file<-self$mirtfscores_similarity_scoresdf_file
    custom_eucli_similarity<-self$custom_eucli_similarity
    scale_num<-self$scale_num
    loopmirtmodellist_keys<-dplyr::filter(distincted_survey_parallelfa_arguments_df_runonly, runmirt_store_key %in% !!names(survey_idealpoints_mirt_models)) %>%
      #dplyr::filter(complete_inf_mirt_models, ncompnfact %in% c(6,12)) %>%
      dplyr::arrange(survey,imp) %>%
      magrittr::use_series("runmirt_store_key")# %>% base::setdiff(names(mirtfscores_similarity_scoresdf))
    mirtfscoresdfs<-loopmirtmodellist_keys %>%
      magrittr::set_names( custom_parallel_lapply(., function(mirt_model_on_survey_key, ...) {
        mirt::fscores(survey_idealpoints_mirt_models[[mirt_model_on_survey_key]], QMC=TRUE, rotate="varimax") %>%
          {magrittr::set_colnames(., paste0(policy_idealpoint_colname_header, colnames(.)))} %>%
          data.frame(.) %>%
          return()
      }, survey_idealpoints_mirt_models=survey_idealpoints_mirt_models,
      mc.cores=thismc.cores,
      method=parallel_method), .)
    surveydataids_list<-loopmirtmodellist_keys %>%
      magrittr::set_names( custom_parallel_lapply(., function(mirt_model_on_survey_key, ...) {
        needrow<-dplyr::filter(distincted_survey_parallelfa_arguments_df_runonly, runmirt_store_key==!!mirt_model_on_survey_key) %>%
          dplyr::mutate_at(c("survey","runmirt_store_key"), as.character)
        dplyr::filter(survey_data_imputed[[needrow$survey]], .imp==!!needrow$imp) %>%
          dplyr::select(SURVEY, id, .imp, myown_wr) %>%
          return()
      }, survey_data_imputed=survey_data_imputed, distincted_survey_parallelfa_arguments_df_runonly=distincted_survey_parallelfa_arguments_df_runonly,
      mc.cores=thismc.cores,
      method=parallel_method), .)
    mirtfscores_median_points<-loopmirtmodellist_keys %>%
      magrittr::set_names( custom_parallel_lapply(., function(mirt_model_on_survey_key, ...) {
        message(paste("now in",mirt_model_on_survey_key))
        needrow<-dplyr::filter(distincted_survey_parallelfa_arguments_df_runonly, runmirt_store_key==!!mirt_model_on_survey_key) #complete_inf_mirt_models
        needsurvey<-as.character(needrow$survey)
        needweight<-survey_data_imputed[[needsurvey]] %>%
          dplyr::filter(.imp==!!needrow$imp) %>%
          magrittr::use_series("myown_wr")
        surveydataids<-surveydataids_list[[mirt_model_on_survey_key]]
        mirtfscoresdf<-mirtfscoresdfs[[mirt_model_on_survey_key]]
        median_policy_idealpoint<- data.frame(surveydataids, mirtfscoresdf) %>%
          #inflate_df_from_weight() %>%
          dplyr::select(dplyr::starts_with(!!policy_idealpoint_colname_header)) %>%
          robustX::L1median(weights=needweight,method="VardiZhang")
          # {
          #   point1<-SpatialNP::spatial.location(., shape=TRUE, score="sign")
          #   point2<-SpatialNP::spatial.location(., shape=TRUE, score="signrank")
          #   list(sign=point1,signrank=point2)
          # }
        return(median_policy_idealpoint)
      }, distincted_survey_parallelfa_arguments_df_runonly=distincted_survey_parallelfa_arguments_df_runonly,
      survey_idealpoints_mirt_models=survey_idealpoints_mirt_models,
      survey_data_imputed=survey_data_imputed,
      mirtfscores_similarity_scoresdf_file=mirtfscores_similarity_scoresdf_file,
      mirtfscoresdfs=mirtfscoresdfs,
      surveydataids_list=surveydataids_list,
      mc.cores=thismc.cores,
      method=parallel_method), .)

    mirtfscores_similarity_scoresdf<-loopmirtmodellist_keys %>%
      magrittr::set_names(custom_parallel_lapply(., function(mirt_model_on_survey_key, ...) {
        mirtfscoresdf<-mirtfscoresdfs[[mirt_model_on_survey_key]]
        medianpoint<-mirtfscores_median_points[[mirt_model_on_survey_key]]
        cos_similarity_to_median_policy_idealpoint<-apply(mirtfscoresdf, 1, FUN=lsa::cosine, y=medianpoint)
        euclid_distance_to_median_policy_idealpoint<-apply(mirtfscoresdf, 1, FUN=custom_eucli_similarity, y=medianpoint)
        more_similar_cos<-quantile(cos_similarity_to_median_policy_idealpoint)[4] %>%
          magrittr::is_weakly_greater_than(cos_similarity_to_median_policy_idealpoint,.) %>%
          as.numeric()
        more_similar_euclid<-quantile(euclid_distance_to_median_policy_idealpoint)[2] %>%
          magrittr::is_weakly_less_than(euclid_distance_to_median_policy_idealpoint, .) %>%
          as.numeric()
        surveydataids<-surveydataids_list[[mirt_model_on_survey_key]]
        sim_col_names<-paste0(
          policy_idealpoint_colname_header,
          "_",
          c("cos_similarity_to_median_policy_idealpoint","euclid_distance_to_median_policy_idealpoint","more_similar_cos","more_similar_euclid")
        )
        mirtfscoresdf<-data.frame(
          cos_similarity_to_median_policy_idealpoint,
          euclid_distance_to_median_policy_idealpoint,
          more_similar_cos,
          more_similar_euclid
        ) %>%
          magrittr::set_colnames(
            sim_col_names
          ) %>%
          dplyr::mutate(cos_similarity_to_median_policy_idealpoint_scaled=scale_num(cos_similarity_to_median_policy_idealpoint)) %>%
          dplyr::mutate(euclid_distance_to_median_policy_idealpoint_scaled=scale_num(euclid_distance_to_median_policy_idealpoint)) %>%
          dplyr::bind_cols(mirtfscoresdf, .) %>%
          dplyr::bind_cols(surveydataids, .)
        return(mirtfscoresdf)
      }, distincted_survey_parallelfa_arguments_df_runonly=distincted_survey_parallelfa_arguments_df_runonly,
      survey_idealpoints_mirt_models=survey_idealpoints_mirt_models,
      survey_data_imputed=survey_data_imputed,
      mirtfscores_similarity_scoresdf_file=mirtfscores_similarity_scoresdf_file,
      mirtfscoresdfs=mirtfscoresdfs,
      mirtfscores_median_points=mirtfscores_median_points,
      surveydataids_list=surveydataids_list,
      scale_num=scale_num,
      mc.cores=thismc.cores,
      method=parallel_method), .)
    if (save==TRUE) {
      save(mirtfscores_similarity_scoresdf, mirtfscores_median_points, file=self$mirtfscores_similarity_scoresdf_backupfile)
    }
    list("mirtfscores_similarity_scoresdf"=mirtfscores_similarity_scoresdf, "mirtfscores_median_points"=mirtfscores_median_points)
  },
  # merge fscore data --------------------------------
  merge_fscore_data = function(mirtfscores_similarity_scoresdf=NULL, save=FALSE) {
    if (is.null(mirtfscores_similarity_scoresdf)) {
      load_env <- new.env()
      load(file=self$mirtfscores_similarity_scoresdf_file, envir=load_env, verbose=TRUE)
      load(file=self$mirtfscores_similarity_scoresdf_backupfile, envir=load_env, verbose=TRUE)
      mirtfscores_similarity_scoresdf <- load_env$mirtfscores_similarity_scoresdf
    }
    survey_data_imputed <- self$get_survey_data_imputed_stage(stage="mirt_lca_clustering")
    for (survey in names(survey_data_imputed)) {
      needsvykeys<-grep(pattern=survey, x=names(mirtfscores_similarity_scoresdf), value=TRUE)
      survey_idp_similarity<-magrittr::extract(mirtfscores_similarity_scoresdf, needsvykeys) %>%
        dplyr::bind_rows()
      survey_data_imputed[[survey]] %<>% dplyr::left_join(survey_idp_similarity)
    }
    survey_data_imputed <- lapply(survey_data_imputed, function(X) {
      dplyr::mutate_at(X, dplyr::vars(dplyr::contains("_more_similar")), as.factor)
    })
    filterstddf<-custom_ret_appro_kamila_clustering_parameters() %>%
      dplyr::rename(SURVEY=survey)
    survey_data_imputed <- lapply(survey_data_imputed, dplyr::semi_join, filterstddf) %>%
      lapply(dplyr::left_join, filterstddf) %>%
      lapply(function(X) {dplyr::select(X,-.imp,-imp) %>% dplyr::rename(.imp=newimp)})
    if (save==TRUE) {
      save(survey_data_imputed, file=self$survey_with_idealpoint_name)
    }
    survey_data_imputed
  },
  # extract previous result to merge into reset dataset --------------------------------
  extract_previous_idealpoint_to_merge = function(save=FALSE) {
    policy_idealpoint_colname_header<-self$policy_idealpoint_colname_header
    clustered_svydata<-self$get_survey_data_imputed_stage(stage="mirt_lca_clustering")
    survey_data_imputed<-self$get_survey_data_imputed_stage(stage="mirt_lca_clustering_idealpoints")
    t<-lapply(survey_data_imputed, function(X, policy_idealpoint_colname_header) {
      dplyr::select(X, SURVEY, id, .id, .imp, tidyselect::contains(!!policy_idealpoint_colname_header))
    }, policy_idealpoint_colname_header=policy_idealpoint_colname_header) %>%
      lapply(function(X,Y) {
        needsyv<-as.character(X$SURVEY[1])
        dplyr::left_join(Y[[needsyv]],X)
      },Y=clustered_svydata)
    survey_data_imputed<-t
    if (save==TRUE) {
      save(survey_data_imputed, file=self$survey_with_idealpoint_name)
    }
    survey_data_imputed
  }
))

# 以下為診斷、檢定與試探性分析區塊，僅保留參考用（不會執行） ==========================================

# checking which model is better among different dimensions --------------------------------
if (FALSE) {
  mirtmodelinfindicators<-c("AIC","AICc","SABIC","HQ","BIC")
  for (survey_title in as.character(unique(complete_inf_mirt_models$survey))) {
    for (needimp in 1:5) {
      mirtcomparei_df<-dplyr::filter(complete_inf_mirt_models, survey==!!survey_title, imp==!!needimp)
      for (baseline_key in 1:nrow(mirtcomparei_df)) {
        baselinerow<-mirtcomparei_df[baseline_key, ]
        mirtcomparei_a<-baselinerow$runmirt_store_key
        othercomparemodel_keys<-dplyr::filter(mirtcomparei_df, runmirt_store_key!=!!mirtcomparei_a) %>%
          magrittr::use_series("runmirt_store_key")
        for (mirtcomparei_b in othercomparemodel_keys) {
          modela<-survey_idealpoints_mirt_models[[mirtcomparei_a]]
          modela_bic<-mirt::extract.mirt(modela, "BIC")
          modelb<-survey_idealpoints_mirt_models[[mirtcomparei_b]]
          bettermodel_text<-data.frame(modela_ind=sapply(mirtmodelinfindicators, function (X,m) {mirt::extract.mirt(m,X)}, m=modela),
                                       modelb_ind=sapply(mirtmodelinfindicators, function (X,m) {mirt::extract.mirt(m,X)}, m=modelb)) %>%
            dplyr::mutate(smaller=magrittr::is_less_than(modela_ind,modelb_ind)) %>%
            magrittr::use_series("smaller") %>%
            sum() %>%
            {if (.>=3) mirtcomparei_a else mirtcomparei_b}
          mirt::anova(modela,modelb) %>%
            print()
          message(paste("better model is",bettermodel_text))
          if (readline(paste("now in",mirtcomparei_b,"and basis is",mirtcomparei_a,"(whose BIC is",modela_bic,") continue?"))=="N") break
        } #end othercomparemodel_keys
      } #end baseline key
    } #end needimp
  } #end survey title
}

# Testing Multivariate Normality of policy preference using R --------------------------------
#MVN package
#mvnormtest::mshapiro.test()
if (FALSE) {
  multivariate_test_args <- data.frame(survey_key=survey_keys) %>%
    cbind(., imp = rep(imputation_sample_i_s, each = nrow(.))) %>%
    cbind(., testm = rep(c("mardia", "hz", "royston", "dh","energy"), each = nrow(.))) %>%
    dplyr::mutate_at(c("survey_key","testm"), as.character) %>%
    dplyr::arrange(survey_key, imp, testm)
  multivariate_test_res <- custom_parallel_lapply(1:nrow(multivariate_test_args), function(rowi, ...) {
    needrow<-multivariate_test_args[rowi,]
    t<-dplyr::filter(survey_data_imputed[[needrow$survey_key]], .imp==!!needrow$imp) %>%
      dplyr::select(dplyr::starts_with("policy"))
    res<-MVN::mvn(t,mvnTest=as.character(needrow$testm))
    dplyr::bind_cols(needrow, res$multivariateNormality) %>%
      return()
  }, multivariate_test_args=multivariate_test_args, survey_data_imputed=survey_data_imputed,
  method=parallel_method) %>%
    plyr::rbind.fill()
  write.csv(multivariate_test_res, "TMP.csv")

  for (n in names(t)) {
    custom_plot(t,n) %>% print()
  }
  custom_plot(t,"policyidealpointF1")
  # ANOVA, mixed ANOVA, ANCOVA and MANOVA, Kruskal-Wallis test and Friedman test
  # Nonparametric Inference for Multivariate Data:
  #   The R Package npmv
  #https://cran.r-project.org/web/packages/MultNonParam/MultNonParam.pdf
  # Testing Mean Differences among Groups: Multivariate and Repeated Measures Analysis with Minimal Assumptions
  #MNM https://cran.r-project.org/web/packages/MNM/index.html
  #SpatialNP https://cran.r-project.org/web/packages/SpatialNP/index.html
}

# Testing Normality of political participation and similarity to median using R --------------------------------
if (FALSE) {
  needimps<-custom_ret_appro_kamila_clustering_parameters()
  survey_with_idealpoint_name<-paste0(save_dataset_in_scriptsfile_directory, "miced_survey_2surveysonly_mirt_lca_clustering_idealpoints.RData")
  load(file=survey_with_idealpoint_name, verbose=TRUE)
  merged_acrossed_surveys_list<-ret_merged_for_idealpoint_and_pp_df_list(survey_data_imputed, dataset_in_scriptsfile_directory, minuspolicy=FALSE)
  normality_test_args <- data.frame(imp = 1:length(merged_acrossed_surveys_list)) %>%
    cbind(., testvar = rep(c("myown_factoredparticip","policyidealpoint_eucli_distance_to_median","policyidealpoint_cos_similarity_to_median"), each = nrow(.))) %>%
    cbind(., testm = rep(c("Shapiro-Wilk", "Anderson-Darling", "Cramer-vonMises", "Lilliefors","PearsonChi-Squared","Shapiro-Francia"), each = nrow(.))) %>%
    dplyr::mutate(storekey=paste0("imp",imp,"_",testvar,"_",testm)) %>%
    dplyr::mutate_at(c("testm","testvar","storekey"), as.character) %>%
    dplyr::mutate_at(c("imp"), as.integer) %>%
    dplyr::arrange(imp, testvar, testm)

  testnormalitycheck<-custom_apply_thr_argdf(normality_test_args, "storekey", function(fikey, loopargdf, datadf, ...) {
    needrow<-dplyr::filter(loopargdf, storekey==!!fikey)
    needv<-needrow$imp %>%
      magrittr::extract2(datadf, .) %>%
      .[, needrow$testvar]
    switch(needrow$testm,
           "Shapiro-Wilk"=shapiro.test(needv),
           "Anderson-Darling"=nortest::ad.test(needv),
           "Cramer-vonMises"=nortest::cvm.test(needv),
           "Lilliefors"=nortest::lillie.test(needv),
           "PearsonChi-Squared"=nortest::pearson.test(needv),
           "Shapiro-Francia"=nortest::sf.test(needv)
    ) %>%
      magrittr::use_series("p.value") %>%
      data.frame("pvalue"=.) %>%
      dplyr::bind_cols(needrow, .) %>%
      return()
  }, datadf=merged_acrossed_surveys_list) %>%
    plyr::rbind.fill()

  write.csv(testnormalitycheck,"TMP.csv")
}

# CFA IRT 驗證性因素分析 問卷因素結構 --------------------------------
#mirt example https://philchalmers.github.io/mirt/html/mirt.html
#https://www.yongxi-stat.com/explore-factor-analysis-introduction/

#2010overall
if (FALSE) {
  needimp<-1
  surveydata<-dplyr::filter(survey_data_imputed$`2010overall`, .imp==!!needimp)
  testvars<-c("v39d","v39e","v40")
  testvars<-c("v27b")
  testvars<-c("v78a","v78b","v78c","v78d","v78e","v78f","v78g","v78h","v78i") #baseline 0.315
  summaryofmirt_results<-list()
  for (i in 1:length(testvars)) {
    needtestvars <- base::setdiff(testvars, testvars[i])
    if (length(needtestvars)<3) next
    testmodel<-mirt::mirt(
      data=dplyr::mutate_all(surveydata[,needtestvars, drop=FALSE], .funs=unclass) ,
      model=1,
      itemtype = "graded",
      technical = list("NCYCLES"=40000),
      survey.weights = surveydata[,c("myown_wr")],
      SE=TRUE
    )
    summaryofmirt_results[[i]]<-capture.output(mirt:::summary(testmodel)) %>%
      grep(pattern="Proportion|SS loadings", x=., value=TRUE) %>%
      {data.frame(drop=testvars[i],ss=.[1], prop=.[2])}
  }
  summaryofmirt_result<-dplyr::bind_rows(summaryofmirt_results)
}

# v40
# myown_indp_atti
# v80
# v68g
# v27b
#
# #2016citizen
# c1a+c1b+c1c+c1d+c1e
# c2+c3+c4+c5+c6
# ? c10+c11+c12+c13+c14
# d1+d2a+d2b+d3a+d3b+d4
# d5a+d5b+d5c+d5d+d5e+d5f
# d7a+d7b+d7c+d7d+d7e+d7f+d7g+d7h+d7i+d7j+d7k+d8a+d8b+d8c  +f9
# d11a+d11b+d12+d13a+d13b+d14a+d14b+d14c
# d17a+d17b+d17c
# f3+f4+f5
# myown_indp_atti
