# 第Ｏ部份：環境設定 --------------------------------
if (!("benchmarkme" %in% rownames(installed.packages()))) try(install.packages("benchmarkme"))
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-try(paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model))
t_sessioninfo_running_with_cpu_locale<-try(gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2])))
source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/shared_functions.R"), encoding="UTF-8")
#load(file=paste0(dataset_in_scriptsfile_directory, "miced_survey_9_with_mirt_lca_clustering.RData"), verbose=TRUE)
load(file=paste0(save_dataset_in_scriptsfile_directory,"miced_survey_2surveysonly_mirt_lca.RData"), verbose=TRUE)
load(file=paste0(save_dataset_in_scriptsfile_directory,"miced_survey_2surveysonly_mirt_lca_clustering.RData"), verbose=TRUE)
load(file=paste0(dataset_in_scriptsfile_directory, "bills_answer_to_bill_bills_billcontent.RData"), verbose=TRUE)
survey_keys <- c("2010overall","2016citizen")
survey_question_category_df<-custom_ret_survey_book_file(dataset_file_directory=dataset_file_directory)

reduce_levels_from_ten_to_seven <- function(X) {
  if (length(unique(X)) %in% c(10,11)) {
    newlist<- list(0,1,2,2,3,3,3,4,4,5,6) %>%
      magrittr::set_names(1:11)
    X<-dplyr::recode(X, !!!newlist)
    #X<-dplyr::recode(X, `0`=,`1`=,`2`=,`3`=,``=,``=,``=,``=,``=,``=,``=,``=,``=,)
  }
  return(X)
}

# fa parallel探測因素數目 --------------------------------
survey_parallelfa_arguments_df<-data.frame("survey"=survey_keys) %>%
  cbind(., imp = rep(imputation_sample_i_s, each = nrow(.))) %>%
  dplyr::left_join(data.frame(
    survey=c("2004citizen","2004citizen","2004citizen","2010env","2010env","2010env","2010overall","2010overall","2010overall","2016citizen"),
    term=c(5,6,"5&6",7,8,"7&8",7,8,"7&8",9)
    )) %>%
  cbind(., fm = rep(c("minres", "ml", "wls", "pa"), each = nrow(.))) %>%
  cbind(., needvars = rep(c("limited", "unlimited"), each = nrow(.))) %>%
  dplyr::mutate(store_key=paste0(survey, "_imp", imp, "_term", term, "_fm", fm, "_needvars", needvars)) %>%
  dplyr::arrange(needvars, survey, imp) %>%
  dplyr::filter(needvars=="unlimited", survey %in% c("2010overall","2016citizen"), term %in% c(7,9))
survey_parallelfa_n_factors_file <- paste0(dataset_in_scriptsfile_directory, "survey_parallelfa_n_factors.RData")
random.polychor.survey_parallelfa_n_factors_file <- paste0(dataset_in_scriptsfile_directory, "random.polychor.survey_parallelfa_n_factors.RData") 
if (FALSE) {
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
      #reduce_levels_from_ten_to_seven(targetsurveydf$d12)
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
        }) #, envir = .GlobalEnv
        if(!is(tryloadsaveresult, 'try-error')) {
          break
        }
      }
      return(res_n_factors)
    }, survey_question_category_df=survey_question_category_df,
    survey_data_imputed=survey_data_imputed,
    survey_parallelfa_arguments_df=survey_parallelfa_arguments_df,
    survey_question_category_df=survey_question_category_df,
    term_related_q=term_related_q,
    survey_parallelfa_n_factors_file=survey_parallelfa_n_factors_file,
    reduce_levels_from_ten_to_seven=reduce_levels_from_ten_to_seven,
    method=parallel_method, mc.cores=1), .)
  
  save(survey_parallelfa_n_factors, file=survey_parallelfa_n_factors_file)
  ##save(random.polychor.survey_parallelfa_n_factors, file=random.polychor.survey_parallelfa_n_factors_file)
}

load(file=survey_parallelfa_n_factors_file, verbose=TRUE)

survey_parallelfa_arguments_df<-lapply(names(survey_parallelfa_n_factors), function(store_key_of_survey_parallelfa_n_factors_args,...) {
  obj_parallel_analysis_result<-survey_parallelfa_n_factors[[store_key_of_survey_parallelfa_n_factors_args]]
  data.frame(store_key=store_key_of_survey_parallelfa_n_factors_args,
             nfact=tryCatch(obj_parallel_analysis_result$nfact, error=function(msg) {return(NA)}),
             ncomp=tryCatch(obj_parallel_analysis_result$ncomp, error=function(msg) {return(NA)})
             )
},survey_parallelfa_n_factors=survey_parallelfa_n_factors) %>%
  dplyr::bind_rows() %>%
  dplyr::left_join(survey_parallelfa_arguments_df,.)

# bills_answer_to_bill
# bills_billcontent
# bills_billcontent_with_relatedq
# term_related_q
# bills_billid_to_relatedq_pairs

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
    cbind(., imp = rep(imputation_sample_i_s, each = nrow(.))) %>%
    cbind(., ncompnfact = rep(c(12,21:23), each = nrow(.))) %>%
    cbind(., reduct_type = rep(c("nfact"), each = nrow(.)))
) %>%
  dplyr::bind_rows(
    data.frame("survey"="2010overall", "term"="7") %>%
      cbind(., imp = rep(imputation_sample_i_s, each = nrow(.))) %>%
      cbind(., ncompnfact = rep(5:8, each = nrow(.))) %>%
      cbind(., reduct_type = rep(c("nfact"), each = nrow(.)))
  ) %>%
  dplyr::distinct_all()

# explore data distributions on issues --------------------------------

if (FALSE) {
  for (surveytitle in survey_keys) {
    needqsdf <- magrittr::extract2(survey_question_category_df,surveytitle)
    needqs <- needqsdf$ID
    needdf <- magrittr::extract2(survey_data_imputed,surveytitle)
    for (needq in needqs) {
      question_detail<-dplyr::filter(needqsdf,ID==!!needq) %>% magrittr::use_series("QUESTION")
      custom_plot(needdf, fvar=needq, weightvar="myown_wr", usingsurveypkg=FALSE) %>% print()
      if (readline(paste("now in",surveytitle,needq,question_detail,"continue?"))=="N") break
    }
  }
}

# exploratory IRT 探測問卷因素結構 --------------------------------

distincted_survey_parallelfa_arguments_df_runonly<- distincted_survey_parallelfa_arguments_df %>%
  dplyr::distinct(survey, imp, term, ncompnfact) %>%
  dplyr::mutate(runmirt_store_key=paste0(survey,"_imp",imp,"_ncompnfact",ncompnfact)) %>%
  dplyr::filter(ncompnfact %in% c(6,12)) %>%
  dplyr::arrange(survey, imp, ncompnfact)

survey_idealpoints_mirt_models_file <- paste0(save_dataset_in_scriptsfile_directory, "survey_idealpoints_mirt_models.RData")
survey_idealpoints_mirt_models_backup_file <- paste0(save_dataset_in_scriptsfile_directory, "survey_idealpoints_mirt_models_backup.RData")
#survey_idealpoints_mirt_models_file <- paste0(save_dataset_in_scriptsfile_directory, "survey_idealpoints_mirt_models_Davidian.RData")
if ({avoid_run_duplicated_models<-FALSE;avoid_run_duplicated_models}) {
  load(file=survey_idealpoints_mirt_models_file, verbose=TRUE)
  processed_idealpoint_mirt_keys<-sapply(survey_idealpoints_mirt_models, class) %>%
    .[.=="SingleGroupClass"] %>%
    names()
  distincted_survey_parallelfa_arguments_df_runonly<-dplyr::filter(distincted_survey_parallelfa_arguments_df_runonly, !(runmirt_store_key %in% !!processed_idealpoint_mirt_keys) )
}

if (FALSE) {
  load(file=survey_idealpoints_mirt_models_file, verbose=TRUE)
  survey_idealpoints_mirt_models<-distincted_survey_parallelfa_arguments_df_runonly$runmirt_store_key[1:24] %>% #[45:25]
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
        }) #, envir = .GlobalEnv
        tryn<-tryn+1
        if(!is(tryloadsaveresult, 'try-error') | tryn>11) {
          break
        }
      }
      return(explor_mirt_model)
    }, survey_question_category_df=survey_question_category_df,
    survey_data_imputed=survey_data_imputed,
    survey_parallelfa_arguments_df=survey_parallelfa_arguments_df,
    survey_question_category_df=survey_question_category_df,
    term_related_q=term_related_q,
    distincted_survey_parallelfa_arguments_df_runonly=distincted_survey_parallelfa_arguments_df_runonly,
    survey_idealpoints_mirt_models_file=survey_idealpoints_mirt_models_file,
    method=parallel_method, mc.cores=1 #1
    ), .) 
  save(survey_idealpoints_mirt_models, file=survey_idealpoints_mirt_models_backup_file)
}

# merge models -----------
if (FALSE) {
  load(file=survey_idealpoints_mirt_models_file, verbose=TRUE)
  t<-survey_idealpoints_mirt_models
  load(file=survey_idealpoints_mirt_models_backup_file, verbose=TRUE)
  t2<-survey_idealpoints_mirt_models
  load(file=paste0(save_dataset_in_scriptsfile_directory,"survey_idealpoints_mirt_models (assume_normal_6and12only).RData"), verbose=TRUE)
  t3<-survey_idealpoints_mirt_models
  survey_idealpoints_mirt_models<-rlist::list.merge(t,t2,t3)
  save(survey_idealpoints_mirt_models, file=survey_idealpoints_mirt_models_file)
  mirt:::summary(survey_idealpoints_mirt_models$`2016citizen_imp1_ncompnfact21`, rotate="none")
}

# Checking factor scores and factor structure and goodness of fit --------------------------------
if (FALSE) {
  load(file=survey_idealpoints_mirt_models_file, verbose=TRUE)
  complete_inf_mirt_models<-lapply(survey_idealpoints_mirt_models,tidymirt:::glance.SingleGroupClass) %>%
    dplyr::bind_rows() %>%
    data.frame(runmirt_store_key=names(survey_idealpoints_mirt_models)) %>%
    dplyr::left_join(distincted_survey_parallelfa_arguments_df_runonly, .) %>%
    dplyr::arrange(survey, SABIC, BIC, AIC, imp)
  write.csv(complete_inf_mirt_models, "TMP.csv")
  mirtmodelinfindicators<-c("AIC","AICc","SABIC","HQ","BIC")
}
if (FALSE) {
  load(file=survey_idealpoints_mirt_models_file, verbose=TRUE)
  for (mirt_model_on_survey_key in names(survey_idealpoints_mirt_models)) {
    mirtsurveyresult<-custom_mirt_coef_to_df(survey_idealpoints_mirt_models[[mirt_model_on_survey_key]], printSE = TRUE)
    write.csv(mirtsurveyresult, file="TMP.csv")
    #View(mirtsurveyresult)
    mirt:::summary(survey_idealpoints_mirt_models[[mirt_model_on_survey_key]], rotate="none") %>% print()
    readline(paste("now in",mirt_model_on_survey_key,"unrotated, continue?"))
    mirt:::summary(survey_idealpoints_mirt_models[[mirt_model_on_survey_key]], rotate="varimax") %>% print()
    #mirt::itemfit(survey_idealpoints_mirt_models[[mirt_model_on_survey_key]], QMC=TRUE) %>% print()
    readline(paste("now in",mirt_model_on_survey_key,"rotated, continue?"))
  }
}

# checking which model is better among different dimensions --------------------------------
if (FALSE) {
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

mirtfscores_similarity_scoresdf_file <- paste0(save_dataset_in_scriptsfile_directory, "mirtfscores_similarity_scoresdf.RData")
mirtfscores_similarity_scoresdf_backupfile <- paste0(save_dataset_in_scriptsfile_directory, "mirtfscores_similarity_scoresdf_backupfile.RData")
policy_idealpoint_colname_header<-"policyidealpoint"
scale_num<-function(X) {
  scale(X) %>% as.numeric() %>% return()
}

# median calculation: similarity to median --------------------------------
if (FALSE) {
  load(file=survey_idealpoints_mirt_models_file, verbose=TRUE)
  load(file=mirtfscores_similarity_scoresdf_file, verbose=TRUE)
  thismc.cores=24
  #mirtfscores_similarity_scoresdf<-list()
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
      #mirt_model_on_survey_key <- loopmirtmodellist_keys[1]
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
      #if (readline(paste("now in",mirt_model_on_survey_key,"continue?"))=="N") break
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
      if (FALSE) {
        median_policy_idealpoint_sign<-mirtfscores_median_points[[mirt_model_on_survey_key]]$sign %>%
          as.numeric()
        median_policy_idealpoint_signrank<-mirtfscores_median_points[[mirt_model_on_survey_key]]$signrank %>%
          as.numeric()
        cos_similarity_to_median_policy_idealpoint_sign<-apply(mirtfscoresdf, 1, FUN=lsa::cosine, y=median_policy_idealpoint_sign)
        cos_similarity_to_median_policy_idealpoint_signrank<-apply(mirtfscoresdf, 1, FUN=lsa::cosine, y=median_policy_idealpoint_signrank)
        cos_similarity_to_median_policy_idealpoint_sign_scaled<- cos_similarity_to_median_policy_idealpoint_sign %>%
          scale() %>%
          as.numeric()
        cos_similarity_to_median_policy_idealpoint_signrank_scaled<- cos_similarity_to_median_policy_idealpoint_signrank %>%
          scale() %>%
          as.numeric()
        euclid_distance_to_median_policy_idealpoint_sign<-apply(mirtfscoresdf, 1, FUN=custom_eucli_similarity, y=median_policy_idealpoint_sign)
        euclid_distance_to_median_policy_idealpoint_signrank<-apply(mirtfscoresdf, 1, FUN=custom_eucli_similarity, y=median_policy_idealpoint_signrank)
        euclid_distance_to_median_policy_idealpoint_sign_scaled<- euclid_distance_to_median_policy_idealpoint_sign %>%
          scale() %>%
          as.numeric()
        euclid_distance_to_median_policy_idealpoint_signrank_scaled<- euclid_distance_to_median_policy_idealpoint_signrank %>%
          scale() %>%
          as.numeric()
        more_similar_cos_sign<-quantile(cos_similarity_to_median_policy_idealpoint_sign)[4] %>%
          magrittr::is_weakly_greater_than(cos_similarity_to_median_policy_idealpoint_sign,.) %>%
          as.numeric()
        more_similar_cos_signrank<-quantile(euclid_distance_to_median_policy_idealpoint_signrank)[4] %>%
          magrittr::is_weakly_greater_than(euclid_distance_to_median_policy_idealpoint_signrank,.) %>%
          as.numeric()
        more_similar_euclid_sign<-quantile(euclid_distance_to_median_policy_idealpoint_sign)[2] %>%
          magrittr::is_weakly_less_than(euclid_distance_to_median_policy_idealpoint_sign, .) %>%
          as.numeric()
        more_similar_euclid_signrank<-quantile(euclid_distance_to_median_policy_idealpoint_signrank)[2] %>%
          magrittr::is_weakly_less_than(euclid_distance_to_median_policy_idealpoint_signrank, .) %>%
          as.numeric()
      }
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
        # cos_similarity_to_median_policy_idealpoint_sign,
        # cos_similarity_to_median_policy_idealpoint_signrank,
        # euclid_distance_to_median_policy_idealpoint_sign,
        # euclid_distance_to_median_policy_idealpoint_signrank,
        # cos_similarity_to_median_policy_idealpoint_sign_scaled,
        # cos_similarity_to_median_policy_idealpoint_signrank_scaled,
        # euclid_distance_to_median_policy_idealpoint_sign_scaled,
        # euclid_distance_to_median_policy_idealpoint_signrank_scaled,
        # more_similar_cos_sign,
        # more_similar_cos_signrank,
        # more_similar_euclid_sign,
        # more_similar_euclid_signrank
      ) %>%
        magrittr::set_colnames(
          sim_col_names
          # paste0(policy_idealpoint_colname_header,"_cos_similarity_to_median_policy_idealpoint_sign"),
          # paste0(policy_idealpoint_colname_header,"_cos_similarity_to_median_policy_idealpoint_signrank"),
          # paste0(policy_idealpoint_colname_header,"_euclid_distance_to_median_policy_idealpoint_sign"),
          # paste0(policy_idealpoint_colname_header,"_euclid_distance_to_median_policy_idealpoint_signrank"),
          # paste0(policy_idealpoint_colname_header,"_cos_similarity_to_median_policy_idealpoint_sign_scaled"),
          # paste0(policy_idealpoint_colname_header,"_cos_similarity_to_median_policy_idealpoint_signrank_scaled"),
          # paste0(policy_idealpoint_colname_header,"_euclid_distance_to_median_policy_idealpoint_sign_scaled"),
          # paste0(policy_idealpoint_colname_header,"_euclid_distance_to_median_policy_idealpoint_signrank_scaled"),
          # paste0(policy_idealpoint_colname_header,"_more_similar_cos_sign"),
          # paste0(policy_idealpoint_colname_header,"_more_similar_cos_signrank"),
          # paste0(policy_idealpoint_colname_header,"_more_similar_euclid_sign"),
          # paste0(policy_idealpoint_colname_header,"_more_similar_euclid_signrank")
        ) %>%
        dplyr::mutate(cos_similarity_to_median_policy_idealpoint_scaled=scale_num(cos_similarity_to_median_policy_idealpoint)) %>%
        dplyr::mutate(euclid_distance_to_median_policy_idealpoint_scaled=scale_num(euclid_distance_to_median_policy_idealpoint)) %>%
        dplyr::bind_cols(mirtfscoresdf, .) %>%
        dplyr::bind_cols(surveydataids, .)
      
      tryn<-1
      while (FALSE) {
        loadsavestatus<-try({
          load(file=mirtfscores_similarity_scoresdf_file, verbose=TRUE)
          mirtfscores_similarity_scoresdf[[mirt_model_on_survey_key]]<-mirtfscoresdf
          save(mirtfscores_similarity_scoresdf, file=mirtfscores_similarity_scoresdf_file)
        })
        tryn<-tryn+1
        if(!is(loadsavestatus, 'try-error') | tryn>11) break
      }
      return(mirtfscoresdf)
      # survey_data_imputed[[needsurvey]] <- dplyr::bind_rows(
      #   dplyr::semi_join(survey_data_imputed[[needsurvey]], mirtfscoresdf, by = c(".imp", "id", "SURVEY")) %>%
      #     dplyr::select(-dplyr::starts_with(!!policy_idealpoint_colname_header)) %>%
      #     dplyr::left_join(mirtfscoresdf, by = c(".imp", "id", "SURVEY")),
      #   dplyr::anti_join(survey_data_imputed[[needsurvey]], mirtfscoresdf, by = c(".imp", "id", "SURVEY") )  
      # )
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
    
  save(mirtfscores_similarity_scoresdf, mirtfscores_median_points, file=mirtfscores_similarity_scoresdf_backupfile)
}
survey_with_idealpoint_name<-paste0(save_dataset_in_scriptsfile_directory, "miced_survey_2surveysonly_mirt_lca_clustering_idealpoints.RData")

# merge fscore data --------------------------------
if (FALSE) {
  load(file=mirtfscores_similarity_scoresdf_file, verbose=TRUE)
  load(file=paste0(save_dataset_in_scriptsfile_directory,"miced_survey_2surveysonly_mirt_lca_clustering.RData"), verbose=TRUE)
  for (survey in names(survey_data_imputed)) {
    needsvykeys<-grep(pattern=survey, x=names(mirtfscores_similarity_scoresdf), value=TRUE)
    survey_idp_similarity<-magrittr::extract(mirtfscores_similarity_scoresdf, needsvykeys) %>%
      dplyr::bind_rows()
    survey_data_imputed[[survey]] %<>% dplyr::left_join(survey_idp_similarity)
  }
  survey_data_imputed <- lapply(survey_data_imputed, function(X) {
    dplyr::mutate_at(X, dplyr::vars(dplyr::contains("_more_similar")), as.factor)
  })
  load(file=paste0(dataset_in_scriptsfile_directory, "kamila_clustering_parameters.Rdata"), verbose=TRUE)
  filterstddf<-custom_ret_appro_kamila_clustering_parameters()
  survey_data_imputed %<>% lapply(dplyr::semi_join, filterstddf) %>%
    lapply(dplyr::left_join, filterstddf) %>%
    lapply(function(X) {dplyr::select(X,-.imp,-imp) %>% dplyr::rename(.imp=newimp)})
  #save(survey_data_imputed, file=survey_with_idealpoint_name)
  load(file=survey_with_idealpoint_name, verbose=TRUE)
  if (readline(paste("now in",mirt_model_on_survey_key,"continue?"))=="N") break
}

# extract previous result to merge into reset dataset --------------------------------
if (FALSE) {
  load(file=mirtfscores_similarity_scoresdf_file, verbose=TRUE)
  load(file=paste0(save_dataset_in_scriptsfile_directory,"miced_survey_2surveysonly_mirt_lca_clustering.RData"), verbose=TRUE)
  clustered_svydata<-survey_data_imputed
  load(file=paste0(save_dataset_in_scriptsfile_directory,"miced_survey_2surveysonly_mirt_lca_clustering_idealpoints.RData"), verbose=TRUE)
  t<-lapply(survey_data_imputed, function(X, policy_idealpoint_colname_header) {
    dplyr::select(X, SURVEY, id, .id, .imp, tidyselect::contains(!!policy_idealpoint_colname_header))
  }, policy_idealpoint_colname_header=policy_idealpoint_colname_header) %>%
    lapply(function(X,Y) {
      needsyv<-as.character(X$SURVEY[1])
      dplyr::left_join(Y[[needsyv]],X)
    },Y=clustered_svydata)
  survey_data_imputed<-t
  #save(survey_data_imputed, file=survey_with_idealpoint_name)
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
