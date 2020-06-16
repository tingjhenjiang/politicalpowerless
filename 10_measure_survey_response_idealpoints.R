# 第Ｏ部份：環境設定 --------------------------------
if (!("benchmarkme" %in% rownames(installed.packages()))) try(install.packages("benchmarkme"))
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-try(paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model))
t_sessioninfo_running_with_cpu_locale<-try(gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2])))
source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/shared_functions.R"), encoding="UTF-8")
load(file=paste0(dataset_in_scriptsfile_directory, "miced_survey_9_with_mirt_lca_clustering.RData"), verbose=TRUE)
load(file=paste0(dataset_in_scriptsfile_directory, "bills_answer_to_bill_bills_billcontent.RData"), verbose=TRUE)
survey_codebook_file<-paste0(dataset_file_directory,"all_survey_questions_englished.xlsx")
survey_keys <- c("2010overall","2016citizen")
survey_question_category_df<-lapply(c(1,3), function(fi,...) {
  openxlsx::read.xlsx(survey_codebook_file,sheet = fi)
},survey_codebook_file=survey_codebook_file) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(grepl(pattern="民主價值", x=CATEGORY, perl=TRUE) | CATEGORY=="議題") %>%
  dplyr::filter(SURVEY %in% !!survey_keys) %>%
  dplyr::filter(MEASUREMENT %in% c("nominal","ordinal")) %>%
  dplyr::filter(IMPUTATION!="ignore" & ID!="myown_indp_atti") %>%
  dplyr::mutate(itemtype=NA) %>%
  mutate_cond(MEASUREMENT=="nominal", itemtype="2PL") %>%
  mutate_cond((grepl(pattern=";3", x=ANSWER) & itemtype=="2PL"), itemtype="nominal") %>%
  mutate_cond(MEASUREMENT=="ordinal", itemtype="graded") %>%
  mutate_cond(grepl(pattern="(1分;22分|22分;33分)", x=ANSWER), itemtype="grsm") %>%
  dplyr::arrange(SURVEYCOMPLETEID) %>%
  lapply(survey_keys, function(k, data) {
    return(dplyr::filter(data, SURVEY==!!k))
  }, data=.) %>%
  magrittr::set_names(survey_keys)
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
  dplyr::arrange(needvars) %>%
  dplyr::filter(needvars=="limited")
survey_parallelfa_n_factors_file <- paste0(dataset_in_scriptsfile_directory, "survey_parallelfa_n_factors.RData")
random.polychor.survey_parallelfa_n_factors_file <- paste0(dataset_in_scriptsfile_directory, "random.polychor.survey_parallelfa_n_factors.RData") 
survey_parallelfa_n_factors <- survey_parallelfa_arguments_df$store_key %>%
  magrittr::set_names(custom_parallel_lapply(., function(fikey, ...) {
    fi<-which(survey_parallelfa_arguments_df$store_key==fikey)
    message(paste("now in", survey_parallelfa_arguments_df[fi,"store_key"],"and fi is",fi))
    argument_imp<-survey_parallelfa_arguments_df$imp[fi]
    parallel_fa_method_fm<-as.character(survey_parallelfa_arguments_df$fm[fi])
    surveyvars<-survey_question_category_df[[survey_parallelfa_arguments_df$survey[fi]]]$ID
    ordinalsurveyvars<-dplyr::filter(survey_question_category_df[[survey_parallelfa_arguments_df$survey[fi]]], MEASUREMENT=="ordinal") %>%
      magrittr::use_series("ID") %>%
      c("myown_indp_atti")
    targetsurveydf<-survey_data_imputed[[survey_parallelfa_arguments_df$survey[fi]]] %>%
      .[magrittr::use_series(., ".imp")==argument_imp,surveyvars] %>% dplyr::mutate_all(unclass)
    notemptyvars<-sapply(targetsurveydf, is.na) %>% colSums() %>% .[.==0] %>% names()
    fewerthan8catgsvars<-sapply(targetsurveydf, function(X) {
      length(unique(X))
    }) %>% .[.<=8] %>% names()
    largerthan8catgsvars<-dplyr::setdiff(names(targetsurveydf), fewerthan8catgsvars)
    finalneedvars<-dplyr::intersect(notemptyvars, fewerthan8catgsvars)
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
    if(!is(res_n_factors, 'try-error')) {
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
  method=parallel_method), .)

save(survey_parallelfa_n_factors, file=survey_parallelfa_n_factors_file)
##save(random.polychor.survey_parallelfa_n_factors, file=random.polychor.survey_parallelfa_n_factors_file)
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
  dplyr::filter(needvars=="limited") %>%
  dplyr::distinct(survey, imp, term, needvars, nfact, ncomp) %>%
  dplyr::arrange(survey, term, imp, needvars, ncomp) %>%
  reshape2::melt(., id.vars=c("survey","imp","term","needvars")) %>%
  dplyr::filter(!(term %in% c(5,6,7,8))) %>%
  dplyr::rename(ncompnfact=value, reduct_type=variable) %>%
  dplyr::filter(!is.na(ncompnfact)) %>%
  dplyr::distinct(survey, term, imp, reduct_type, ncompnfact)

# exploratory IRT 探測問卷因素結構 --------------------------------

distincted_survey_parallelfa_arguments_df_runonly<- distincted_survey_parallelfa_arguments_df %>%
  dplyr::distinct(survey, imp, term, ncompnfact) %>%
  dplyr::mutate(runmirt_store_key=paste0(survey,"_imp",imp,"_ncompnfact",ncompnfact))
survey_idealpoints_mirt_models_file <- paste0(dataset_in_scriptsfile_directory, "survey_idealpoints_mirt_models.RData")
survey_idealpoints_mirt_models<-distincted_survey_parallelfa_arguments_df_runonly$runmirt_store_key %>%
  magrittr::set_names(custom_parallel_lapply(., function(fikey, ...) {
    needrow<-dplyr::filter(distincted_survey_parallelfa_arguments_df_runonly, runmirt_store_key==!!fikey)
    argdf_term <- as.character(needrow$term)
    if (grepl(pattern="&", x=argdf_term) ) {
      argdf_term <- unlist(stringr::str_split(argdf_term, pattern="&"))
    }
    extractedneedvars <- magrittr::extract(term_related_q, argdf_term) %>%
      unlist() %>%
      unique() %>%
      sort()
    extractedneedvars_without_survey <- gsub(pattern=paste0(as.character(needrow$survey),"@"), replacement="", x=extractedneedvars)
    to_explor_IRT_itemtypes<-data.frame(SURVEYCOMPLETEID=extractedneedvars, ID=extractedneedvars_without_survey, SURVEY=needrow$survey) %>%
      dplyr::left_join(survey_question_category_df[[needrow$survey]]) %>%
      mutate_cond(grepl(pattern="myown_indp_atti", x=SURVEYCOMPLETEID), itemtype="graded")
    to_explor_IRT_data<-survey_data_imputed[[as.character(needrow$survey)]] %>%
      .[.$.imp==needrow$imp,to_explor_IRT_itemtypes$ID] %>%
      dplyr::mutate_all(unclass)
    mirtmethod<-if (as.integer(needrow$ncomp)>=3) "MCEM" else "EM"
    explor_mirt_model<-mirt::mirt(
      to_explor_IRT_data,
      model=as.integer(needrow$ncomp),
      itemtype=to_explor_IRT_itemtypes$itemtype,
      technical=list(NCYCLES=250,MAXQUAD=40000),
      method=mirtmethod
    )
    while (TRUE) {
      tryloadsaveresult<-try({
        load(file=survey_idealpoints_mirt_models_file, verbose=TRUE)
        survey_idealpoints_mirt_models[[fikey]]<-explor_mirt_model
        save(survey_idealpoints_mirt_models, file=survey_idealpoints_mirt_models_file)
      }) #, envir = .GlobalEnv
      if(!is(tryloadsaveresult, 'try-error')) {
        break
      }
    }
    return(to_explor_mirt_model)
  }, survey_question_category_df=survey_question_category_df,
  survey_data_imputed=survey_data_imputed,
  survey_parallelfa_arguments_df=survey_parallelfa_arguments_df,
  survey_question_category_df=survey_question_category_df,
  term_related_q=term_related_q,
  distincted_survey_parallelfa_arguments_df_runonly=distincted_survey_parallelfa_arguments_df_runonly,
  survey_idealpoints_mirt_models_file=survey_idealpoints_mirt_models_file,
  method=parallel_method
  ), .) 

View(custom_mirt_coef_to_df(to_explor_mirt_model))
