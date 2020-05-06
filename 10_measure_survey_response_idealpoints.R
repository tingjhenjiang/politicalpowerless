# 第Ｏ部份：環境設定 --------------------------------
if (!("benchmarkme" %in% rownames(installed.packages()))) install.packages("benchmarkme")
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
t_sessioninfo_running_with_cpu_locale<-gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2]))
source(file = "shared_functions.R", encoding="UTF-8")
load(file=paste0(dataset_in_scriptsfile_directory, "miced_survey_9_with_mirt_lca.RData"), verbose=TRUE)
survey_codebook_file<-paste0(dataset_file_directory,"all_survey_questions_englished.xlsx")
survey_keys <- c("2004citizen","2010env","2010overall","2016citizen")
survey_question_category_df<-lapply(c(1,3), function(fi,...) {
  openxlsx::read.xlsx(survey_codebook_file,sheet = fi)
},survey_codebook_file=survey_codebook_file) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(grepl(pattern="民主價值", x=CATEGORY, perl=TRUE) | CATEGORY=="議題") %>%
  dplyr::filter(SURVEY %in% !!survey_keys) %>%
  dplyr::filter(MEASUREMENT %in% c("nominal","ordinal")) %>%
  dplyr::filter(IMPUTATION!="ignore") %>%
  dplyr::mutate(itemtype=NA) %>%
  mutate_cond(MEASUREMENT=="ordinal", itemtype="graded") %>%
  mutate_cond(MEASUREMENT=="nominal", itemtype="2PL") %>%
  mutate_cond(grepl(pattern="(1分;22分|22分;33分)", x=ANSWER), itemtype="grsm") %>%
  dplyr::arrange(SURVEYCOMPLETEID) %>%
  lapply(survey_keys, function(k, data) {
    return(dplyr::filter(data, SURVEY==!!k))
  }, data=.) %>%
  magrittr::set_names(survey_keys)
# fa parallel探測因素數目 --------------------------------
survey_parallelfa_arguments_df<-data.frame("survey"=survey_keys) %>%
  cbind(., imp = rep(imputation_sample_i_s, each = nrow(.))) %>%
  dplyr::mutate(store_key=paste0(survey, "_imp", imp))
survey_parallelfa_n_factors_file<-paste0(dataset_in_scriptsfile_directory, "survey_parallelfa_n_factors.RData")
survey_parallelfa_n_factors <- custom_parallel_lapply(1:nrow(survey_parallelfa_arguments_df), function(fi, ...) {
  message(paste("now in", survey_parallelfa_arguments_df[fi,"store_key"],"and fi is",fi))
  argument_imp<-survey_parallelfa_arguments_df$imp[fi]
  surveyvars<-survey_question_category_df[[survey_parallelfa_arguments_df$survey[fi]]]$ID
  ordinalsurveyvars<-dplyr::filter(survey_question_category_df[[survey_parallelfa_arguments_df$survey[fi]]], MEASUREMENT=="ordinal") %>%
    magrittr::use_series("ID")
  targetsurveydf<-survey_data_imputed[[survey_parallelfa_arguments_df$survey[fi]]] %>%
    .[magrittr::use_series(., ".imp")==argument_imp,surveyvars] %>% dplyr::mutate_all(unclass)
  notemptyvars<-sapply(targetsurveydf, is.na) %>% colSums() %>% .[.==0] %>% names()
  fewerthan8catgsvars<-sapply(targetsurveydf, function(X) {
    length(unique(X))
  }) %>% .[.<=8] %>% names()
  targetsurveydf<-targetsurveydf[,intersect(notemptyvars, fewerthan8catgsvars)]
  res_n_factors <- capture.output(tryCatch({
    #https://rmc.ehe.osu.edu/files/2018/08/Parallel-AnalysisOct2017.pdf
    #psych::fa.parallel(targetsurveydf, fm="ml", main="Parallel Analysis Scree Plots", cor="poly")
    random.polychor.pa::random.polychor.pa(nrep=50, data.matrix = targetsurveydf, q.eigen=.99)
  }, error = function(msg) {
    message(paste0(msg,"\n"))
    return("ERROR")
  }))
  return(res_n_factors)
}, survey_question_category_df=survey_question_category_df,
survey_data_imputed=survey_data_imputed,
survey_parallelfa_arguments_df=survey_parallelfa_arguments_df,
survey_question_category_df=survey_question_category_df,
method=parallel_method) %>% #,  method=parallel_method ,  mc.cores = 1
  magrittr::set_names(survey_parallelfa_arguments_df$store_key)

save(survey_parallelfa_n_factors, file=survey_parallelfa_n_factors_file)
