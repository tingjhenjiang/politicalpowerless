if (!("benchmarkme" %in% rownames(installed.packages()))) try(install.packages("benchmarkme"))
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-try(paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model))
t_sessioninfo_running_with_cpu_locale<-try(gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2])))
source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."

source(file = file.path(source_sharedfuncs_r_path,"shared_functions.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"01_preprocessing_fetch_ly_meeting_record.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"02_preprocessing_fetch_ly_decision_and_votes_term56older.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"02_preprocessing_fetch_ly_decision_and_votes.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"03_preprocessing_legislators_and_elections.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"04_preprocessing_merge_votingrecords_surveyquestions.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"05_preprocessing_survey_adding_area_settingNA.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"06_preprocessing_imputation.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"07_preprocessing_fa_and_irt_process.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"08_preprocessing_lca_analysis_process_commonpart.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"08_preprocessing_lca_analysis_process.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"09_preprocessing_clustering.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"10_preprocessing_get_similarities.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"10_preprocessing_measure_survey_response_idealpoints.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"12_preprocessing_transform_clean_design_survey_data.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"13_preprocessing_merge_all_datasets.R"), encoding="UTF-8")


lymeetingfetcher <- lymeetingfetcher_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory)
testdf<-lymeetingfetcher$get_meetingdata(loadExisted=FALSE,preparedata=TRUE,save=TRUE)#loadExisted=FALSE,preparedata=TRUE

lyterm56_vote_recorder <- lyterm56votes_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory
)
testdf<-lyterm56_vote_recorder$get_term56_voting_records(loadExisted = FALSE,save=FALSE)

lyvotes_instance <- lyvotes_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
lyvotes <- lyvotes_instance$get_voting_records(loadExisted=FALSE,save=TRUE,startUrlN=1,endUrlN=-1)



legislators_and_elections_parser_instance <- legislators_and_elections_parser_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory
)
legislators_with_elections_df <- legislators_and_elections_parser_instance$get_legislators_with_elections_df()


merge_votingrecords_surveyquestions_instance <- merge_votingrecords_surveyquestions_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory
)
t<- merge_votingrecords_surveyquestions_instance$merging_terms_56789()
# t<- merge_votingrecords_surveyquestions_instance$writing_bill_record()
# t<- merge_votingrecords_surveyquestions_instance$overall_futher_preprocess_merged_dataset()
# t<- merge_votingrecords_surveyquestions_instance$compose_dataset_for_factor_analysis()
t<- merge_votingrecords_surveyquestions_instance$merge_legislators_voterecords()
t<- merge_votingrecords_surveyquestions_instance$merge_voterecords_billcontents()


survey_adding_area_settingNA_instance <- survey_adding_area_settingNA_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
t<-survey_adding_area_settingNA_instance$processing_duplicated_area()


survey_imputation_instance <- survey_imputation_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
# 第一次填補（loadExisted=FALSE會重新以mice/randomForest計算，非常耗時）
survey_data_imputed <- survey_imputation_instance$get_survey_data_imputed(loadExisted=TRUE,save=FALSE)
# 針對每個imputed dataset的再填補
# survey_data_imputed <- survey_imputation_instance$get_further_imputed_survey_data(loadExisted=FALSE,save=TRUE)
# t<-survey_imputation_instance$check_imputed_survey_data(survey_data_imputed)


# 07 factor analysis與IRT：社經地位、政治效能感、政治參與latent variables
fa_and_irt_instance <- fa_and_irt_process_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
# survey_data_imputed <- fa_and_irt_instance$get_survey_data_imputed_stage(stage="") #讀取06輸出
# survey_data_imputed <- fa_and_irt_instance$compute_factoredses(survey_data_imputed)
# survey_data_imputed <- fa_and_irt_instance$compute_factoredefficacy(survey_data_imputed)
# survey_data_imputed <- fa_and_irt_instance$reorder_particip_vars(survey_data_imputed)
# particip_res <- fa_and_irt_instance$compute_factoredparticip(survey_data_imputed)
# survey_data_imputed <- particip_res$survey_data_imputed
# fa_and_irt_instance$save_survey_data_imputed_stage(survey_data_imputed, stages=c("mirt"))

# 08 LCA潛在類別分析（統獨傾向、政策態度縮減）
lca_analysis_instance <- lca_analysis_process_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
# lca_analysis_instance$build_dbconnect_info() #需要輸入資料庫密碼
# lca_analysis_instance$install_weighted_polca() #首次需安裝external/poLCA子模組版（支援sample weights）
# 之後custom_generate_LCA_model可傳weights="myown_wr"改用加權pseudo-ML估計
# lca_analysis_instance$run_lca_model_search(survey_data_imputed) #模型搜尋，寫入資料庫
# condense_res <- lca_analysis_instance$run_condense_polca_models(survey_data_imputed)
# survey_data_imputed <- lca_analysis_instance$apply_indp_atti(survey_data_imputed, poLCA_survey_results, recode_indp_list)
# lca_analysis_instance$save_survey_data_imputed_stage(survey_data_imputed, stages=c("mirt_lca"))

# 09 clustering（論文定案採kamila）
clustering_instance <- clustering_process_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
# survey_data_imputed <- clustering_instance$get_survey_data_imputed_for_clustering()
# kamila_results <- clustering_instance$run_kamila_clustering(survey_data_imputed) #非常耗時（舊法：inflate近似加權）
# survey_data_imputed <- clustering_instance$apply_kamila_results_to_survey_data(survey_data_imputed, rate=10)
# 新法：external/kamila子模組原生支援sample weights（obsWeights=myown_wr），毋須擴增資料
# clustering_instance$install_weighted_kamila() #首次需安裝子模組版kamila
# kamila_results <- clustering_instance$run_kamila_clustering_weighted(survey_data_imputed) #非常耗時
# survey_data_imputed <- clustering_instance$apply_weighted_kamila_results_to_survey_data(survey_data_imputed)
# clustering_instance$save_survey_data_imputed_stage(survey_data_imputed, stages=c("mirt_lca_clustering"))

# 10 受訪者與立委相似度、政策理想點
similarities_instance <- similarities_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
# similarities_bet_pp_ly_longdf <- similarities_instance$get_similarities_bet_pp_ly_longdf(loadExisted=FALSE, save=TRUE)

measure_idealpoints_instance <- measure_idealpoints_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
# survey_idealpoints_mirt_models <- measure_idealpoints_instance$run_idealpoint_mirt_models(survey_data_imputed) #非常耗時
# sim_res <- measure_idealpoints_instance$compute_mirtfscores_similarity(survey_data_imputed)
# survey_data_imputed <- measure_idealpoints_instance$merge_fscore_data(sim_res$mirtfscores_similarity_scoresdf, save=TRUE)

# 12 問卷資料變形（wide to long）
transform_survey_instance <- transform_clean_design_survey_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
# complete_survey_dataset <- transform_survey_instance$build_complete_survey_dataset(save=TRUE)

# 13 大串連資料成建模主檔
merge_all_instance <- merge_all_datasets_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
# overall_nonagenda_df <- merge_all_instance$build_overall_nonagenda_df(save=TRUE) #需要大量記憶體
# overall_nonagenda_df <- merge_all_instance$shrink_overall_nonagenda_df_to_tiny(overall_nonagenda_df, save=TRUE)
# overall_nonagenda_df <- merge_all_instance$get_overall_nonagenda_df(size="tiny")


# 14 分析建模階段（各類別互相獨立，皆繼承merge_all_datasets_class；依需要source） --------------------------------
source(file = file.path(source_sharedfuncs_r_path,"14_analysis_VIF.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"14_analyse_ordinallogistic.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"14_analyse_plot.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"14_analyse_pp.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"14_analyse_responsiveness.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"14_analyse_idealpoint.R"), encoding="UTF-8")
source(file = file.path(source_sharedfuncs_r_path,"14_analyse_sem.R"), encoding="UTF-8")

# 14 VIF共線性檢查
analyse_vif_instance <- analyse_vif_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
# res.custom_vif <- analyse_vif_instance$run_vif_test(overall_nonagenda_df, save=TRUE)
# t <- analyse_vif_instance$run_vif_test_on_responsiveness(overall_nonagenda_df)

# 14 ordinal logistic（svyolr；運算量大且吃記憶體，未來將移至Python/GPU）
analyse_ordinallogistic_instance <- analyse_ordinallogistic_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
# des <- analyse_ordinallogistic_instance$build_svydesign_implist(overall_nonagenda_df, save=TRUE)
# ordinallogisticmodelonrespondopinion <- analyse_ordinallogistic_instance$run_ordinal_logistic_model(des)
# poolresult <- analyse_ordinallogistic_instance$pool_ordinal_logistic_result(ordinallogisticmodelonrespondopinion)
# GPU替代方案（svyolr OOM時）：匯出parquet後改用python/ordinal_logistic_pymc.py（見python/README.md）
# analyse_ordinallogistic_instance$export_model_data_to_parquet(overall_nonagenda_df)

# 14 敘述統計與繪圖
analyse_plot_instance <- analyse_plot_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
# res.survey_summary_statistics <- analyse_plot_instance$compute_survey_summary_statistics()
# analyse_plot_instance$plot_responsiveness_vars(overall_nonagenda_df)

# 14 政治參與模型（ordinal::clmm）
analyse_pp_instance <- analyse_pp_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
# merged_acrossed_surveys_list <- analyse_pp_instance$get_merged_acrossed_surveys_list(minuspolicy=TRUE)
# ppmodels <- analyse_pp_instance$run_ppmodels(merged_acrossed_surveys_list)
# poolres <- analyse_pp_instance$pool_ppmodels()

# 14 立委回應民意模型（ordinal::clmm）
analyse_responsiveness_instance <- analyse_responsiveness_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
# respondmodel_args <- analyse_responsiveness_instance$get_respondmodel_args()
# respondmodels <- analyse_responsiveness_instance$run_respond_clmm_models(respondmodel_args, overall_nonagenda_df) #非常耗時
# poolres <- analyse_responsiveness_instance$pool_respondmodels()

# 14 理想點模型（lmerTest/robustlmm/svylme/jrfit）
analyse_idealpoint_instance <- analyse_idealpoint_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
# merged_acrossed_surveys_list <- analyse_idealpoint_instance$get_merged_acrossed_surveys_list()
# idealpoint_models_args <- analyse_idealpoint_instance$get_idealpoint_models_args()
# idealpoint_models <- analyse_idealpoint_instance$run_idealpoint_models_lmer(idealpoint_models_args, merged_acrossed_surveys_list, usingpackage="robustlmm")
# poolres <- analyse_idealpoint_instance$pool_idealpoint_models(all_idealpoint_models_robust=idealpoint_models)

# 14 SEM（semTools::sem.mi；實驗性質）
analyse_sem_instance <- analyse_sem_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
# semmodelonrespondopinion <- analyse_sem_instance$run_sem_model(overall_nonagenda_df)
# analyse_sem_instance$inspect_sem_model(semmodelonrespondopinion)


# 15 因果分析：傾向分數方法（MatchIt二元PSM／WeightIt連續處理GPS-IPW） --------------------------------
source(file = file.path(source_sharedfuncs_r_path,"15_analyse_causal_psm.R"), encoding="UTF-8")
causal_instance <- analyse_causal_psm_class$new(
  dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory,
  filespath=filespath,
  dataset_file_directory=dataset_file_directory,
  debug_func_mode=FALSE
)
# 連續處理（政治參與；GPS/IPW加權；SES為參與之前置因須另控制）：
# gps_res <- causal_instance$run_gps_continuous(overall_nonagenda_df,
#   treatment="myown_factoredparticip_overallscaled", outcome="respondopinion",
#   extra_confounders=c("myown_factoredses_overallscaled"))
# causal_instance$inspect_causal_balance(gps_res)
# pooled <- causal_instance$pool_causal_models(gps_res)
# doseresp <- causal_instance$compute_dose_response(gps_res, treatment="myown_factoredparticip_overallscaled")
# 二元處理（加權中位數二分後PSM；建議搭配caliper確保平衡）：
# overall_nonagenda_df$particip_high <- causal_instance$binarize_treatment_by_median(overall_nonagenda_df, "myown_factoredparticip_overallscaled")
# psm_res <- causal_instance$run_psm_binary(overall_nonagenda_df, treatment="particip_high",
#   outcome="respondopinion", caliper=0.1, extra_confounders=c("myown_factoredses_overallscaled"))
