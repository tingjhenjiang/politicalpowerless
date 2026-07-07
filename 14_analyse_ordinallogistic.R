source(file = "13_preprocessing_merge_all_datasets.R")
# modeling on survey Ordinal Logistic --------------------------------
#DEM 7283 - Example 2 - Logit and Probit Models
#https://rpubs.com/corey_sparks/577954
#DEM 7283 - Example 7 Multiple Imputation & Missing Data
#https://rpubs.com/corey_sparks/477390
#DEM 7283 - Example 1 - Survey Statistics using BRFSS data
#https://rpubs.com/corey_sparks/571267
#DEM 7283 Example 10 - Survey Information and Small Area Estimation
#https://rpubs.com/corey_sparks/484730
#DEM 7283 - Example 3 - Ordinal & Multinomial Logit Models
#https://rpubs.com/corey_sparks/356551
#calculate p-value
#https://www.researchgate.net/post/p_value_calculator
#weight assigning method
#https://cran.r-project.org/web/packages/survey/vignettes/pps.pdf
analyse_ordinallogistic_class <- R6::R6Class("analyse_ordinallogistic", inherit=merge_all_datasets_class, public = list(
  ordinallogistic_des_filepath = NULL,
  ordinallogistic_model_filepath = NULL,
  ordinallogistic_result_csv_filepath = NULL,
  python_ordinal_export_dir = NULL,
  initialize = function(dataset_in_scriptsfile_directory="/mnt", filespath="/mnt", dataset_file_directory="/mnt", debug_func_mode=TRUE) {
    super$initialize(dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory, filespath=filespath, dataset_file_directory=dataset_file_directory, debug_func_mode=debug_func_mode)
    self$ordinallogistic_des_filepath <- file.path(save_dataset_in_scriptsfile_directory, "ordinallogisticmodelonrespondopinion_des.RData")
    self$ordinallogistic_model_filepath <- file.path(save_dataset_in_scriptsfile_directory, "ordinallogisticmodelonrespondopinion.RData")
    self$ordinallogistic_result_csv_filepath <- file.path(save_dataset_in_scriptsfile_directory, "ordinallogisticmodelresult.csv")
    self$python_ordinal_export_dir <- file.path(save_dataset_in_scriptsfile_directory, "python_ordinal")
  },
  # 匯出建模資料為parquet供python/ordinal_logistic_pymc.py以GPU估計加權ordinal logistic
  # （svyolr記憶體不足與運算時間過長之替代方案）；每個插補樣本一檔＋modelvars.json
  export_model_data_to_parquet = function(overall_nonagenda_df, exportdir=self$python_ordinal_export_dir, imps=1:6) {
    dir.create(exportdir, showWarnings=FALSE, recursive=TRUE)
    meta<-list(
      outcome="respondopinion",
      weight="myown_wr",
      conti_vars=unique(c(self$modelvars_latentrelated, self$modelvars_ex_conti)),
      catg_vars=unique(c(self$modelvars_ex_catg, self$modelvars_clustervars, self$modelvars_controllclustervars))
    )
    jsonlite::write_json(meta, path=file.path(exportdir,"modelvars.json"), auto_unbox=TRUE)
    needcols<-unique(c(meta$outcome, meta$weight, "newimp", meta$conti_vars, meta$catg_vars))
    for (m in imps) {
      dplyr::filter(overall_nonagenda_df, newimp==!!m) %>%
        dplyr::select(tidyselect::any_of(needcols)) %>%
        arrow::write_parquet(., sink=file.path(exportdir, paste0("ordinal_model_data_imp",m,".parquet")))
      message(paste("exported imp",m))
    }
    invisible(exportdir)
  },
  get_ordinal_modelformula = function() {
    overalldf_all_vars<-c("cluster_varsellcm","cluster_kamila","cluster_clustrd","imp","id_of_imp","myown_sex","myown_age","myown_selfid","myown_marriage","id","SURVEY","myown_areakind","myown_wsel","myown_wr","myown_factoredses","myown_factoredefficacy","myown_factoredparticip","psu","ssu","stratum","term","legislator_name","partyGroup","seniority","elec_dist_type","similarity_distance","billid_myown","party_pressure","adminparty","salient","variable_on_q","respondopinion","success_on_bill","days_diff_survey_bill","issuefield","cluster_varsellcm2","cluster_varsellcm3","cluster_varsellcm4","cluster_varsellcm5","cluster_varsellcm6","cluster_kamila2","cluster_kamila3","cluster_kamila4","cluster_clustrd2","cluster_clustrd3","cluster_clustrd4","cluster_clustrd5","cluster_clustrd6","cluster_clustrd7")
    afterdummyc_cluster_vars<- c(self$modelvars_clustervars[1]) %>%
      paste0(collapse="|") %>%
      paste0("(",.,")") %>%
      grep(pattern=.,x=overalldf_all_vars,value=TRUE) %>%
      base::setdiff(self$modelvars_clustervars[1])
    #paste0(afterdummyc_vars,collapse="+")
    modelformula<-c(self$modelvars_latentrelated,self$modelvars_ex_catg,self$modelvars_ex_conti) %>%
      c(afterdummyc_cluster_vars, self$modelvars_controllclustervars) %>%
      paste0(., collapse="+") %>%
      paste0("respondopinion~",.) %>%
      as.formula()
    modelformula
  },
  build_svydesign_implist = function(overall_nonagenda_df, save=TRUE) {
    des <- self$overalldf_to_implist_func(overall_nonagenda_df, usinglib="survey") %>%
      survey::svydesign(ids=~1, weight=~myown_wr, data=.)
    if (save==TRUE) {
      save(des, file=self$ordinallogistic_des_filepath)
    }
    des
  },
  # 擬合ordinal logistic（svyolr）；運算量大且吃記憶體
  run_ordinal_logistic_model = function(des=NULL, modelformula=self$get_ordinal_modelformula(), save=TRUE) {
    options(survey.multicore = TRUE)
    if (is.null(des)) {
      load_env <- new.env()
      load(file=self$ordinallogistic_des_filepath, envir=load_env, verbose=TRUE)
      des <- load_env$des
    }
    ordinallogisticmodelonrespondopinion<-survey:::with.svyimputationList(des,survey::svyolr(modelformula),multicore=TRUE)
    if (save==TRUE) {
      while (TRUE) {
        savestatus<-try({save(ordinallogisticmodelonrespondopinion, file=self$ordinallogistic_model_filepath)})
        if(!is(savestatus, 'try-error')) break
      }
    }
    ordinallogisticmodelonrespondopinion
  },
  # 合併多重插補模型結果（Rubin's rules）並輸出csv
  pool_ordinal_logistic_result = function(ordinallogisticmodelonrespondopinion=NULL, writecsv=TRUE) {
    if (is.null(ordinallogisticmodelonrespondopinion)) {
      load_env <- new.env()
      load(file=self$ordinallogistic_model_filepath, envir=load_env, verbose=TRUE)
      ordinallogisticmodelonrespondopinion <- load_env$ordinallogisticmodelonrespondopinion
    }
    poolresult<-micombineresult(ordinallogisticmodelonrespondopinion)
    if (writecsv==TRUE) {
      write.csv(poolresult,file=self$ordinallogistic_result_csv_filepath,row.names = FALSE)
    }
    poolresult
  }
))
