source(file = "09_preprocessing_clustering.R")
# 第十部份：計算受訪者與立法委員的相似性 --------------------------------
similarities_class <- R6::R6Class("similarities", inherit=clustering_process_class, public = list(
  legislators_with_elections_rdata_filepath = NULL,
  legislators_additional_attr_filepath = NULL,
  similarities_match_filepath = NULL,
  initialize = function(dataset_in_scriptsfile_directory="/mnt", filespath="/mnt", dataset_file_directory="/mnt", debug_func_mode=TRUE) {
    super$initialize(dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory, filespath=filespath, dataset_file_directory=dataset_file_directory, debug_func_mode=debug_func_mode)
    self$legislators_with_elections_rdata_filepath <- file.path(dataset_in_scriptsfile_directory, "legislators_with_elections.RData")
    self$legislators_additional_attr_filepath <- file.path(dataset_in_scriptsfile_directory, "legislators_additional_attr.RData")
    self$similarities_match_filepath <- file.path(dataset_in_scriptsfile_directory, "similarities_match.RData")
  },
  get_people_legislator_match = function(survey_data_imputed, imps=imputation_sample_i_s) {
    data.frame(
      key=c("2004citizen","2004citizen","2010env","2010env","2010overall","2010overall","2016citizen"),
      term=c(5,6,7,8,7,8,9), stringsAsFactors=FALSE) %>%
      cbind(., imp = rep(imps, each = nrow(.))) %>%
      dplyr::mutate(pp_ly_match = paste0(key,"_term",term,"_imp",imp) ) %>%
      dplyr::filter(key %in% names(survey_data_imputed), term %in% c(7,9))
  },
  #legislators_with_elections:  "term" "legislator_name" "legislator_sex" "legislator_party" "partyGroup" "areaName" "degree" "experience" "servingdayslong_in_this_term" "seniority" "legislator_age" "education" "incumbent" "wonelection" "election_party" "electionarea" "admincity" "admindistrict" "adminvillage" "elec_dist_type"
  #legislators_additional_attr: "term" "legislator_name" "legislator_eduyr" "legislator_occp" "legislator_ses" "legislator_ethnicity"
  #"myown_sex" "myown_eduyr" "myown_ses" "myown_age" "myown_selfid"
  get_legislators_sim_basis = function() {
    load_env <- new.env()
    load(file=self$legislators_with_elections_rdata_filepath, envir=load_env, verbose=TRUE)
    load(file=self$legislators_additional_attr_filepath, envir=load_env, verbose=TRUE)
    dplyr::distinct(load_env$legislators_with_elections, term, legislator_name, legislator_sex, legislator_age) %>%
      dplyr::left_join(load_env$legislators_additional_attr)
    #legislators_with_elections %<>% dplyr::select(-legislator_party,-legislator_age)
    #save(legislators_with_elections, file=self$legislators_with_elections_rdata_filepath)
  },
  # 以Gower distance計算每位受訪者與該屆立委的相似程度（長表）
  compute_similarities_bet_pp_ly_longdf = function(survey_data_imputed=NULL, save=FALSE) {
    if (is.null(survey_data_imputed)) {
      #load(file=paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt_lca_clustering.RData"), verbose=TRUE)
      survey_data_imputed <- self$get_survey_data_imputed_stage(stage="mirt_lca_clustering")
    }
    legislators_sim_basis <- self$get_legislators_sim_basis()
    people_legislator_match <- self$get_people_legislator_match(survey_data_imputed)
    needimps<-custom_ret_appro_kamila_clustering_parameters() %>%
      dplyr::rename(SURVEY=survey)
    similarities_bet_pp_ly_longdf<-custom_apply_thr_argdf(people_legislator_match, "pp_ly_match", function(fikey, loopargdf, datadf, legislators_sim_basis=legislators_sim_basis, ...) {
      needrow<-dplyr::filter(loopargdf, pp_ly_match==!!fikey)
      key<-needrow$key
      imp<-needrow$imp
      term<-needrow$term
      targetcolnames<-c("sex","eduyr","ses","age","selfid")
      basissurveydf<-survey_data_imputed[[key]] %>%
        .[.$.imp==imp,]
      respondentids<-basissurveydf$id
      x<-basissurveydf[,c("myown_sex","myown_eduyr","myown_ses","myown_age","myown_selfid")] %>%
        dplyr::mutate_at("myown_sex", as.character) %>%
        mutate_cond(customgrepl(myown_sex,"男"), myown_sex="男") %>%
        mutate_cond(customgrepl(myown_sex,"女"), myown_sex="女") %>%
        dplyr::mutate_at("myown_sex", as.factor) %>%
        dplyr::mutate_at("myown_selfid", as.character) %>%
        magrittr::set_colnames(targetcolnames) %>%
        magrittr::set_rownames(respondentids)
      y<-legislators_sim_basis[legislators_sim_basis$term==term,c("legislator_sex","legislator_eduyr","legislator_ses","legislator_age","legislator_ethnicity")] %>%
        dplyr::mutate_at("legislator_ethnicity", as.character) %>%
        magrittr::set_colnames(targetcolnames) %>%
        as.data.frame()
      similarities <- try({StatMatch::gower.dist(x, y, rngs=NULL, KR.corr=TRUE, var.weights = NULL) %>%
          magrittr::set_colnames(legislators_sim_basis[legislators_sim_basis$term==term,]$legislator_name) %>%
          magrittr::set_rownames(respondentids)})
      t<-lapply(1:nrow(similarities), function(rowi, needmatrix, needrow, ...) {
        data.frame(needrow, id=rownames(needmatrix)[rowi], legislator_name=colnames(needmatrix), similarity_distance=needmatrix[rowi,], stringsAsFactors=FALSE)
      }, needmatrix=similarities, needrow=needrow) %>%
        plyr::rbind.fill() %>%
        dplyr::rename(SURVEY=key) %>%
        dplyr::select(-pp_ly_match) %>%
        dplyr::mutate_at("legislator_name", as.factor)
      return(t)
    }, datadf=survey_data_imputed, legislators_sim_basis=legislators_sim_basis, method=parallel_method) %>%
      plyr::rbind.fill() %>%
      dplyr::semi_join(needimps) %>%
      dplyr::left_join(needimps)# %>% dplyr::filter(newimp %in% 1)
    #similarities_bet_pp_ly_longdf %<>% data.table::as.data.table()
    if (save==TRUE) {
      save(similarities_bet_pp_ly_longdf, file = self$similarities_match_filepath)
    }
    similarities_bet_pp_ly_longdf
  },
  get_similarities_bet_pp_ly_longdf = function(loadExisted=TRUE, save=FALSE, survey_data_imputed=NULL) {
    if (loadExisted==TRUE) {
      load_env <- new.env()
      load(file=self$similarities_match_filepath, envir=load_env, verbose=TRUE)
      return(load_env$similarities_bet_pp_ly_longdf)
    }
    self$compute_similarities_bet_pp_ly_longdf(survey_data_imputed=survey_data_imputed, save=save)
  }
))
