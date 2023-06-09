# 第Ｏ部份：環境設定 --------------------------------
if (!("benchmarkme" %in% rownames(installed.packages()))) try(install.packages("benchmarkme"))
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-try(paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model))
t_sessioninfo_running_with_cpu_locale<-try(gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2])))
source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/shared_functions.R"), encoding="UTF-8")

# 第一部份：計算受訪者與立法委員的相似性 --------------------------------
#load(file=paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt_lca_clustering.RData"), verbose=TRUE)
load(file=paste0(save_dataset_in_scriptsfile_directory,"miced_survey_2surveysonly_mirt_lca_clustering.RData"), verbose=TRUE)
load(file=paste0(dataset_in_scriptsfile_directory, "legislators_with_elections.RData"), verbose=TRUE)
load(file=paste0(dataset_in_scriptsfile_directory, "legislators_additional_attr.RData"), verbose=TRUE)
people_legislator_match <- data.frame(
  key=c("2004citizen","2004citizen","2010env","2010env","2010overall","2010overall","2016citizen"),
  term=c(5,6,7,8,7,8,9), stringsAsFactors=FALSE) %>%
  cbind(., imp = rep(imputation_sample_i_s, each = nrow(.))) %>%
  dplyr::mutate(pp_ly_match = paste0(key,"_term",term,"_imp",imp) ) %>%
  dplyr::filter(key %in% names(survey_data_imputed), term %in% c(7,9))
#legislators_with_elections:  "term" "legislator_name" "legislator_sex" "legislator_party" "partyGroup" "areaName" "degree" "experience" "servingdayslong_in_this_term" "seniority" "legislator_age" "education" "incumbent" "wonelection" "election_party" "electionarea" "admincity" "admindistrict" "adminvillage" "elec_dist_type"
#legislators_additional_attr: "term" "legislator_name" "legislator_eduyr" "legislator_occp" "legislator_ses" "legislator_ethnicity"
#"myown_sex" "myown_eduyr" "myown_ses" "myown_age" "myown_selfid"
legislators_sim_basis <- dplyr::distinct(legislators_with_elections, term, legislator_name, legislator_sex, legislator_age) %>%
  dplyr::left_join(legislators_additional_attr)


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
#save(similarities_bet_pp_ly_longdf, file = paste0(dataset_in_scriptsfile_directory, "similarities_match.RData"))

#legislators_with_elections %<>% dplyr::select(-legislator_party,-legislator_age)
#save(legislators_with_elections, file=paste0(dataset_in_scriptsfile_directory, "legislators_with_elections.RData"))