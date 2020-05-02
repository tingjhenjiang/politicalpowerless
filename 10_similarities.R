# 第Ｏ部份：環境設定 --------------------------------
if (!("benchmarkme" %in% rownames(installed.packages()))) install.packages("benchmarkme")
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
t_sessioninfo_running_with_cpu_locale<-gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2]))
source(file = "shared_functions.R", encoding="UTF-8")

# 第一部份：計算受訪者與立法委員的相似性 --------------------------------
load(file=paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt_lca.RData"), verbose=TRUE)
load(file=paste0(dataset_in_scriptsfile_directory, "legislators_with_elections.RData"), verbose=TRUE)
load(file=paste0(dataset_in_scriptsfile_directory, "legislators_additional_attr.RData"), verbose=TRUE)
people_legialator_match <- data.frame(
  key=c("2004citizen","2004citizen","2010env","2010env","2010overall","2010overall","2016citizen"),
  term=c(5,6,7,8,7,8,9)) %>%
  cbind(., imp = rep(imputation_sample_i_s, each = nrow(.))) %>%
  dplyr::mutate(pp_ly_match = paste0(key,"_term",term,"_imp",imp) )
#legislators_with_elections:  "term" "legislator_name" "legislator_sex" "legislator_party" "partyGroup" "areaName" "degree" "experience" "servingdayslong_in_this_term" "seniority" "legislator_age" "education" "incumbent" "wonelection" "election_party" "electionarea" "admincity" "admindistrict" "adminvillage" "elec_dist_type"
#legislators_additional_attr: "term" "legislator_name" "legislator_eduyr" "legislator_occp" "legislator_ses" "legislator_ethnicity"
#"myown_sex" "myown_eduyr" "myown_ses" "myown_age" "myown_selfid"
legislators_sim_basis <- dplyr::distinct(legislators_with_elections, term, legislator_name, legislator_sex, legislator_age) %>%
  dplyr::left_join(dplyr::select(legislators_additional_attr, -legislator_occp))
distance_dissimilarities_pply<-mapply(function(key,term,imp,survey_data_imputed,legislators_sim_basis) {
  targetcolnames<-c("sex","eduyr","ses","age","selfid")
  x<-survey_data_imputed[[key]][survey_data_imputed[[key]]$.imp==imp,c("myown_sex","myown_eduyr","myown_ses","myown_age","myown_selfid")] %>%
    dplyr::mutate_at("myown_sex", as.character) %>%
    mutate_cond(customgrepl(myown_sex,"男"), myown_sex="男") %>%
    mutate_cond(customgrepl(myown_sex,"女"), myown_sex="女") %>%
    dplyr::mutate_at("myown_sex", as.factor) %>%
    dplyr::mutate_at("myown_selfid", as.character) %>%
    magrittr::set_colnames(targetcolnames)
  y<-legislators_sim_basis[legislators_sim_basis$term==term,c("legislator_sex","legislator_eduyr","legislator_ses","legislator_age","legislator_ethnicity")] %>%
    dplyr::mutate_at("legislator_ethnicity", as.character) %>%
    magrittr::set_colnames(targetcolnames) %>%
    as.data.frame()
  similarities <- StatMatch::gower.dist(x, y, rngs=NULL, KR.corr=TRUE, var.weights = NULL) %>%
    magrittr::set_colnames(legislators_sim_basis[legislators_sim_basis$term==term,]$legislator_name) %>%
    magrittr::set_rownames(survey_data_imputed[[key]][survey_data_imputed[[key]]$.imp==imp,]$.id)
  return(similarities)
}, key=people_legialator_match$key, term=people_legialator_match$term, imp=people_legialator_match$imp, MoreArgs = list(survey_data_imputed,legislators_sim_basis), SIMPLIFY=FALSE) %>%
  set_names(people_legialator_match$pp_ly_match)
