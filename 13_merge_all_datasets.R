# 第Ｏ部份：環境設定 --------------------------------
# /pkg/rproject/R-latest/bin/R
#setwd("/home/u4/dowbatw1133/Documents/vote_record")
running_platform<-if (exists("running_platform")) running_platform else "guicluster"
running_bigdata_computation<-if (exists("running_bigdata_computation")) running_bigdata_computation else FALSE
if (!("benchmarkme" %in% rownames(installed.packages()))) try(install.packages("benchmarkme"))
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-try(paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model))
t_sessioninfo_running_with_cpu_locale<-try(gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2])))
source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/shared_functions.R"), encoding="UTF-8")
save_dataset_in_scriptsfile_directory<-if (running_platform=="guicluster") dataset_in_scriptsfile_directory else "/work1/dowbatw1133/"

#選舉資料
# terms<-c(5,6,7,8,9)
# overall_elec_dist_types<-c('district','ab_m','ab_plain','partylist')
# supplement_election_termseven<-c('supp2009miaoli1','supp2009nantou1','supp2009yunlin2','supp2009taipei6','supp2010taichungs3','supp2010hualian','supp2010taoyuan2','supp2010taoyuan3','supp2010hsinchus','supp2010chiayi2','supp2010taitung','supp2011tainan4','supp2011kaoshiung4')
# survey_time_range <- as.data.frame(list(yrmonth=c('099/07', '099/11', '099/12', '100/01', '100/04', '100/06', '105/09', '105/10', '105/11', '105/12', '106/01', '106/04', '106/05')))
# survey_time_range <- as.data.frame(list(yrmonth=c()))
# survey_time_range <- list(
#   "2004citizen"=data.frame("SURVEY"="2004citizen","yrmonth"=c("093/07","093/08","093/09","093/10","093/11","093/12","094/01","094/02","094/03","094/04","094/05","094/06","094/07","094/08","094/09","094/10","094/11","094/12","095/01","095/02","095/03","095/04","095/05","095/06","095/07","095/08","095/09")), #,"095/10","095/11","095/12"
#   "2010env"=data.frame("SURVEY"="2010env","yrmonth"=c("099/07","099/08","099/09","099/10","099/11","099/12","100/01","100/02","100/03","100/04","100/05","100/06","100/07","100/08","100/09","100/10","100/11","100/12","101/01","101/02","101/03","101/04","101/05","101/06","101/07","101/08")),
#   "2010overall"=data.frame("SURVEY"="2010overall","yrmonth"=c("099/07","099/08","099/09","099/10","099/11","099/12","100/01","100/02","100/03","100/04","100/05","100/06","100/07","100/08","100/09","100/10","100/11","100/12","101/01","101/02","101/03","101/04","101/05","101/06","101/07","101/08","101/09","101/10","101/11")),
#   "2016citizen"=data.frame("SURVEY"="2016citizen","yrmonth"=c("105/09","105/11","105/12","106/01","106/04","106/05","106/06","106/08","106/10","106/11","106/12","107/01","107/03","107/04","107/05","107/06","107/07","107/08","107/09","107/10","107/11"))
# )
# survey_time_range_df <- plyr::rbind.fill(survey_time_range)

term_to_survey <- data.frame("term"=c(5,6,7,7,8,8,9), "SURVEY"=c("2004citizen","2004citizen","2010env","2010overall","2010env","2010overall","2016citizen")) %>%
  data.table::as.data.table()
gc(verbose=TRUE)

# 第八部份：大串連資料 ---------------------------------

if (TRUE) {
  micombineresult<-function(mimodel) {
    poolresult<-mitools:::summary.MIresult(mitools::MIcombine(mimodel)) %>%
      dplyr::select(missInfo) %>%
      dplyr::bind_cols( mice::pool(mimodel) %>%
                          mice:::summary.mipo(conf.int=TRUE),.)
    return(poolresult)
  }
  dummycode_of_a_dataframe<-function(df,catgvars=c()) {
    detectedcatgvars<-custom_pickcolnames_accordingtoclass(df,needclass="factor")
    detectedcatgvars<-detectedcatgvars[(sapply(dplyr::select(df,!!detectedcatgvars), nlevels ))>=2]
    catgvars<-if (length(catgvars)==0) detectedcatgvars else base::intersect(catgvars, detectedcatgvars)
    if (length(catgvars)==0) return(df)
    dplyr::bind_cols(dplyr::select(df, -!!catgvars), custom_parallel_lapply(catgvars, function(factorvar,df,...) {
      psych::dummy.code(dplyr::pull(df,!!factorvar)) %>%
        {.[,gtools::mixedsort(colnames(.))]} %>%
        {magrittr::set_colnames(., paste0(factorvar,colnames(.)))} %>%
        {(.[,2:ncol(.),drop=FALSE])} %>%
        return()
    }, df=df, method=parallel_method) %>%
      data.frame() ) %>%
      return()
  }
  overalldf_general_inter_func<-function(targetdf) {
    targetdf %>%
      dplyr::select(-tidyselect::any_of(c("admincity","admindistrict","adminvillage","legislator_sex","legislator_age","legislator_party","incumbent"))) %>%
      return()
  }
  overalldf_general_func<-function(targetdf, agendavoting=0, similarities_bet_pp_ly_longdf=similarities_bet_pp_ly_longdf, mergedf_votes_bills_surveyanswer=mergedf_votes_bills_surveyanswer) {
    targetdf %<>%
      dplyr::left_join(similarities_bet_pp_ly_longdf) %>% #Joining, by = c("imp", "id", "SURVEY", "term", "legislator_name")
      dplyr::inner_join({
        dplyr::filter(mergedf_votes_bills_surveyanswer,research_period==1,pp_agendavoting %in% !!agendavoting) %>%
          dplyr::select(-pp_agendavoting,-pp_lawamendment,-research_period)
      }) %>% #Joining, by = c("SURVEY", "ansv_and_label", "value_on_q_variable", "term", "elec_dist_type", "legislator_name")
      dplyr::filter(!is.na(respondopinion)) %>%
      dplyr::mutate(days_diff_survey_bill=difftime(stdbilldate, stdsurveydate, units = "days")) %>%
      dplyr::mutate_at("days_diff_survey_bill",~as.numeric(scale(.))) %>%
      dplyr::mutate_at(c("SURVEY","value_on_q_variable","legislator_name","term"),as.factor) %>%
      dplyr::select(-tidyselect::any_of(c("stdsurveydate","stdbilldate","ansv_and_label","variablename_of_issuefield","value_on_q_variable")))#
    
    targetdfcolnames<-colnames(targetdf)
    reserve_cols<-base::setdiff(targetdfcolnames,c("issue_field1","issue_field2"))
    data.table::melt(targetdf, id.vars=reserve_cols, variable.name = "variablename_of_issuefield", value.name="issuefield") %>%
      dplyr::select(-tidyselect::any_of(c("variablename_of_issuefield"))) %>%
      dplyr::mutate_at("issuefield",as.factor) %>%
      dplyr::filter(!is.na(issuefield)) %>%
      return()
  }
  overalldf_to_implist_func<-function(targetdf, usinglib="lavaan") {
    targetdf %>% lapply(1:5, function(needimp,df) {
      return(dplyr::filter(df, imp==!!needimp))
    }, df=.) %>%
      {if (usinglib=="survey") mitools::imputationList(.) else .} %>%
      return()
  }
}


if ({mergingoverlldf<-FALSE; mergingoverlldf & running_bigdata_computation}) {
  load(paste0(dataset_in_scriptsfile_directory, "complete_survey_dataset.RData"), verbose=TRUE)
  load(paste0(dataset_in_scriptsfile_directory, "mergedf_votes_bills_surveyanswer.RData"), verbose=TRUE)
  load(paste0(dataset_in_scriptsfile_directory, "legislators_with_elections.RData"), verbose=TRUE)
  load(paste0(dataset_in_scriptsfile_directory, "legislators_additional_attr.RData"), verbose=TRUE)
  load(paste0(dataset_in_scriptsfile_directory, "similarities_match.RData"), verbose=TRUE)
}

if (FALSE) {
  complete_survey_dataset %<>% dtplyr::lazy_dt()
  mergedf_votes_bills_surveyanswer %<>% dtplyr::lazy_dt()
  legislators_with_elections %<>% dtplyr::lazy_dt()
  legislators_additional_attr %<>% dtplyr::lazy_dt()
  similarities_bet_pp_ly_longdf %<>% dtplyr::lazy_dt()
}

# overalldf_district<-merge(complete_survey_dataset, term_to_survey, all.x=TRUE, allow.cartesian=TRUE, by = "SURVEY") %>%
#   merge(legislators_with_elections, all.x=TRUE, allow.cartesian=TRUE, by = c("admincity", "admindistrict", "adminvillage", "term")) %>%
#   merge(legislators_additional_attr, all.x=TRUE, allow.cartesian=TRUE, by = c("term", "legislator_name")) %>%
#   merge(similarities_bet_pp_ly_longdf, all.x=TRUE, allow.cartesian=TRUE, by = c("id", "SURVEY", "term", "legislator_name")) %>%
#   merge(mergedf_votes_bills_surveyanswer, all.x=TRUE, allow.cartesian=TRUE, by = c("SURVEY", "ansv_and_label", "value_on_q_variable", "term", "legislator_name", "legislator_sex", "legislator_party", "seniority", "legislator_age", "incumbent", "elec_dist_type")) %>%
#   overalldf_general_func() #64.7GB

# merge of load data --------------------------------

if (mergingoverlldf & running_bigdata_computation) {
  overall_nonagenda_df<-list(
    dplyr::left_join(complete_survey_dataset, term_to_survey) %>% #Joining, by = "SURVEY"
      dplyr::left_join(legislators_with_elections) %>% #Joining, by = c("admincity", "admindistrict", "adminvillage", "term")
      overalldf_general_inter_func() %>%
      #dplyr::left_join(legislators_additional_attr) %>% #Joining, by = c("term", "legislator_name")
      overalldf_general_func(agendavoting=0,similarities_bet_pp_ly_longdf=similarities_bet_pp_ly_longdf,mergedf_votes_bills_surveyanswer=mergedf_votes_bills_surveyanswer),
    
    dplyr::left_join(complete_survey_dataset, term_to_survey) %>% #Joining, by = "SURVEY"
      dplyr::mutate(elec_dist_type="partylist") %>%
      dplyr::left_join({
        dplyr::filter(legislators_with_elections, elec_dist_type=="partylist") %>%
          dplyr::distinct_at(.vars=vars(-admincity,-admindistrict,-adminvillage))
      }) %>% #Joining, by = c("term", "elec_dist_type")
      overalldf_general_inter_func() %>%
      #dplyr::left_join(legislators_additional_attr) %>% #Joining, by = c("term", "legislator_name")
      overalldf_general_func(agendavoting=0,similarities_bet_pp_ly_longdf=similarities_bet_pp_ly_longdf,mergedf_votes_bills_surveyanswer=mergedf_votes_bills_surveyanswer)
  )
  overall_nonagenda_df %<>% dplyr::bind_rows() %>%
    dplyr::mutate_at("elec_dist_type",as.factor) %>%
    dplyr::mutate_at("elec_dist_type", droplevels) %>%
    dplyr::mutate_at(c("cluster_clustrd","cluster_varsellcm","cluster_kamila"), as.ordered) #11.6GB
  #save(overall_nonagenda_df, file=paste0(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df.RData"))
}

# modeling data prepare when bigdata exists --------------------------------

if (running_bigdata_computation) {
  #load(file=paste0(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df.RData"), verbose=TRUE)
  #load(file=paste0(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df_fullydummycoded.RData"), verbose=TRUE)
  load(file=paste0(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df_dummycoded.RData"), verbose=TRUE)
}

modelvars_ex_conti<-c("myown_age","similarity_distance","party_pressure","seniority","days_diff_survey_bill")
modelvars_ex_catg<-c("myown_sex","myown_selfid","myown_marriage","adminparty","issuefield") %>%
  c("elec_dist_type")
modelvars_latentrelated<-c("myown_factoredses","myown_factoredefficacy","myown_factoredparticip")
modelvars_clustervars<-c("cluster_varsellcm","cluster_kamila","cluster_clustrd")
modelvars_controllclustervars<-c("term","myown_areakind")
if (running_bigdata_computation) {
  #if(!is(overalldf, 'try-error')) {
  #  overalldf_to_implist_func() #34.2GB
  #litrature
  #Regression Models for Ordinal Responses: A Review of Methods and Applications
  #https://pubmed.ncbi.nlm.nih.gov/9447413/
  #software
  #http://r-survey.r-forge.r-project.org/survey/svymi.html
  #http://docs.zeligproject.org/articles/zelig_logitsurvey.html
  #https://zeligproject.org/
  #https://stats.stackexchange.com/questions/69130/how-to-get-pooled-p-values-on-tests-done-in-multiple-imputed-datasets
  #https://stats.stackexchange.com/questions/89204/fitting-multilevel-models-to-complex-survey-data-in-r
  #https://www.hcp.med.harvard.edu/statistics/survey-soft/
  #https://cran.r-project.org/web/packages/BIFIEsurvey/index.html
  #https://cran.r-project.org/web/packages/brms/index.html
  #Handle Missing Values with brms
  #https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html

  #myown_areakind
  #sample_n_for_df<-sample(1:nrow(overall_nonagenda_df),50000)
  #overall_nonagenda_df_sampled<-overall_nonagenda_df[sample_n_for_df,]
  #save(overall_nonagenda_df_sampled, file=paste0(dataset_in_scriptsfile_directory, "overall_nonagenda_df_sampled.RData"))
  #load(file=paste0(dataset_in_scriptsfile_directory, "overall_nonagenda_df_sampled.RData"), verbose=TRUE)
  #overall_nonagenda_df<-overall_nonagenda_df_sampled
  dummyc_vars<-custom_pickcolnames_accordingtoclass(overall_nonagenda_df, needclass="factor") %>%
    base::intersect(c(modelvars_ex_catg,modelvars_clustervars,modelvars_controllclustervars))
}

# dummy coding bigdata --------------------------------
if ({dummycoding<-FALSE; dummycoding & running_platform=="guicluster"}) {
  partlydummycoding<-FALSE
  partlydummycoding<-TRUE
  dummycodingvars<-if (partlydummycoding) base::intersect(dummyc_vars,modelvars_clustervars) else dummyc_vars
  if (partlydummycoding) {
    overall_nonagenda_df <- dplyr::bind_cols(
      dplyr::select(overall_nonagenda_df, !!modelvars_clustervars),
      dummycode_of_a_dataframe(overall_nonagenda_df, catgvars=modelvars_clustervars)
    )
    savedummycodeddffilename<-paste0(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df_dummycoded.RData")
  } else {
    overall_nonagenda_df <- dplyr::bind_cols(
      dplyr::select(overall_nonagenda_df, !!modelvars_controllclustervars),
      dummycode_of_a_dataframe(., catgvars=dummycodingvars)
    )
    savedummycodeddffilename<-paste0(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df_fullydummycoded.RData")
  }
  while (TRUE) {
    savestatus<-try(save(overall_nonagenda_df, file=savedummycodeddffilename))
    if(!is(savestatus, 'try-error')) break
  }
}





if (FALSE) {
  des<-list()
  responseopinion_model_overall<-list()
  save(des, responseopinion_model_overall, file=paste0(dataset_in_scriptsfile_directory, "responseopinion_model_overall.RData"))
}

# [1] "imp"                    "myown_sex"              "myown_age"             
# [4] "myown_selfid"           "myown_marriage"         "id"                    
# [7] "SURVEY"                 "myown_areakind"         "myown_wr"              
# [10] "myown_factoredses"      "myown_factoredefficacy" "myown_factoredparticip"
# [13] "cluster_varsellcm"      "cluster_clustrd"        "cluster_kamila"        
# [16] "term"                   "legislator_name"        "partyGroup"            
# [19] "seniority"              "elec_dist_type"         "similarity_distance"   
# [22] "billid_myown"           "party_pressure"         "adminparty"            
# [25] "salient"                "variable_on_q"          "opinionstrength"       
# [28] "respondopinion"         "success_on_bill"        "days_diff_survey_bill" 
# [31] "issuefield"
# overalldf_district %<>% as.data.frame()
# overalldf_partylist %<>% as.data.frame()



##注意有遺漏的部分委員
#list(
#  complete_survey_dataset,
#  mergedf_votes_bills_surveyanswer,
#  legislators_with_election
#  inner_join(mergedf_votes_bills_surveyanswer, distinct(survey_time_range[['2004citizen']],yrmonth) ),
#  inner_join(mergedf_votes_bills_surveyanswer, distinct(survey_time_range[['2004citizen']],yrmonth) ) %>%
#  left_join(test_tiny_legislators_with_election, by=c("term","legislator_name"))
#) %>%
#  sapply(nrow)
#有多個村里會重複所以join時會膨脹
# overall_district_legislators_only_power_dfdata <- custom_parallel_lapply(survey_data_title, function(needsurvey,...) {
#   filter(complete_survey_dataset, SURVEY==needsurvey) %>% #135654
#     left_join(term_to_survey) %>% #135654
#     left_join(legislators_with_elections) %>% #135654
#     left_join(legislators_additional_attr) %>%
#     mutate(gap_eduyr=NA,gap_ses=NA,gap_sex=NA,gap_ethnicity=NA,gap_age=NA) %>%
#     mutate_cond(myown_selfid==legislator_ethnicity, gap_ethnicity=0) %>%
#     mutate_cond(myown_selfid!=legislator_ethnicity, gap_ethnicity=1) %>%
#     mutate_cond(!is.na(myown_age), gap_age=abs(myown_age-legislator_age)) %>%
#     mutate_cond(!is.na(myown_eduyr), gap_eduyr=abs(myown_eduyr-legislator_eduyr)) %>%
#     mutate_cond(!is.na(myown_ses), gap_ses=abs(myown_ses-legislator_ses)) %>%
#     mutate_cond((myown_sex=="[1] 男" & legislator_sex=="男") | (myown_sex=="[2] 女" & legislator_sex=="女"), gap_sex=0) %>%
#     mutate_cond((myown_sex=="[2] 女" & legislator_sex=="男") | (myown_sex=="[1] 男" & legislator_sex=="女"), gap_sex=1) %>%
#     mutate_at(c("gap_sex","gap_ethnicity"), as.factor) %>% #135654
#     left_join(mergedf_votes_bills_surveyanswer) %>% #346050
#     inner_join(survey_time_range_df) %>% #231990  %>% nrow()
#     mutate(days_diff_survey_bill=difftime(stdbilldate, stdsurveydate, units = "days"))
#   }, exportlib=c("base","magrittr","dplyr","parallel"),
#   exportvar=c("complete_survey_dataset","custom_parallel_lapply","mergedf_votes_bills_surveyanswer","legislators_with_elections","legislators_additional_attr","term_to_survey","survey_time_range_df","mutate_cond"))
#設定對照政黨
# for (i in 1:length(rulingparty)) {
#   party<-rulingparty[[i]]
#   overall_district_legislators_only_power_dfdata[[i]]$legislator_party %<>% relevel(ref=party)
# }
# overall_partylist_legislators_only_power_dfdata <- lapply(survey_data_title, function(needsurvey,...) {
#   filter(complete_survey_dataset, SURVEY==needsurvey) %>% #135654
#     left_join(term_to_survey) %>% #135654
#     left_join({
#       filter(legislators_with_elections,elec_dist_type=="partylist") %>%
#         select(-admincity,-admindistrict,-adminvillage)
#       }) %>% #135654
#     left_join(legislators_additional_attr) %>%
#     mutate(gap_eduyr=NA,gap_ses=NA,gap_sex=NA,gap_ethnicity=NA,gap_age=NA) %>%
#     mutate_cond(myown_selfid==legislator_ethnicity, gap_ethnicity=0) %>%
#     mutate_cond(myown_selfid!=legislator_ethnicity, gap_ethnicity=1) %>%
#     mutate_cond(!is.na(myown_age), gap_age=abs(myown_age-legislator_age)) %>%
#     mutate_cond(!is.na(myown_eduyr), gap_eduyr=abs(myown_eduyr-legislator_eduyr)) %>%
#     mutate_cond(!is.na(myown_ses), gap_ses=abs(myown_ses-legislator_ses)) %>%
#     mutate_cond((myown_sex=="[1] 男" & legislator_sex=="男") | (myown_sex=="[2] 女" & legislator_sex=="女"), gap_sex=0) %>%
#     mutate_cond((myown_sex=="[2] 女" & legislator_sex=="男") | (myown_sex=="[1] 男" & legislator_sex=="女"), gap_sex=1) %>%
#     mutate_at(c("gap_sex","gap_ethnicity"), as.factor) %>% #135654
#     left_join({
#       inner_join(mergedf_votes_bills_surveyanswer,survey_time_range_df)
#     }) %>% #  %>% nrow()
#     mutate(days_diff_survey_bill=difftime(stdbilldate, stdsurveydate, units = "days"))
# }, exportlib=c("base","magrittr","dplyr","parallel"),
# exportvar=c("complete_survey_dataset","custom_parallel_lapply","mergedf_votes_bills_surveyanswer","legislators_with_election","legislators_additional_attr","term_to_survey","survey_time_range_df","mutate_cond"))
# lapply(survey_data_title[2], function(SURVEY) {SURVEY})
#只有針對議案的決定，而非有無代理
# testdf <- mutate_at(complete_survey_dataset,"term", as.numeric) %>%
#   inner_join(only_bill_to_survey_information)