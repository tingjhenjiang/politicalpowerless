# 第Ｏ部份：環境設定 --------------------------------
# /pkg/rproject/R-latest/bin/R
#setwd("/home/u4/dowbatw1133/Documents/vote_record")
running_platform<-if (exists("running_platform")) running_platform else "guicluster"
running_bigdata_computation<-if (exists("running_bigdata_computation")) running_bigdata_computation else FALSE
loadbigdatadf<-if (exists("loadbigdatadf")) loadbigdatadf else TRUE
if (!("benchmarkme" %in% rownames(installed.packages()))) try(install.packages("benchmarkme"))
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-try(paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model))
t_sessioninfo_running_with_cpu_locale<-try(gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2])))
source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/shared_functions.R"), encoding="UTF-8")

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
  dplyr::filter(term %in% !!c(7,9)) %>%
  data.table::as.data.table()
gc(verbose=TRUE)

# 第八部份：大串連資料 ---------------------------------

if (TRUE) {
  overalldf_general_inter_func<-function(targetdf) {
    targetdf %>%
      dplyr::select(-tidyselect::any_of(c("legislator_sex","legislator_age","legislator_party","incumbent"))) %>%
      return()
  }
  overalldf_general_func<-function(targetdf, agendavoting=0, similarities_bet_pp_ly_longdf=similarities_bet_pp_ly_longdf, mergedf_votes_bills_surveyanswer=mergedf_votes_bills_surveyanswer) {
    # targetdf<-dplyr::left_join(complete_survey_dataset, term_to_survey) %>% #Joining, by = "SURVEY"
    #   dplyr::left_join(legislators_with_elections)
    # 板橋市
    # 廣徳里
    # filter(targetdf, admindistrict=="西區") %>% left_join(
    #   filter(legislators_with_elections, term==7, admindistrict=="內湖區")#, adminvillage=="廣徳里"
    # )
    #內湖區 | 内湖區
    #内湖區    端陽里 內湖區    端陽里 瑞陽里
    #板橋市 廣徳里 | 廣德里(legislator_with_elec)
    #蘆竹鄉    内厝村 | 內厝村(legislator_with_elec)
    #蘆竹鄉    瓦薰村 | 瓦窯村(legislator_with_elec)
    #烏日鄉    仁徳村 | 仁德村(legislator_with_elec)
    #西區    磚瑤里 | 磚磘里(legislator_with_elec)
    #嘉義市 西區    西榮里 | 
    #内埔鄉    東寧村 | 內埔鄉(legislator_with_elec)
    #内埔鄉    内田村 | 內埔鄉(legislator_with_elec)
    #崁頂鄉    圍内村 | 圍內村(legislator_with_elec)
    #崁頂鄉    炭頂村 | 崁頂村(legislator_with_elec)
    #屏東市    民權里 | 光榮里、民權里(legislator_with_elec)
    
    #input is complete_survey_dataset, term_to_survey, legislators_with_elections
    #filter(t1, id_wth_survey=="2010overall114104") %>% View()
    #filter(targetdf, id_wth_survey=="2010overall114104") %>% View()
    #dplyr::filter(mergedf_votes_bills_surveyanswer, legislator_name=="蔣孝嚴")
    #anti_join(t2, t1) %>% vhead()
    #neglected_id<-distinct(t1, SURVEY, id) %>% anti_join(t2, .) %>%  distinct(SURVEY, id, admincity, admindistrict, adminvillage, term, legislator_name, billid_myown ) %>%  View()
    #distinct(t1, SURVEY, id) %>% anti_join(t2, .) %>%  distinct(SURVEY, id, admincity, admindistrict, adminvillage, term, legislator_name, billid_myown ) %>%   View()
    #anti_join(t2, t1) %>% distinct(SURVEY, id, admincity, admindistrict, adminvillage, term, legislator_name, billid_myown ) %>% distinct(SURVEY,id) %>% nrow() View()
    #filter(legislators_with_elections, adminvillage=="西湖里", term==7) %>% View()
    #dplyr::filter(mergedf_votes_bills_surveyanswer, is.na())
    #dplyr::filter(myown_vote_record_df, term==7, legislator_name=="蔣孝嚴")
    # dplyr::filter(myown_vote_record_df, term==7, legislator_name=="蔣孝嚴", billid_myown %in% !!c("7-6-0-14-3","7-6-0-14-9","7-6-0-14-24","7-6-0-14-27",
    #                                                                                            "7-6-0-15-8","7-6-0-15-11","7-6-0-15-12"))
    #t1<- targetdf %>%
    targetdf %<>%
      dplyr::left_join(similarities_bet_pp_ly_longdf) %>% #Joining, by = c("imp", "id", "SURVEY", "term", "legislator_name")
      dplyr::inner_join({
        dplyr::filter(mergedf_votes_bills_surveyanswer,research_period==1,pp_agendavoting %in% !!agendavoting) %>%
          dplyr::select(-pp_agendavoting,-pp_lawamendment,-research_period)
      }) %>% #Joining, by = c("SURVEY", "ansv_and_label", "value_on_q_variable", "term", "elec_dist_type", "legislator_name")
      dplyr::filter(!is.na(respondopinion)) %>%
      dplyr::mutate(days_diff_survey_bill=difftime(stdbilldate, stdsurveydate, units = "days")) %>%
      #dplyr::mutate_at("days_diff_survey_bill",~as.numeric(scale(.))) %>%
      dplyr::mutate_at(c("SURVEY","value_on_q_variable","legislator_name","term"),as.factor) %>%
      dplyr::select(-tidyselect::any_of(c("stdsurveydate","stdbilldate","ansv_and_label","variablename_of_issuefield","fpc")))#,"value_on_q_variable"
    targetdfcolnames<-colnames(targetdf)
    widentolongbasiscols<-grep(pattern="billarea",x=targetdfcolnames,value=TRUE)
    reserve_cols<-base::setdiff(targetdfcolnames,widentolongbasiscols)
    data.table::melt(targetdf, id.vars=reserve_cols, variable.name = "variablename_of_issuefield", value.name="issuefield") %>%
       dplyr::select(-tidyselect::any_of(c("variablename_of_issuefield"))) %>%
       dplyr::mutate_at("issuefield",as.factor) %>%
       dplyr::filter(!is.na(issuefield)) %>%
       return()
    #return(targetdf)
  }
  overalldf_to_implist_func<-function(targetdf, usinglib="lavaan", impns=1:5) {
    targetdf %>% lapply(impns, function(needimp,df) {
      return(dplyr::filter(df, imp==!!needimp))
    }, df=.) %>%
      {if (usinglib=="survey") mitools::imputationList(.) else .} %>%
      return()
  }
}


if ({mergingoverlldf<-FALSE; mergingoverlldf & running_bigdata_computation}) {
  load(paste0(dataset_in_scriptsfile_directory, "complete_survey_dataset.RData"), verbose=TRUE)
  complete_survey_dataset %<>% dplyr::filter(newimp %in% 1:6) %>%
    dplyr::mutate(id_wth_survey=paste0(SURVEY,id)) %>%
    dplyr::mutate_at("id_wth_survey", as.factor)
  load(paste0(dataset_in_scriptsfile_directory, "mergedf_votes_bills_surveyanswer.RData"), verbose=TRUE)
  load(paste0(dataset_in_scriptsfile_directory, "legislators_with_elections.RData"), verbose=TRUE)
  #load(paste0(dataset_in_scriptsfile_directory, "legislators_additional_attr.RData"), verbose=TRUE)
  load(paste0(dataset_in_scriptsfile_directory, "similarities_match.RData"), verbose=TRUE)
  similarities_bet_pp_ly_longdf %<>% dplyr::mutate_at("id",as.integer)
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
    
    if (FALSE) { #merge partylist
      dplyr::left_join(complete_survey_dataset, term_to_survey) %>% #Joining, by = "SURVEY"
        dplyr::mutate(elec_dist_type="partylist") %>%
        dplyr::left_join({
          dplyr::filter(legislators_with_elections, elec_dist_type=="partylist") %>%
            dplyr::distinct_at(.vars=dplyr::vars(-admincity,-admindistrict,-adminvillage))
        }) %>% #Joining, by = c("term", "elec_dist_type")
        overalldf_general_inter_func() %>%
        #dplyr::left_join(legislators_additional_attr) %>% #Joining, by = c("term", "legislator_name")
        overalldf_general_func(agendavoting=0,similarities_bet_pp_ly_longdf=similarities_bet_pp_ly_longdf,mergedf_votes_bills_surveyanswer=mergedf_votes_bills_surveyanswer)
    } else {
      data.frame()
    }
    
  )
  overall_nonagenda_df <- overall_nonagenda_df[[1]] %>% #plyr::rbind.fill() %>%
    dplyr::select(-elec_dist_type) %>%
    #dplyr::mutate_at("elec_dist_type",as.factor) %>%
    dplyr::mutate_at("cluster_kamila", as.ordered) %>%
    dplyr::mutate_at("issuefield", ~relevel(., ref = 5)) %>%
    dplyr::mutate_at("seniority", ~seniority+as.numeric(days_diff_survey_bill)/365) %>%
    dplyr::mutate_at("myown_age", ~myown_age+as.numeric(days_diff_survey_bill)/365)  %>%
    dplyr::mutate_at("myown_sex", dplyr::recode_factor, "[1] 男"="male", "[2] 女"="female") %>%
    dplyr::mutate_at("myown_selfid", dplyr::recode_factor, "[1] 台灣閩南人"="fulo", "[2] 台灣客家人"="hakka", "[3] 台灣原住民"="aboriginal", "[4] 大陸各省市(含港澳金馬)"="foreignstate", "[5] 新移民"="newresid") %>%
    dplyr::mutate_at("myown_marriage", dplyr::recode_factor, "[1] 單身且從沒結過婚"="notmarriaged", "[2] 已婚且與配偶同住"="marriaged", "[3] 已婚但沒有與配偶同住"="marriaged_nolivtog", "[4] 同居"="livtog", "[5] 離婚"="divorced", "[6] 分居"="sep", "[7] 配偶去世"="spousedead") %>%
    dplyr::mutate_at("myown_areakind", dplyr::recode_factor, "[1] 都會核心"="metrocore", "[2] 工商市區"="industrial", "[3] 新興市鎮"="newlydeveloped", "[4] 傳統產業市鎮"="traditional", "[5] 低度發展鄉鎮"="underdev", "[6] 高齡化鄉鎮+偏遠鄉鎮"="oldandfar") %>%
    dplyr::mutate_at("issuefield", dplyr::recode_factor, "社會福利與經社文權利"="seright", "財政"="finance", "經濟"="eco", "衛生及環境"="env", "公民政治權"="pocivright", "政府組織及法制"="lawaff", "教育文化科研"="esc", "統獨"="indp", "外交"="diplo", "內政"="interaff") %>%
    dplyr::mutate(days_diff_survey_bill_overallscaled=as.numeric(scale(days_diff_survey_bill))) %>%
    dplyr::mutate(seniority_overallscaled=as.numeric(scale(seniority))) %>%
    dplyr::mutate(myown_age_overallscaled=as.numeric(scale(myown_age))) %>%
    dplyr::mutate(myown_factoredses_overallscaled=as.numeric(scale(myown_factoredses))) %>%
    dplyr::mutate(myown_factoredefficacy_overallscaled=as.numeric(scale(myown_factoredefficacy))) %>%
    dplyr::mutate(myown_factoredparticip_overallscaled=as.numeric(scale(myown_factoredparticip)))  %>%
    dplyr::mutate(myown_factoredparticip_overallscaled_inverse=myown_factoredparticip_overallscaled*-1) %>%
    dplyr::mutate(similarity_distance_overallscaled=as.numeric(scale(similarity_distance))) %>%
    dplyr::mutate(party_pressure_overallscaled=as.numeric(scale(party_pressure))) %>%
    dplyr::mutate_at("adminvillage", ~paste0(admincity,admindistrict,adminvillage)) %>%
    dplyr::mutate_at("admindistrict", ~paste0(admincity,admindistrict)) %>%
    dplyr::mutate_at(c("adminvillage","admindistrict"), as.factor) %>%
    dplyr::mutate_if(is.factor, droplevels)
  #district 780457 14199356
  #partylist 26775408
  #dplyr::filter(overall_nonagenda_df, id==104104, grepl("孝嚴", legislator_name)) %>% View()
  #sapply(overall_nonagenda_df,class)
  #14199356
  
  while (TRUE) {
    savestatus<-try(save(overall_nonagenda_df, file=paste0(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df.RData")))
    if(!is(savestatus, 'try-error')) break
  }
}

# modeling data prepare when bigdata exists --------------------------------

if (running_bigdata_computation & loadbigdatadf) {
  if (FALSE) {
    tinycolumns<-c("myown_wr","admindistrict","adminvillage","billid_myown","cluster_kamila","days_diff_survey_bill","days_diff_survey_bill_overallscaled","elec_dist_type","id_wth_survey","issuefield","legislator_name","myown_age_overallscaled","myown_areakind","myown_factoredparticip_overallscaled","myown_factoredses_overallscaled","myown_income_scaled","myown_marriage","myown_selfid","myown_sex","myown_religion","seniority_overallscaled","party_pressure_overallscaled","partyGroup","partysize","adminparty","respondopinion","similarity_distance_overallscaled","SURVEY","newimp") %>%
      base::setdiff(c("elec_dist_type","admincity"))
    overall_nonagenda_df %<>% dplyr::select(!!tinycolumns)
    try(save(overall_nonagenda_df, file=paste0(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df_tiny.RData")))
  }
  load(file=paste0(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df_tiny.RData"), verbose=TRUE)
  #load(file=paste0(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df.RData"), verbose=TRUE)
  #load(file=paste0(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df_fullydummycoded.RData"), verbose=TRUE)
  #load(file=paste0(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df_dummycoded.RData"), verbose=TRUE)
}

modelvars_ex_conti<-c("myown_age_overallscaled","similarity_distance_overallscaled","party_pressure_overallscaled","seniority_overallscaled","days_diff_survey_bill_overallscaled") #%>%
  #c("policyidealpoint_cos_similarity_to_median","policyidealpoint_eucli_distance_to_median_scaled")
modelvars_ex_catg<-c("myown_sex","myown_areakind","myown_selfid","myown_marriage","issuefield","myown_religion","myown_areakind") %>% #,"adminparty"
  c("partysize") #"elec_dist_type"
modelvars_latentrelated<-c("myown_factoredses_overallscaled","myown_factoredparticip_overallscaled") #,"myown_factoredefficacy"
modelvars_clustervars<-c("cluster_kamila")
modelvars_controllclustervars<-c("SURVEY","myown_areakind","partyGroup")
if (running_bigdata_computation & loadbigdatadf) {
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
  dummycodingvars<-if (partlydummycoding) base::intersect(dummyc_vars,modelvars_clustervars) else base::union(dummyc_vars,modelvars_clustervars)
  if (partlydummycoding) {
    overall_nonagenda_df <- dplyr::bind_cols(
      dplyr::select(overall_nonagenda_df, !!modelvars_clustervars),
      dummycode_of_a_dataframe(overall_nonagenda_df, catgvars=modelvars_clustervars)
    )
    savedummycodeddffilename<-paste0(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df_dummycoded.RData")
  } else {
    overall_nonagenda_df <- #dplyr::bind_cols(
      #dplyr::select(overall_nonagenda_df, !!modelvars_controllclustervars),
      dummycode_of_a_dataframe(overall_nonagenda_df, catgvars=dummycodingvars)
    #)
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