source(file = "12_preprocessing_transform_clean_design_survey_data.R")
# 第八部份：大串連資料 ---------------------------------
# /pkg/rproject/R-latest/bin/R
#setwd("/home/u4/dowbatw1133/Documents/vote_record")
merge_all_datasets_class <- R6::R6Class("merge_all_datasets", inherit=transform_clean_design_survey_class, public = list(
  term_to_survey_dt = NULL,
  mergedf_votes_bills_surveyanswer_filepath = NULL,
  overall_nonagenda_df_filepath = NULL,
  overall_nonagenda_df_tiny_filepath = NULL,
  overall_nonagenda_df_dummycoded_filepath = NULL,
  overall_nonagenda_df_fullydummycoded_filepath = NULL,
  responseopinion_model_overall_filepath = NULL,
  modelvars_ex_conti = NULL,
  modelvars_ex_catg = NULL,
  modelvars_latentrelated = NULL,
  modelvars_clustervars = NULL,
  modelvars_controllclustervars = NULL,
  initialize = function(dataset_in_scriptsfile_directory="/mnt", filespath="/mnt", dataset_file_directory="/mnt", debug_func_mode=TRUE) {
    super$initialize(dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory, filespath=filespath, dataset_file_directory=dataset_file_directory, debug_func_mode=debug_func_mode)
    #選舉資料
    self$term_to_survey_dt <- self$term_to_survey %>%
      dplyr::filter(term %in% !!c(7,9)) %>%
      data.table::as.data.table()
    self$mergedf_votes_bills_surveyanswer_filepath <- file.path(dataset_in_scriptsfile_directory, "mergedf_votes_bills_surveyanswer.RData")
    self$overall_nonagenda_df_filepath <- file.path(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df.RData")
    self$overall_nonagenda_df_tiny_filepath <- file.path(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df_tiny.RData")
    self$overall_nonagenda_df_dummycoded_filepath <- file.path(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df_dummycoded.RData")
    self$overall_nonagenda_df_fullydummycoded_filepath <- file.path(save_dataset_in_scriptsfile_directory, "overall_nonagenda_df_fullydummycoded.RData")
    self$responseopinion_model_overall_filepath <- file.path(dataset_in_scriptsfile_directory, "responseopinion_model_overall.RData")
    self$modelvars_ex_conti<-c("myown_age_overallscaled","similarity_distance_overallscaled","party_pressure_overallscaled","seniority_overallscaled","days_diff_survey_bill_overallscaled") #%>%
      #c("policyidealpoint_cos_similarity_to_median","policyidealpoint_eucli_distance_to_median_scaled")
    self$modelvars_ex_catg<-c("myown_sex","myown_areakind","myown_selfid","myown_marriage","issuefield","myown_religion","myown_areakind") %>% #,"adminparty"
      c("partysize") #"elec_dist_type"
    self$modelvars_latentrelated<-c("myown_factoredses_overallscaled","myown_factoredparticip_overallscaled") #,"myown_factoredefficacy"
    self$modelvars_clustervars<-c("cluster_kamila")
    self$modelvars_controllclustervars<-c("SURVEY","myown_areakind","partyGroup")
  },
  overalldf_general_inter_func = function(targetdf) {
    targetdf %>%
      dplyr::select(-tidyselect::any_of(c("legislator_sex","legislator_age","legislator_party","incumbent"))) %>%
      return()
  },
  overalldf_general_func = function(targetdf, agendavoting=0, similarities_bet_pp_ly_longdf=similarities_bet_pp_ly_longdf, mergedf_votes_bills_surveyanswer=mergedf_votes_bills_surveyanswer) {
    #input is complete_survey_dataset, term_to_survey, legislators_with_elections
    #（歷次除錯用行政區名對照註解請見git歷史版本）
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
  },
  overalldf_to_implist_func = function(targetdf, usinglib="lavaan", impns=1:5) {
    targetdf %>% lapply(impns, function(needimp,df) {
      return(dplyr::filter(df, imp==!!needimp))
    }, df=.) %>%
      {if (usinglib=="survey") mitools::imputationList(.) else .} %>%
      return()
  },
  # merge of load data --------------------------------
  load_merge_inputs = function(needimprange=1:6) {
    load_env <- new.env()
    load(self$complete_survey_dataset_filepath, envir=load_env, verbose=TRUE)
    load_env$complete_survey_dataset %<>% dplyr::filter(newimp %in% needimprange) %>%
      dplyr::mutate(id_wth_survey=paste0(SURVEY,id)) %>%
      dplyr::mutate_at("id_wth_survey", as.factor)
    load(self$mergedf_votes_bills_surveyanswer_filepath, envir=load_env, verbose=TRUE)
    load(self$legislators_with_elections_rdata_filepath, envir=load_env, verbose=TRUE)
    #load(self$legislators_additional_attr_filepath, envir=load_env, verbose=TRUE)
    load(self$similarities_match_filepath, envir=load_env, verbose=TRUE)
    load_env$similarities_bet_pp_ly_longdf %<>% dplyr::mutate_at("id",as.integer)
    load_env
  },
  # 大串連：把長表問卷、選區立委、相似度與法案投票回應合併成建模主檔
  build_overall_nonagenda_df = function(merge_inputs=NULL, save=TRUE, merge_partylist=FALSE) {
    if (is.null(merge_inputs)) {
      merge_inputs <- self$load_merge_inputs()
    }
    complete_survey_dataset<-merge_inputs$complete_survey_dataset
    mergedf_votes_bills_surveyanswer<-merge_inputs$mergedf_votes_bills_surveyanswer
    legislators_with_elections<-merge_inputs$legislators_with_elections
    similarities_bet_pp_ly_longdf<-merge_inputs$similarities_bet_pp_ly_longdf
    term_to_survey<-self$term_to_survey_dt
    overall_nonagenda_df<-list(
      dplyr::left_join(complete_survey_dataset, term_to_survey) %>% #Joining, by = "SURVEY"
        dplyr::left_join(legislators_with_elections) %>% #Joining, by = c("admincity", "admindistrict", "adminvillage", "term")
        self$overalldf_general_inter_func() %>%
        #dplyr::left_join(legislators_additional_attr) %>% #Joining, by = c("term", "legislator_name")
        self$overalldf_general_func(agendavoting=0,similarities_bet_pp_ly_longdf=similarities_bet_pp_ly_longdf,mergedf_votes_bills_surveyanswer=mergedf_votes_bills_surveyanswer),

      if (merge_partylist) { #merge partylist
        dplyr::left_join(complete_survey_dataset, term_to_survey) %>% #Joining, by = "SURVEY"
          dplyr::mutate(elec_dist_type="partylist") %>%
          dplyr::left_join({
            dplyr::filter(legislators_with_elections, elec_dist_type=="partylist") %>%
              dplyr::distinct_at(.vars=dplyr::vars(-admincity,-admindistrict,-adminvillage))
          }) %>% #Joining, by = c("term", "elec_dist_type")
          self$overalldf_general_inter_func() %>%
          #dplyr::left_join(legislators_additional_attr) %>% #Joining, by = c("term", "legislator_name")
          self$overalldf_general_func(agendavoting=0,similarities_bet_pp_ly_longdf=similarities_bet_pp_ly_longdf,mergedf_votes_bills_surveyanswer=mergedf_votes_bills_surveyanswer)
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
    if (save==TRUE) {
      while (TRUE) {
        savestatus<-try(save(overall_nonagenda_df, file=self$overall_nonagenda_df_filepath))
        if(!is(savestatus, 'try-error')) break
      }
    }
    overall_nonagenda_df
  },
  # modeling data prepare when bigdata exists --------------------------------
  shrink_overall_nonagenda_df_to_tiny = function(overall_nonagenda_df, save=TRUE) {
    tinycolumns<-c("myown_wr","admindistrict","adminvillage","billid_myown","cluster_kamila","days_diff_survey_bill","days_diff_survey_bill_overallscaled","elec_dist_type","id_wth_survey","issuefield","legislator_name","myown_age_overallscaled","myown_areakind","myown_factoredparticip_overallscaled","myown_factoredses_overallscaled","myown_income_scaled","myown_marriage","myown_selfid","myown_sex","myown_religion","seniority_overallscaled","party_pressure_overallscaled","partyGroup","partysize","adminparty","respondopinion","similarity_distance_overallscaled","SURVEY","newimp") %>%
      base::setdiff(c("elec_dist_type","admincity"))
    overall_nonagenda_df %<>% dplyr::select(!!tinycolumns)
    if (save==TRUE) {
      try(save(overall_nonagenda_df, file=self$overall_nonagenda_df_tiny_filepath))
    }
    overall_nonagenda_df
  },
  get_overall_nonagenda_df = function(size="tiny") {
    load_env <- new.env()
    needfilepath<-switch(size,
                         "tiny"=self$overall_nonagenda_df_tiny_filepath,
                         "full"=self$overall_nonagenda_df_filepath,
                         "dummycoded"=self$overall_nonagenda_df_dummycoded_filepath,
                         "fullydummycoded"=self$overall_nonagenda_df_fullydummycoded_filepath)
    load(file=needfilepath, envir=load_env, verbose=TRUE)
    load_env$overall_nonagenda_df
  },
  get_dummyc_vars = function(overall_nonagenda_df) {
    custom_pickcolnames_accordingtoclass(overall_nonagenda_df, needclass="factor") %>%
      base::intersect(c(self$modelvars_ex_catg,self$modelvars_clustervars,self$modelvars_controllclustervars))
  },
  # dummy coding bigdata --------------------------------
  dummycode_overall_nonagenda_df = function(overall_nonagenda_df, partlydummycoding=TRUE, save=TRUE) {
    dummyc_vars<-self$get_dummyc_vars(overall_nonagenda_df)
    dummycodingvars<-if (partlydummycoding) base::intersect(dummyc_vars,self$modelvars_clustervars) else base::union(dummyc_vars,self$modelvars_clustervars)
    if (partlydummycoding) {
      overall_nonagenda_df <- dplyr::bind_cols(
        dplyr::select(overall_nonagenda_df, !!self$modelvars_clustervars),
        dummycode_of_a_dataframe(overall_nonagenda_df, catgvars=self$modelvars_clustervars)
      )
      savedummycodeddffilename<-self$overall_nonagenda_df_dummycoded_filepath
    } else {
      overall_nonagenda_df <-
        dummycode_of_a_dataframe(overall_nonagenda_df, catgvars=dummycodingvars)
      savedummycodeddffilename<-self$overall_nonagenda_df_fullydummycoded_filepath
    }
    if (save==TRUE) {
      while (TRUE) {
        savestatus<-try(save(overall_nonagenda_df, file=savedummycodeddffilename))
        if(!is(savestatus, 'try-error')) break
      }
    }
    overall_nonagenda_df
  }
))

#選舉資料（歷史參考註解）
# terms<-c(5,6,7,8,9)
# overall_elec_dist_types<-c('district','ab_m','ab_plain','partylist')
# supplement_election_termseven<-c('supp2009miaoli1','supp2009nantou1','supp2009yunlin2','supp2009taipei6','supp2010taichungs3','supp2010hualian','supp2010taoyuan2','supp2010taoyuan3','supp2010hsinchus','supp2010chiayi2','supp2010taitung','supp2011tainan4','supp2011kaoshiung4')
# survey_time_range <- list(
#   "2004citizen"=data.frame("SURVEY"="2004citizen","yrmonth"=c("093/07","093/08","093/09","093/10","093/11","093/12","094/01","094/02","094/03","094/04","094/05","094/06","094/07","094/08","094/09","094/10","094/11","094/12","095/01","095/02","095/03","095/04","095/05","095/06","095/07","095/08","095/09")), #,"095/10","095/11","095/12"
#   "2010env"=data.frame("SURVEY"="2010env","yrmonth"=c("099/07","099/08","099/09","099/10","099/11","099/12","100/01","100/02","100/03","100/04","100/05","100/06","100/07","100/08","100/09","100/10","100/11","100/12","101/01","101/02","101/03","101/04","101/05","101/06","101/07","101/08")),
#   "2010overall"=data.frame("SURVEY"="2010overall","yrmonth"=c("099/07","099/08","099/09","099/10","099/11","099/12","100/01","100/02","100/03","100/04","100/05","100/06","100/07","100/08","100/09","100/10","100/11","100/12","101/01","101/02","101/03","101/04","101/05","101/06","101/07","101/08","101/09","101/10","101/11")),
#   "2016citizen"=data.frame("SURVEY"="2016citizen","yrmonth"=c("105/09","105/11","105/12","106/01","106/04","106/05","106/06","106/08","106/10","106/11","106/12","107/01","107/03","107/04","107/05","107/06","107/07","107/08","107/09","107/10","107/11"))
# )
# survey_time_range_df <- plyr::rbind.fill(survey_time_range)

if (FALSE) { #lazy_dt試驗（僅保留參考用）
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

if (FALSE) {
  des<-list()
  responseopinion_model_overall<-list()
  save(des, responseopinion_model_overall, file=paste0(dataset_in_scriptsfile_directory, "responseopinion_model_overall.RData"))
}

#modeling文獻與軟體參考（僅保留參考用）
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

##注意有遺漏的部分委員
#（overall_district_legislators_only_power_dfdata / overall_partylist_legislators_only_power_dfdata
#  等gap變數建構的舊版合併方式請見git歷史版本）
#有多個村里會重複所以join時會膨脹
#設定對照政黨
# for (i in 1:length(rulingparty)) {
#   party<-rulingparty[[i]]
#   overall_district_legislators_only_power_dfdata[[i]]$legislator_party %<>% relevel(ref=party)
# }
#只有針對議案的決定，而非有無代理
# testdf <- mutate_at(complete_survey_dataset,"term", as.numeric) %>%
#   inner_join(only_bill_to_survey_information)
