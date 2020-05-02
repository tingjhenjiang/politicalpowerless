# 第Ｏ部份：環境設定 --------------------------------
if (!("benchmarkme" %in% rownames(installed.packages()))) install.packages("benchmarkme")
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
t_sessioninfo_running_with_cpu_locale<-gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2]))
source(file = "shared_functions.R", encoding="UTF-8")
#選舉資料
#overall_elec_dist_types<-c('district','ab_m','ab_plain','partylist')
#supplement_election_termseven<-c('supp2009miaoli1','supp2009nantou1','supp2009yunlin2','supp2009taipei6','supp2010taichungs3','supp2010hualian','supp2010taoyuan2','supp2010taoyuan3','supp2010hsinchus','supp2010chiayi2','supp2010taitung','supp2011tainan4','supp2011kaoshiung4')
terms<-c(5,6,7,8,9)
#survey_time_range <- as.data.frame(list(yrmonth=c('099/07', '099/11', '099/12', '100/01', '100/04', '100/06', '105/09', '105/10', '105/11', '105/12', '106/01', '106/04', '106/05')))
#survey_time_range <- as.data.frame(list(yrmonth=c()))
survey_time_range <- list(
  "2004citizen"=data.frame("SURVEY"="2004citizen","yrmonth"=c("093/07","093/08","093/09","093/10","093/11","093/12","094/01","094/02","094/03","094/04","094/05","094/06","094/07","094/08","094/09","094/10","094/11","094/12","095/01","095/02","095/03","095/04","095/05","095/06","095/07","095/08","095/09")), #,"095/10","095/11","095/12"
  "2010env"=data.frame("SURVEY"="2010env","yrmonth"=c("099/07","099/08","099/09","099/10","099/11","099/12","100/01","100/02","100/03","100/04","100/05","100/06","100/07","100/08","100/09","100/10","100/11","100/12","101/01","101/02","101/03","101/04","101/05","101/06","101/07","101/08")),
  "2010overall"=data.frame("SURVEY"="2010overall","yrmonth"=c("099/07","099/08","099/09","099/10","099/11","099/12","100/01","100/02","100/03","100/04","100/05","100/06","100/07","100/08","100/09","100/10","100/11","100/12","101/01","101/02","101/03","101/04","101/05","101/06","101/07","101/08","101/09","101/10","101/11")),
  "2016citizen"=data.frame("SURVEY"="2016citizen","yrmonth"=c("105/09","105/11","105/12","106/01","106/04","106/05","106/06","106/08","106/10","106/11","106/12","107/01","107/03","107/04","107/05","107/06","107/07","107/08","107/09","107/10","107/11"))
)
survey_time_range_df <- plyr::rbind.fill(survey_time_range)
term_to_survey <- data.frame("term"=c(5,6,7,7,8,8,9), "SURVEY"=c("2004citizen","2004citizen","2010env","2010overall","2010env","2010overall","2016citizen"))
gc(verbose=TRUE)




# 第八部份：問卷資料串連立委資料、選舉資料 ---------------------------------

library(parallel)
rulingparty <- list("2004citizen"=c("民主進步黨"),"2010env"=c("中國國民黨"),"2010overall"=c("中國國民黨"),"2016citizen"=c("民主進步黨"))
#load(paste0(filespath,"data",slash,"elections_df_test.RData"))

#直接讀取分析立法通過的資料集
#as glmdata_pass_on_bill
#distinct(glmdata,)
#load(file=paste0(dataset_file_directory,"rdata",slash,"pass_on_bill.RData"))
#load(paste0(filespath,"data",slash,"mergedf__votes_bills_surveyanswer.RData"))
#new_mergedf_votes_bills_surveyanswer<-mergedf_votes_bills_surveyanswer
load(paste0(dataset_in_scriptsfile_directory, "complete_survey_dataset.RData"))
load(paste0(dataset_in_scriptsfile_directory, "mergedf_votes_bills_surveyanswer.RData"))
load(paste0(dataset_in_scriptsfile_directory, "legislators_additional_attr.RData"))
load(paste0(dataset_in_scriptsfile_directory, "legislators_with_elections.RData"))
legislators_with_elections %<>% select(-degree,-experience,-education,-wonelection)
#admincity admindistrict adminvillage

#View(mergedf_votes_bills_surveyanswer)
#remove(mergedf_votes_bills_surveyanswer)
#sapply(names(mergedf_votes_bills_surveyanswer), function(X,df1,df2) {
#  message(X,identical(df1$X,df2$X))
#},df1=new_mergedf_votes_bills_surveyanswer, df2=mergedf_votes_bills_surveyanswer)

#only_bill_to_survey_information<-distinct(mergedf_votes_bills_surveyanswer,stdbilldate,term,period,meetingno,temp_meeting_no,billn,billresult,billid_myown,SURVEY,variable_on_q,value_on_q_variable,SURVEYQUESTIONID,SURVEYANSWERVALUE,LABEL,QUESTION,opinionfromconstituent,opinionfrombill,issue_field1,issue_field2,opinionstrength,opiniondirectionfromconstituent,opiniondirectionfrombill,success_on_bill) %>%
#  mutate_at("SURVEYANSWERVALUE", as.character)
#save(only_bill_to_survey_information,file=paste0(filespath,"data",slash,"only_bill_to_survey_information.RData"))
#load(paste0(filespath,"data",slash,"only_bill_to_survey_information.RData"))
#filter(mergedf_votes_bills_surveyanswer[1:1000,], respondopinion==1) %>% View()
#test_tiny_legislators_with_election <- distinct(legislators_with_election, term, legislator_name, legislator_sex, legislator_party, partyGroup, areaName, birthday, legislator_age, incumbent, election_party, electionarea, admincity)
#term, legislator_name, legislator_sex, legislator_party, partyGroup, areaName, birthday, legislator_age, incumbent, election_party, electionarea, admincity, admindistrict, adminvillage, plranking, elec_dist_type
##注意有遺漏的部分委員
#list(
#  complete_survey_dataset,
#  mergedf_votes_bills_surveyanswer,
#  legislators_with_election
  #inner_join(mergedf_votes_bills_surveyanswer, distinct(survey_time_range[['2004citizen']],yrmonth) ),
  #inner_join(mergedf_votes_bills_surveyanswer, distinct(survey_time_range[['2004citizen']],yrmonth) ) %>%
  #left_join(test_tiny_legislators_with_election, by=c("term","legislator_name"))
#) %>%
#  sapply(nrow)
#有多個村里會重複所以join時會膨脹
overall_district_legislators_only_power_dfdata <- custom_parallel_lapply(survey_data_title, function(needsurvey,...) {
  filter(complete_survey_dataset, SURVEY==needsurvey) %>% #135654
    left_join(term_to_survey) %>% #135654
    left_join(legislators_with_elections) %>% #135654
    left_join(legislators_additional_attr) %>%
    mutate(gap_eduyr=NA,gap_ses=NA,gap_sex=NA,gap_ethnicity=NA,gap_age=NA) %>%
    mutate_cond(myown_selfid==legislator_ethnicity, gap_ethnicity=0) %>%
    mutate_cond(myown_selfid!=legislator_ethnicity, gap_ethnicity=1) %>%
    mutate_cond(!is.na(myown_age), gap_age=abs(myown_age-legislator_age)) %>%
    mutate_cond(!is.na(myown_eduyr), gap_eduyr=abs(myown_eduyr-legislator_eduyr)) %>%
    mutate_cond(!is.na(myown_ses), gap_ses=abs(myown_ses-legislator_ses)) %>%
    mutate_cond((myown_sex=="[1] 男" & legislator_sex=="男") | (myown_sex=="[2] 女" & legislator_sex=="女"), gap_sex=0) %>%
    mutate_cond((myown_sex=="[2] 女" & legislator_sex=="男") | (myown_sex=="[1] 男" & legislator_sex=="女"), gap_sex=1) %>%
    mutate_at(c("gap_sex","gap_ethnicity"), as.factor) %>% #135654
    left_join(mergedf_votes_bills_surveyanswer) %>% #346050
    inner_join(survey_time_range_df) %>% #231990  %>% nrow()
    mutate(days_diff_survey_bill=difftime(stdbilldate, stdsurveydate, units = "days"))
  }, exportlib=c("base","magrittr","dplyr","parallel"),
  exportvar=c("complete_survey_dataset","custom_parallel_lapply","mergedf_votes_bills_surveyanswer","legislators_with_elections","legislators_additional_attr","term_to_survey","survey_time_range_df","mutate_cond"))

#設定對照政黨
for (i in 1:length(rulingparty)) {
  party<-rulingparty[[i]]
  overall_district_legislators_only_power_dfdata[[i]]$legislator_party %<>% relevel(ref=party)
}


overall_partylist_legislators_only_power_dfdata <- lapply(survey_data_title, function(needsurvey,...) {
  filter(complete_survey_dataset, SURVEY==needsurvey) %>% #135654
    left_join(term_to_survey) %>% #135654
    left_join({
      filter(legislators_with_elections,elec_dist_type=="partylist") %>%
        select(-admincity,-admindistrict,-adminvillage)
      }) %>% #135654
    left_join(legislators_additional_attr) %>%
    mutate(gap_eduyr=NA,gap_ses=NA,gap_sex=NA,gap_ethnicity=NA,gap_age=NA) %>%
    mutate_cond(myown_selfid==legislator_ethnicity, gap_ethnicity=0) %>%
    mutate_cond(myown_selfid!=legislator_ethnicity, gap_ethnicity=1) %>%
    mutate_cond(!is.na(myown_age), gap_age=abs(myown_age-legislator_age)) %>%
    mutate_cond(!is.na(myown_eduyr), gap_eduyr=abs(myown_eduyr-legislator_eduyr)) %>%
    mutate_cond(!is.na(myown_ses), gap_ses=abs(myown_ses-legislator_ses)) %>%
    mutate_cond((myown_sex=="[1] 男" & legislator_sex=="男") | (myown_sex=="[2] 女" & legislator_sex=="女"), gap_sex=0) %>%
    mutate_cond((myown_sex=="[2] 女" & legislator_sex=="男") | (myown_sex=="[1] 男" & legislator_sex=="女"), gap_sex=1) %>%
    mutate_at(c("gap_sex","gap_ethnicity"), as.factor) %>% #135654
    left_join({
      inner_join(mergedf_votes_bills_surveyanswer,survey_time_range_df)
    }) %>% #  %>% nrow()
    mutate(days_diff_survey_bill=difftime(stdbilldate, stdsurveydate, units = "days"))
}, exportlib=c("base","magrittr","dplyr","parallel"),
exportvar=c("complete_survey_dataset","custom_parallel_lapply","mergedf_votes_bills_surveyanswer","legislators_with_election","legislators_additional_attr","term_to_survey","survey_time_range_df","mutate_cond"))

lapply(survey_data_title[2], function(SURVEY) {SURVEY})
#只有針對議案的決定，而非有無代理
testdf <- mutate_at(complete_survey_dataset,"term", as.numeric) %>%
  inner_join(only_bill_to_survey_information)

#沒有投票權也會串到立委，也就是只串選區的串法
testdf <- inner_join(complete_survey_dataset, testdf, by = c("term", "electionarea", "SURVEY", "SURVEYQUESTIONID", "SURVEYANSWERVALUE"))
#只串到支持的候選人的串法
#testdf <- inner_join(complete_survey_dataset, testdf, by = c("term", "electionarea", "SURVEY", "SURVEYQUESTIONID", "SURVEYANSWERVALUE", "myown_constituency_party_vote"="election_party"))
#串全國，不限選區
#testdf <- inner_join(complete_survey_dataset, testdf, by = c("term", "SURVEY", "SURVEYQUESTIONID", "SURVEYANSWERVALUE", "myown_constituency_party_vote"="election_party"))

# only observe if bills are passed
testdf<-inner_join(complete_survey_dataset, only_bill_to_survey_information,by = c("SURVEY", "term", "SURVEYQUESTIONID", "SURVEYANSWERVALUE"))