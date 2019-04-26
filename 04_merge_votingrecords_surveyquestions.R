# 第Ｏ部份：環境設定 --------------------------------
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
source(file = "shared_functions.R")
terms<-c(5,6,7,8,9)
survey_time_range <- list(
  "2004citizen"=data.frame("SURVEY"="2004citizen","yrmonth"=c("093/07","093/08","093/09","093/10","093/11","093/12","094/01","094/02","094/03","094/04","094/05","094/06","094/07","094/08","094/09","094/10","094/11","094/12","095/01","095/02","095/03","095/04","095/05","095/06","095/07","095/08","095/09")), #,"095/10","095/11","095/12"
  "2010env"=data.frame("SURVEY"="2010env","yrmonth"=c("099/07","099/08","099/09","099/10","099/11","099/12","100/01","100/02","100/03","100/04","100/05","100/06","100/07","100/08","100/09","100/10","100/11","100/12","101/01","101/02","101/03","101/04","101/05","101/06","101/07","101/08")),
  "2010overall"=data.frame("SURVEY"="2010overall","yrmonth"=c("099/07","099/08","099/09","099/10","099/11","099/12","100/01","100/02","100/03","100/04","100/05","100/06","100/07","100/08","100/09","100/10","100/11","100/12","101/01","101/02","101/03","101/04","101/05","101/06","101/07","101/08","101/09","101/10","101/11")),
  "2016citizen"=data.frame("SURVEY"="2016citizen","yrmonth"=c("105/09","105/11","105/12","106/01","106/04","106/05","106/06","106/08","106/10","106/11","106/12","107/01","107/03","107/04","107/05","107/06","107/07","107/08","107/09","107/10","107/11"))
)
survey_time_range_df <- plyr::rbind.fill(survey_time_range)
gc(verbose=TRUE)

# 第二部分：投票及議案及「問卷答案對照意向」資料,主要也就是RData&excel檔  -------------------------------------------

if ({incoporate_party_seats<-FALSE; incoporate_party_seats}) { #舊方法暫時忽略
  partyseats <- data.frame(
    "term"=7,
    "party"=c("中國國民黨","民主進步黨","無黨團結聯盟","親民黨","無黨籍及未經政黨推薦"),                       
    "seats"=c(81,27,3,1,1),
    "rulingparty"=factor(c(1,0,0,0,0)),
    "seatsgaptorulingparty"=c(0,54,78,80,80)
  ) %>%
    bind_rows(
      data.frame(
        "term"=9,
        "party"=c("中國國民黨","民主進步黨","時代力量","親民黨","無黨團結聯盟","無黨籍及未經政黨推薦"),
        "seats"=c(35,68,5,3,1,1),
        "rulingparty"=factor(c(0,1,0,0,0,0)),
        "seatsgaptorulingparty"=c(33,0,63,65,67,67)
      )
    ) %>%
    bind_rows(
      data.frame(
        "term"=5,
        "party"=c("中國國民黨","民主進步黨","親民黨","台灣團結聯盟","新黨","台灣吾黨","無黨籍及未經政黨推薦"),
        "seats"=c(68,87,46,13,1,1,9),
        "rulingparty"=factor(c(0,1,0,0,0,0,0)),
        "seatsgaptorulingparty"=c(19,0,41,41,44,86,78)
      )
    ) %>%
    bind_rows(
      data.frame(
        "term"=6,
        "party"=c("中國國民黨","民主進步黨","親民黨","台灣團結聯盟","新黨","無黨團結聯盟","無黨籍及未經政黨推薦"),
        "seats"=c(79,89,34,12,1,6,4),
        "rulingparty"=factor(c(0,1,0,0,0,0,0)),
        "seatsgaptorulingparty"=c(10,0,55,77,88,83,85)
      )
    )
}

#as.character(unique(bills_billcontent$pp_related_q_1))

#讀取投票紀錄資料-此處通常預處理好，直接load下面 mergedf_votes_bills_surveyanswer
load(paste0(dataset_in_scriptsfile_directory, "myown_vote_record_df.RData"))
load(paste0(dataset_in_scriptsfile_directory, "myown_vote_record_detailed_part_df.RData"))
duplicated_meeting_in_vote_record <- list(
  distinct(myown_vote_record_df,term,period,temp_meeting_no,meetingno) %>% arrange(term,period,temp_meeting_no,meetingno),
  distinct(myown_vote_record_detailed_part_df,term,period,temp_meeting_no,meetingno) %>% arrange(term,period,temp_meeting_no,meetingno)
)
duplicated_meeting_in_vote_record <- dplyr::inner_join(duplicated_meeting_in_vote_record[[1]],duplicated_meeting_in_vote_record[[2]])
myown_vote_record_df <- dplyr::anti_join(myown_vote_record_df, duplicated_meeting_in_vote_record) %>%
  bind_rows(myown_vote_record_detailed_part_df) %>%
  dplyr::arrange(term, period, temp_meeting_no, meetingno, billn) %>%
  mutate_cond(votedecision %in% c("棄權","未投票","未出席"), votedecision="棄權/未投票/未出席") %>% 
  dplyr::mutate_at(c("votedecision","billresult"), as.factor) %>%
  mutate_at("legislator_name", .funs = list(legislator_name = ~customgsub(legislator_name, "　", ""))) %>%#funs(customgsub(legislator_name, "　", ""))
  mutate_at(c("billcontent","term","period","meetingno","temp_meeting_no","billn"), as.character) %>%
  mutate_at("billcontent", trimws) %>%
  mutate(billid_myown = paste(term, "-", period, "-", temp_meeting_no, "-", meetingno, "-", billn, sep = "")) %>%
  mutate_at("billid_myown", as.character) %>%
  mutate_at(c("term","period","meetingno","temp_meeting_no","billn"), as.numeric) %>%
  mutate_cond(term==9 & customgrepl(legislator_name,"簡東明"),legislator_name="簡東明Uliw．Qaljupayare") %>%
  mutate_cond(term==9 & customgrepl(legislator_name,"廖國棟"),legislator_name="廖國棟Sufin．Siluko") %>%
  mutate_cond(term==9 & customgrepl(legislator_name,"鄭天財"),legislator_name="鄭天財Sra．Kacaw") %>%
  mutate_cond(term==9 & customgrepl(legislator_name,"高潞"),legislator_name="高潞．以用．巴魕剌Kawlo．Iyun．Pacidal") %>%
  mutate_cond(term==9 & customgrepl(legislator_name,"陳秀霞"),legislator_name="周陳秀霞") %>%
  select(-billcontent,-url)
#save(myown_vote_record_df,file=paste0(dataset_in_scriptsfile_directory, "myown_vote_record_df_across2004.RData"))

#filter(myown_vote_record_df,term==9,customgrepl(legislator_name,"陳秀霞")) %>% select(legislator_name) %>% unique()

#mutate_at("legislator_name", funs(recode(opinionstrength)),
#  "n" = 1, "nn" = 2, "m" = 1, "mm" = 2, "b" = 0
#) #%>%


##算出同黨票數
load(file=paste0(dataset_in_scriptsfile_directory, "myown_vote_record_df_across2004.RData"))
load(file=paste0(dataset_in_scriptsfile_directory, "legislators_with_elections.RData"))

myown_vote_bills_file <- paste0(dataset_file_directory, "votingdf_datafile_myown_englished.xlsx", sep="")
bills_answer_to_bill <- openxlsx::read.xlsx(myown_vote_bills_file, sheet = 4)
bills_billcontent <- openxlsx::read.xlsx(myown_vote_bills_file, sheet = 1) %>%
  mutate_at("billcontent", as.character) %>%
  select(-starts_with("pp_related_q_")) #因為第四個表格問卷對政策實現與否表已經有了variable_on_q所以此處略過

mergedf_votes_bills_surveyanswer <- distinct(legislators_with_elections,term,legislator_name,legislator_party) %>%
  #mutate(party=legislator_party) %>%
  mutate_at("term", as.numeric) %>%
  left_join(myown_vote_record_df, ., by=c("term","legislator_name")) %>%
  distinct(votedecision,legislator_name,billid_myown,legislator_party) %>%
  filter(!is.na(legislator_party)) %>%
  #above are myown_vote_record_df_with_party
  left_join(filter(myown_vote_record_df, term %in% terms), .) %>%
  #left_join(partyseats) %>%
  right_join(bills_billcontent, by = c("billid_myown","term","period","meetingno","temp_meeting_no","billn","billresult","date")) %>% ##,"url"
  right_join(bills_answer_to_bill, by = c("billid_myown")) %>%  ##問題在這邊
  #篩選出研究範圍
  inner_join(distinct(survey_time_range_df,yrmonth)) %>%
  left_join(survey_time_range_df) %>%
  mutate(stdbilldate=as.Date(paste(
    as.integer(substr(date,0,3))+1911,
    substr(date,5,6),
    substr(date,8,9)
  ),"%Y %m %d")) %>%
  mutate(opinionstrength=dplyr::recode(opinionfromconstituent, `n`=1, `nn`=2, `m`=1, `mm`=2, `b`=0)) %>%
  #mutate(opinionstrength=opinionfrombill) %>%
  #mutate_at("opinionstrength",.funs=(recode(opinionfromconstituent)),
  #          "n"=1,"nn"=2,"m"=1,"mm"=2,"b"=0
  #) %>%#.funs = list(legislator_name = ~customgsub(legislator_name, "　", ""))
  mutate(opiniondirectionfromconstituent=dplyr::recode(opinionfromconstituent, `n`="n", `nn`="n", `nnn`="n", `m`="m", `mm`="m", `mmm`="m", `b`="b")) %>%
  #mutate("opiniondirectionfromconstituent"=opinionfromconstituent) %>%
  #mutate_at("opiniondirectionfromconstituent",funs(recode(opiniondirectionfromconstituent)),
  #          "n"="n","nn"="n","nnn"="n","m"="m","mm"="m","mmm"="m","b"="b"
  #) %>%
  mutate(opiniondirectionfrombill=dplyr::recode(opinionfrombill,`n`="n",`nn`="n",`m`="m",`mm`="m",`b`="b")) %>%
  #mutate("opiniondirectionfrombill"=opinionfrombill) %>%
  #mutate_at("opiniondirectionfrombill",funs(recode(opiniondirectionfrombill)),
  #          "n"="n","nn"="n","m"="m","mm"="m","b"="b"
  #) %>%
  mutate(opiniondirectionfromlegislator=NA,respondopinion=NA,success_on_bill=NA) %>%
  #mutate(respondopinion=paste0(votedecision,opiniondirectionfrombill)) %>%
  #mutate(respondopinion) %>%
  #mutate_cond(votedecision=="贊成" & opiniondirectionfromconstituent==opiniondirectionfrombill, respondopinion=2) %>%
  #mutate_cond(votedecision=="贊成" & opiniondirectionfromconstituent!=opiniondirectionfrombill, respondopinion=0) %>%
  #mutate_cond( (opiniondirectionfromconstituent==opiniondirectionfrombill), success_on_bill=ifelse(billresult=="Passed",1,0) ) %>%
  mutate_cond( (opiniondirectionfromconstituent==opiniondirectionfrombill) & (billresult=="Passed"), success_on_bill=1 ) %>%
  mutate_cond( (opiniondirectionfromconstituent==opiniondirectionfrombill) & (billresult=="NotPassed"), success_on_bill=0 ) %>%
  #mutate_cond( (opiniondirectionfromconstituent!=opiniondirectionfrombill & opiniondirectionfromconstituent != "x" & opiniondirectionfromconstituent != "b"), success_on_bill=ifelse(billresult=="Passed",0,1) ) %>%
  mutate_cond( (opiniondirectionfromconstituent!=opiniondirectionfrombill & opiniondirectionfromconstituent != "x" & opiniondirectionfromconstituent != "b") & (billresult=="Passed"), success_on_bill=0 ) %>%
  mutate_cond( (opiniondirectionfromconstituent!=opiniondirectionfrombill & opiniondirectionfromconstituent != "x" & opiniondirectionfromconstituent != "b") & (billresult=="NotPassed"), success_on_bill=1 ) %>%
  mutate_cond(votedecision=="贊成", opiniondirectionfromlegislator=opiniondirectionfrombill) %>%
  mutate_cond(votedecision=="反對", opiniondirectionfromlegislator=recode(opiniondirectionfrombill,
                                                                        "n"="m","m"="n",
                                                                        "cpg"="NOTcpg","NOTcpg"="cpg","envenerg"="NOTenvenerg","NOTenvenerg"="envenerg","nuenerg"="NOTnuenerg","NOTnuenerg"="nuenerg",
                                                                        "crop"="NOTcrop","NOTcrop"="crop","otherenerg"="NOTotherenerg","NOTotherenerg"="otherenerg",
                                                                        "NOTgovpushrichpeoplemore"="govpushrichpeoplemore","NOTgovmore"="govmore","NOTnoint"="noint","NOTpoorontheirown"="poorontheirown",
                                                                        "govpushrichpeoplemore"="NOTgovpushrichpeoplemore","govmore"="NOTgovmore","noint"="NOTnoint","poorontheirown"="NOTpoorontheirown",
                                                                        "bygov"="NOTbygov","byent"="NOTbyent","bynpo"="NOTbynpo","byrelg"="NOTbyrelg","byfamily"="NOTbyfamily",
                                                                        "NOTbygov"="bygov","NOTbyent"="byent","NOTbynpo"="bynpo","NOTbyrelg"="byrelg","NOTbyfamily"="byfamily"
  )) %>%
  #==opiniondirectionfrombill & !(opiniondirectionfrombill %in% c('b','x')), respondopinion=2
  # 反對核電怎麼編碼？
  mutate_cond(opiniondirectionfromconstituent!=opiniondirectionfromlegislator, respondopinion=0) %>%
  mutate_cond(opiniondirectionfromconstituent==opiniondirectionfromlegislator, respondopinion=2) %>%
  mutate_cond(votedecision=="棄權/未投票/未出席", respondopinion=1, opiniondirectionfromlegislator='ig/gu') %>% #ignore/giveup
  #mutate_cond(votedecision=="棄權", respondopinion=1, opiniondirectionfromlegislator='giveup', respondopinion=2) %>%
  mutate_cond(opiniondirectionfromconstituent=='x' | opiniondirectionfromconstituent=='b' | opiniondirectionfrombill=='x', respondopinion=NA, success_on_bill=1) %>%
  #mutate_at("respondopinion",funs(recode(respondopinion)),
  #          "反對n"=2,"反對nn"=2,"贊成m"=2,"贊成mm"=2,
  #          "棄權n"=1,"棄權nn"=1,"棄權m"=1,"棄權mm"=1,
  #          "贊成n"=0,"贊成nn"=0,"反對m"=0,"反對mm"=0
  #) %>% 
  select(-date,-urln,-pp_committee,-votecontent,-pp_enactment,-pp_enforcement,-pp_res_bynew,-pp_res_bycompete,-pp_res_notjudged,-pp_ignored,-billconflict,-pol_score,-eco_score,-SURVEYQUESTIONID) %>%
  mutate(ansv_and_label=paste0("[",SURVEYANSWERVALUE,"] ",LABEL)) %>%
  mutate_at(c("SURVEY","billresult","legislator_party","pp_agendavoting","pp_propose_advanceforagenda","value_on_q_variable","variable_on_q","pp_lawamendment","issue_field1","issue_field2","respondopinion","success_on_bill","ansv_and_label"), as.factor) %>%
  select(-url.x,-url.y,-pp_keyword.x,-pp_keyword.y,-billcontent.x,-billcontent.y, -SURVEYANSWERVALUE, -LABEL, -QUESTION)


#%>%
#mutate(term=stringi::stri_sub(electionname,from=1,length=1)) #%>%
#mutate(electionarea=last(unlist(strsplit(electionname,split="\\."))))
#names(bulletin_links) %>% sapply(function(X) {
#last(unlist(strsplit(X,split="\\.")))
#})
#可以看到有回應也有不回應
#distinct(mergedf_votes_bills_surveyanswer,votedecision,billid_myown,variable_on_q,value_on_q_variable,name,party,opiniondirectionfromconstituent,opiniondirectionfrombill,opiniondirectionfromlegislator,respondopinion) %>%
#  #testdf %>%
#  filter(billid_myown=="9-2-0-16-67",variable_on_q=="pp_related_q_1",value_on_q_variable=="2016citizen@c2") %>%
#  arrange(name,party) %>%
#  View()

#mergedf_votes_bills_surveyanswer %>%
#  filter(!is.na(respondopinion),billid_myown=="9-2-0-17-88",variable_on_q=="pp_related_q_1") %>%
#  distinct(name,votedecision,variable_on_q,respondopinion,billid_myown,party) %>%
#  arrange(party,billid_myown,variable_on_q,respondopinion) %>%
#  View()

#save(mergedf_votes_bills_surveyanswer, file = paste0(dataset_in_scriptsfile_directory, "mergedf_votes_bills_surveyanswer.RData"))

