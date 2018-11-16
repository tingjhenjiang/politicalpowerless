t_sessioninfo<-sessionInfo()
t_sessioninfo_running<-gsub(" ","",t_sessioninfo$running)
t_sessioninfo_running<-gsub("[>=()]","",t_sessioninfo_running)
filespath<-switch(t_sessioninfo_running,
                  Ubuntu16.04.4LTS="/mnt/e/Software/scripts/R/",
                  Windows7x64build7601ServicePack1="C:\\Users\\r03a21033\\DOWNLOADS\\",
                  Windows10x64build16299 = "E:\\Software\\scripts\\R\\",
                  Windows8x64build9200 = "E:\\Software\\scripts\\R\\"
                  )
#filespath <- "E:\\Software\\scripts\\R\\"
#filespath <- "/mnt/e/Software/scripts/R/"
source(file = paste(filespath, "shared_functions.R", sep = ""))
dataset_file_directory <- switch(t_sessioninfo_running,
                                 Windows7x64build7601ServicePack1="C:\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\",
                                 Windows8x64build9200 = "D:\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\",
                                 Windows10x64build16299 = "D:\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\",
                                 Ubuntu16.04.4LTS="/mnt/d/OneDrive/OnedriveDocuments/NTU/Work/thesis/dataset(2004-2016)/"
                                 )
#選舉資料
overall_elec_dist_types<-c('district','ab_m','ab_plain','partylist')
supplement_election_termseven<-c('supp2009miaoli1','supp2009nantou1','supp2009yunlin2','supp2009taipei6','supp2010taichungs3','supp2010hualian','supp2010taoyuan2','supp2010taoyuan3','supp2010hsinchus','supp2010chiayi2','supp2010taitung','supp2011tainan4','supp2011kaoshiung4')
terms<-c(5,6,7,9)
gc(verbose=TRUE)
############################################################################################################################################################
# 第一部份：立委及選區資料
##############################################################################
elections_df<-data.frame()
for (termi in 1:length(terms)) {
  term<-terms[termi]
  message("term=",term)
  term_character<-paste0("0",term)
  if (term==7) {
    elec_types<-c(overall_elec_dist_types,supplement_election_termseven)
  } else {
    elec_types<-overall_elec_dist_types
  }
  for (i in 1:length(elec_types)) {
    elec_dist_type<-elec_types[i]
    message("")
    message("term=",term," AND type=",elec_dist_type," AND nrow=", nrow(elections_df))
    message("")
    elections_cand_csv <- paste0(dataset_file_directory,"cec_vote_dataset",slash,"term",term,slash,elec_dist_type,slash,"elcand.csv")
    elections_dist_csv <- paste0(dataset_file_directory,"cec_vote_dataset",slash,"term",term,slash,elec_dist_type,slash,"elbase.csv")
    elections_party_csv <- paste0(dataset_file_directory,"cec_vote_dataset",slash,"term",term,slash,elec_dist_type,slash,"elpaty.csv")
    elections_voteresult_csv <- paste0(dataset_file_directory,"cec_vote_dataset",slash,"term",term,slash,elec_dist_type,slash,"elprof.csv")
    elections_df_dist <-read_csv(file=elections_dist_csv,col_types="cccccc")
    elections_df_party <-read_csv(file=elections_party_csv)
    elections_df_cand <-read_csv(file=elections_cand_csv)
    #elections_df_voteresult <-read_csv(file=elections_voteresult_csv)
    if (elec_dist_type=='partylist') {
      elections_plcan_csv <- paste0(dataset_file_directory,"cec_vote_dataset",slash,"term",term,slash,elec_dist_type,slash,"elrepm.csv")
      elections_df_plcan <-read_csv(file=elections_plcan_csv)
      elections_df_plcan$性別<- customgsub(elections_df_plcan$性別,"'(\\d+)","\\1")
      elections_df_plcan$出生日期<- customgsub(elections_df_plcan$出生日期,"'(\\d+)","\\1")
    }
    elections_df_cand<-select(elections_df_cand,-starts_with("鄉鎮市區"),-starts_with("村里別"))
    elections_df_cand$省市別<- customgsub(elections_df_cand$省市別,"'(\\d+)","\\1")
    elections_df_cand$縣市別<- customgsub(elections_df_cand$縣市別,"'(\\d+)","\\1")
    elections_df_cand$選區別<- customgsub(elections_df_cand$選區別,"'(\\d+)","\\1")
    #elections_df_cand$鄉鎮市區<- customgsub(elections_df_cand$鄉鎮市區,"'(\\d+)","\\1")
    #elections_df_cand$村里別<- customgsub(elections_df_cand$村里別,"'(\\d+)","\\1")
    elections_df_cand$性別<- customgsub(elections_df_cand$性別,"'(\\d+)","\\1")
    elections_df_cand$出生日期<- customgsub(elections_df_cand$出生日期,"'(\\d+)","\\1")
    elections_df_cand$政黨代號<- customgsub(elections_df_cand$政黨代號,"'(\\d+)","\\1") %>%
      as.integer()
    elections_df_cand<-left_join(elections_df_cand,elections_df_party)
    #找出真正的選取區定義，但在補選時好像也定義為000，需轉換
    election_real_elec_dist<- filter(elections_df_dist,customgrepl(名稱,"選區|選舉區|全國|政黨")) %>%
      select("省市別","縣市別","選區別","名稱") %>%
      rename("選舉區名稱"="名稱")
    election_admin_county<-filter(elections_df_dist,鄉鎮市區!="000",村里別!="0000",!customgrepl(名稱,"選區|選舉區|全國|政黨")) %>%
      rename("村里名稱"="名稱")
    #省市別  縣市別  選區別  鄉鎮市區  村里別  村里名稱
    election_admin_dist<-filter(elections_df_dist,鄉鎮市區!="000",村里別=="0000",!customgrepl(名稱,"選區|選舉區|全國|政黨")) %>%
      rename("鄉鎮市區名稱"="名稱") %>%
      select("省市別","縣市別","選區別","鄉鎮市區","鄉鎮市區名稱")
    election_admin_city<-filter(elections_df_dist,鄉鎮市區=="000",選區別=="00",!customgrepl(名稱,"選區|選舉區|全國|政黨")) %>%
      rename("縣市名稱"="名稱") %>%
      select("省市別","縣市別","縣市名稱")
    election_admin_to_elecdist<-left_join(election_real_elec_dist,election_admin_city) %>% #left_join(election_admin_county,election_admin_dist) %>%
      left_join(election_admin_dist) %>%
      left_join(election_admin_county)
    #left_join(election_admin_city) %>%
    #left_join(election_real_elec_dist)
    elections_df_cand$省市別<-as.character(elections_df_cand$省市別)
    election_admin_to_elecdist$省市別<-as.character(election_admin_to_elecdist$省市別)
    elections_df_onekind<-left_join(elections_df_cand,election_admin_to_elecdist,by = c("省市別", "縣市別", "選區別"))
    if (elec_dist_type=='partylist') {
      elections_df_onekind<-elections_df_onekind[,c("省市別",	"縣市別",	"選區別",	"鄉鎮市區",	"村里別",	"號次",	"政黨代號",	"副手",	"政黨名稱",	"選舉區名稱")]
      elections_df_onekind<-left_join(elections_df_plcan,elections_df_onekind,by = c("政黨代號")) #%>%
      #rename(鄉鎮市區.y=鄉鎮市區.x,村里別.y=村里別.x)
      #pick_vector<-c( "省市別",       "縣市別",       "選區別",       "鄉鎮市區.x",  
      #                "村里別.x",     "號次",         "名字",         "政黨代號",    
      #                "性別",         "出生日期",     "年齡",         "出生地",      
      #                "學歷",         "現任",         "當選註記",     "副手",        
      #                "X17",          "政黨名稱",     "選舉區名稱",   "縣市名稱",    
      #                "鄉鎮市區.y",   "鄉鎮市區名稱", "村里別.y",     "村里名稱")
      #名字 名字.x 號次 排名 性別 性別.x 出生日期 出生日期.x 年齡 年齡.x 出生地 出生地.x 
    }
    elections_df_onekind <- cbind(term = term_character, elections_df_onekind, elec_dist_type = elec_dist_type)
    elections_df<-bind_rows(elections_df,elections_df_onekind) #結合參選人以及選區的資料
  } #分區、全國區結束
  #check: filter(elections_df,is.na(選舉區名稱)) %>% View()
  #check: distinct(legislators_needed,areaName,選舉區名稱) %>% View()
}
elections_df <- elections_df[, c("term", "號次", "名字", "性別", "出生日期", "年齡", "出生地", "學歷", "現任", "當選註記", "政黨名稱", "選舉區名稱", "縣市名稱", "鄉鎮市區名稱", "村里名稱", "排名", "elec_dist_type")] %>%
  rename(ballotid = 號次, name = 名字, sex = 性別, birthday = 出生日期, age = 年齡, birthplace = 出生地, education = 學歷, incumbent = 現任, wonelection = 當選註記, party = 政黨名稱, electionarea = 選舉區名稱, admincity = 縣市名稱, admindistrict = 鄉鎮市區名稱, adminvillage = 村里名稱, plranking = 排名) %>%
  mutate_at(c("sex"), funs(customgsub(sex, "2", "女"))) %>%
  mutate_at(c("sex"), funs(customgsub(sex, "1", "男")))

#names(elections_df) <- c("term", "ballotid", "name", "sex", "birthday", "age", "birthplace", "education", "incumbent", "wonelection", "party", "electionarea", "admincity", "admindistrict", "adminvillage", "plranking", "elec_dist_type")


#透過全國行政區的行政區名稱，比對不完整鄉鎮市區名稱的郵遞區號行政區，組裝出行政區郵遞區號
zipcodecsv<-paste0(dataset_file_directory,"zip3.csv")
zipcode_df <- read_csv(zipcodecsv) %>%
  rename(admincity = 縣市名稱, admindistrict = 鄉鎮市區名稱) %>%
  mutate_at(c("admindistrict"), funs(customgsub(admindistrict, "區", ""))) %>% ##鄉鎮市區名稱還沒有統一
  mutate_at("term",funs(as.character))
##從選區資料抓出舊制全國縣市鄉鎮市區
all_admin_dist <- distinct(elections_df, term, admincity, admindistrict) %>%
  filter(!is.na(admincity)) %>%
  mutate_at(c("term"), funs(customgsub(term, "0(\\d{1})", "\\1", perl = TRUE))) %>%
  mutate_at(c("term"), as.character)
all_admin_dist_try <- cbind(all_admin_dist, "fullcountyname" = all_admin_dist$admindistrict) %>%
  mutate_at(c("admindistrict"), funs(stri_sub(admindistrict, from = 1, to = -2))) %>%
  mutate_at("fullcountyname",funs(as.character))
all_admin_dist_with_zip <- left_join(all_admin_dist_try, zipcode_df) %>%
  select(term, admincity, fullcountyname, zip, zip3rocyear) %>%
  rename(admindistrict = fullcountyname)

elections_df_test <- elections_df %>%
  mutate_at(c("term"), funs(customgsub(term, "0(\\d{1})", "\\1", perl = TRUE))) %>%
  mutate_at(c("term"), as.character) %>%
  left_join(all_admin_dist_with_zip)

#立委資料與選區資料合併
legislators <- read_csv(file = paste0(dataset_file_directory, "legislators.csv"))
legislators_needed <- filter(legislators, term %in% c("05", "06", "07", "09")) %>%
  mutate_at(c("term"), funs(customgsub(term, "0(\\d{1})", "\\1", perl = TRUE))) %>%
  mutate_at(c("term"), as.numeric)
legislators_with_election <- left_join(legislators_needed, elections_df_test, by = c("name", "term", "sex")) #
#save(elections_df_test,file=paste0(dataset_file_directory,"rdata",slash,"elections_df_test.RData"))
#save(legislators_with_election, file=paste0(dataset_file_directory,"rdata",slash,"legislators_with_election.RData"))
#test result: filter(legislators_needed,is.na(zip)) %>% View()


##############################################################################
# 第二部分：投票及議案及「問卷答案對照意向」資料,主要也就是RData&excel檔
##############################################################################
myown_vote_bills_file <- "votingdf_datafile_myown_englished.xlsx"
#survey_time_range <- as.data.frame(list(yrmonth=c('099/07', '099/11', '099/12', '100/01', '100/04', '100/06', '105/09', '105/10', '105/11', '105/12', '106/01', '106/04', '106/05')))
survey_time_range <- as.data.frame(list(yrmonth=c("093/07","093/08","093/09","093/10","093/11","093/12","094/01","094/02","094/03","094/04","094/05","094/06","094/07","094/08","094/09","094/10","094/11","094/12","095/01","095/02","095/03","095/04","095/05","095/06","095/07","095/08","095/09","095/10","095/11","095/12","099/11","099/12","100/01","100/02","100/03","100/04","100/05","100/06","100/07","100/08","100/09","100/10","100/11","100/12","101/01","101/02","101/03","101/04","101/05","101/06","101/07","101/08","101/09","101/10","101/11","105/09","105/11","105/12","106/01","106/04","106/05","106/06","106/08","106/10","106/11","106/12","107/01","107/03","107/04","107/05","107/06","107/07")))
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
bills_answer_to_bill <- read.xlsx(myown_vote_bills_file, sheet = 4)
bills_billcontent <- read.xlsx(myown_vote_bills_file, sheet = 1) %>%
  mutate_at("billcontent", funs(as.character)) %>%
  select(-starts_with("pp_related_q_"))
#as.character(unique(bills_billcontent$pp_related_q_1))

#讀取投票紀錄資料-此處通常預處理好，直接load下面mergedf_votes_bills_election_surveyanswer
load(paste0(dataset_file_directory,"rdata",slash,"myown_vote_record_df.RData"))
load(paste0(dataset_file_directory,"rdata",slash,"myown_vote_record_detailed_part_df.RData"))
myown_vote_record_df<-bind_rows(myown_vote_record_df,myown_vote_record_detailed_part_df)
#save(myown_vote_record_df,file=paste0(dataset_file_directory,"rdata",slash,"myown_vote_record_df.RData"))


myown_vote_record_df %<>%
  mutate_at("legislator_name", funs(customgsub(legislator_name, "　", ""))) %>%
  mutate_at(c("billcontent","term","period","meetingno","temp_meeting_no","billn"), funs(as.character)) %>%
  mutate_at("billcontent", funs(trimws)) %>%
  mutate(billid_myown = paste(term, "-", period, "-", temp_meeting_no, "-", meetingno, "-", billn, sep = "")) %>%
  mutate_at("billid_myown", funs(as.character)) %>%
  mutate_at(c("term","period","meetingno","temp_meeting_no","billn"), funs(as.numeric)) %>%
  mutate_cond(term==9 & customgrepl(legislator_name,"簡東明"),legislator_name="簡東明Uliw．Qaljupayare") %>%
  mutate_cond(term==9 & customgrepl(legislator_name,"廖國棟"),legislator_name="廖國棟Sufin．Siluko") %>%
  mutate_cond(term==9 & customgrepl(legislator_name,"鄭天財"),legislator_name="鄭天財Sra．Kacaw") %>%
  mutate_cond(term==9 & customgrepl(legislator_name,"高潞"),legislator_name="高潞．以用．巴魕剌Kawlo．Iyun．Pacidal") %>%
  mutate_cond(term==9 & customgrepl(legislator_name,"陳秀霞"),legislator_name="周陳秀霞")
#filter(myown_vote_record_df,term==9,customgrepl(legislator_name,"陳秀霞")) %>% select(legislator_name) %>% unique()

#mutate_at("legislator_name", funs(recode(opinionstrength)),
#  "n" = 1, "nn" = 2, "m" = 1, "mm" = 2, "b" = 0
#) #%>%


##算出同黨票數
load(paste0(dataset_file_directory,"rdata",slash,"legislators_with_election.RData"))
legislator_term_name_party<-distinct(legislators_with_election,term,name,party.x) %>%
  rename(party=party.x) %>%
  mutate_at("term",funs(as.numeric))
myown_vote_record_df_with_party<-left_join(myown_vote_record_df,legislator_term_name_party,by=c("term","legislator_name"="name")) %>%
  distinct(votedecision,legislator_name,billid_myown,party) %>%
  filter(!is.na(party)) %>%
  group_by(billid_myown,party) %>%
  mutate("total_votes_from_same_party"=n()) %>%
  ungroup() %>%
  group_by(votedecision,billid_myown,party) %>%
  mutate("same_votes_from_same_party"=n()) %>%
  ungroup() %>%
  mutate("percent_of_same_votes_from_same_party"=same_votes_from_same_party/total_votes_from_same_party*100) %>%
  mutate("vote_along_with_majority_in_party"=ifelse(percent_of_same_votes_from_same_party>50,1,0))



mergedf_votes_bills_election_surveyanswer <- filter(myown_vote_record_df, term %in% terms) %>%
  left_join(myown_vote_record_df_with_party) %>%
  left_join(partyseats) %>%
  right_join(bills_billcontent, by = c("billid_myown","term","period","meetingno","temp_meeting_no","billn","billresult","date")) %>% ##,"url"
  right_join(bills_answer_to_bill) %>%  ##問題在這邊
  #篩選出研究範圍
  inner_join(survey_time_range) %>%
  mutate(stdbilldate=as.Date(paste(
    as.integer(substr(date,0,3))+1911,
    substr(date,5,6),
    substr(date,8,9)
  ),"%Y %m %d")) %>%
  mutate(opinionstrength=opinionfrombill) %>%
  mutate_at("opinionstrength",funs(recode(opinionfromconstituent)),
            "n"=1,"nn"=2,"m"=1,"mm"=2,"b"=0
  ) %>%
  mutate("opiniondirectionfromconstituent"=opinionfromconstituent) %>%
  mutate_at("opiniondirectionfromconstituent",funs(recode(opiniondirectionfromconstituent)),
            "n"="n","nn"="n","nnn"="n","m"="m","mm"="m","mmm"="m","b"="b"
  ) %>%
  mutate("opiniondirectionfrombill"=opinionfrombill) %>%
  mutate_at("opiniondirectionfrombill",funs(recode(opiniondirectionfrombill)),
            "n"="n","nn"="n","m"="m","mm"="m","b"="b"
  ) %>%
  mutate(opiniondirectionfromlegislator=NA,respondopinion=NA,success_on_bill=NA) %>%
  #mutate(respondopinion=paste0(votedecision,opiniondirectionfrombill)) %>%
  #mutate(respondopinion) %>%
  #mutate_cond(votedecision=="贊成" & opiniondirectionfromconstituent==opiniondirectionfrombill, respondopinion=2) %>%
  #mutate_cond(votedecision=="贊成" & opiniondirectionfromconstituent!=opiniondirectionfrombill, respondopinion=0) %>%
  mutate_cond( (opiniondirectionfromconstituent==opiniondirectionfrombill), success_on_bill=ifelse(billresult=="Passed",1,0) ) %>%
  mutate_cond( (opiniondirectionfromconstituent!=opiniondirectionfrombill & opiniondirectionfromconstituent != "x" & opiniondirectionfromconstituent != "b"), success_on_bill=ifelse(billresult=="Passed",0,1) ) %>%
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
  mutate_cond(opiniondirectionfromconstituent==opiniondirectionfromlegislator, respondopinion=3) %>%
  mutate_cond(votedecision %in% c("未投票","未出席"), respondopinion=1, opiniondirectionfromlegislator='ignore', respondopinion=1) %>%
  mutate_cond(votedecision=="棄權", respondopinion=1, opiniondirectionfromlegislator='giveup', respondopinion=2) %>%
  mutate_cond(opiniondirectionfromconstituent=='x' | opiniondirectionfrombill=='x', respondopinion=NA) %>%
  #mutate_at("respondopinion",funs(recode(respondopinion)),
  #          "反對n"=2,"反對nn"=2,"贊成m"=2,"贊成mm"=2,
  #          "棄權n"=1,"棄權nn"=1,"棄權m"=1,"棄權mm"=1,
  #          "贊成n"=0,"贊成nn"=0,"反對m"=0,"反對mm"=0
  #) %>%
  rename(name=legislator_name)

#可以看到有回應也有不回應
#distinct(mergedf_votes_bills_election_surveyanswer,votedecision,billid_myown,variable_on_q,value_on_q_variable,name,party,opiniondirectionfromconstituent,opiniondirectionfrombill,opiniondirectionfromlegislator,respondopinion) %>%
#  #testdf %>%
#  filter(billid_myown=="9-2-0-16-67",variable_on_q=="pp_related_q_1",value_on_q_variable=="2016citizen@c2") %>%
#  arrange(name,party) %>%
#  View()

#mergedf_votes_bills_election_surveyanswer %>%
#  filter(!is.na(respondopinion),billid_myown=="9-2-0-17-88",variable_on_q=="pp_related_q_1") %>%
#  distinct(name,votedecision,variable_on_q,respondopinion,billid_myown,party) %>%
#  arrange(party,billid_myown,variable_on_q,respondopinion) %>%
#  View()
#save(mergedf_votes_bills_election_surveyanswer, file = paste0(dataset_file_directory,"rdata",slash,"mergedf_votes_bills_election_surveyanswer.RData"))
#mergedf_votes_bills_election_surveyanswer <- data.frame()
##############################################################################
# 第三部份：把問卷檔加上行政區、選區屬性
##############################################################################
library(haven)
library(labelled)
load(paste0(dataset_file_directory,"rdata",slash,"duplicatedarea.RData"))

##=================以下部分因為已有既存資料檔，讀取後略過不執行#=================
##=================以下部分因為已有既存資料檔，讀取後略過不執行#=================
#找出所有行政區對選區資料，並且找出同一鄉鎮市區有不同選區的部分
#admin_dist_to_elect_dist <- distinct(elections_df_test, term, admincity, electionarea, admindistrict, adminvillage) %>%
#  filter(!is.na(admincity)) %>%
#  left_join(all_admin_dist_with_zip)
#duplicated_area <- distinct(admin_dist_to_elect_dist,term,electionarea,admincity,admindistrict,zip,zip3rocyear) %>%
#  extract(duplicated(.[, c("term", "admincity", "admindistrict")]),)
#把某些共用同一個郵遞區號的行政區合併
#unique_dist_for_elect_dist <- anti_join(admin_dist_to_elect_dist, duplicated_area[, c("term", "admincity", "admindistrict")]) %>%
#  group_by(term, electionarea, admincity, zip, zip3rocyear) %>%
#  summarise(admindistrict = paste0(admindistrict, collapse = "、"))
#以下註解部分為找出多選區的樣本
#duplicated_area[duplicated_area$term == 6, c("zip")] %>%
#  intersect(survey_data[[4]]$zip) %>%
#  unique() %>% 
#  sort()
##=================以上部分因為已有既存資料檔，讀取後略過不執行#=================
##=================以上部分因為已有既存資料檔，讀取後略過不執行#=================

#save(admin_dist_to_elect_dist,duplicated_area,unique_dist_for_elect_dist,file=paste0(dataset_file_directory,"rdata",slash,"duplicatedarea.RData"))
#重要！2010環境的資料因為補選選區有改變，所以在一些鄉鎮市區村里會重複出現多筆紀錄，要先處理一下join的選舉資料
#duplicated_area_just_one_electionarea <- group_by(duplicated_area, term, admincity, admindistrict, zip, zip3rocyear) %>%
#  summarise(electionarea = paste0(electionarea, collapse = "、"))
minus_electionarea <- as.data.frame(list("term" = 7, "electionarea" = "桃園縣第06選區", "admincity" = "桃園縣", "admindistrict" = "中壢市", zip = 320, zip3rocyear = 99))
survey_restricted_data<-c(1,2,3,4) %>%
  lapply(function (X) read.xlsx(paste0(dataset_file_directory, "basic_social_survey_restricted_data.xlsx"), sheet = X))
survey_data<-c("2016_citizen.sav","2010_env.sav","2010_overall.sav","2004_citizen.sav") %>%
  sapply(function (X,...) paste0(...,X), dataset_file_directory, "merger_survey_dataset",slash) %>%
  lapply(haven::read_sav) %>%
  lapply(function (X) {
    othervar<-setdiff(names(X),c("term1","term2"))
    reshape2::melt(X,id.vars = othervar, variable.name = "variable_on_term", value.name = "term") %>%
      dplyr::filter(!is.na(term))
  })  %>%
  lapply(mutate,stdsurveydate=as.Date(paste(year,sm,sd),"%Y %m %d"))

#shaped: 299 295 571
#先依據是否有多數選區存在於單一鄉鎮市區拆開，先串有同一鄉鎮市區內有多選區的，再串同一鄉鎮市區內只有一選區的，然後分別join之後再合併
survey_data <- mapply(function(X,Y) {
  in_complicated_district<-filter(X, id %in% Y$id) %>%
    left_join(Y) %>%
    mutate_at("term",funs(as.character)) %>%
    left_join(admin_dist_to_elect_dist,by=c("term","admincity","admindistrict","adminvillage")) %>%
    select(-zip.y) %>%
    rename(zip=zip.x)
  in_simple_district <- filter(X, !(id %in% Y$id)) %>%
    mutate_at(c("zip"), as.integer) %>%
    mutate_at("term",funs(as.character)) %>%
    left_join(unique_dist_for_elect_dist)
  bind_rows(in_simple_district, in_complicated_district) %>%
    arrange(id)
},X=survey_data,Y=survey_restricted_data)


#save(survey_data,file=paste0(dataset_file_directory,"rdata",slash,"all_survey_combined.RData"))
load(paste0(dataset_file_directory,"rdata",slash,"all_survey_combined.RData"))


####################################################
############### 清理資料：填補遺漏值 ###############
####################################################

#assinging missing value
library(mice)
imputingcalculatebasiscolumn<-list(
  "2004citizen"=c("myown_age","post","v1","v100","v101_1","v101_10","v101_11","v101_2","v101_3","v101_4","v101_5","v101_6","v101_7","v101_8","v101_9","v102","v103","v103a","v104","v105a","v105b","v105c","v106a","v106b","v106c","v107a","v107b","v107c","v108","v109","v110","v111","v112","v113","v114","v115","v115a","v116","v117","v118a","v118b","v118c","v118d","v119","v11b","v11br","v120","v121","v122a","v122ar","v122b1","v122b1r","v122b2","v122c","v122d","v122e","v122f","v122g","v123b","v123br","v123c","v123d","v124","v125b","v125br","v125c","v127","v127a","v128_1a","v128_1b","v128_1c","v128_1d","v128_1e","v128_1f","v128_1g","v128_2a","v128_2b","v128_2c","v128_2d","v128_2e","v128_2f","v128_2g","v129","v13","v130","v131","v132a","v132b","v132c","v132d","v132dr","v132e1","v132e2","v133","v134","v135","v136","v136a","v137","v138a","v138b","v138c","v138d","v138dr","v138e1","v138e2","v139","v14","v140","v141","v142","v15","v16","v17","v18","v19","v20","v21","v22","v23","v24","v25","v26","v27","v28","v29","v3","v30","v31","v32","v33","v34","v35","v36","v37","v38","v39","v3r","v4","v40","v41","v42","v43","v44","v45","v46","v47","v48","v49","v4r","v5","v50","v51","v52","v53","v54","v55","v56","v57","v58","v59","v6","v60","v61","v62","v63","v64","v65","v66","v67","v68","v69","v7","v70","v71","v72","v73","v74","v75","v76","v77","v78","v79","v7a","v80","v81","v82","v83","v84","v85","v86_1","v86_2","v86_3","v86_4","v86_5","v86_6","v86_7","v86_8","v86_9","v87_1","v87_2","v87_3","v87_4","v87_5","v87_6","v88","v89","v8ar","v8b","v8c","v8d","v8dr","v90","v91","v91a","v91b","v92_1","v92_2","v92_3","v92_4","v92_5","v93a","v93b","v94","v94a","v95","v96","v97","v98","v98a","v98b","v98c","v99","v9a","v9b","weight","zip","myown_areakind","myown_eduyr","myown_ses","myown_occp","myown_income","myown_family_income","myown_sex","myown_dad_ethgroup","myown_mom_ethgroup","myown_selfid","myown_int_pol_efficacy","myown_ext_pol_efficacy"),
  "2010env"=c("myown_age","stratum2","stratum3","v1","v10","v102","v103","v104","v105","v105a","v105b","v105c","v106","v107","v11","v12","v13","v14a","v14b","v15a","v15b","v16a","v16b","v17a","v17b","v18a","v18b","v19","v20a","v20b","v21a","v21b","v21c","v22a","v22b","v22c","v23a","v23b","v23c","v24a","v24b","v24c","v25a","v25b","v25c","v26a","v26b","v26c","v26d","v26e","v26f","v26g","v27a","v27b","v27c","v27d","v27e","v27f","v27g","v28a","v28b","v29","v30a","v30b","v31","v32a","v32b","v32c","v33a","v33b","v33c","v33d","v33e","v33f","v34","v35a","v35b","v35c","v36a","v36b","v37a","v37b","v37c","v37d","v37e","v37f","v37g","v37h","v37i","v38a1","v38a2","v38b1","v38b2","v38c1","v38c2","v38d1","v38d2","v38e1","v38e2","v39a","v39b","v39c","v3b","v3br","v4","v40","v41a1","v41a2","v41b","v42a","v42b","v42c","v42d","v43","v44","v45","v46","v47a","v47b","v47c","v47d","v47e","v48","v49","v5","v50","v51","v52","v53","v54","v55","v56","v57","v58","v59","v6","v60","v61","v62","v63","v64","v65","v66a","v66b","v66c","v66d","v66e","v66f","v67","v68","v69","v7","v70a","v70b","v70c","v70d","v70e","v70f","v71","v72","v73","v74","v75","v76","v77","v78","v79","v8","v80","v81","v82","v82c","v82d","v83c","v83d","v84c","v84d","v85c","v85d","v86c","v86d","v87c","v87d","v88c","v88d","v89","v9","v90","v91","v91a","v91b","v92","v92a","v93a3","v93a3r","v93a4","v93a4r","v93b3","v93b3r","v93b5","v93b5r","v93c","v94","v95","v96","v97","v98","v99","zip","myown_areakind","myown_eduyr","myown_ses","myown_occp","myown_income","myown_family_income","myown_sex","myown_dad_ethgroup","myown_mom_ethgroup","myown_selfid","myown_int_pol_efficacy","myown_ext_pol_efficacy"),
  "2010overall"=c("myown_age","stratum2","v1","v10","v100","v100a3","v100a3r","v100a4","v100a4r","v100b3","v100b3r","v100b5","v100b5r","v101a3","v101a3r","v101a4","v101a4r","v101b3","v101b3r","v101b5","v101b5r","v101c","v102","v102a1","v102a2","v102a2a","v102a3","v102b1","v102b2","v102b2a","v102b3","v102b4","v102c","v102d","v102e","v102f1","v102f2","v102f2a","v102f3","v102g1","v102g2","v102g2a","v102g3","v102g4","v102h","v105","v106","v107","v108","v109","v109a","v110","v110r1","v110r2","v111","v111r1","v111r2","v11a","v11bm","v11by","v12","v12h","v12min","v13a","v14","v14h","v14min","v15","v15h","v15min","v16a","v17","v17h","v17min","v18","v18h","v18min","v19a","v20","v20h","v20min","v21a_1","v21a_2","v21a_3","v21a_4","v21a_5","v21a_6","v21a_7","v21a_8","v21a_96","v21a_97","v21a_98","v21b","v21b_h","v21b_min","v21c1","v22","v22h","v22min","v23","v24","v25","v25a_1","v25a_10","v25a_11","v25a_2","v25a_3","v25a_4","v25a_5","v25a_6","v25a_7","v25a_8","v25a_9","v25a_96","v25a_97","v25a_98","v26a","v26a1","v26b","v26b1","v27a","v27b","v28","v29","v3","v30","v31","v32","v33","v34","v35","v36","v37a","v37b","v38a","v38b","v39a","v39b","v39c","v39d","v39e","v3r1","v3r2","v4","v40","v41","v42","v43a","v43b","v44a","v44b","v44c","v44d","v44e","v44f","v44g","v45","v46","v47","v48","v49","v5","v50","v51","v52","v52a","v53","v54","v55","v56a","v56b","v57","v58a","v58b","v58c","v58d","v58e","v58f","v58g","v59","v60","v61","v62","v63","v64_1","v64_2","v64_3","v64_4","v64_5","v64_96","v64_97","v64_98","v64_99","v65a","v65b","v66a","v66b","v66c","v66d","v67a","v67b","v67c","v67d","v67e","v67f","v67g","v67h","v67i","v68a","v68b","v68c","v68d","v68e","v68f","v68g","v68h","v68i","v69a","v69b","v69c","v69d","v69e","v69f","v69g","v6a","v6b","v70a","v70b","v70c","v70d","v70e","v70f","v70g","v71","v72a","v72b","v72c","v72d","v72e","v72f","v72g","v72h","v72i","v72j","v72k","v72l","v73","v74","v74a_1","v74a_2","v74a_3","v74a_96","v74a_97","v74a_98","v75","v76","v77","v78a","v78b","v78c","v78d","v78e","v78f","v78g","v78h","v78i","v79a","v79b","v79c","v79d","v7a","v7a1","v7b","v7b1","v80","v81","v82a","v82b","v82c","v82d","v83","v84","v85","v86","v87","v87a1","v88","v89","v8a1","v9","v90","v91","v92","v93","v94","zip","myown_areakind","myown_eduyr","myown_ses","myown_occp","myown_income","myown_family_income","myown_sex","myown_dad_ethgroup","myown_mom_ethgroup","myown_selfid","myown_int_pol_efficacy","myown_ext_pol_efficacy"),
  "2016citizen"=c("myown_age","a1","a10","a11","a12","a13","a14","a2a","a2m","a2y","a3city","a3zip","a4","a5","a6","a7","a8","a9","b1","b2","b3h","b3m","b4","b5","c10","c11","c12","c13","c14","c15","c16a","c16b","c16c","c1a","c1b","c1c","c1d","c1e","c2","c3","c4","c5","c6","c7","c8","c8r","c9","c9r","d1","d10","d11a","d11b","d12","d13a","d13b","d14a","d14b","d14c","d15","d16a","d16b","d16c","d16d","d16e","d16f","d17a","d17b","d17c","d18a","d18b","d19a","d19b","d20","d21","d22","d23","d2a","d2b","d3a","d3b","d4","d5a","d5b","d5c","d5d","d5e","d5f","d6a","d6b","d6c","d6d","d6e","d6f","d6g","d6h","d7a","d7b","d7c","d7d","d7e","d7f","d7g","d7h","d7i","d7j","d7k","d8a","d8b","d8c","d9a","d9b","e1","e2a","e2b","e2c","e2d","e2e","e2f","e2g","e2h","e2i","edt1","edt2","f1","f2","f3","f4","f5","f6","f7","f8","f9","g1","g2","g3","g4","g5","g6a","g6b","g6c","g6d","g7a","g7b","g7c","g8a","g8b","g8c","h1_01","h1_02","h1_03","h1_04","h1_05","h1_06","h1_07","h1_08","h1_09","h1_10","h1_11","h1_12","h1_13","h1_14","h1_15","h10","h2a","h2b","h2c","h2d","h2e","h2f","h2g","h2h","h3a","h3b","h3c","h4","h5","h6","h6r","h7","h8","h9","id","j10","j11","j12a","j12b","j13a3","j13b4","j13b5","j13b5_88","j13c","j14","j15","j16","j17","j1a","j1b","j1c","j2","j2r","j3","j4","j5a","j5b","j6a3","j6b4","j6b5","j6b5_88","j6c","j7","j7a","j8","j9","ka10","ka11","ka13","ka14","ka5","ka6","ka7","ka8","ka9","kc8","kc9","kh10","kh6","kh8","kh9","kj10","kj14","kj3","kj7","psu","qtype","r_stratum2014","region","region2014","sd","sdt1","sdt2","sm","ssu","stratum2","stratum2014","wave","wr_19_4","wr_19_5","wsel","wsel0","year","year_m","zip","myown_areakind","myown_eduyr","myown_ses","myown_occp","myown_income","myown_family_income","myown_sex","myown_dad_ethgroup","myown_mom_ethgroup","myown_selfid","myown_int_pol_efficacy","myown_ext_pol_efficacy")
)
imputedvaluecolumn<-list(
  "2004citizen"=c("v28","v29","v30","v31","v32","v33","v34","v35","v36","v37","v38","v39","v40","v59","myown_eduyr","myown_ses","myown_occp","myown_income","myown_family_income","myown_sex","myown_dad_ethgroup","myown_mom_ethgroup","myown_selfid","myown_int_pol_efficacy","myown_ext_pol_efficacy"),
  "2010env"=c("v34","v35a","v35b","v35c","myown_eduyr","myown_ses","myown_occp","myown_income","myown_family_income","myown_sex","myown_dad_ethgroup","myown_mom_ethgroup","myown_selfid","myown_int_pol_efficacy","myown_ext_pol_efficacy"),
  "2010overall"=c("v79a","v79b","v79c","v79d","myown_eduyr","myown_ses","myown_occp","myown_workincome","myown_family_income","myown_sex","myown_dad_ethgroup","myown_mom_ethgroup","myown_selfid","myown_int_pol_efficacy","myown_ext_pol_efficacy"),
  "2016citizen"=c("h2a","h2b","h2c","h2d","h2e","h2f","h2g","h2h","h3a","h3b","h3c","myown_eduyr","myown_ses","myown_occp","myown_income","myown_family_income","myown_sex","myown_dad_ethgroup","myown_mom_ethgroup","myown_selfid","myown_int_pol_efficacy","myown_ext_pol_efficacy")
)
generate_predictor_matrix<-function(df,calculationbasisvar=c(),imputedOnlyVars=c()) {
  #calculationbasisvar<-imputingcalculatebasiscolumn_assigned
  #imputedOnlyVars<-imputedvaluecolumn_assigned
  ## Extract all variable names in dataset
  allVars <- names(df)
  ## names of variables with missingness
  missVars <- names(df)[colSums(is.na(df)) > 0]
  predictorMatrix <- matrix(0, ncol = length(allVars), nrow = length(allVars))
  rownames(predictorMatrix) <- allVars
  colnames(predictorMatrix) <- allVars
  calculationbasisvar <- if(length(calculationbasisvar)==0) allVars else calculationbasisvar
  imputerVars <- intersect(allVars,calculationbasisvar)
  imputerMatrix <- predictorMatrix
  imputerMatrix[,imputerVars] <- 1
  imputedOnlyVars <- if(length(imputedOnlyVars)==0) missVars else imputedOnlyVars
  ## Imputers that have missingness must be imputed.
  imputedVars <- intersect(unique(c(imputedOnlyVars, imputerVars)), missVars)
  imputedMatrix <- predictorMatrix
  imputedMatrix[imputedVars,] <- 1
  predictorMatrix <- imputerMatrix * imputedMatrix
  ## Diagonals must be zeros (a variable cannot impute itself)
  diag(predictorMatrix) <- 0
  return(predictorMatrix)
}
survey_data_test <- list()
for (i in 1:length(survey_data)) {
  #lapply(survey_data,function(X,missingvaluecolumn_assigned,imputingcalculatebasiscolumn_assigned) {
  #missingvaluecolumn_assigned<-missingvaluecolumn
  #imputingcalculatebasiscolumn_assigned<-imputingcalculatebasiscolumn
  X<-survey_data[[i]]
  imputingcalculatebasiscolumn_assigned <- extract2(imputingcalculatebasiscolumn,X$SURVEY[1]) %>%
    intersect(names(X))
  imputedvaluecolumn_assigned <- extract2(imputedvaluecolumn,X$SURVEY[1]) %>%
    intersect(names(X))
  proceeding_na_var<-union(imputingcalculatebasiscolumn_assigned,imputedvaluecolumn_assigned) %>%
    setdiff(c("myown_age"))
  X %<>% dplyr::mutate_at(proceeding_na_var,dplyr::funs(replace(.,. %in% c(93:99,996:999,9996:9999),NA ) ) )
  predictor_matrix<-generate_predictor_matrix(X,imputingcalculatebasiscolumn_assigned,imputedvaluecolumn)
  #mice::md.pattern(X[,missingvaluecolumn_assigned])
  miceMod <- mice::mice(X,
                        method="rf",
                        predictorMatrix = predictor_matrix)  # perform mice imputation, based on random forests.
  survey_data_test[[i]]<- mice::complete(miceMod)  # generate the completed data.
  save(survey_data_test,file=paste0(dataset_file_directory,"rdata",slash,"miced_survey_2_df.RData"))
  #},missingvaluecolumn,imputingcalculatebasiscolumn)
}
#conditional random field



#填補遺漏值
#filling in missing value


#檢查亂報投票意向

survey_data_test <- lapply(survey_data,function(X,need_ses_var_assigned) {
  need_ses_var_assigned %<>% extract2(X$SURVEY[1]) %>%
    intersect(names(X))
  X %<>% mutate_at(missingvaluecolumn_assigned,funs(replace(.,. %in% c(93:99,996:999,9996:9999),NA ) ) )
  efa.results<-factanal(x=as.matrix(X[,need_ses_var_assigned]) ,factors=4, rotation="promax")
},need_ses_var)

zip_to_party<-distinct(elections_df_test,term,zip,party,wonelection) %>%
  mutate_at("zip",funs(as.character)) %>%
  mutate_at("party",funs(as.character)) %>%
  unique()
#正確的選區與參選人
lieing_check<-read.xlsx(paste(dataset_file_directory,"merger_survey_dataset",slash,"recode_record.xlsx",sep=""), sheet = 4) %>% #, endRow = 1896
  distinct(lieing_check,h5,h6r,h7,h8,h9,id,zip,code) %>% #,h5,h6r,h7,h8,h9 #,v84,v85,v86,v88,v93,v94
  mutate("term"=9) %>%
  rename(party=code) %>%
  mutate_at(c("zip","party"),funs(as.character)) #要檢驗的所有投票意向
zip_to_party_with_jump_answer<-distinct(lieing_check,term,party,zip) %>%
  filter(customgrepl(party,"廢票|沒有投票權")) %>%
  cbind("wonelection"=NA) %>%
  rbind(zip_to_party)

lieing_check<-mutate(lieing_check,bluepoints=0,greenpoints=0) %>%
  #mutate(bluepoints=ifelse(v84 %in% c(1),bluepoints+1,bluepoints)) %>% #
  mutate(bluepoints=ifelse(h5 %in% c(1,3),bluepoints+1,bluepoints)) %>%
  #mutate(bluepoints=ifelse(v86 %in% c(1,3),bluepoints+1,bluepoints)) %>% #
  mutate(bluepoints=ifelse(h6r %in% c(1,3),bluepoints+1,bluepoints)) %>%
  #mutate(bluepoints=ifelse(v88 %in% c(1,3),bluepoints+1,bluepoints)) %>% #
  mutate(bluepoints=ifelse(h7 %in% c(1,3,4),bluepoints+1,bluepoints)) %>%
  #mutate(bluepoints=ifelse(v93 %in% c(1,3,5),bluepoints+1,bluepoints)) %>% #
  mutate(bluepoints=ifelse(h8 %in% c(1,3,4,9),bluepoints+1,bluepoints)) %>%
  #mutate(bluepoints=ifelse(v94 %in% c(1,3,5),bluepoints+1,bluepoints)) %>% #
  mutate(bluepoints=ifelse(h9 %in% c(1,3,4,9),bluepoints+1,bluepoints)) %>%
  #mutate(greenpoints=ifelse(v84 %in% c(2),greenpoints+1,greenpoints)) %>% #
  mutate(greenpoints=ifelse(h5 %in% c(2),greenpoints+1,greenpoints)) %>%
  #mutate(greenpoints=ifelse(v86 %in% c(2,4),greenpoints+1,greenpoints)) %>% #
  mutate(greenpoints=ifelse(h6r %in% c(2,6,9,19),greenpoints+1,greenpoints)) %>%
  #mutate(greenpoints=ifelse(v88 %in% c(2,4),greenpoints+1,greenpoints)) %>% #
  mutate(greenpoints=ifelse(h7 %in% c(2,6,9,19),greenpoints+1,greenpoints)) %>%
  #mutate(greenpoints=ifelse(v93 %in% c(2,4),greenpoints+1,greenpoints)) %>% #
  mutate(greenpoints=ifelse(h8 %in% c(2,5,7,10),greenpoints+1,greenpoints)) %>%
  #mutate(greenpoints=ifelse(v94 %in% c(2,4),greenpoints+1,greenpoints))#
  mutate(greenpoints=ifelse(h9 %in% c(2,5,7,10),greenpoints+1,greenpoints))

clear_observed_value_green<-filter(lieing_check, greenpoints>=3, greenpoints>bluepoints, term==9) %>%
  anti_join(zip_to_party_with_jump_answer) %>%
  select(-party) %>%
  left_join(zip_to_party) %>%
  filter(customgrepl(party,"民主進步黨|台灣團結聯盟|時代力量|綠黨社會民主黨聯盟|綠黨"))
clear_observed_value_blue<-filter(lieing_check, bluepoints>=3, bluepoints>greenpoints, term==9) %>%
  anti_join(zip_to_party_with_jump_answer) %>%
  select(-party) %>%
  left_join(zip_to_party) %>%
  filter(customgrepl(party,"中國國民黨|新黨|親民黨"))


#lieing_check,h6r %in% c(1,3), h7 %in% c(1,3,4), h8 %in% c(1,3,4,9)

#c("跳答","忘記了、不知道","拒答")
lieing_check_with_value<-filter(lieing_check,!(h6r %in% c(96,97,98,99)) ) %>%
  #v86 %in% c(7,9,96,97,98,99) 
  anti_join(zip_to_party_with_jump_answer) %>%
  anti_join(clear_observed_value_green,by=c("id")) %>%
  anti_join(clear_observed_value_blue,by=c("id")) %>%
  mutate("party"=NA)
lieing_check_missing<-filter(lieing_check, h6r %in% c(96,97,98,99)) %>%
  #v86 %in% c(7,9,96,97,98,99) & (v85!=99)
  mutate("party"=NA) %>%
  anti_join(clear_observed_value_green,by=c("id")) %>%
  anti_join(clear_observed_value_blue,by=c("id"))

exclude_result_blue<-cbind("bluepoints"=rep(3:5,each=4),party=c("民主進步黨","時代力量","台灣團結聯盟","綠黨社會民主黨聯盟")) %>%
  as.data.frame() %>%
  mutate_at(c("party","bluepoints"),funs(as.character)) %>%
  mutate_at("bluepoints",funs(as.numeric))
exclude_result_green<-cbind("greenpoints"=rep(3:5,each=3),party=c("中國國民黨","親民黨","新黨")) %>%
  as.data.frame() %>%
  mutate_at(c("party","greenpoints"),funs(as.character)) %>%
  mutate_at("greenpoints",funs(as.numeric))

correct_check_result<-filter(lieing_check,!(party %in% c("跳答","忘記了、不知道","拒答","忘記了,不知道","跳答或不適用","選人不選黨") ) ) %>% #(v85==99) | 
  semi_join(zip_to_party_with_jump_answer) %>%
  bind_rows(clear_observed_value_green,clear_observed_value_blue)# %>%
#mutate("party"=h6r)# %>%
#mutate_at("party",funs(as.factor))# %>%
#bind_rows(lieing_check_result) %>%
#bind_rows(lieing_check_missing)
binded_check_result<-bind_rows(correct_check_result,lieing_check_with_value,lieing_check_missing) %>%
  #anti_join(exclude_result_blue) %>%
  #anti_join(exclude_result_green) %>% %>%
  #mutate_cond(paste0(bluepoints, party) %in% do.call(paste0, exclude_result_blue),party=NA) %>%
  #mutate_cond(paste0(greenpoints, party) %in% do.call(paste0, exclude_result_green),party=NA) %>%
  mutate_at(c("id","zip","h5","h6r","h7","h8","h9","party"),funs(as.factor)) %>%
  #,"v84","v86","v88","v93","v94"
  arrange(id,wonelection) %>%
  group_by(id) %>%
  filter(!(duplicated(id)))


#group_by(binded_check_result,id) %>%
#  filter(n()>1) %>%
#  View()
#binded_check_result<-binded_check_result[order(testorder),]
#binded_check_result<-group_by(binded_check_result,id) %>%
#  filter(!(duplicated(id)))
#select(X2016_citizen_with_restricted,id) %>%
#  group_by(id) %>% 
#  filter(n()>1) %>%
#  View()
#select(X2016_citizen_with_restricted,id) %>%
#group_by(id) %>% 
#filter(duplicated(id)) %>%
#View()
duplicated(X2016_citizen_with_restricted$id)
length(X2016_citizen_with_restricted$id[duplicated(X2016_citizen_with_restricted$id)])

#identical(filter(zip_to_party_with_jump_answer,zip==103)[3,2],filter(lieing_check_with_value,zip==103)[1,8])
#標準化z-normalization或min-max scale






## Extract all variable names in dataset
allVars <- names(binded_check_result)
## names of variables with missingness
missVars <- names(binded_check_result)[colSums(is.na(binded_check_result)) > 0]
predictorMatrix <- matrix(0, ncol = length(allVars), nrow = length(allVars))
rownames(predictorMatrix) <- allVars
colnames(predictorMatrix) <- allVars
imputerVars <- c("zip","h6r","party","bluepoints","greenpoints")#,"v86"
imputerMatrix <- predictorMatrix
imputerMatrix[,imputerVars] <- 1
imputedOnlyVars <- c("party")
## Imputers that have missingness must be imputed.
imputedVars <- intersect(unique(c(imputedOnlyVars, imputerVars)), missVars)
imputedMatrix <- predictorMatrix
imputedMatrix[imputedVars,] <- 1
predictorMatrix <- imputerMatrix * imputedMatrix
## Diagonals must be zeros (a variable cannot impute itself)
diag(predictorMatrix) <- 0
predictorMatrix
require(mice)
i<-1
repeat {
  original_binded_check_result<-if(i==1) {
    binded_check_result %>%
      filter(!is.na(party))
  } else {
    original_binded_check_result %>%
      filter(!is.na(party))
  }
  binded_check_result$party <- factor(binded_check_result$party) 
  mice.lieing_check_imputing <- mice(binded_check_result,
                                     m = 1,           # 產生三個被填補好的資料表
                                     maxit = 7,      # max iteration
                                     method = "rf", # 使用CART決策樹，進行遺漏值預測
                                     predictorMatrix = predictorMatrix,
                                     seed = 188)      # set.seed()，令抽樣每次都一樣
  complete(mice.lieing_check_imputing, 1)
  mice.lieing_check_imputing_result<-complete(mice.lieing_check_imputing, 1) %>%
    mutate_at(c("zip","party","bluepoints","greenpoints"),funs( as.character )) %>%
    mutate_at(c("bluepoints","greenpoints"),funs( as.numeric ))
  predict_binded_check_result<-anti_join(mice.lieing_check_imputing_result,original_binded_check_result,by=c("id"))
  correct_part_mice.lieing_check_imputing_result<-predict_binded_check_result %>%
    semi_join(zip_to_party_with_jump_answer,by=c("zip","party","term")) %>%
    anti_join(exclude_result_blue) %>%
    anti_join(exclude_result_green) %>%
    mutate_cond(paste0(bluepoints, party) %in% do.call(paste0, exclude_result_blue),party=NA) %>%
    mutate_cond(paste0(greenpoints, party) %in% do.call(paste0, exclude_result_green),party=NA) %>%
    mutate_cond(customgrepl(party,"沒有投票權"),party=NA) %>%
    filter(!is.na(party))
  incorrect_part_mice.lieing_check_imputing_result<-predict_binded_check_result %>%
    anti_join(correct_part_mice.lieing_check_imputing_result,by=c("id"))
  if (nrow(incorrect_part_mice.lieing_check_imputing_result)==0) {
    binded_check_result<-mice.lieing_check_imputing_result
    break
  } else {
    message("i=",i,"; ",nrow(incorrect_part_mice.lieing_check_imputing_result))
    incorrect_part_mice.lieing_check_imputing_result<-mutate(incorrect_part_mice.lieing_check_imputing_result,"party"=NA)
    binded_check_result<-bind_rows(original_binded_check_result,correct_part_mice.lieing_check_imputing_result,incorrect_part_mice.lieing_check_imputing_result) %>%
      mutate_at(c("party","zip"),funs(as.factor))
    original_binded_check_result<-bind_rows(original_binded_check_result,correct_part_mice.lieing_check_imputing_result)
  }
  i <- i + 1
}
mutate_at(binded_check_result,c("zip","party","bluepoints","greenpoints"),funs(as.character)) %>%
  mutate_at(c("bluepoints","greenpoints"),funs(as.numeric)) %>%
  mutate_cond( !(paste0(zip, party) %in% do.call(paste0, zip_to_party_with_jump_answer[,c(1,2)]) ),party=NA) %>%
  #semi_join(zip_to_party_with_jump_answer,by=c("zip","party","term")) %>%
  #anti_join(exclude_result_blue) %>%
  #anti_join(exclude_result_green) %>%
  arrange(id) %>%
  write_csv(path="predict_party_tendancy.csv")
#fit <- with ( mice.lieing_check_correct_result, glm( party ~ zip + h5 + h6r + h7 + h8 + h9 ) )
#pooled <- pool( fit )
summary( mice.lieing_check_correct_result )


require(mice)
mice.binaryglmdata<-mice(binaryglmdata,
                         m = 1,           # 產生三個被填補好的資料表
                         maxit = 5,      # max iteration
                         method = "cart", # 使用CART決策樹，進行遺漏值預測
                         seed = 188)
complete(mice.binaryglmdata, 1) # 1st data


mice.X2010_overall_with_restricted <- mice(X2010_overall_with_restricted,
                                           m = 3,           # 產生三個被填補好的資料表
                                           maxit = 5,      # max iteration
                                           method = "cart", # 使用CART決策樹，進行遺漏值預測
                                           seed = 188)      # set.seed()，令抽樣每次都一樣
complete(mice.X2010_overall_with_restricted, 1) # 1st data
complete(mice.X2010_overall_with_restricted, 2) # 2nd data
complete(mice.X2010_overall_with_restricted, 3) # 2nd data


mice.X2010_env_with_restricted <- mice(X2010_env_with_restricted,
                                       m = 3,           # 產生三個被填補好的資料表
                                       maxit = 5,      # max iteration
                                       method = "cart", # 使用CART決策樹，進行遺漏值預測
                                       seed = 188)      # set.seed()，令抽樣每次都一樣
complete(mice.X2010_env_with_restricted, 1) # 1st data
complete(mice.X2010_env_with_restricted, 2) # 2nd data
complete(mice.X2010_env_with_restricted, 3) # 2nd data


mice.X2016_citizen_with_restricted <- mice(X2016_citizen_with_restricted,
                                           m = 3,           # 產生三個被填補好的資料表
                                           maxit = 5,      # max iteration
                                           method = "cart", # 使用CART決策樹，進行遺漏值預測
                                           seed = 188)      # set.seed()，令抽樣每次都一樣
complete(mice.X2016_citizen_with_restricted, 1) # 1st data
complete(mice.X2016_citizen_with_restricted, 2) # 2nd data
complete(mice.X2016_citizen_with_restricted, 3) # 3rd data






####################################################
####  latent variables 
####  將職業社經地位、家庭收入、教育程度萃取成為階級
####################################################
#reset
load(paste0(dataset_file_directory,"rdata",slash,"all_survey_combined.RData"))
#load imputed survey
load(paste0(dataset_file_directory,"rdata",slash,"miced_survey_df.RData"))
survey_data<-miced_surveydata
for(i in 1:length(X$myown_age)) {
  A=survey_data[[1]]$myown_age[i] ##source
  B=X$myown_age[i] ##miced
  if (is.na(A)) {
    message("NA at source")
    next
  } else {
    if (A != B) {
      message(i, ":", A, "not equals to", B)
    } else {
      #message(i, "done!")
    }
  }
}

need_ses_var<-list(
  "2004citizen"=c("myown_eduyr","myown_ses","myown_income"), #myown_income, myown_occp,"myown_family_income", v127 同住家人數
  "2010env"=c("myown_eduyr","myown_ses","myown_income"),#myown_income,"myown_family_income", myown_occp, v105 請問您家中,包含您本人在內,現在有幾個人住在一起?
  "2010overall"=c("myown_eduyr","myown_ses","myown_workincome"),#myown_workincome,"myown_family_income", myown_occp
  "2016citizen"=c("myown_eduyr","myown_ses","myown_income")#myown_income,"myown_family_income", myown_occp, j1 請問您家中,包含您本人在內,現在有幾個人住在一起?a 包含您本人在內,一共有幾位?
)
survey_data_test <- lapply(survey_data,function(X,need_ses_var_assigned) {
  need_ses_var_assigned %<>% extract2(X$SURVEY[1]) %>%
    intersect(names(X))
  X %<>% mutate_at(missingvaluecolumn_assigned,funs(replace(.,. %in% c(93:99,996:999,9996:9999),NA ) ) )
  efa.results<-factanal(x=as.matrix(X[,need_ses_var_assigned]) ,factors=4, rotation="promax")
  },need_ses_var)

dataset.for.fa<-distinct(complete_survey_dataset,SURVEY,id,myown_eduyr,myown_ses,myown_family_income) %>%
  filter(!is.na(myown_eduyr),!is.na(myown_ses),!is.na(myown_family_income))
fa.class<-factanal(x= ~myown_eduyr+myown_ses+myown_family_income, 1, data = dataset.for.fa, rotation="varimax", scores=c("regression"),na.action = na.omit)
complete_survey_dataset<-left_join(complete_survey_dataset,cbind(dataset.for.fa,"myown_factoredclass"=fa.class$scores[,1]))
#install.packages("psy")
#library(psy)
#psy::scree.plot(fa.class$correlation)


################################################
#### 用填補好的問卷部分取代原始問卷
################################################


load(paste0(dataset_file_directory,"rdata",slash,"miced_survey_df.RData"))

comparison <- dplyr::anti_join(miced_surveydata[[1]],survey_data[[1]])
mergedf <- function (old,new) {
  common_col<-intersect(colnames(old),colnames(new))
  return(common_col)
}
comparison <- mapply(mergedf, old=survey_data, new=miced_surveydata)
comparison %>% View()


################################################
#### latent variables 政治參與
#### 用item respond抓出隱藏變數「政治參與程度」
#### GRM Model暫時先頂著用
################################################

#2004citizen: v28 v29 v30 v31 v32 v33 v34 v35 v36 v37 v38 v39 v40 v59
#2016citizen-fit2-z1: b4 h2a h2b h2c h2d h2e h2f h2g h2h h3a h3b h3c h4
#2010overall-fit2: v79a v79b v79c v79d 
#2010env-fit1: v34 v35a v35b v35c ( v33f v75 v76 v77-為了環保而刻意不買某些產品,常不常參與社區的環保工作,常不常反應社區中容易造成天災危險的情況,常不常反應社區中造成環境污染的情況)


library(ltm)
library(eRm)
need_particip_var<-list(
  "2004citizen"=c("v28","v29","v30","v31","v32","v33","v34","v35","v36","v37","v38","v39","v40","v59"),
  "2016citizen"=c("h2a","h2b","h2c","h2d","h2e","h2f","h2g","h2h","h3a","h3b","h3c"),
  "2010overall"=c("v79a","v79b","v79c","v79d"),
  "2010env"=c("v34","v35a","v35b","v35c")
)
survey_data %<>% lapply(function(X,need_particip_var_assigned) {
  need_particip_var_assigned %<>% extract2(X$SURVEY[1]) %>%
    intersect(names(X))
  X %<>% mutate_at(missingvaluecolumn_assigned,funs(replace(.,. %in% c(93:99,996:999,9996:9999),NA ) ) )
  recode_list<-list(
    "2004citizen"=list("1"=4,"2"=3,"3"=2,"4"=1),
    "2016citizen"=list("1"=4,"2"=3,"3"=2,"4"=1),
    "2010overall"=list("1"=3,"2"=2,"3"=1),
    "2010env"=list("1"=2,"2"=1)
  ) %>%
    extract2(X$SURVEY[1])
  X %<>% mutate_at(need_particip_var_assigned,funs(dplyr::recode),!!!recode_list)
},need_particip_var
)


#################### GRM Model ####################
survey_data <- lapply(survey_data,function(X,need_particip_var_assigned) {
  need_particip_var_assigned %<>% extract2(X$SURVEY[1]) %>%
    intersect(names(X))
  fit1 <- ltm::grm(X[,need_particip_var_assigned], constrained = TRUE, na.action = na.omit, start.val = "random")
  fit2 <- ltm::grm(X[,need_particip_var_assigned], na.action = na.omit, start.val = "random")
  fit_testresult<-anova(fit1, fit2)
  if ((fit_testresult$p.value<=0.05) & (fit_testresult$L0 < fit_testresult$L1) ) {
    fit<-fit2
  } else {
    fit<-fit1
  }
  margins(fit)
  summary(fit)
  coef(fit)
  #if (fit_testresult$aic0>fit_testresult$aic1 & fit_testresult$bic0>fit_testresult$bic1) {
  #  fit<-fit2
  #} else {
  #  fit<-fit1
  #}
  X %<>% left_join(
    fit %>%
      factor.scores() %>%
      use_series("score.dat") %>%
      dplyr::select(-contains("Exp"),-contains("Obs"),-contains("se.z1")) %>%
      rename(myown_factored_partcip=z1)
    )
  X$myown_factored_partcip %<>% scale() %>% as.numeric()
  X
},need_particip_var)

information(fit, c(-4, 4))
sapply(1:length(participation_var[[itrn]]),function (X) information(fit, c(-4, 4), items = c(X)) )
#characteristic curve for each item
plot(fit, lwd = 0.8, cex = 0.8, legend = TRUE, cx = "left", xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
#information curve
plot(fit, type = "IIC", lwd = 0.8, cex = 0.5, legend = TRUE, cx = "topleft",xlab = "Latent Trait", cex.main = 0.8, cex.lab = 1, cex.axis = 1)
#test information curve
plot(fit, type = "IIC", items = 0, lwd = 2, xlab = "Latent Trait",cex.main = 1, cex.lab = 1, cex.axis = 1)
info1 <- information(fit, c(-4, 0))
info2 <- information(fit, c(0, 4))
text(-2.5, 8, labels = paste("Information in (-4, 0):",paste(round(100 * info1$PropRange, 1), "%", sep = ""),"\n\nInformation in (0, 4):",paste(round(100 * info2$PropRange, 1), "%", sep = "")), cex = 0.7)
par(mfrow = c(1, 1)) #configure how many figures would show in row and column
#characteristic curve overall in different category
#plot(fit, category = 1, lwd = 0.8, cex = 0.8, legend = TRUE, cx = -0.8,cy = 0.85, xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8,cex.axis = 0.8)
for (ctg in 1:4) {
  plot(fit, category = ctg, lwd = 0.8, cex = 0.8, annot = TRUE,
       xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8,
       cex.axis = 0.8)
  Sys.sleep(2)
}


#################### Extended Rasch Model ####################
pcm1 <- PCM(survey_data[[itrn]][,participation_var[[itrn]]])
rsm1 <- RSM(survey_data[[itrn]][,participation_var[[itrn]]])
lrsm1<- LRSM(survey_data[[itrn]][,participation_var[[itrn]]])
thresholds(pcm1)
plotPImap(pcm1)
LRtest(pcm1)


#################### Generalized Partial Credit Model - Polytomous IRT ####################
#################### Finch, W. Holmes＆French, Brian F. (2015). Latent Variable Modeling with R. Florence: Taylor and Francis
## 2016 not fit: gpcm, rasch 1PL all not fit;
gpcmconstraint<-"gpcm" #c("gpcm", "1PL", "rasch")
survey_data.gpcm<-gpcm(survey_data[[itrn]][,participation_var[[itrn]]],constraint=gpcmconstraint,na.action=na.omit,start.val="random")
summary(survey_data.gpcm)
plot(survey_data.gpcm,, lwd = 0.8, cex = 0.8, legend = TRUE, cx = "left", xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
plot(survey_data.gpcm,type=c("IIC"),, lwd = 0.8, cex = 0.8, legend = TRUE, cx = "left", xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
GoF.gpcm(survey_data.gpcm)



survey_data[[itrn]][,participation_var[[itrn]]] <- lapply(survey_data[[itrn]][,participation_var[[itrn]]],function (X) {
  X<-ifelse(X %in% c(93:99,996:999,9996:9999), NA, X)
})
survey_data[[itrn]][,participation_var[[itrn]]]<-mutate_at(survey_data[[itrn]][,participation_var[[itrn]]],participation_var[[itrn]],as.factor) %>%
  mutate_at(participation_var[[itrn]],factor)
#margins(fit1)






#############################把問卷資料變形以便串連及行政區、選舉資料#################################
library(reshape2)

survey_q_id<-list(
    c("c1a",	"c1b",	"c1c",	"c1d",	"c1e",	"c2",	"c3",	"c4",	"c5",	"c6",	"c10",	"c11",	"c12",	"c13",	"c14",	"d1",	"d2a",	"d2b",	"d3a",	"d3b",	"d4",	"d5a",	"d5b",	"d5c",	"d5d",	"d5e",	"d5f",	"d6a",	"d6b",	"d6c",	"d6d",	"d6e",	"d6f",	"d6g",	"d6h",	"d7a",	"d7b",	"d7c",	"d7d",	"d7e",	"d7f",	"d7g",	"d7h",	"d7i",	"d7j",	"d7k",	"d8a",	"d8b",	"d8c",	"d11a",	"d11b",	"d12",	"d13a",	"d13b",	"d14a",	"d14b",	"d14c",	"d17a",	"d17b",	"d17c",	"e2a",	"e2b",	"e2c",	"e2d",	"e2e",	"e2f",	"e2g",	"e2h",	"e2i",	"f3",	"f4",	"f5",	"f8",	"f9",	"h10"),
    c("kv21c_0", "kv31_0", "kv67_0", "v14a", "v14b", "v15a", "v15b", "v16a", "v16b", "v19", "v20a", "v20b", "v21c", "v22a", "v22b", "v22c", "v23a", "v23b", "v23c", "v24a", "v24b", "v24c", "v25a", "v25b", "v25c", "v26a", "v26b", "v26c", "v26d", "v26e", "v26f", "v26g", "v27a", "v27b", "v27c", "v27d", "v27e", "v27f", "v27g", "v28a", "v28b", "v29", "v30a", "v30b", "v31", "v32a", "v32b", "v32c", "v36a", "v36b", "v37a", "v37b", "v37c", "v37d", "v37e", "v37f", "v37g", "v37h", "v37i", "v38a1", "v38a2", "v38b1", "v38b2", "v38c1", "v38c2", "v38d1", "v38d2", "v38e1", "v38e2", "v39a", "v39b", "v39c", "v40", "v57", "v58", "v59", "v63", "v66c", "v66f", "v67", "v68", "v69", "v70b", "v70c", "v70d", "v70e", "v70f"),
    c("v39c", "v39d", "v39e", "v40", "v41", "v78a", "v78b", "v78c", "v78d", "v78e", "v78f", "v78g", "v78h", "v78i", "v90", "v91", "v92"),
    c("v25","v26","v27","v41","v42","v43","v44","v45","v46","v60","v61","v62","v65","v74","v91a","v91b","v92_1","v92_2","v92_3","v92_4","v92_5","v93a","v93b","v95","v96","v97","v105a","v105b","v105c","v106a","v106b","v106c","v107a","v107b","v107c","v114","v118a","v118b","v118c","v118d")
  )

survey_data_melted<-mapply(function(X,Y) {
  other_var_as_id<-setdiff(names(X),Y)
  reshape2::melt(X, id.vars = other_var_as_id, variable.name = "SURVEYQUESTIONID", value.name = "SURVEYANSWERVALUE") %>%
    dplyr::mutate_at("SURVEYANSWERVALUE",funs(as.character)) %>%
    dplyr::group_by( SURVEYQUESTIONID ) %>%
    dplyr::mutate("all_pos_on_same_q_by_nation"=n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SURVEYQUESTIONID,SURVEYANSWERVALUE) %>%
    dplyr::mutate("same_pos_on_same_q_by_nation"=n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate("same_pos_to_all_ratio_by_nation"=same_pos_on_same_q_by_nation/all_pos_on_same_q_by_nation*100) %>%
    dplyr::group_by( SURVEYQUESTIONID,electionarea ) %>%
    dplyr::mutate("all_pos_on_same_q_by_electionarea"=n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SURVEYQUESTIONID,SURVEYANSWERVALUE,electionarea) %>%
    dplyr::mutate("same_pos_on_same_q_by_electionarea"=n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate("same_pos_to_all_ratio_by_electionarea"=same_pos_on_same_q_by_electionarea/all_pos_on_same_q_by_electionarea*100)
}, X=survey_data, Y=survey_q_id)

survey_data_melted_names<-lapply(survey_data_melted,names)
common_var<-Reduce(intersect, survey_data_melted_names)
complete_survey_dataset<-lapply(survey_data_melted,extract,common_var) %>%
  bind_rows() %>%
  mutate_at("SURVEYANSWERVALUE", funs(as.character)) %>%
  reshape2::melt(id.vars = setdiff(colnames(.),c("term1","term2")), variable.name = "variable_on_term", value.name = "term")

#save(complete_survey_dataset,file=paste0(dataset_file_directory,"rdata",slash,"complete_survey_dataset.RData"))
##針對調查問卷資料處理變形，以便合併

#"c1a","c1b","c1c","c1d","c1e","c2","c3","c4","c5","c6","c10","c11","c12","c13","c14","d1","d2a","d2b","d3a","d3b","d4","d5a","d5b","d5c","d5d","d5e","d5f","d6a","d6b","d6c","d6d","d6e","d6f","d6g","d6h","d7a","d7b","d7c","d7d","d7e","d7f","d7g","d7h","d7i","d7j","d7k","d8a","d8b","d8c","d11a","d11b","d12","d13a","d13b","d14a","d14b","d14c","d17a","d17b","d17c","e2a","e2b","e2c","e2d","e2e","e2f","e2g","e2h","e2i","f3","f4","f5","f8","f9","h10","kh10"

