t_sessioninfo<-sessionInfo()
t_sessioninfo_running<-gsub(" ","",t_sessioninfo$running)
t_sessioninfo_running<-gsub("[>=()]","",t_sessioninfo_running)
filespath<-switch(t_sessioninfo_running,
                  Ubuntu16.04.4LTS="/mnt/e/Software/scripts/R/",
                  Windows7x64build7601ServicePack1="C:\\NTUSpace\\",
                  Windows10x64build16299 = "E:\\Software\\scripts\\R\\",
                  Windows8x64build9200 = "E:\\Software\\scripts\\R\\"
                  )
#filespath <- "E:\\Software\\scripts\\R\\"
#filespath <- "/mnt/e/Software/scripts/R/"
source(file = paste(filespath, "shared_functions.R", sep = ""))
dataset_file_directory <- switch(t_sessioninfo_running,
                                 Windows7x64build7601ServicePack1="C:\\NTUSpace\\dataset\\",
                                 Windows8x64build9200 = "D:\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\",
                                 Windows10x64build16299 = "D:\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\",
                                 Ubuntu16.04.4LTS="/mnt/d/OneDrive/OnedriveDocuments/NTU/Work/thesis/dataset(2004-2016)/"
                                 )
#選舉資料
overall_elec_dist_types<-c('區域','山原','平原','不分區政黨')
supplement_election_termseven<-c('補選2009苗栗縣1','補選2009南投縣1','補選2009雲林縣2','補選2009臺北市6','補選2010台中縣3','補選2010花蓮縣','補選2010桃園縣2','補選2010桃園縣3','補選2010新竹縣','補選2010嘉義縣2','補選2010臺東縣','補選2011台南市4','補選2011高雄市4')
terms<-c(7,9)

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
    elections_cand_csv <- paste0(dataset_file_directory,"中選會選舉區",slash,"第",term,"屆",slash,elec_dist_type,slash,"elcand.csv")
    elections_dist_csv <- paste0(dataset_file_directory,"中選會選舉區",slash,"第",term,"屆",slash,elec_dist_type,slash,"elbase.csv")
    elections_party_csv <- paste0(dataset_file_directory,"中選會選舉區",slash,"第",term,"屆",slash,elec_dist_type,slash,"elpaty.csv")
    elections_voteresult_csv <- paste0(dataset_file_directory,"中選會選舉區",slash,"第",term,"屆",slash,elec_dist_type,slash,"elprof.csv")
    elections_df_dist <-read_csv(file=elections_dist_csv,col_types="cccccc")
    elections_df_party <-read_csv(file=elections_party_csv)
    elections_df_cand <-read_csv(file=elections_cand_csv)
    #elections_df_voteresult <-read_csv(file=elections_voteresult_csv)
    if (elec_dist_type=='不分區政黨') {
      elections_plcan_csv <- paste0(dataset_file_directory,"中選會選舉區",slash,"第",term,"屆",slash,elec_dist_type,slash,"elrepm.csv")
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
      select(省市別,縣市別,選區別,名稱) %>%
      rename(選舉區名稱=名稱)
    election_admin_county<-filter(elections_df_dist,鄉鎮市區!="000",村里別!="0000",!customgrepl(名稱,"選區|選舉區|全國|政黨")) %>%
      rename(村里名稱=名稱)
    #省市別  縣市別  選區別  鄉鎮市區  村里別  村里名稱
    election_admin_dist<-filter(elections_df_dist,鄉鎮市區!="000",村里別=="0000",!customgrepl(名稱,"選區|選舉區|全國|政黨")) %>%
      rename(鄉鎮市區名稱=名稱) %>%
      select(省市別,縣市別,選區別,鄉鎮市區,鄉鎮市區名稱)
    election_admin_city<-filter(elections_df_dist,鄉鎮市區=="000",選區別=="00",!customgrepl(名稱,"選區|選舉區|全國|政黨")) %>%
      rename(縣市名稱=名稱) %>%
      select(省市別,縣市別,縣市名稱)
    election_admin_to_elecdist<-left_join(election_real_elec_dist,election_admin_city) %>% #left_join(election_admin_county,election_admin_dist) %>%
      left_join(election_admin_dist) %>%
      left_join(election_admin_county)
    #left_join(election_admin_city) %>%
    #left_join(election_real_elec_dist)
    elections_df_cand$省市別<-as.character(elections_df_cand$省市別)
    election_admin_to_elecdist$省市別<-as.character(election_admin_to_elecdist$省市別)
    elections_df_onekind<-left_join(elections_df_cand,election_admin_to_elecdist,by = c("省市別", "縣市別", "選區別"))
    if (elec_dist_type=='不分區政黨') {
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
  mutate_at(c("admindistrict"), funs(customgsub(admindistrict, "區", ""))) ##鄉鎮市區名稱還沒有統一
##從選區資料抓出舊制全國縣市鄉鎮市區
all_admin_dist <- distinct(elections_df, term, admincity, admindistrict) %>%
  filter(!is.na(admincity))
all_admin_dist_try <- cbind(all_admin_dist, "fullcountyname" = all_admin_dist$admindistrict) %>%
  mutate_at(c("admindistrict"), funs(stri_sub(admindistrict, from = 1, to = -2)))
all_admin_dist_with_zip <- left_join(all_admin_dist_try, zipcode_df) %>%
  select(term, admincity, fullcountyname, zip, zip3rocyear) %>%
  rename(admindistrict = fullcountyname) %>%
  mutate_at(c("term"), as.numeric)

elections_df_test <- elections_df %>%
  mutate_at(c("term"), funs(customgsub(term, "0", ""))) %>%
  mutate_at(c("term"), as.numeric) %>%
  left_join(all_admin_dist_with_zip)

#立委資料與選區資料合併
legislators <- read_csv(file = paste0(dataset_file_directory, "legislators.csv"))
legislators_needed <- filter(legislators, term %in% c("07", "09")) %>%
  mutate_at(c("term"), funs(customgsub(term, "0", ""))) %>%
  mutate_at(c("term"), as.numeric)
legislators_with_election <- left_join(legislators_needed, elections_df_test, by = c("name", "term", "sex")) #
#save(elections_df_test,file="elections_df_test.RData")
#save(legislators_with_election, file = "legislators_with_election.RData")
#test result: filter(legislators_needed,is.na(zip)) %>% View()


##############################################################################
# 第二部分：投票及議案及「問卷答案對照意向」資料,主要也就是RData&excel檔
##############################################################################
myown_vote_bills_file <- "votingdf_datafile_myown_englished.xlsx"
survey_time_range <- as.data.frame(list(yrmonth=c('099/07', '099/11', '099/12', '100/01', '100/04', '100/06', '105/09', '105/10', '105/11', '105/12', '106/01', '106/04', '106/05')))
partyseats <- data.frame(
  "term"=7,
  "party"=c("中國國民黨","民主進步黨","無黨團結聯盟","親民黨","無黨籍及未經政黨推薦"),                       
  "seats"=c(81,27,3,1,1)
  ) %>%
  bind_rows(
    data.frame(
      "term"=9,
      "party"=c("中國國民黨","民主進步黨","時代力量","親民黨","無黨團結聯盟","無黨籍及未經政黨推薦"),
      "seats"=c(35,68,5,3,1,1)
      )
  ) 
#bills_answer_to_bill <- read.xlsx(myown_vote_bills_file, sheetIndex = 3, encoding = "UTF-8", endRow = 5144)
bills_answer_to_bill <- read.xlsx(myown_vote_bills_file, sheet = 4)
#bills_billcontent <- read.xlsx(myown_vote_bills_file, sheetIndex = 1, encoding = "UTF-8", endRow = 3498) %>%
bills_billcontent <- read.xlsx(myown_vote_bills_file, sheet = 1) %>%
  mutate_at("billcontent", funs(as.character)) %>%
  select(-starts_with("pp_related_q_"))
#as.character(unique(bills_billcontent$pp_related_q_1))
load("myown_vote_record_df.RData")




myown_vote_record_df <- myown_vote_record_df %>%
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
load("legislators_with_election.RData")
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



mergedf_votes_bills_election_surveyanswer <- filter(myown_vote_record_df, term %in% c(7, 9)) %>%
  left_join(myown_vote_record_df_with_party) %>%
  right_join(bills_billcontent, by = c("billid_myown","term","period","meetingno","temp_meeting_no","billn","billresult","url","date")) %>%
  right_join(bills_answer_to_bill) %>%
  #篩選出研究範圍
  inner_join(survey_time_range) %>%
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
  mutate(opiniondirectionfromlegislator=NA) %>%
  mutate(respondopinion=NA) %>%
  #mutate(respondopinion=paste0(votedecision,opiniondirectionfrombill)) %>%
  #mutate(respondopinion) %>%
  #mutate_cond(votedecision=="贊成" & opiniondirectionfromconstituent==opiniondirectionfrombill, respondopinion=2) %>%
  #mutate_cond(votedecision=="贊成" & opiniondirectionfromconstituent!=opiniondirectionfrombill, respondopinion=0) %>%
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
  mutate_cond(opiniondirectionfromconstituent=='x' | opiniondirectionfrombill=='x', respondopinion=NA) %>%
  mutate_cond(votedecision %in% c("棄權","未投票","未出席"), respondopinion=1, opiniondirectionfromconstituent='giveup') %>%
  mutate_cond(opiniondirectionfromconstituent==opiniondirectionfromlegislator, respondopinion=2) %>%
  mutate_cond(opiniondirectionfromconstituent!=opiniondirectionfromlegislator, respondopinion=0) %>%
  #mutate_at("respondopinion",funs(recode(respondopinion)),
  #          "反對n"=2,"反對nn"=2,"贊成m"=2,"贊成mm"=2,
  #          "棄權n"=1,"棄權nn"=1,"棄權m"=1,"棄權mm"=1,
  #          "贊成n"=0,"贊成nn"=0,"反對m"=0,"反對mm"=0
  #) %>%
  rename(name=legislator_name)

#可以看到有回應也有不回應
distinct(mergedf_votes_bills_election_surveyanswer,votedecision,billid_myown,variable_on_q,value_on_q_variable,name,party,opiniondirectionfromconstituent,opiniondirectionfrombill,opiniondirectionfromlegislator,respondopinion) %>%
  #testdf %>%
  filter(billid_myown=="9-2-0-16-67",variable_on_q=="pp_related_q_1",value_on_q_variable=="2016citizen@c2") %>%
  arrange(name,party) %>%
  View()

#write_csv(mergedf_votes_bills_election_surveyanswer,"new_to_replace_b.csv")
#mergedf_votes_bills_election_surveyanswer %>%
#  filter(!is.na(respondopinion),billid_myown=="9-2-0-17-88",variable_on_q=="pp_related_q_1") %>%
#  distinct(name,votedecision,variable_on_q,respondopinion,billid_myown,party) %>%
#  arrange(party,billid_myown,variable_on_q,respondopinion) %>%
#  View()
#save(mergedf_votes_bills_election_surveyanswer, file = "mergedf_votes_bills_election_surveyanswer.RData")
#mergedf_votes_bills_election_surveyanswer <- data.frame()
##############################################################################
# 第三部份：把問卷檔加上行政區、選區屬性
##############################################################################
library(haven)
library(labelled)
load(file = "duplicatedarea.RData")

##=================以下部分因為已有既存資料檔，讀取後略過不執行#=================
##=================以下部分因為已有既存資料檔，讀取後略過不執行#=================
#找出所有行政區對選區資料，並且找出同一鄉鎮市區有不同選區的部分
#admin_dist_to_elect_dist <- distinct(elections_df_test, term, admincity, electionarea, admindistrict) %>%
#  filter(!is.na(admincity)) %>%
#  left_join(all_admin_dist_with_zip)
#duplicated_area <- admin_dist_to_elect_dist[duplicated(admin_dist_to_elect_dist[, c("term", "admincity", "admindistrict")]),]
#把某些共用同一個郵遞區號的行政區合併
#unique_dist_for_elect_dist <- anti_join(admin_dist_to_elect_dist, duplicated_area[, c("term", "admincity", "admindistrict")]) %>%
#  group_by(term, electionarea, admincity, zip, zip3rocyear) %>%
#  summarise(admindistrict = paste0(admindistrict, collapse = "、"))
#以下註解部分為找出多選區的樣本
#duplicated_area[duplicated_area$term == 9, c("zip")] %>%
#  intersect(X2016_citizen$zip) %>%
#  unique() %>% 
#  sort()
##=================以上部分因為已有既存資料檔，讀取後略過不執行#=================
##=================以上部分因為已有既存資料檔，讀取後略過不執行#=================

#save(duplicated_area,unique_dist_for_elect_dist,file="duplicatedarea.RData")
#重要！2010環境的資料因為補選選區有改變，所以在一些鄉鎮市區村里會重複出現多筆紀錄，要先處理一下join的選舉資料
#duplicated_area_just_one_electionarea <- group_by(duplicated_area, term, admincity, admindistrict, zip, zip3rocyear) %>%
#  summarise(electionarea = paste0(electionarea, collapse = "、"))
minus_electionarea <- as.data.frame(list("term" = 7, "electionarea" = "桃園縣第06選區", "admincity" = "桃園縣", "admindistrict" = "中壢市", zip = 320, zip3rocyear = 99))
#
survey_restricted_data<-c(1,2,3) %>%
  lapply(function (X) read.xlsx(paste0(dataset_file_directory, "basic_social_survey_restricted_data.xlsx"), sheet = X))
survey_data<-c("2016_citizen.sav","2010_env.sav","2010_overall.sav") %>%
  sapply(function (X,...) paste0(...,X), dataset_file_directory, "merger_survey_dataset",slash) %>%
  lapply(haven::read_sav)
#先依據是否有多數選區存在於單一鄉鎮市區拆開，先串有同一鄉鎮市區內有多選區的，再串同一鄉鎮市區內只有一選區的，然後分別join之後再合併
survey_data <- mapply(function(X,Y) {
  in_complicated_district<-filter(X, id %in% Y$id) %>%
    left_join(Y)
  in_simple_district <- filter(X, !(id %in% Y$id)) %>%
    mutate_at(c("zip"), as.integer) %>%
    left_join(unique_dist_for_elect_dist)
  bind_rows(in_simple_district, in_complicated_district) %>%
    arrange(id)
},X=survey_data,Y=survey_restricted_data)

#X2016_citizen <- read_sav(paste0(dataset_file_directory, "merger_survey_dataset",slash,"2016_citizen.sav")) #%>%
##限制版資料併入選舉資料
##which(X2016_citizen$myown_age==997)
#X2016_citizen_restricted <- read.xlsx(paste0(dataset_file_directory, "basic_social_survey_restricted_data.xlsx"), sheetIndex = 1, encoding = "UTF-8") %>%
#  left_join(duplicated_area) %>%
#  filter(!is.na(id))
#先依據是否有多數選區存在於單一鄉鎮市區拆開，先串有同一鄉鎮市區內有多選區的，再串同一鄉鎮市區內只有一選區的，然後分別join之後再合併
#X2016_citizen_in_complicated_district <- filter(X2016_citizen, id %in% X2016_citizen_restricted$id) %>%
#  left_join(X2016_citizen_restricted) # %>%
##left_join(legislators_with_election)
#X2016_citizen_in_simple_district <- filter(X2016_citizen, !(id %in% X2016_citizen_restricted$id)) %>%
#  mutate_at(c("zip"), as.integer) %>%
#  left_join(unique_dist_for_elect_dist)
#X2016_citizen_with_restricted <- bind_rows(X2016_citizen_in_simple_district, X2016_citizen_in_complicated_district)
##X2016_citizen_with_restricted_with_elections <- left_join(X2016_citizen_with_restricted,elections_df)

#X2010_env <- read_sav(paste0(dataset_file_directory, "merger_survey_dataset",slash,"2010_env.sav"))
#X2010_env_restricted <- read.xlsx(paste0(dataset_file_directory, "basic_social_survey_restricted_data.xlsx"), sheetIndex = 2, encoding = "UTF-8") %>%
#  filter(!is.na(id)) %>%
#  left_join(duplicated_area) %>%
#  anti_join(minus_electionarea)
#X2010_env_in_complicated_district <- filter(X2010_env, id %in% X2010_env_restricted$id) %>%
#  left_join(X2010_env_restricted)
#X2010_env_in_simple_district <- filter(X2010_env, !(id %in% X2010_env_restricted$id)) %>%
#  mutate_at(c("zip"), as.integer) %>%
#  left_join(unique_dist_for_elect_dist)
#X2010_env_with_restricted <- bind_rows(X2010_env_in_simple_district, X2010_env_in_complicated_district)

#X2010_overall <- read_sav(paste0(dataset_file_directory, "merger_survey_dataset",slash,"2010_overall.sav")) %>%
#  rename()
#X2010_overall_restricted <- read.xlsx(paste0(dataset_file_directory, "basic_social_survey_restricted_data.xlsx"), sheetIndex = 3, encoding = "UTF-8") %>%
#  left_join(duplicated_area) %>%
#  filter(!is.na(id))
#X2010_overall_in_complicated_district <- filter(X2010_overall, id %in% X2010_overall_restricted$id) %>%
#  left_join(X2010_overall_restricted)
#X2010_overall_in_simple_district <- filter(X2010_overall, !(id %in% X2010_overall_restricted$id)) %>%
#  mutate_at(c("zip"), as.integer) %>%
#  left_join(unique_dist_for_elect_dist)
#X2010_overall_with_restricted <- bind_rows(X2010_overall_in_simple_district, X2010_overall_in_complicated_district)

#anti_join(X2016_citizen_with_restricted,X2016_citizen,by=c("id")) %>% sapply(table) %>% View()

#save(survey_data,file="all_survey_combined.RData")
#View(X2010_env_with_restricted[duplicated(X2010_env_with_restricted$id),])
#View(group_by(X2016_citizen_with_restricted, id) %>% filter(n()>1))
load(file="all_survey_combined.RData")

#############################把問卷資料變形以便串連及行政區、選舉資料#################################
library(reshape2)

survey_q_id<-list(
    c("c1a",	"c1b",	"c1c",	"c1d",	"c1e",	"c2",	"c3",	"c4",	"c5",	"c6",	"c10",	"c11",	"c12",	"c13",	"c14",	"d1",	"d2a",	"d2b",	"d3a",	"d3b",	"d4",	"d5a",	"d5b",	"d5c",	"d5d",	"d5e",	"d5f",	"d6a",	"d6b",	"d6c",	"d6d",	"d6e",	"d6f",	"d6g",	"d6h",	"d7a",	"d7b",	"d7c",	"d7d",	"d7e",	"d7f",	"d7g",	"d7h",	"d7i",	"d7j",	"d7k",	"d8a",	"d8b",	"d8c",	"d11a",	"d11b",	"d12",	"d13a",	"d13b",	"d14a",	"d14b",	"d14c",	"d17a",	"d17b",	"d17c",	"e2a",	"e2b",	"e2c",	"e2d",	"e2e",	"e2f",	"e2g",	"e2h",	"e2i",	"f3",	"f4",	"f5",	"f8",	"f9",	"h10"),
    c("kv21c_0", "kv31_0", "kv67_0", "v14a", "v14b", "v15a", "v15b", "v16a", "v16b", "v19", "v20a", "v20b", "v21c", "v22a", "v22b", "v22c", "v23a", "v23b", "v23c", "v24a", "v24b", "v24c", "v25a", "v25b", "v25c", "v26a", "v26b", "v26c", "v26d", "v26e", "v26f", "v26g", "v27a", "v27b", "v27c", "v27d", "v27e", "v27f", "v27g", "v28a", "v28b", "v29", "v30a", "v30b", "v31", "v32a", "v32b", "v32c", "v36a", "v36b", "v37a", "v37b", "v37c", "v37d", "v37e", "v37f", "v37g", "v37h", "v37i", "v38a1", "v38a2", "v38b1", "v38b2", "v38c1", "v38c2", "v38d1", "v38d2", "v38e1", "v38e2", "v39a", "v39b", "v39c", "v40", "v57", "v58", "v59", "v63", "v66c", "v66f", "v67", "v68", "v69", "v70b", "v70c", "v70d", "v70e", "v70f"),
    c("v39c", "v39d", "v39e", "v40", "v41", "v78a", "v78b", "v78c", "v78d", "v78e", "v78f", "v78g", "v78h", "v78i", "v90", "v91", "v92")
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

for (comm_var_i in 1:(length(survey_data_melted)-1)) {
  if (comm_var_i==1) {
    common_var<-intersect(
      names(survey_data_melted[[comm_var_i]]),
      names(survey_data_melted[[comm_var_i+1]])
      )
    complete_survey_dataset<-bind_rows(survey_data_melted[[comm_var_i]],survey_data_melted[[comm_var_i+1]])
  } else {
    common_var<-intersect(common_var,names(survey_data_melted[[comm_var_i+1]]))
    complete_survey_dataset<-bind_rows(complete_survey_dataset,survey_data_melted[[comm_var_i+1]])
  }
}


complete_survey_dataset<-complete_survey_dataset[,common_var]
#save(complete_survey_dataset,file="complete_survey_dataset.RData")
##針對調查問卷資料處理變形，以便合併
#"c1a","c1b","c1c","c1d","c1e","c2","c3","c4","c5","c6","c10","c11","c12","c13","c14","d1","d2a","d2b","d3a","d3b","d4","d5a","d5b","d5c","d5d","d5e","d5f","d6a","d6b","d6c","d6d","d6e","d6f","d6g","d6h","d7a","d7b","d7c","d7d","d7e","d7f","d7g","d7h","d7i","d7j","d7k","d8a","d8b","d8c","d11a","d11b","d12","d13a","d13b","d14a","d14b","d14c","d17a","d17b","d17c","e2a","e2b","e2c","e2d","e2e","e2f","e2g","e2h","e2i","f3","f4","f5","f8","f9","h10","kh10"
load(file="complete_survey_dataset.RData")
##############################################################################
# 第四部份：總結階段
##############################################################################
#legislators_with_election
#mergedf_votes_bills_election_surveyanswer
load("elections_df_test.RData")
load("legislators_with_election.RData")
load("mergedf_votes_bills_election_surveyanswer.RData")

legislators_with_election <- legislators_with_election[!is.na(legislators_with_election$wonelection),] %>%
  distinct(term, name, ename, sex, party.x, partyGroup, areaName,
           committee, onboardDate, degree, experience, picUrl,
           leaveFlag, leaveDate, leaveReason, ballotid, birthday,
           age, birthplace, education, incumbent, wonelection,
           party.y, electionarea, plranking, elec_dist_type) %>%
  rename(legislator_sex=sex,
         legislator_party=party.x,election_party=party.y,
         legislator_age=age
         )
testdf <- left_join(mergedf_votes_bills_election_surveyanswer, legislators_with_election) %>%
  mutate_at("SURVEYANSWERVALUE", funs(as.character))#%>%
#沒有投票權也會串到立委，也就是只串選區的串法
testdf <- inner_join(complete_survey_dataset, testdf, by = c("term", "electionarea", "SURVEY", "SURVEYQUESTIONID", "SURVEYANSWERVALUE"))
#只串到支持的候選人的串法
testdf <- inner_join(complete_survey_dataset, testdf, by = c("term", "electionarea", "SURVEY", "SURVEYQUESTIONID", "SURVEYANSWERVALUE", "myown_constituency_party_vote"="election_party"))



##############################################################################
# 第O部份：清理資料：設定遺漏值
##############################################################################

sapply(testdf,class)

testdf <- testdf %>%
  select(-billcontent.y,-billcontent.x) %>%
  mutate_cond(myown_eduyr %in% c(96:99,996:999,9996:9999), myown_eduyr=NA) %>%
  mutate_cond(myown_pol_efficacy %in% c(94:99,996:999,9996:9999), myown_pol_efficacy=NA) %>%
  mutate_cond(myown_working_status %in% c(96:99,996:999,9996:9999), myown_working_status=NA) %>%
  mutate_cond(SURVEYANSWERVALUE %in% c(96:99,996:999,9996:9999), respondopinion=NA, opiniondirection=NA) %>%
  mutate_cond(respondopinion=="x", respondopinion=NA) %>%
  mutate_at(c("SURVEY","zip","stratum2","myown_areakind","psu","ssu",
              "myown_sex","myown_dad_ethgroup","myown_mom_ethgroup",
              "myown_marriage","myown_religion","myown_pol_efficacy",
              "myown_approach_to_politician_or_petition",
              "myown_protest","myown_constituency_party_vote",
              "myown_working_status","myown_industry","myown_job_status",
              "term","electionarea","admincity","admindistrict",
              "village","adminvillage","votedecision","billresult",
              "pp_committee","pp_lawamendment","pp_enactment",
              "pp_enforcement","pp_res_bynew","pp_res_bycompete",
              "pp_groupbased","pp_res_notjudged","pp_ignored",
              "billconflict","variable_on_q","value_on_q_variable",
              "opinionfromconstituent","opinionfrombill",
              "opiniondirectionfromconstituent","opiniondirectionfrombill",
              "opiniondirectionfromlegislator","respondopinion",
              "legislator_sex","legislator_party","partyGroup",
              "areaName","leaveFlag","education","incumbent",
              "wonelection","elec_dist_type",
              "vote_along_with_majority_in_party"
              ), funs(as.factor)) %>%
  mutate_at(c("wsel","myown_wsel","year","year_m","myown_age","myown_eduyr",
              "myown_occp","myown_workers_numbers","myown_hire_people_no",
              "myown_manage_people_no","myown_family_income",
              "myown_family_income_ranking","myown_family_income_stdev",
              "opinionstrength"  #,
              #"percent_of_same_vote_from_same_party",
              #"opinion_pressure_from_party"
              #"same_pos_to_all_ratio",
              #"same_direction_on_same_bill_and_interest_toall_ratio",
              #"same_direction_on_same_bill_and_interest_in_same_legislator_toall_ratio"
              ),funs(as.numeric)) %>%
  mutate_at(c("wave","qtype","SURVEYQUESTIONID","SURVEYANSWERVALUE",
              "name","url","date","pp_keyword","votecontent",
              "billcontent","LABEL","QUESTION","ename","committee",
              "onboardDate","degree","experience","picUrl","leaveDate",
              "leaveReason","ballotid","birthday","birthplace",
              "yrmonth"
              ),funs(as.character)) %>%
  #,"billcontent.x","billcontent.y"
  mutate_at(c("zip3rocyear","period","meetingno","temp_meeting_no",
              "billn","urln","pp_duplicated_item",
              "legislator_age","plranking"  #,
              #"total_votes_from_same_party",
              #"same_pos_from_same_party",
              #"all_pos_on_same_q",
              #"same_pos_on_same_q",
              #"same_opinion_from_same_party",
              #"all_opinion_from_same_party"
              #"same_direction_on_same_bill_and_interest",
              #"all_direction_on_same_bill_and_interest",
              #"same_direction_on_same_bill_and_interest_in_same_legislator",
              #"all_direction_on_same_bill_and_interest_in_same_legislator"
              ),funs(as.integer)) #%>%
  #filter(!is.na(respondopinion))
#,"billid_myown"
#有序factor 無序factor

filter(testdf,is.na(SURVEYANSWERVALUE) | is.na(respondopinion)) %>%
  #distinct(LABEL) %>%
  #unique() %>%
  View()

testdf$myown_pol_efficacy %>% table()

##
sapply(testdf, function(x) sum(is.na(x))) %>% View()
sapply(testdf, function(x) length(unique(x))) %>% View()
#tmp_realage<-glmdata$r_realage
#glmdata[]<-lapply(glmdata, car::recode,"94:999=NA")
#glmdata$r_realage<-tmp_realage
#glmdata$r_realage[glmdata$r_realage == 997] <- NA
sapply(glmdata, table)
sapply(glmdata, class)

#glmdata[glmdata$SURVEYANSWERVALUE %in% c(96:99,996:999,9996:9999),]

for (missingvalue in c(96:99,996:999,9996:9999)) {
  missingvaluei<-as.numeric(missingvalue)
  #glmdata[glmdata==missingvaluei] <- NA
}



##############################################################################
# 第O部份：清理資料：填補遺漏值
##############################################################################

#填補遺漏值
#filling in missing value
require(DMwR)
glmdata <- knnImputation(glmdata)
#X2016_citizen_with_restricted <- knnImputation(X2016_citizen_with_restricted)


#檢查亂報投票意向
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






#用類神經網路
#combine dummy variables
#cars$type <- names(cars[14:18])[max.col(cars[14:18])]
#or
#cars$type <- names(cars[14:18])[apply(cars[14:18], 1, match, x = 1)] 
#or
#ind <- apply(cars[,14:18],1,function(x) which(as.logical(x)))
#cars$type <- colnames(cars[,14:18])[ind]

#require(neuralnet) # for neuralnet(), nn model
#require(nnet)      # for class.ind()
#require(caret)     # for train(), tune parameters
#data<-cbind("h5"=correct_check_result$h5,class.ind(correct_check_result$h5),
#            "h6r"=correct_check_result$h5,class.ind(correct_check_result$h5),)
#formula.bpn <- setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width









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

##############################################################################
# 第O部份：分析前處理資料
##############################################################################

#計算出同立場的人數
#total_votes_from_same_party,same_pos_from_same_party,percent_of_same_vote_from_same_party
glmdata <- testdf %>%
  #mutate("same_direction_opinion"=n()) %>%
  #ungroup() %>%
  #name,billid_myown,votedecision,term,SURVEY,variable_on_q,SURVEYQUESTIONID,SURVEYANSWERVALUE,respondopinion
  #group_by(billid_myown,variable_on_q,value_on_q_variable,opiniondirectionfromlegislator,party) %>%
  #mutate("same_opinion_from_same_party"=n()) %>%
  #ungroup() %>%
  #group_by(billid_myown,variable_on_q,value_on_q_variable,party) %>%
  #mutate("all_opinion_from_same_party"=n()) %>%
  #ungroup() %>%
  #mutate("opinion_pressure_from_party"=same_opinion_from_same_party/all_opinion_from_same_party) %>%
  group_by(billid_myown,variable_on_q,value_on_q_variable,opiniondirectionfromconstituent) %>%
  mutate("same_opiniondirection_from_constituent_by_nation"=n()) %>%
  ungroup() %>%
  group_by(billid_myown,variable_on_q,value_on_q_variable) %>%
  mutate("all_opiniondirection_from_constituent_by_nation"=n()) %>%
  ungroup() %>%
  mutate("opinion_pressure_from_constituent_by_nation"=same_opiniondirection_from_constituent_by_nation/all_opiniondirection_from_constituent_by_nation) %>%
  filter(!(value_on_q_variable %in% c("2016citizen@d5a","2016citizen@d6a","2016citizen@d6b","2016citizen@d6d","2016citizen@d6g","2016citizen@d6h"))) #%>%   #忽略預算支出題組
  #filter(issue_field1=='公民與政治權' | issue_field2=='公民與政治權')
  #scale()
#group_by(billid_myown,variable_on_q,respondopinion) %>%
#mutate("same_opinion_on_same_bill_and_interest"=n()) %>%
#ungroup() %>%
#group_by(billid_myown,term,SURVEY,variable_on_q,SURVEYQUESTIONID) %>%
#mutate("all_direction_on_same_bill_and_interest"=n()) %>%
#ungroup() %>%
#mutate("same_direction_on_same_bill_and_interest_toall_ratio"=same_direction_on_same_bill_and_interest/all_direction_on_same_bill_and_interest) %>%
#group_by(name,billid_myown,term,SURVEY,variable_on_q,SURVEYQUESTIONID,respondopinion) %>%
#mutate("same_direction_on_same_bill_and_interest_in_same_legislator"=n()) %>%
#ungroup() %>%
#group_by(name,billid_myown,term,SURVEY,variable_on_q,SURVEYQUESTIONID) %>%
#mutate("all_direction_on_same_bill_and_interest_in_same_legislator"=n()) %>%
#ungroup() %>%
#mutate("same_direction_on_same_bill_and_interest_in_same_legislator_toall_ratio"=same_direction_on_same_bill_and_interest_in_same_legislator/all_direction_on_same_bill_and_interest_in_same_legislator) #%>%
#magrittr::extract(1:30,)# %>%
#View()

#可以看到有回應也有不回應
distinct(testdf,id,votedecision,billid_myown,variable_on_q,value_on_q_variable,name,party,opiniondirectionfromconstituent,opiniondirectionfromlegislator,respondopinion) %>%
  #testdf %>%
  filter(billid_myown=="9-2-0-16-67",variable_on_q=="pp_related_q_1",value_on_q_variable=="2016citizen@c2") %>%
  arrange(name,party) %>%
  View()

distinct(glmdata,id,votedecision,billid_myown,variable_on_q,value_on_q_variable,name,party,opiniondirectionfromconstituent,opiniondirectionfromlegislator,same_opinion_from_same_party,all_opinion_from_same_party,opinion_pressure_from_party,respondopinion) %>%
  group_by(billid_myown,variable_on_q,value_on_q_variable,opiniondirectionfromconstituent) %>%
  #testdf %>%
  filter(all_opinion_from_same_party!=same_opinion_from_same_party) %>%
  arrange(name,party) %>%
  View()


#left_join(X2016_citizen_melted_with_restricted, by = c("term", "electionarea", "SURVEY", "SURVEYQUESTIONID","SURVEYANSWERVALUE"))
#分別串分區和全國
#inner_join可以處理有些立委並無選民在問卷中被涵蓋
#依變項 respondopinion
testdf <- filter(testdf, !is.na(respondopinion))# %>%
#filter(term==9)



##############################################################################
# 第O部份：分析資料
##############################################################################

##探索性資料分析
library(ggplot2)
g <- ggplot(glmdata, aes(x = myown_family_income, y = respondopinion))
g + geom_bar(stat = "identity")



ggplot(glmdata, aes(x = myown_family_income, y = respondopinion)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  #facet_grid(pared ~ public, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#TO SPSS
library(foreign)
write.foreign(glmdata, "glmdata.txt", "glmdata.sps",   package="SPSS")

##檢定

contrasts(glmdata$respondopinion)<-contr.treatment(3, base=1)   #以contrast()設定虛變數classf，contr.treatment()設定三個艙等中，以編碼3當作參考點。
contrasts(glmdata$myown_approach_to_politician_or_petition)<-contr.treatment(4, base=4)   #以contrast()設定虛變數classf，contr.treatment()設定三個艙等中，以編碼3當作參考點。
contrasts(glmdata$myown_protest)<-contr.treatment(4, base=4)   #以contrast()設定虛變數classf，contr.treatment()設定三個艙等中，以編碼3當作參考點。
contrasts(glmdata$myown_pol_efficacy)<-contr.treatment(5, base=5)   #以contrast()設定虛變數classf，contr.treatment()設定三個艙等中，以編碼3當作參考點。

glmdata$percent_of_same_votes_from_same_party<-scale(glmdata$percent_of_same_votes_from_same_party)
glmdata$myown_family_income<-scale(glmdata$myown_family_income)
glmdata$percent_of_same_votes_from_same_party<-glmdata$percent_of_same_votes_from_same_party/100
#累積迴歸
library(ordinal)
model <- clm(respondopinion~(vote_along_with_majority_in_party)+opinion_pressure_from_constituent_by_nation,
             data=glmdata)
summary(model)
#glmdata$vote_along_with_majority_in_party
glmdata$percent_of_same_votes_from_same_party %>% scale() %>% table()

#決策樹
require(rpart)
require(rpart.plot)
set.seed(22)
train.index <- sample(x=1:nrow(glmdata), size=ceiling(0.8*nrow(glmdata) ))
train <- glmdata[train.index, ]
test <- glmdata[-train.index, ]
cart.model<- rpart(respondopinion ~ myown_areakind+myown_sex+myown_dad_ethgroup+myown_mom_ethgroup+myown_marriage+myown_religion+myown_pol_efficacy+myown_approach_to_politician_or_petition+myown_protest+myown_working_status+myown_age+myown_eduyr+myown_occp+myown_family_income+opinionstrength+opinion_pressure_from_party, 
                   data=train)
prp(cart.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)  

#累積迴歸
require(MASS)
## fit ordered logit model and store results 'm'
model <- polr(respondopinion ~ myown_family_income, data = glmdata, Hess=TRUE)
## view a summary of the model
summary(model)

binaryglmdata<-filter(glmdata,respondopinion %in% c(0,2))
model<-glm(
#myown_areakind+myown_sex+myown_dad_ethgroup+myown_mom_ethgroup+myown_marriage+myown_religion+myown_pol_efficacy+myown_approach_to_politician_or_petition+myown_protest+myown_working_status+myown_age+myown_eduyr+myown_occp+myown_family_income+opinionstrength+opinion_pressure_from_party
  formula = respondopinion ~ percent_of_same_votes_from_same_party,
  family = binomial(
    link = "logit"),
  data = binaryglmdata)
summary(model)
save(model,file="thesis_output.RData")



#可以看到沒有繼續當的立委沒串到
distinct(legislators_with_election, term, name) %>%
  filter(customgrepl(name, "廖國棟|簡東明|鄭天財|秀霞|高潞"))
distinct(mergedf_votes_bills_election_surveyanswer, term, name) %>%
  filter(customgrepl(name, "廖國棟|簡東明|鄭天財|秀霞|高潞"))


setdiff(distinct(legislators_with_election, term, name), distinct(mergedf_votes_bills_election_surveyanswer, term, name)) %>%
  filter(customgrepl(name, "廖國棟|簡東明|鄭天財|秀霞|高潞")) #%>%
#%>% View()
setdiff(distinct(mergedf_votes_bills_election_surveyanswer, term, name), distinct(legislators_with_election, term, name)) %>%
  filter(customgrepl(name, "廖國棟|簡東明|鄭天財|秀霞|高潞")) #%>%
#%>% View()
distinct(legislators_with_election, term, name) %>% View()
#廖國棟,簡東明,鄭天財,陳秀霞,高潞‧以用‧巴魕剌Kawlo．Iyun．Pacidal
#簡東明Uliw．Qaljupayare,#廖國棟Sufin．Siluko,#鄭天財Sra．Kacaw,#周陳秀霞,#高潞．以用．巴魕剌Kawlo．Iyun．Pacidal
filter(mergedf_votes_bills_election_surveyanswer, customgrepl(name, "高潞")) %>%
  distinct(name)

##############################################################################
# 第O部份：產出報告
##############################################################################



library(rmarkdown)
render(input='analysis_result.Rmd',output_dir=getwd(),encoding="UTF-8")
getwd()











#贊成n=FALSE
#=TRUE
#overall 330107 807148
#testdf <- left_join(X2010_env, legislators_needed)
#filter(elections_df_test, term == 7, electionarea %in% c('桃園縣第06選區', '桃園縣第3選舉區','桃園縣第2選舉區')) %>%
#    View()

#除錯，比對問卷,find for duplicated representation
#zipcode_remove_dupl<-distinct(zipcode_df,Zip3,City,Area)
#duplicated_area<-left_join(duplicated_area,zipcode_remove_dupl,by=c("縣市名稱"="City","鄉鎮市區名稱"="Area"))
#troublezip<-intersect(X2010_env$zip,duplicated_area$Zip3)
#trouble_obs<-filter(X2010_env,zip %in% troublezip)
#trouble_survey_id<-trouble_obs$id


#election_real_elec_dist<-if (elec_dist_type=="區域") {
#  filter(elections_df_dist,鄉鎮市區=="000",選區別!="00") %>%
#    select(省市別,縣市別,選區別,名稱) %>%
#    rename(選舉區名稱=名稱)
#} else if (elec_dist_type=="山原") {
#  select(elections_df_dist,省市別,縣市別,選區別) %>%
#    cbind("選舉區名稱"="山地原住民") %>%
#    distinct(省市別,縣市別,選區別,名稱,選舉區名稱)
#} else if (elec_dist_type=="平原") {
#  select(elections_df_dist,省市別,縣市別,選區別) %>%
#    cbind("選舉區名稱"="平地原住民") %>%
#    distinct(省市別,縣市別,選區別,名稱,選舉區名稱)
#}


#    cbind("縣市名稱" = old_all_admin_dist$縣市名稱, "鄉鎮市區名稱" = old_all_admin_dist$鄉鎮市區名稱)
##縣市改制前郵遞區號開始嘗試
#oldzipcode_df <- zipcode_df
#oldzipcode_df$City <- customgsub(zipcode_df$City, "新北市", "臺北縣") %>%
#    customgsub("桃園市", "桃園縣")
#臺中縣411-439
#oldzipcode_df$City[oldzipcode_df$Zip3 >= 411 & oldzipcode_df$Zip3 <= 439] <- customgsub(oldzipcode_df$City[oldzipcode_df$Zip3 >= 411 & oldzipcode_df$Zip3 <= 439], "臺中市", "臺中縣")
##臺南縣710-745
#oldzipcode_df$City[oldzipcode_df$Zip3 >= 710 & oldzipcode_df$Zip3 >= 745] <- customgsub(oldzipcode_df$City[oldzipcode_df$Zip3 >= 710 & oldzipcode_df$Zip3 >= 745], "臺南市", "臺南縣")
##高雄縣814-852
#oldzipcode_df$City[oldzipcode_df$Zip3 >= 814 & oldzipcode_df$Zip3 >= 852] <- customgsub(oldzipcode_df$City[oldzipcode_df$Zip3 >= 814 & oldzipcode_df$Zip3 >= 852], "高雄市", "高雄縣")
#oldzipcode_df$City <- stri_sub(oldzipcode_df$City, from = 1, to = -2)
#oldzipcode_df$Area <- stri_sub(oldzipcode_df$Area, from = 1, to = -2)
#oldzipcode_df <- distinct(oldzipcode_df, Zip3, City, Area)
##old_all_admin_dist_try$Area[134],oldzipcode_df$Area[151]都有福興
##差異：setdiff(oldzipcode_df[, c("City", "Area")], old_all_admin_dist_try[, c("City", "Area")]) %>% View()
#old_all_admin_dist_try_with_zip <- left_join(old_all_admin_dist_try, oldzipcode_df) %>%
#    distinct(縣市名稱, 鄉鎮市區名稱, Zip3) %>%
#    filter(!is.na(鄉鎮市區名稱)) %>%
#    cbind("term" = "07") %>%
#    rename(zip = Zip3) %>%
#    arrange(zip)
##合併郵遞區號和選區
#zipcode_df_distinct <- distinct(zipcode_df, Zip3, City, Area) %>%
#    rename(縣市名稱 = City, 鄉鎮市區名稱 = Area, zip = Zip3) %>%
#    cbind(term = "09") %>%
#    rbind(old_all_admin_dist_try_with_zip) %>%
#    arrange(desc(term), zip)
