# 第Ｏ部份：環境設定 --------------------------------
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
filespath<-switch(
  paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model),
  "Windows8x64build9200Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="E:\\Software\\scripts\\R\\vote_record\\",
  "Windows10x64build17763Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="E:\\Software\\scripts\\R\\vote_record\\",
  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="/mnt/e/Software/scripts/R/vote_record/",
  "Ubuntu18.04.2LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="/mnt/e/Software/scripts/R/vote_record/",
  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"="/home/j/rscripts/vote_record/",
  "Ubuntu18.04.2LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"="/home/j/rscripts/vote_record/",
  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\vote_record\\",
  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2660 v4 @ 2.00GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\vote_record\\",
  "Windows8x64build9200Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\Downloads\\vote_record\\"
)
source(file = paste(filespath, "shared_functions.R", sep = ""))
#選舉資料
overall_elec_dist_types<-c('district','ab_m','ab_plain','partylist')
supplement_election_termseven<-c('supp2009miaoli1','supp2009nantou1','supp2009yunlin2','supp2009taipei6','supp2010taichungs3','supp2010hualian','supp2010taoyuan2','supp2010taoyuan3','supp2010hsinchus','supp2010chiayi2','supp2010taitung','supp2011tainan4','supp2011kaoshiung4')
terms<-c(5,6,7,8,9)
survey_data_title<-c("2004citizen","2010env","2010overall","2016citizen") %>% sort()
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

survey_imputation_and_measurement<-openxlsx::read.xlsx(paste0(dataset_file_directory,"merger_survey_dataset",slash,"imputationcomputingbasis.xlsx"),sheet = 1)
survey_codebook<-openxlsx::read.xlsx(paste0(dataset_file_directory,"all_survey_questions_englished.xlsx"),sheet = 4)

# 第一部份：立委及選區資料 -------------------------------------------

elections_df<-data.frame()
for (term in terms) {
  message("term=",term)
  term_character<-paste0("0",term)
  if (term==7) {
    elec_types<-c(overall_elec_dist_types,supplement_election_termseven)
  } else {
    elec_types<-overall_elec_dist_types
  }
  for (elec_dist_type in elec_types) {
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
    #找出真正的選舉區定義，但在補選時好像也定義為000，需轉換
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
    if (elec_dist_type=='partylist') { #把不分區名單和政黨代號名稱串起來
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
    elections_df <- bind_rows(elections_df,elections_df_onekind) #結合參選人以及選區的資料
  } #分區、全國區結束
  #check: filter(elections_df,is.na(選舉區名稱)) %>% View()
  #check: distinct(legislators_needed,areaName,選舉區名稱) %>% View()
}
elections_df <- elections_df[, c("term", "號次", "名字", "性別", "出生日期", "年齡", "出生地", "學歷", "現任", "當選註記", "政黨名稱", "選舉區名稱", "縣市名稱", "鄉鎮市區名稱", "村里名稱", "排名", "elec_dist_type")] %>%
  rename(ballotid = 號次, name = 名字, sex = 性別, birthday = 出生日期, age = 年齡, birthplace = 出生地, education = 學歷, incumbent = 現任, wonelection = 當選註記, party = 政黨名稱, electionarea = 選舉區名稱, admincity = 縣市名稱, admindistrict = 鄉鎮市區名稱, adminvillage = 村里名稱, plranking = 排名) %>%
  mutate_at(c("sex"), funs(customgsub(sex, "2", "女"))) %>%
  mutate_at(c("sex"), funs(customgsub(sex, "1", "男")))

#透過全國行政區的行政區名稱，比對不完整鄉鎮市區名稱的郵遞區號行政區，組裝出行政區郵遞區號
zipcodecsv<-paste0(dataset_file_directory,"zip3.xlsx")
zipcode_df <- openxlsx::read.xlsx(zipcodecsv, sheet = 1) %>%
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
  mutate_at(c("term"), as.character) #%>%
#left_join(all_admin_dist_with_zip)

#立委資料與選區資料合併
#legislators <- read_csv(file = paste0(dataset_file_directory, "legislators.csv"))
legislators <- openxlsx::read.xlsx(paste0(dataset_file_directory, "legislators.xlsx"), sheet = 1)
legislators_needed <- filter(legislators, term %in% terms) %>% #c("05", "06", "07", "09")
  mutate_at(c("term"), funs(customgsub(term, "0(\\d{1})", "\\1", perl = TRUE))) %>%
  mutate_at(c("term"), as.character)
legislators_with_election <- inner_join(legislators_needed, elections_df_test, by = c("name", "term", "sex"))  %>%
  rename(legislator_sex=sex,
         legislator_party=party.x,
         election_party=party.y,
         legislator_age=age,
         legislator_name=name
  ) %>%
  mutate_at(c("term"), as.numeric) %>%
  mutate_at(c("legislator_sex","legislator_party","partyGroup","areaName","leaveFlag","incumbent","wonelection","election_party","elec_dist_type"), as.factor)#inner_join目的是要排除沒有當選也沒有遞補進來的立法委員
#save(elections_df_test,file=paste0(filespath,"data",slash,"elections_df_test.RData"))
#save(legislators_with_election, file=paste0(filespath,"data",slash,"legislators_with_election.RData"))
#test result: filter(legislators_needed,is.na(zip)) %>% View()

load(paste0(filespath,"data",slash,"legislators_with_election.RData"))
legislators_with_election <- legislators_with_election %>% #[!is.na(legislators_with_election$wonelection),]
  distinct(term, legislator_name, ename, legislator_sex, legislator_party, partyGroup, areaName,
           committee, onboardDate, degree, experience, picUrl,
           leaveFlag, leaveDate, leaveReason, ballotid, birthday,
           legislator_age, birthplace, education, incumbent, wonelection,
           election_party, electionarea, plranking, elec_dist_type)
legislators_ethicity <- list(
  'aboriginal'='谷辣斯．尤達卡Kolas．Yotaka|高潞．以用．巴魕剌Kawlo．Iyun．Pacidal',
  'foreignstates'='尹伶瑛|段宜康|趙麗雲|吳育昇|丁守中|朱鳳芝|周守訓|邱毅|帥化民|洪秀柱|孫大千|李慶安|李慶華|潘維剛|蔣孝嚴|賴士葆|費鴻泰|盧秀燕|王榮璋|顧立雄|段宜康|王定宇|趙天麟|梁文傑|王鍾渝|李永萍|江綺雯|沈智慧',
  'fulo'='尤清|王拓|王金平|王政中|王昱婷|王雪峰|江丙坤|江昭儀|何金松|何敏豪|余政道|吳東昇|吳敦義|呂新民|李文忠|李全教|蔡正元|李嘉進|林豐正|郭素春|邱毅|李明憲|李俊毅|李鴻鈞|杜文卿|沈富雄|邱永仁',
  'hakka'='羅文嘉|林郁方|羅志明|彭添富|邱垂貞|張昌財|邱創良|鄭金玲|張學舜|邱鏡淳|陳進興|呂學樟|何智輝|徐耀昌|林豐喜|邱太三|郭俊銘|鍾紹和|傅崐萁|饒穎奇|李桐豪|鍾榮吉|徐中雄|吳志揚|彭紹瑾|鄭金玲|葉芳雄|管碧玲|張慶惠|劉盛良|廖正井|趙麗雲|邱志偉|呂玉玲|徐欣瑩|邱文彥|陳碧涵|吳宜臻|李應元|陳賴素美|徐志榮|鍾佳濱|鍾孔炤|林為洲|陳明真|馬文君|劉建國|黃昭順|蘇震清',
  #from 圖解客家政治與經濟 馬文君|劉建國|黃昭順|邱議瑩|邱志偉|管碧玲|蘇震清|
  "newresident"="",
  'other'='吳成典',
  'unknown'='王幸男|王淑慧|朱星羽|李和順|李雅景|李鎮楠|李顯榮|李雅景'
)
legislators_ethicity_json <- paste0(dataset_file_directory, "legislators_ethicity_originalcollection.txt") %>%
  jsonlite::fromJSON()
legislators_ethicity_df <- mapply(c, legislators_ethicity, legislators_ethicity_json, SIMPLIFY=FALSE) %>% 
  lapply(function(X) {
    #stringi::stri_split(X[1],regex="|")
    joined_str_c <- c(X[1],X[2])
    joined_str_c <- joined_str_c[joined_str_c != ""]
    joined_str <- paste0(joined_str_c,sep="",collapse="|")
    Y <- stringi::stri_split(joined_str,regex="\\|") %>%
      unlist() %>%
      unique()
    return(Y)
  }) %>%
  lapply(names(.), function(X,lst=list()) {
    data.frame("legislator_name"=lst[[X]], "legislator_ethnicity"=X)
  },lst = .) %>%
  plyr::rbind.fill()
legislators_additional_attr <- distinct(legislators_with_election,term,legislator_name,electionarea,degree,experience,education) %>%
  mutate(legislator_eduyr=NA,legislator_occp=NA,legislator_ses=NA) %>%
  mutate_at("legislator_occp",funs(as.character)) %>%
  mutate_cond(is.na(education), education=degree) %>%
  left_join(legislators_ethicity_df,by=c("legislator_name")) %>%
  #朱星羽@國會雙週刊 江綺雯@中央綜合月刊 江綺雯@遠見
  mutate_cond(customgrepl(legislator_name,"簡東明Uliw．Qaljupayare"), education=paste0(education,"省立屏東師專畢業")) %>%
  mutate_cond(customgrepl(legislator_name,"周陳秀霞"), education=paste0(education,"臺南縣立官田國民中學畢業")) %>%
  mutate_cond(customgrepl(legislator_name,"吳琪銘"), education=paste0(education,"德霖技術學院畢")) %>%
  mutate_cond(customgrepl(legislator_name,"林國正"), education=paste0(education,"臺灣大學國家發展訮究所博士班")) %>%
  mutate_cond(customgrepl(legislator_name,"林郁方"), education=paste0(education,"美國維吉尼亞大學國際政治學博士")) %>%
  mutate_cond(customgrepl(legislator_name,"呂玉玲"), education=paste0(education,"南亞技術學院企業管理科")) %>%
  mutate_cond(customgrepl(legislator_name,"劉銓忠"), education=paste0(education,"培元高級職業學校畢業")) %>%
  mutate_cond(customgrepl(legislator_name,"蔡煌瑯"), education=paste0(education,"政治大學行政專科")) %>%
  mutate_cond(customgrepl(legislator_name,"陳節如"), education=paste0(education,"國立臺灣師範大學英語系")) %>%
  mutate_cond(customgrepl(legislator_name,"林淑芬"), education=paste0(education,"國立中興大學社會系")) %>%
  mutate_cond(customgrepl(legislator_name,"林淑芬") & term==9, education=paste0(education,"世新大學社會發展研究所")) %>%
  mutate_cond(customgrepl(legislator_name,"蔡家福"), education=paste0(education,"育達高職")) %>%
  mutate_cond(customgrepl(legislator_name,"饒穎奇"), education=paste0(education,"中興大學社會學系畢業")) %>%
  mutate_cond(customgrepl(legislator_name,"程振隆"), education=paste0(education,"美國加州人文大學碩士 http://www.csea.org.tw/index/index.php?index=../03/01")) %>%
  mutate_cond(customgrepl(legislator_name,"謝鈞惠"), education=paste0(education,"美國舊金山大學公共行政研究所結業 83年台南縣省議員選舉公報")) %>%
  mutate_cond(customgrepl(legislator_name,"陳唐山|黃昭輝|徐欣瑩"), experience=paste0(experience,"學術科研機構研究員")) %>%
  mutate_cond(customgrepl(legislator_name,"王廷升|張顯耀|費鴻泰|林郁方|孫國華|李全教|陳碧涵"), experience=paste0(experience,"副教授 助理教授"), education="博士") %>%
  mutate_cond(customgrepl(education,"國小|小學"), legislator_eduyr=6) %>%
  mutate_cond(customgrepl(education,"國中"), legislator_eduyr=9) %>%
  mutate_cond(customgrepl(education,"中學|高中|高職|高工畢|高商畢|高級職業學校畢"), legislator_eduyr=12) %>%
  mutate_cond(customgrepl(education,"專科畢業|學士班結業|商專畢|工專畢|大學進修|師專畢"), legislator_eduyr=14) %>%
  mutate_cond(
    (customgrepl(education,"大專|大學|學系|技術學院|學士") & !customgrepl(education,"學士班|學士班結業|大學進修|研究班|研究班進修")) |
      customgrepl(education,"系畢|系畢業|學系畢|大學畢業")  , legislator_eduyr=16) %>%
  mutate_cond(
    (customgrepl(education,"研究") & !customgrepl(education,"研究所|研究所研究|研究班|研究班進修") ) |
      customgrepl(education,"研究所研究|碩士班研究|碩士班|研究生|碩士生|研究所結業|研究所肄業") , legislator_eduyr=17.5) %>%
  mutate_cond(customgrepl(education,"碩士|研究所|研究所碩士") & !customgrepl(education,"碩士班|研究所結業|研究所研究|研究生|碩士學分班|研究所肄業"), legislator_eduyr=19) %>%
  mutate_cond(customgrepl(education,"碩士班畢業|研究所畢業"), legislator_eduyr=19) %>%
  mutate_cond(customgrepl(education,"博士班|博士研究"), legislator_eduyr=21) %>%
  mutate_cond(customgrepl(education,"博士") & !customgrepl(education,"博士班|博士研究|榮譽博士"), legislator_eduyr=23) %>%
  mutate_cond(customgrepl(legislator_name,"陳東榮"),
              legislator_eduyr=0) %>%
  mutate_cond(customgrepl(legislator_name,"林文郎|林德福|劉政鴻|李鎮楠|蔡家福|劉銓忠"),
              legislator_eduyr=12) %>%
  mutate_cond(customgrepl(legislator_name,"王幸男"),
              legislator_eduyr=13) %>%
  mutate_cond(customgrepl(legislator_name,"黃逢時"), # 專科沒畢業
              legislator_eduyr=13) %>%
  mutate_cond(customgrepl(legislator_name,"李文忠|李明憲|李鎮楠|林惠官|高仲源|黃宗源|鄭美蘭|劉俊雄|曾華德|盧博基|康世儒|蔡煌瑯"),
              legislator_eduyr=14) %>%
  mutate_cond(customgrepl(legislator_name,"陳朝龍") & term==5,
              legislator_eduyr=14) %>%
  mutate_cond(customgrepl(legislator_name,"周雅淑|黃政哲|楊仁福|劉松藩|顧崇廉|張俊雄|林淑芬|陳節如"),
              legislator_eduyr=16) %>%
  mutate_cond(customgrepl(legislator_name,"何智輝|邱垂貞|曾華德|陳景峻|謝鈞惠"),
              legislator_eduyr=17.5) %>%
  mutate_cond(customgrepl(legislator_name,"陳景峻") & term==5,
              legislator_eduyr=17.5) %>%
  mutate_cond(customgrepl(legislator_name,"陳景峻") & term==6,
              legislator_eduyr=19) %>%
  mutate_cond(customgrepl(legislator_name,"王雪峰|邱太三|陳茂男|陳金德|陳健治|張秀珍|蔡豪|程振隆|鄭國忠"),
              legislator_eduyr=19) %>%
  mutate_cond(customgrepl(legislator_name,"李全教|李顯榮"),
              legislator_eduyr=23) %>%
  mutate_cond(customgrepl(legislator_name,"李鎮楠|李雅景|李明憲|王雪峰|王幸男|江玲君|吳清池|邱鏡淳|邱議瑩|林益世|林淑芬|余政道|呂學樟|翁重鈞|郭玟成|陳明文|陳杰|陳啟昱|陳瑩|馬文君|康世儒|黃昭順|楊瓊瓔|蔡煌瑯|鄭汝芬|鄭金玲|鄭麗文|劉銓忠|潘孟安|潘維剛|盧嘉辰|蕭景田|羅明才|王定宇|何欣純|蘇震清|吳思瑤|吳琪銘|呂孫綾|李俊俋|李彥秀|李應元|周陳秀霞|林俊憲|林為洲|林德福|段宜康|徐榛蔚|陳超明|張宏陸|黃秀芳|許淑華|鄭麗君|蕭美琴|蘇治芬|蘇嘉全|王昱婷|朱星羽|何智輝|李和順|杜文卿|沈智慧|邱垂貞|邱創進|卓榮泰|卓伯源|周雅淑|周慧瑛|林文郎|林育生|柯淑敏|唐碧娥|徐志明|郭俊銘|陳宗義|陳志彬|陳茂男|陳金德|陳健治|陳進丁|陳景峻|陳朝龍|陳麗惠|張秀珍|張蔡美|張學舜|章仁香|許舒博|彭添富|曾華德|廖本煙|蔡啟芳|蔡鈴蘭|鄭余鎮|鄭美蘭|鄭朝明|鄭貴蓮|劉文雄|劉松藩|劉俊雄|劉政鴻|盧博基|賴勁麟|藍美津|謝明源|謝章捷|尹伶瑛|朱俊曉|林耘生|林國慶|陳東榮|陳朝容|陳憲中|曹來旺|葉芳雄|楊宗哲|蔡錦隆|顏文章|林國正|張嘉郡|楊應雄"),
              experience=paste0(experience,"職業民意代表")) %>%
  mutate_cond(customgrepl(legislator_name,"余天|高金素梅"), experience=paste0(experience,"藝人")) %>%
  mutate_cond(customgrepl(legislator_name,"林滄敏"), experience=paste0(experience,"商店售貨")) %>%
  mutate_cond(customgrepl(legislator_name,"柯建銘|涂醒哲|沈富雄|林進興|洪奇昌|陳其邁|侯水盛"), experience=paste0(experience,"醫師")) %>%
  mutate_cond(customgrepl(legislator_name,"孫大千"), experience=paste0(experience,"化工研究員")) %>%
  mutate_cond(customgrepl(legislator_name,"吳成典|黃劍輝"), experience=paste0(experience,"總經理")) %>%
  mutate_cond(customgrepl(legislator_name,"徐少萍|林正二|林春德|許榮淑|楊仁福"), experience=paste0(experience,"國中教師")) %>%
  mutate_cond(customgrepl(legislator_name,"劉盛良|謝鈞惠|顏錦福"), experience=paste0(experience,"高中教師")) %>%
  mutate_cond(customgrepl(legislator_name,"陳宗仁"), experience=paste0(experience,"商專教師")) %>%
  mutate_cond(customgrepl(legislator_name,"吳清池"), experience=paste0(experience,"固定攤販與市場售貨")) %>%
  mutate_cond(customgrepl(legislator_name,"何金松"), experience=paste0(experience,"金屬機械技術工")) %>%
  mutate_cond(customgrepl(legislator_name,"林惠官"), experience=paste0(experience,"金屬機械技術工 鐵道工人")) %>%
  mutate_cond(customgrepl(legislator_name,"李昆澤"), experience=paste0(experience,"電器維修工")) %>%
  mutate_cond(customgrepl(legislator_name,"林炳坤|郭素春|張花冠|王金平|許毓仁|林國華|陳建銘|湯火聖|何敏豪"),
              experience=paste0(experience,"總經理 創業主管")) %>%
  mutate_cond(customgrepl(legislator_name,"徐耀昌|張慶忠|薛凌|顏清標|余宛如|呂玉玲|林南生|陳宏昌|梁牧養|許登宮|程振隆|楊文欣|蔡豪|鍾金江|羅世雄|黃良華|葉津鈴|詹凱臣"),
              experience=paste0(experience,"董事長")) %>%
  mutate_cond(customgrepl(legislator_name,"李俊毅|黃偉哲|鍾紹和|洪宗熠|蔡適應|鄭運鵬|鍾佳濱|顏寬恒|蔡其昌|李文忠|趙永清|羅文嘉"),
              experience=paste0(experience,"國會助理")) %>%
  mutate_cond(customgrepl(legislator_name,"張川田|林重謨|魏明谷"),
              experience=paste0(experience,"政治人物幕僚")) %>%
  mutate_cond(customgrepl(legislator_name,"楊富美"),
              experience=paste0(experience,"醫藥專業人員")) %>%
  mutate_cond(customgrepl(legislator_name,"林岱樺|吳育昇|林鴻池|陳淑慧|葉宜津"),
              experience=paste0(experience,"訓練班教師")) %>%
  mutate_cond(customgrepl(legislator_name,"吳志揚"), experience=paste0(customgsub(experience,"教授",""),"律師")) %>%
  mutate_cond(customgrepl(legislator_name,"黃義交|蔣孝嚴|鄭天財|饒穎奇"),
              experience=paste0(experience,"主管級公務員")) %>%
  mutate_cond(customgrepl(legislator_name,"林明溱|蔣乃辛"),
              experience=paste0(experience,"事務工作公務員")) %>%
  mutate_cond(customgrepl(legislator_name,"蔡正元"),
              experience=paste0(experience,"商學專業人員")) %>%
  mutate_cond(customgrepl(legislator_name,"李復興|李嘉進|郭榮宗|曹爾忠|曾永權|陳雪生|陳歐珀|楊曜"),
              experience=paste0(experience,"科長 課長 股長 組長 辦公室監督")) %>%
  mutate_cond(customgrepl(legislator_name,"侯彩鳳|許智傑|劉世芳"),
              experience=paste0(experience,"工程師")) %>%
  mutate_cond(customgrepl(legislator_name,"陳忠信|張俊宏"),
              experience=paste0(experience,"編輯")) %>%
  mutate_cond(customgrepl(legislator_name,"林濁水|李敖"),
              experience=paste0(experience,"作家")) %>%
  mutate_cond(customgrepl(legislator_name,"李顯榮"),
              experience=paste0(experience,"建築師")) %>%
  mutate_cond(customgrepl(legislator_name,"陳根德"), experience=paste0(experience,"漁民")) %>%
  mutate_cond(customgrepl(legislator_name,"傅崐萁"), experience=paste0(experience,"監察人")) %>%
  mutate_cond(customgrepl(legislator_name,"黃志雄|鄭志龍"), experience=paste0(experience,"職業選手")) %>%
  mutate_cond(customgrepl(legislator_name,"廖婉汝"), experience=paste0(experience,"托兒所負責人")) %>%
  mutate_cond(customgrepl(legislator_name,"陳賴素美"), experience=paste0(experience,"地政士")) %>%
  mutate_cond(customgrepl(legislator_name,"蔡家福"), experience=paste0(experience,"土地登記代理人")) %>%
  mutate_cond(customgrepl(legislator_name,"高志鵬"),
              experience=paste0(experience,"律師")) %>%
  mutate_cond(customgrepl(legislator_name,"張麗善|莊和子"), experience=paste0(experience,"護理師")) %>%
  mutate_cond(customgrepl(legislator_name,"陳亭妃|陳學聖|張廖萬堅|趙天麟|李永萍|王世勛"),
              experience=paste0(experience,"記者")) %>%
  mutate_cond(customgrepl(legislator_name,"田秋堇|陳節如|黃淑英|王育敏|王榮璋|吳玉琴|李麗芬|林麗蟬|陳曼麗|高潞|鍾孔炤"),
              experience=paste0(experience,"NGO理事長 NGO執行長 NGO秘書長 工會理事長")) %>%
  mutate_cond(customgrepl(experience,"漁民|討海人"), legislator_occp=620, legislator_ses=65.9) %>%
  mutate_cond(customgrepl(experience,"固定攤販與市場售貨"), legislator_occp=532, legislator_ses=67.3) %>%
  mutate_cond(customgrepl(experience,"商店售貨"), legislator_occp=531, legislator_ses=71.8) %>%
  mutate_cond(customgrepl(experience,"營建採礦技術工|水泥公司工人"), legislator_occp=710, legislator_ses=72.0) %>%
  mutate_cond(customgrepl(experience,"電器維修工|金屬機械技術工|鐵道工人"), legislator_occp=720, legislator_ses=74.2) %>%
  mutate_cond(customgrepl(experience,"辦公室事務性工作|公所秘書|事務工作公務員"), legislator_occp=410, legislator_ses=76.5) %>%
  mutate_cond(customgrepl(experience,"職業選手"), legislator_occp=322, legislator_ses=77.5) %>%
  mutate_cond(customgrepl(experience,"補習班教師|訓練班教師"), legislator_occp=303, legislator_ses=78.4) %>%
  mutate_cond(customgrepl(experience,"社會工作員|輔導員|社工"), legislator_occp=312, legislator_ses=74.5) %>%
  mutate_cond(customgrepl(experience,"護理師|醫藥專業人員"), legislator_occp=223, legislator_ses=79.1) %>%
  mutate_cond(customgrepl(experience,"記者|主播|採訪中心主任|作家|編輯"), legislator_occp=212, legislator_ses=80.0) %>%
  mutate_cond(customgrepl(experience,"藝人|主唱"), legislator_occp=213, legislator_ses=80.0) %>%
  mutate_cond(customgrepl(experience,"國會助理|省議員助理|政治人物幕僚"), legislator_occp=311, legislator_ses=80.1) %>%
  mutate_cond(customgrepl(experience,"高中教師|中學教師|中學教員|國中教師|國小教師|國中小教師|商工教師|商專教師|補校教師"), legislator_occp=202, legislator_ses=81.1) %>%
  mutate_cond(customgrepl(experience,"股長|襄理|課長|科長|副理|環保署資深科學主管") | customgrepl(legislator_name,"吳光訓"), legislator_occp=370, legislator_ses=81.9) %>%
  mutate_cond(customgrepl(experience,"專案經理"), legislator_occp=120, legislator_ses=81.4) %>%
  mutate_cond(customgrepl(experience,"牧師|宗教專業人員"), legislator_occp=214, legislator_ses=80.0) %>%
  mutate_cond(customgrepl(experience,"商學專業人員"), legislator_occp=230, legislator_ses=85.1) %>%
  mutate_cond(customgrepl(experience,"測量技士|土木技師|化工研究員|工程師|建築師|水利技師"), legislator_occp=250, legislator_ses=83.2) %>%
  mutate_cond(customgrepl(experience,"(基金會){0}(集團){0,1}(托兒所){0,1}董事長|總經理|監察人|(托兒所){0,1}負責人"), legislator_occp=110, legislator_ses=83.3) %>%
  mutate_cond(customgrepl(experience,"會計師"), legislator_occp=230, legislator_ses=85.1) %>%
  mutate_cond(customgrepl(experience,"法官|律師|地政士|土地登記代理人") | customgrepl(legislator_name,"吳志揚") & !customgrepl(legislator_name,"鄭天財Sra．Kacaw"), legislator_occp=211, legislator_ses=86.0) %>%
  mutate_cond(customgrepl(experience,"(兼任){0}副?教授|學系主任|系主任|學術科研機構研究員|大專講師"), legislator_occp=201, legislator_ses=87.9) %>%
  mutate_cond(customgrepl(experience,"醫師|產科主任"), legislator_occp=221, legislator_ses=86.0) %>%
  mutate_cond(customgrepl(experience,"旅長|軍總司令|國防管理學院院長"), legislator_occp="012", legislator_ses=81.4) %>%
  mutate_cond(customgrepl(experience,"NGO理事長|NGO執行長|NGO秘書長|產業總工會理事長|主管級公務員|職業民意代表") | customgrepl(legislator_name,"劉建國"), legislator_occp=140, legislator_ses=81.4) %>%
  mutate_cond(!is.na(legislator_ses), legislator_ses=(legislator_ses-55)*3) %>%
  select(term,legislator_name,electionarea,legislator_eduyr,education,experience,legislator_occp,legislator_ses,legislator_ethnicity) %>%
  mutate_at(c("legislator_occp"),as.factor)
save(legislators_additional_attr,file=paste0(filespath,"data",slash,"legislator_additional_attributes.RData"))
#陳東榮 no degree
#孫國華 僑選
#write.xlsx(legislators_additional_attr,file=paste0(dataset_file_directory,"legislator_additional_attributes.xlsx"))
#filter(legislators_additional_attr,is.na(legislator_ses)|is.na(legislator_eduyr)) %>%
#  select(name,experience,term,legislator_occp,legislator_ses,legislator_eduyr,education,legislator_ethnicity,electionarea) %>%
#  View()





# 第二部分：投票及議案及「問卷答案對照意向」資料,主要也就是RData&excel檔  -------------------------------------------

if (FALSE) { #舊方法暫時忽略
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

#讀取投票紀錄資料-此處通常預處理好，直接load下面mergedf_votes_bills_election_surveyanswer
load(paste0(filespath, "data", slash, "myown_vote_record_df.RData"))
load(paste0(filespath, "data", slash, "myown_vote_record_detailed_part_df.RData"))
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
  mutate_cond(term==9 & customgrepl(legislator_name,"陳秀霞"),legislator_name="周陳秀霞") %>%
  select(-billcontent,-url)
#save(myown_vote_record_df,file=paste0(filespath, "data", slash, "myown_vote_record_df_across2004.RData"))

#filter(myown_vote_record_df,term==9,customgrepl(legislator_name,"陳秀霞")) %>% select(legislator_name) %>% unique()

#mutate_at("legislator_name", funs(recode(opinionstrength)),
#  "n" = 1, "nn" = 2, "m" = 1, "mm" = 2, "b" = 0
#) #%>%


##算出同黨票數
load(file=paste0(filespath, "data", slash, "myown_vote_record_df_across2004.RData"))
load(file=paste0(filespath, "data", slash, "legislators_with_election.RData"))
load(file=paste0(filespath, "data", slash, "legislator_additional_attributes.RData"))

myown_vote_bills_file <- paste0(dataset_file_directory, "votingdf_datafile_myown_englished.xlsx", sep="")
bills_answer_to_bill <- openxlsx::read.xlsx(myown_vote_bills_file, sheet = 4)
bills_billcontent <- openxlsx::read.xlsx(myown_vote_bills_file, sheet = 1) %>%
  mutate_at("billcontent", funs(as.character)) %>%
  select(-starts_with("pp_related_q_"))

mergedf_votes_bills_election_surveyanswer <- distinct(legislators_with_election,term,legislator_name,legislator_party) %>%
  #mutate(party=legislator_party) %>%
  mutate_at("term",funs(as.numeric)) %>%
  left_join(myown_vote_record_df, ., by=c("term","legislator_name")) %>%
  distinct(votedecision,legislator_name,billid_myown,legislator_party) %>%
  filter(!is.na(legislator_party)) %>%
  group_by(billid_myown,legislator_party) %>%
  mutate("total_votes_from_same_party"=n()) %>%
  ungroup() %>%
  group_by(votedecision,billid_myown,legislator_party) %>%
  mutate("same_votes_from_same_party"=n()) %>%
  ungroup() %>%
  mutate("percent_of_same_votes_from_same_party"=same_votes_from_same_party/total_votes_from_same_party*100) %>%
  mutate("vote_along_with_majority_in_party"=ifelse(percent_of_same_votes_from_same_party>50,1,0)) %>%
  #above are myown_vote_record_df_with_party
  left_join(filter(myown_vote_record_df, term %in% terms), .) %>%
  left_join(legislators_additional_attr) %>%
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
  select(-date,-urln,-pp_committee,-votecontent,-pp_enactment,-pp_enforcement,-pp_res_bynew,-pp_res_bycompete,-pp_res_notjudged,-pp_ignored,-billconflict,-pol_score,-eco_score) %>%
  mutate_at(c("SURVEY","billresult","legislator_party","electionarea","pp_agendavoting","pp_propose_advanceforagenda","value_on_q_variable","variable_on_q","pp_lawamendment","issue_field1","issue_field2","respondopinion","success_on_bill"), as.factor) %>%
  select(-url.x,-url.y,-experience,-education,-pp_keyword.x,-pp_keyword.y,-billcontent.x,-billcontent.y)


#%>%
#mutate(term=stringi::stri_sub(electionname,from=1,length=1)) #%>%
#mutate(electionarea=last(unlist(strsplit(electionname,split="\\."))))
#names(bulletin_links) %>% sapply(function(X) {
#last(unlist(strsplit(X,split="\\.")))
#})
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

#save(mergedf_votes_bills_election_surveyanswer, file = paste0(filespath, "data", slash, "mergedf_votes_bills_election_surveyanswer.RData"))


# 第三部份：把問卷檔加上行政區、選區屬性  -------------------------------------------

library(haven)
library(labelled)
load(paste0(dataset_file_directory,"rdata",slash,"duplicatedarea.RData"))

##以下部分因為已有既存資料檔，讀取後略過不執行#
#找出所有行政區對選區資料，並且找出同一鄉鎮市區有不同選區的部分
#admin_dist_to_elect_dist <- distinct(elections_df_test, term, admincity, electionarea, admindistrict, adminvillage) %>%
#  filter(!is.na(admincity))# %>%
##  left_join(all_admin_dist_with_zip)
#duplicated_area <- distinct(admin_dist_to_elect_dist,term,electionarea,admincity,admindistrict) %>% #,zip,zip3rocyear
#  extract(duplicated(.[, c("term", "admincity", "admindistrict")]),)
#把某些共用同一個郵遞區號的行政區合併
#unique_dist_for_elect_dist <- anti_join(admin_dist_to_elect_dist, duplicated_area[, c("term", "admincity", "admindistrict")]) %>%
#  group_by(term, electionarea, admincity) %>% #, zip, zip3rocyear
#  summarise(admindistrict = paste0(admindistrict, collapse = "、"))
#以下註解部分為找出多選區的樣本
#duplicated_area[duplicated_area$term == 6, c("zip")] %>%
#  intersect(survey_data[[4]]$zip) %>%
#  unique() %>% 
#  sort()
#save(admin_dist_to_elect_dist,duplicated_area,unique_dist_for_elect_dist,file=paste0(dataset_file_directory,"rdata",slash,"duplicatedarea.RData"))
#以上部分因為已有既存資料檔，讀取後略過不執行#

#重要！2010環境的資料因為補選選區有改變，所以在一些鄉鎮市區村里會重複出現多筆紀錄，要先處理一下join的選舉資料
#duplicated_area_just_one_electionarea <- group_by(duplicated_area, term, admincity, admindistrict, zip, zip3rocyear) %>%
#  summarise(electionarea = paste0(electionarea, collapse = "、"))
minus_electionarea <- as.data.frame(list(
  "term" = 7, 
  "electionarea" = "桃園縣第06選區", 
  "admincity" = "桃園縣", 
  "admindistrict" = "中壢市", 
  zip = 320, 
  zip3rocyear = 99))
survey_restricted_data<-read.xlsx(paste0(dataset_file_directory, "basic_social_survey_restricted_data.xlsx"), sheet = 1)
survey_data<-paste0(survey_data_title,".sav") %>%
  sapply(function (X,...) paste0(...,X), dataset_file_directory, "merger_survey_dataset",slash) %>%
  lapply(haven::read_sav) %>%
  lapply(dplyr::mutate,stdsurveydate=as.Date(paste(year,sm,sd),"%Y %m %d")) %>%
  lapply(function(X,survey_imp_measure) {
    labelledcolumns<-purrr::map_lgl(X,haven::is.labelled) %>% which(isTRUE(.)) %>% names()
    spsssavsurvey<-X$SURVEY[1]
    message(spsssavsurvey)
    need_survey_measure_scale<-dplyr::filter(survey_imp_measure,SURVEY==spsssavsurvey,MEASUREMENT=="scale",ID %in% names(X)) %>%
      dplyr::select(ID) %>% unlist() %>% as.character() %>% intersect(labelledcolumns)
    need_survey_measure_ordinal<-filter(survey_imp_measure,SURVEY==spsssavsurvey,MEASUREMENT=="ordinal",ID %in% names(X)) %>%
      dplyr::select(ID) %>% unlist() %>% as.character() %>% intersect(labelledcolumns)
    need_survey_measure_categorical<-setdiff(names(X),need_survey_measure_scale) %>%
      setdiff(need_survey_measure_ordinal) %>% intersect(labelledcolumns)
    X %<>% dplyr::mutate_at(need_survey_measure_scale,funs(as.numeric)) %>%
      dplyr::mutate_at(need_survey_measure_ordinal,haven::as_factor,levels='both',ordered=TRUE) %>%
      dplyr::mutate_at(need_survey_measure_categorical,haven::as_factor,levels='both',only_labelled = TRUE)
    return(X)
  },survey_imp_measure=survey_imputation_and_measurement)

#設定遺漏值
missing_value_labels<-lapply(survey_data,function(X) {
  missingvaluepattern<-paste0("\\[",c(92:99,992:999,9992:9999),"\\]",collapse="|")
  labelsofdf<-sapply(X,levels) %>% unlist() %>% unique() %>%
    customgrep(pattern=missingvaluepattern,value=TRUE) %>%
    {extract(.,which(!customgrepl(.,pattern="(不固定|人或以上|到處跑|業|機構|學術|國外|從不聽|新雲林|竹塹|台北勞工|新農|草嶺|濁水溪|蘭潭|飛揚|11個或更多)",perl=TRUE)))}
  #(拒答|遺漏值|忘記|不適用|不知道|跳答|無法選擇|忘記了|拒答）|不知道）|缺漏|不記得)
  return(labelsofdf)
})
survey_data <- mapply(function(X,Y) {
  newdf <- lapply(X, function(dfcolumnvectors,replaced_keys) {
    if (is.factor(dfcolumnvectors)) {
      replace_key_value_pairs<-rep(NA,times=length(replaced_keys))
      names(replace_key_value_pairs)<-replaced_keys
      dfcolumnvectors<-plyr::revalue(dfcolumnvectors, replace_key_value_pairs)
    }
    return(dfcolumnvectors)
  },replaced_keys=Y) %>%
    as.data.frame() %>%
    mutate_if(is.factor,droplevels)
  newdf
  #to_replace_column<-setdiff(colnames(X),c("myown_age","myown_occp","myown_ses"))
  #  dplyr::mutate_at(X,to_replace_column,dplyr::funs(replace(.,. %in% c(93:99,996:999,9996:9999,99996:99999),NA ) )  )
},X=survey_data,Y=missing_value_labels) #%>%
#lapply(function (X) { #較早的串連方式，區分會期
#  othervar<-setdiff(names(X),c("term1","term2"))
#  reshape2::melt(X,id.vars = othervar, variable.name = "variable_on_term", value.name = "term") %>%
#    dplyr::filter(!is.na(term))
#})  %>%
survey_data <- survey_data[order(names(survey_data))] %>%
  lapply(dplyr::left_join,survey_restricted_data) %>%
  lapply(dplyr::mutate_at,c("myown_job","admincity","admindistrict"),.funs=as.factor)
#save(survey_data,file=paste0(filespath,"data",slash,"all_survey_combined.RData"))
#save(survey_data,file=paste0(filespath,"data",slash,"all_survey_combined_NAuntransformed.RData"))

labeladjusmentagain <- FALSE
if (labeladjusmentagain) {
  survey_data_labels <- lapply(survey_data,function(X) {
    sapply(X,FUN=attr,which="levels") %>%
      #sapply(X,FUN=function(X) {
      #  as.numeric(levels(X))[X]
      #  }) %>%
      return()
  })
  save(survey_data_labels,file=paste0(dataset_file_directory,"rdata",slash,"survey_data_labels.RData"))
}
#survey_data_labels已經預處理過，直接load即可
load(paste0(dataset_file_directory,"rdata",slash,"survey_data_labels.RData"))

writingfeather<-FALSE
if (writingfeather) {
  forwritingfeather<-mapply(function(X,Y,A,B) {
    #testing purpose
    #df<-as.data.frame(survey_data[[1]])
    #dfcoltypes<-sapply(Y,class)
    #to_dummy_cols<-names(which(dfcoltypes=="factor"))
    #to_dummy_cols_global <<- to_dummy_cols
    #print(to_dummy_cols)
    #for (to_dummy_col in to_dummy_cols) {
    #  print(to_dummy_col)
    #df<-dummies::dummy.data.frame(data=df,names=to_dummy_cols,sep="_")
    #}
    #View(df[68,])
    #grep("v28",names(df))
    #df<-dummies::dummy.data.frame(data=Y,names=to_dummy_cols,sep="_")#%>%
    #dplyr::filter(variable_on_term=="term1") #2004的問卷橫跨立法院多會期，為了節省運算資源所以只保留一期
    path<-paste0(ntuspace_file_directory,"shared",slash,"dataset",slash,"rdata",slash,"all_survey_combined",X,".feather")
    #tomutatecol<-setdiff(names(Y),"myown_age")
    #df %<>% mutate_at(tomutatecol,dplyr::funs(replace(.,. %in% c(93:99,996:999,9996:9999),NA ) ))
    message("-----writing ",path,"--------------------")
    Y<-droplevels(Y)
    needvars<-c("id", union(A,B)  ) %>%
      intersect(names(Y))
    needcols <<- needvars
    feather::write_feather(Y[,needvars], path=path)
  },X=1:4,Y=survey_data,A=imputingcalculatebasiscolumn,B=imputedvaluecolumn)
  #A=imputingcalculatebasiscolumn,B=imputedvaluecolumn
  #A=1:4,B=1:4
  #feather::write_feather(survey_data, path=paste0(dataset_file_directory,"rdata",slash,"all_survey_combined.feather"))
}



#shaped: 299 295 571
#先依據是否有多數選區存在於單一鄉鎮市區拆開，先串有同一鄉鎮市區內有多選區的，再串同一鄉鎮市區內只有一選區的，然後分別join之後再合併
#mapply(function(X,Y) {
##X=survey_data[[1]]; Y=survey_restricted_data[[1]]; Z<-survey_data_labels[[1]] #for testing purpose
#stopifnot(X$SURVEY[1]==Y$SURVEY[1])
#Y %<>% mutate_at(c("village","zip","admincity","admindistrict","adminvillage"),funs(as.factor))
#in_complicated_district<-filter(X, id %in% Y$id) %>%
#  #mutate_at(c("zip","id"),funs(as.character)) %>%
#  left_join(Y,by=c("SURVEY","id")) %>% #不用zip join 因為會有label, factor的問題
#  #mutate_at("term",funs(as.character)) %>%
#  left_join(admin_dist_to_elect_dist,by=c("admincity","admindistrict","adminvillage")) %>% #by=c("term","admincity","admindistrict","adminvillage"
#  rename(restricted_zip=zip.y) %>%
#  rename(zip=zip.x)
##findduplicatedrowsindf(in_complicated_district,c("id")) %>% View()
#in_simple_district <- filter(X, !(id %in% Y$id)) %>%
#  mutate_at(c("zip"), as.integer) %>%
#  left_join(unique_dist_for_elect_dist)#串連選區和行政區資料
##mutate_at("term",funs(as.character)) %>%
#bind_rows(in_simple_district, in_complicated_district) %>%
#  arrange(id) %>%
#  mutate_at(c("zip","id","myown_sex","myown_dad_ethgroup","myown_mom_ethgroup","myown_selfid","myown_int_pol_efficacy","myown_ext_pol_efficacy"),funs(as.factor)) %>%
#  mutate_at(setdiff(names(.),c("myown_age")),dplyr::funs(replace(.,. %in% c(93:99,996:999,9996:9999),NA ) ) ) %>%
#  mutate_if(is.factor,funs(droplevels))
#},X=survey_data,Y=survey_restricted_data)

load(paste0(filespath,"data",slash,"all_survey_combined.RData"))
#load(paste0(filespath,"data",slash,"all_survey_combined_NAuntransformed.RData"))


# 第四部份：清理資料：填補遺漏值 -------------------------------------------

#assinging missing value
library(mice)
library(VIM)
library(parallel)
#job_status暫時先刪掉，因為不同問卷概念與選項不一樣難以合併
imputingcalculatebasiscolumn<-lapply(survey_data_title,function(X,df) {
  filter(df,SURVEY==X,IMPUTATION %in% c("basis","both")) %>%
    select(ID) %>% unlist() %>% union(c("admincity")) %>% unname()
},df=survey_imputation_and_measurement) %>%
  setNames(survey_data_title)
imputedvaluecolumn<-lapply(survey_data_title,function(X,df) {
  filter(df,SURVEY==X,IMPUTATION %in% c("both")) %>%
    select(ID) %>% unlist() %>% unname()
},df=survey_imputation_and_measurement) %>%
  setNames(survey_data_title)

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

VIMtestplot<-FALSE
if (VIMtestplot) {
  survey_data_test<-survey_data
  #survey_data_test <- lapply(survey_data,function(X) {
  #  dplyr::mutate_at(X,setdiff(colnames(X),c("myown_age","myown_occp","myown_ses")),dplyr::funs(replace(.,. %in% c(93:99,996:999,9996:9999),NA ) )  )
  #  #設定遺漏值
  #})
  survey_data_test <- lapply(survey_data_test,function(X,imputedvaluecolumn,imputingcalculatebasiscolumn) {
    X<-droplevels(X)
    imputingcalculatebasiscolumn_assigned <- extract2(imputingcalculatebasiscolumn,X$SURVEY[1]) %>%
      intersect(names(X))
    imputedvaluecolumn_assigned <- extract2(imputedvaluecolumn,X$SURVEY[1]) %>%
      intersect(names(X))
    needcols<-union(imputingcalculatebasiscolumn_assigned,imputedvaluecolumn_assigned)[1:50]
    #testresult<-fastDummies::dummy_cols(X[,needcols]) %>% dplyr::select_if(is.numeric) %>% 
    #  MissMech::TestMCARNormality()
    testresult<-BaylorEdPsych::LittleMCAR(X[,needcols])
    #testresult<-X[,needcols]
    return(testresult)
  },imputedvaluecolumn=imputedvaluecolumn,imputingcalculatebasiscolumn=imputingcalculatebasiscolumn)
}

survey_data_test <- na_count <- missingvaluepattern <- imputed_survey_data <- list()
#Package ‘MissMech’
#To test whether the missing data mechanism, in a set of incompletely ob-served data, is one of missing completely at random (MCAR).For detailed description see Jamshidian, M. Jalal, S., and Jansen, C. (2014). ``Miss-Mech: An R Package for Testing Homoscedasticity, Multivariate Normality, and Missing Com-pletely at Random (MCAR),'' Journal of Statistical Software,  56(6), 1-31. URL http://www.jstatsoft.org/v56/i06/.

survey_data_test <- custom_parallel_lapply(
  X=survey_data,
  FUN=function(X,imputedvaluecolumn,imputingcalculatebasiscolumn,...) {
    #missingvaluecolumn_assigned<-missingvaluecolumn
    #imputingcalculatebasiscolumn_assigned<-imputingcalculatebasiscolumn
    #X<-survey_data[[i]] %>%
    # droplevels()
    X<-droplevels(X)
    imputingcalculatebasiscolumn_assigned <- extract2(imputingcalculatebasiscolumn,X$SURVEY[1]) %>%
      intersect(names(X))
    imputedvaluecolumn_assigned <- extract2(imputedvaluecolumn,X$SURVEY[1]) %>%
      intersect(names(X))
    foundationvar<-union(imputingcalculatebasiscolumn_assigned,imputedvaluecolumn_assigned)
    ini <- mice(X[,foundationvar], maxit = 0)
    sapply(c("----------------", X$SURVEY[1], "----------------"),print)
    print(table(ini$nmis))
    outlist4 <- as.character(ini$loggedEvents[, "out"])
    print(ini$loggedEvents, 2)
    fx2 <- flux(X[,foundationvar])
    outlist2<-row.names(fx2)[fx2$outflux < 0.45]
    outlist <- unique(c(outlist2, outlist4))
    foundationvar %<>% setdiff(outlist)
    print(paste0(c("foundationvar are ",foundationvar), collapse=" "))
    unusefulcolumns <- setdiff(names(X),foundationvar)
    #proceeding_na_var<-union(imputingcalculatebasiscolumn_assigned,imputedvaluecolumn_assigned) %>%
    #  setdiff(c("myown_age"))
    #predictor_matrix<-generate_predictor_matrix(X,imputingcalculatebasiscolumn_assigned,imputedvaluecolumn)
    predictor_matrix<-mice::quickpred(X[,foundationvar], mincor=0.2)
    #return(predictor_matrix)
    #X %<>% dplyr::mutate_at(proceeding_na_var,dplyr::funs(replace(.,. %in% c(93:99,996:999,9996:9999),NA ) ) ) %>%
    #  mutate_if(is.factor,funs(factor))
    #sol: https://stackoverflow.com/questions/13495041/random-forests-in-r-empty-classes-in-y-and-argument-legth-0
    #sol: https://stackoverflow.com/questions/24239595/error-using-random-forest-mice-package-during-imputation
    #The frequency distribution of the missing cases per variable can be obtained as:
    #survey_data_test[[i]] <- mice::mice(X, maxit = 0)
    #table(survey_data_test[[i]]$nmis)
    #colSums(is.na(X))
    #na_count[[i]] <- sapply(X, function(y) sum(length(which(is.na(y)))))
    #analysisdfonmissingvalue<-X[,imputedvaluecolumn_assigned]
    #missingvaluepattern[[i]]<-mice::md.pattern(analysisdfonmissingvalue,plot=FALSE)
    #visdat::vis_miss(analysisdfonmissingvalue)
    miceMod <- mice::mice(
      X[,foundationvar],
      predictorMatrix = predictor_matrix,
      m=5,
      method="rf"
    )  # perform mice imputation, based on random forests.
    #linear imputation might have error message: system is computationally singular: reciprocal condition number
    #https://stats.stackexchange.com/questions/214267/why-do-i-get-an-error-when-trying-to-impute-missing-data-using-pmm-in-mice-packa
    #print(imputingcalculatebasiscolumn_assigned)
    imputed_survey_data<- mice::complete(miceMod)  # generate the completed data.
    complete_imputed_survey_data<-bind_cols(X[,unusefulcolumns],imputed_survey_data)
    complete_imputed_survey_data<-complete_imputed_survey_data[,names(X)]
    complete_imputed_survey_data
    return(complete_imputed_survey_data)
  },
  imputedvaluecolumn=imputedvaluecolumn,
  imputingcalculatebasiscolumn=imputingcalculatebasiscolumn,
  exportvar=c("survey_data","imputedvaluecolumn","imputingcalculatebasiscolumn"),
  exportlib=c("base",lib,"mice","randomForest"), #,"MissMech","fastDummies"
  outfile=paste0(dataset_file_directory,"rdata",slash,"parallel_handling_process-",t_sessioninfo_running,".txt"),
  mc.set.seed = TRUE,
  mc.cores=parallel::detectCores()
)
#survey_data_test[[3]]<-t_survey_data_test[[1]]
#kNN imputation
#survey_data_test[[i]]<-VIM::kNN(X,variable=imputedvaluecolumn_assigned,k=5,dist_var=imputingcalculatebasiscolumn_assigned)
#}
#conditional random field
save(survey_data_test,file=paste0(dataset_file_directory,"rdata",slash,"miced_survey_8_",t_sessioninfo_running,"df.RData"))
load(file=paste0(dataset_file_directory,"rdata",slash,"miced_survey_7_",t_sessioninfo_running,"df.RData"))
#write.xlsx(as.data.frame(furtherusefulpredictor),file="furtherusefulpredictor.xlsx")
lapply(survey_data_test,View)
View(survey_data$`2010env.sav`[1:20,union(imputedvaluecolumn$`2010env`,imputingcalculatebasiscolumn$`2010env`)])

#checking imputed df
lapply(survey_data_test,function(X,need_particip_var,need_ses_var_assigned,imputedvaluecolumn) {
  survey<-X$SURVEY[1]
  checkdf<-extract(X,dplyr::intersect(names(X),unique(c(
    getElement(need_particip_var,survey),
    need_ses_var_assigned,
    getElement(imputedvaluecolumn,survey)
  )))) %>%
  {colSums(is.na(.))}
  return(checkdf)
},need_particip_var=need_particip_var,need_ses_var_assigned=need_ses_var_assigned,imputedvaluecolumn=imputedvaluecolumn)

if (FALSE) { #此部分屬於舊code，僅保留參考用
  #1) numeric data
  which(as.vector(sapply(survey_data$`2010env.sav`,is.numeric)))
  #2) factor data with 2 levels
  which(as.vector(sapply(survey_data$`2010env.sav`,function (X) {
    if (!is.factor(X) | !is.ordered(X)) {
      return(FALSE)
    }
    if (length(levels(X))!=2) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  })))
  #3) factor data with > 2 unordered levels
  which(as.vector(sapply(survey_data$`2010env.sav`,function (X) {
    if (!is.factor(X)) {
      return(FALSE)
    }
    if (length(levels(X))>2) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })))
  #4) factor data with > 2 ordered levels
  which(as.vector(sapply(survey_data$`2010env.sav`,function(X) {
    if (!is.ordered(X)) {
      return(FALSE)
    }
    if (length(levels(X))>2) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })))
  
  #Himsc填補遺漏值 filling in missing value
  library(Hmisc)
  
  survey_data_test <- custom_parallel_lapply(
    data=t_survey_data_test,
    f=function(X,imputedvaluecolumn,imputingcalculatebasiscolumn) {
      X <- droplevels(X)
      imputingcalculatebasiscolumn_assigned <- extract2(imputingcalculatebasiscolumn,X$SURVEY[1]) %>%
        intersect(names(X))
      imputedvaluecolumn_assigned <- extract2(imputedvaluecolumn,X$SURVEY[1]) %>%
        intersect(names(X))
      allimpcolumns<-union(imputedvaluecolumn_assigned,imputingcalculatebasiscolumn_assigned) %>%
        setdiff(c("myown_marriage","v89","v90","v98b","v128_2b","v128_2c","v132b","v125br","v138dr","v43","myown_working_status","v122ar","v122b1r",
                  "v41","myown_vote","v87_3","v88","v91","v17","v16","v87_4","myown_dad_ethgroup","v87_5")) #too few
      cal_formula<-as.formula(paste("~", paste(allimpcolumns, collapse="+")))
      print(c("SURVEY IS ",X$SURVEY[1]," AND FORMULA IS ",cal_formula))
      impute_arg <- aregImpute(cal_formula, data = X[,allimpcolumns], n.impute = 5)
      return(impute_arg)
    },
    imputedvaluecolumn=imputedvaluecolumn,
    imputingcalculatebasiscolumn=imputingcalculatebasiscolumn,
    exportvar=c("survey_data","imputedvaluecolumn","imputingcalculatebasiscolumn"),
    exportlib=c("base",lib,"Hmisc"),
    outfile=paste0(dataset_file_directory,"rdata",slash,"parallel_handling_process-",t_sessioninfo_running,".txt"),
    mc.set.seed = TRUE,
    mc.cores=parallel::detectCores()
  )
  
  
  
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
}


# 第五部份：IRT latent variables 環境設定 -------------------------------------------
#reset
#load(paste0(filespath,"data",slash,"all_survey_combined.RData"))
#load imputed survey
load(paste0(dataset_file_directory,"rdata",slash,"miced_survey_7_Ubuntu18.04.2LTSdf.RData"))

# 第五-1部份：IRT latent variables 將職業社經地位、家庭收入、教育程度萃取成為階級  =================================


survey_data_test <- lapply(survey_data_test,function(X) {
  #need_ses_var_assigned %<>% extract2(X$SURVEY[1]) %>%
  #  intersect(names(X))
  need_ses_var_assigned<-c("myown_eduyr","myown_ses","myown_income","myown_family_income")
  message("need ses var is ",X$SURVEY[1],need_ses_var_assigned)
  efa.results<-factanal(
    as.formula(paste0("~",need_ses_var_assigned,collapse = "+")),
    data=X[,need_ses_var_assigned],
    factors=1,
    rotation="promax",
    scores=c("regression")
  )
  X$myown_factoredses<-efa.results$scores
  return(X)
})

#install.packages("psy")
#library(psy)
#psy::scree.plot(fa.class$correlation)

# 第五-2部份：IRT latent variables 政治效能感 =================================
library(ltm)
library(eRm)
library(mirt)

analysingefficacy <- FALSE
if (analysingefficacy) {
  need_efficacy_var<-list(
    "2004citizen"=c("v47","v48","v52","v49","v50","v51"),
    "2010env"=c("v61","v70a","v78","v21a","v21b","v26a","v26b","v26c","v26d","v79"),
    "2010overall"=c("v67d","v67h","v67i","v67f"),
    "2016citizen"=c("d16a","d16b","d16c","d16d")
  )
  need_efficacy_recode_var<-list(#效能感越高要重編碼為數字越大
    "onetofour"=list(
      "2004citizen"=c("v52","v51"),
      "2010env"=c("v61","v78"),
      "2010overall"=c(),
      "2016citizen"=c()
    ),
    "onetofive"=list(#效能感越高要重編碼為數字越大
      "2004citizen"=c("v49","v50"),
      "2010env"=c("v26b","v79"),
      "2010overall"=c(),
      "2016citizen"=c("d16b","d16c")
    ),
    "onetosix"=list(#效能感越高要重編碼為數字越大
      "2004citizen"=c(),
      "2010env"=c(),
      "2010overall"=c("v67d","v67h","v67i"),
      "2016citizen"=c()
    )
  )
  survey_data_test <- lapply(survey_data_test,function(X,need_efficacy_var_assigned,need_efficacy_recode_var_assigned) {
    recode_list<-list(
      list("1"=4,"2"=3,"3"=2,"4"=1),
      list("1"=5,"2"=4,"4"=2,"5"=1),
      list("1"=6,"2"=5,"3"=4,"4"=3,"5"=1,"6"=1)
    )
    for (alteri in 1:3) {
      need_efficacy_recode_var <- extract2(need_efficacy_recode_var_assigned[[alteri]],X$SURVEY[1]) %>%
        intersect(names(X))
      X %<>% mutate_at(need_efficacy_recode_var,funs(dplyr::recode),!!!recode_list[[alteri]])
    }
    return(X)
  },need_efficacy_var_assigned=need_efficacy_var,need_efficacy_recode_var_assigned=need_efficacy_recode_var)
}
# 第五-3部份：IRT latent variables  latent variables 政治參與；用item respond抓出隱藏變數「政治參與程度」 =================================
# https://www.researchgate.net/post/How_to_conduct_item_analysis_with_a_likert_scale_questionaire
# mirt help: https://github.com/philchalmers/mirt/wiki
# http://moodle.ncku.edu.tw/pluginfile.php/977679/mod_resource/content/1/item_response_theory.pdf
library(ltm)
library(eRm)
library(mirt)


#2004citizen: v28 v29 v30 v31 v32 v33 v34 v35 v36 v37 v38 v39 v40 v59
#2016citizen-fit2-z1: b4 h2a h2b h2c h2d h2e h2f h2g h2h h3a h3b h3c h4
#2010overall-fit2: v79a v79b v79c v79d 
#2010env-fit1: v34 v35a v35b v35c ( v33f v75 v76 v77-為了環保而刻意不買某些產品,常不常參與社區的環保工作,常不常反應社區中容易造成天災危險的情況,常不常反應社區中造成環境污染的情況)
#load(paste0(filespath,"data",slash,"all_survey_combined.RData"))
#load imputed survey

need_particip_var<-list(
  "2004citizen"=c("v28","v29","v30","v31","v32","v33","v34","v35","v36","v37","v38","v39","v40"), #,"v59",v88 v90
  "2010env"=c("v34","v35a","v35b","v35c"), #投票"v104",
  "2010overall"=c("v79a","v79b","v79c","v79d"),
  "2016citizen"=c("h2a","h2b","h2c","h2d","h2e","h2f","h2g","h2h","h3a","h3b","h3c")#h4 投票
)
survey_data_test <- lapply(survey_data_test,function(X,need_particip_var_assigned) {
  #X<-lapply(X,function(X,need_particip_var_assigned) {
  #for testng prupose
  #X<-dummyremoved_imputed_survey_data[[1]][[1]]
  #need_particip_var_assigned<-need_particip_var
  need_particip_var_assigned %<>% extract2(X$SURVEY[1]) %>%
    intersect(names(X))
  customreordercatbylabelname<-function(X,desc=FALSE) {
    forcats::fct_reorder(X,as.character(X),.fun=unique,.desc=desc) %>%
      return()
  }
  X <- mutate_at(X,need_particip_var_assigned,funs(customreordercatbylabelname),desc=TRUE)
  #forcats::fct_reorder(f,sort(levels(f),decreasing=FALSE))
  #forcats::fct_reorder(f,sort(levels(f),decreasing=TRUE))
  #recode_list<-list( #把越參與的答案改為數字越多，比較好解釋
  #  "2004citizen"=list("1"=4,"2"=3,"3"=2,"4"=1),
  #  "2010env"=list("1"=2,"2"=1,"是"=2,"否"=1,"有"=2,"沒有"=1),
  #  "2010overall"=list("1"=3,"2"=2,"3"=1),
  #  "2016citizen"=list("1"=4,"2"=3,"3"=2,"4"=1)
  #) %>%
  #  extract2(X$SURVEY[1])
  #X %<>% mutate_at(need_particip_var_assigned,funs(dplyr::recode),!!!recode_list) %>%
  #  mutate_at(need_particip_var_assigned,funs(as.ordered))
  return(X)
},need_particip_var_assigned=need_particip_var)


# 第五-3-1部份：parametric IRT non-Rasch models - GRM Model ####################
# mirt::mirt by 'graded'
# ltm:grm
survey_data_test <- lapply(survey_data_test, function(X,need_particip_var_assigned) {
  #X<-survey_data_test[[4]]
  #need_particip_var_assigned<-need_particip_var
  needparticip_surveyi<-X$SURVEY[1]
  need_detailed_particip_var<-extract2(need_particip_var_assigned,needparticip_surveyi)
  irt_target_d<-X[,need_detailed_particip_var] %>%
    dplyr::mutate_all(.funs=function(f) {
      #return(as.numeric(levels(f))[f])
      (seq(from=1,to=length(f)))[f] %>% return()
    })
  estimatemodel<-mirt::mirt(
    data=irt_target_d,
    model=1,
    itemtype = "graded",
    technical = list("NCYCLES"=40000))
  poliparticipt<-mirt::fscores(estimatemodel,method="EAP") %>%
    as.data.frame()
  names(poliparticipt)<-c("myown_factoredparticip")
  X<-bind_cols(X,poliparticipt)
  #X<-estimatemodel
  #View(X[,c(need_detailed_particip_var,"myown_factoredparticip")])
  return(X)
},need_particip_var_assigned=need_particip_var)

using_ltm_package <- FALSE
if (using_ltm_package) {
  survey_data_with_particip <- lapply(survey_data_test,function(X,need_particip_var_assigned) {
    #for testing purpose
    X<-survey_data_test[[1]]
    need_particip_var_assigned<-need_particip_var
    
    need_particip_var_assigned %<>% extract2(X$SURVEY[1]) %>%
      intersect(names(X))
    fit1 <- ltm::grm(X[,need_particip_var_assigned], constrained = TRUE, start.val = "random")
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
        dplyr::select(-contains("Exp"),-contains("Obs")) %>%
        rename(myown_factored_partcip=z1,myown_factored_partcip.se=se.z1)
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
}
# 第五-3-2部份：non-parametric IRT Mokken scale analysis Model ####################
#################### mokken, Mokken Scale Analysis in R
#################### read: https://www.jstatsoft.org/article/view/v020i11/v20i11.pdf
mokken::coefH(as.data.frame(X[,need_particip_var_assigned]))
checkmokkenresult<-mokken::check.monotonicity(as.data.frame(X[,need_particip_var_assigned]))
summary(checkmokkenresult)
plot(checkmokkenresult)
scale.checkmokkenresult <- mokken::aisp(as.data.frame(X[,need_particip_var_assigned]))

# 第五-3-3部份：parametric IRT Rasch models - Partial Credit Model ####################
# mirt::Rasch
# eRm::PCM
# 第五-3-4部份：parametric IRT Rasch models - Rating Scale Model ####################
# eRm::RSM
# mirt:mirt
# 'grsm' and 'grsmIRT' - graded ratings scale model in the slope-interceptand classical IRT parameterization.
# 'grsmIRT'is restricted to unidimensional models (Muraki, 1992)

rst_mirt1 <- mirt::mirt(data = X[,need_particip_var_assigned], model = 1, verbose = T, itemtype= "grsmIRT")
coef(rst_mirt1)
for (itemplotn in 1:length(need_particip_var_assigned)) {
  mirt::itemplot(rst_mirt1, itemplotn)
  Sys.sleep(1)
}
summary(rst_mirt1)
residuals(rst_mirt1)
mirt::fscores(rst_mirt1,method = "EAP") %>% View()

# 第五-3-5部份：parametric IRT non-Rasch models - Generalized Partial Credit Model - Polytomous IRT ####################
#################### Finch, W. Holmes＆French, Brian F. (2015). Latent Variable Modeling with R. Florence: Taylor and Francis
## ltm::gpcm
## mirt::mirt by gpcmIRT
## 2016 not fit: gpcm, rasch 1PL all not fit;
gpcmconstraint<-"gpcm" #c("gpcm", "1PL", "rasch")
X.gpcm<-ltm::gpcm(X[,need_particip_var_assigned],constraint=gpcmconstraint,start.val="random")
summary(X.gpcm)
plot(survey_data.gpcm, lwd = 0.8, cex = 0.8, legend = TRUE, cx = "left", xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
plot(survey_data.gpcm,type=c("IIC"), lwd = 0.8, cex = 0.8, legend = TRUE, cx = "left", xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
ltm::GoF.gpcm(X.gpcm)

#margins(fit1)
#save(survey_data_test, file=paste0(filespath,"data",slash,"miced_survey_7_Ubuntu18.04.2LTSdf_with_mirt.RData"))
load(paste0(filespath,"data",slash,"miced_survey_7_Ubuntu18.04.2LTSdf_with_mirt.RData"))

# 第六部份：LCA latent variables 潛在類別模式資料清理  ================================= 

library(poLCA)
library(parallel)

lcaneed_independence_attitude<-list(
  "2004citizen"=c("v95","v96","v97"),#公投選項共變
  "2010env"=c(),#沒有統獨
  "2010overall"=c("v90","v91","v92"),
  "2016citizen"=c("h10r")#2016只有一題統獨傾向"
)
lcaneed_party_constituency<-list(
  "2004citizen"=c("v88","v89","v90","v99","v98b"),#可用 v88 v89 v90 v98b v99 (多類別)"v98r"
  "2010env"=c("v103r"),#2010env只有一題政黨傾向
  "2010overall"=c("v84","v93v94sumup"), #可用 v84 v86 v87a1r v93 v94(多類):v88,"v85v86v87sumup","v93v94sumup","v85v88sumup"
  "2016citizen"=c("h5","h6r_recode_party_for_forgotten","h7","h8r","h9r")
)
lcaneed_ethnicity<-list(
  "2004citizen"=c("myown_selfid"),#"myown_dad_ethgroup","myown_mom_ethgroup",
  "2010env"=c("myown_selfid"),#"myown_dad_ethgroup","myown_mom_ethgroup",
  "2010overall"=c("myown_selfid"),#,"myown_dad_ethgroup","myown_mom_ethgroup"
  "2016citizen"=c("myown_selfid") #到這裡，但還沒有輸出上述的v94r #"myown_dad_ethgroup","myown_mom_ethgroup",
)
lcaneed_identity<-list(
  "2004citizen"=c("v94r"),
  "2010env"=c(),
  "2010overall"=c("v89r"),
  "2016citizen"=c() #到這裡，但還沒有輸出上述的v94r
)
lcaneed_other_cov<-list(
  "2004citizen"=c("v87_1","v87_2","v87_3","v87_4","v87_5","v91","v91a","v91b","v92_1","v92_2","v92_3","v92_4"),#共變(公投投票選擇及參與政治活動例如凱道選擇)
  "2010env"=c(),
  "2010overall"=c(),
  "2016citizen"=c()
)
# 第六-1部份：LCA latent variables 潛在類別模式統獨傾向 =====================
#load(paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_eth_iden_othercovWindows8x64build9200.RData"))
#LCAmodel_with_indp

t_survey_data_test<-survey_data_test[3]
needsurveyi<-1

custom_generate_LCA_model<-function(X, n_latentclasses=2:5, n_rep=30, firstlcaneed, secondlcaneed=c(), ..., exportlib=c("base"), exportvar=c(), outfile="") {#,thirdlcaneed=c(),fourthlcaneed=c(),fifthlcaneed=c(), exportlib=c("base"), exportvar=c(), outfile=""
  #X此時就是一個問卷的dataset
  #lcaneed_independence_attitude
  #lcaneed_party_constituency
  #lcaneed_ethnicity
  #lcaneed_identity
  #lcaneed_other_cov
  message("<===== at custom_generate_LCA_model exportlib is ", exportlib, " and exportvar is ", exportvar, " and outfile is ", outfile, "=====>")
  otherlcaneed<-unlist(list(...))
  needsurveyi<-X$SURVEY[1]
  cov_parameter_in_formula<-dplyr::union_all(
    magrittr::extract2(secondlcaneed,needsurveyi),
    magrittr::extract2(otherlcaneed,needsurveyi)
  )
  if (class(n_latentclasses)=='list') {
    n_latentclasses <- magrittr::extract2(n_latentclasses,needsurveyi)
  }
  if (identical(cov_parameter_in_formula,logical())) {
    cov_parameter_in_formula<-"1"
  }
  modelformula<-paste0(
    "cbind(",
    paste(magrittr::extract2(firstlcaneed,needsurveyi),collapse=","),
    ") ~ ",
    paste0(cov_parameter_in_formula,collapse="+"),
    collapse=""
  )
  poLCAresult<-switch(
    as.character(length(magrittr::extract2(firstlcaneed,needsurveyi))),
    "0"=NULL,
    "1"=X[,magrittr::extract2(firstlcaneed,needsurveyi)],
    {
      custom_parallel_lapply(X=n_latentclasses,FUN=function(poXi,s_survey_data,modelformula,...)
      {
        library(poLCA) #for fixing stange situation that Windows does not fork well
        #### lapply(2:7,function(poXi,s_survey_data) {
        lcamodelbuildtresult<-poLCA(
          data = s_survey_data,
          formula = as.formula(modelformula),
          nclass = poXi,
          #graphs = TRUE,
          maxiter = 1000,
          nrep = n_rep
        )
        lcamodelbuildtresult$nclass <- poXi
        lcamodelbuildtresult$modelformula <- modelformula
        return(lcamodelbuildtresult)
        #poXi
      },s_survey_data = X,
      modelformula = modelformula,
      exportvar = exportvar,
      exportlib = exportlib,
      outfile = outfile
      )
    }
  ) #end of switch
  return(poLCAresult)
}

###LCAmodel_with_indp_covparty <- list()
###for (lcamodelit in 1:1) {#length(t_survey_data_test)
###  LCAmodel_with_indp_covparty[[lcamodelit]]<-custom_parallel_lapply(
###    X=t_survey_data_test[[lcamodelit]],
###    FUN=custom_generate_LCA_model,
###    exportvar=c("t_survey_data_test","lcaneed_independence_attitude","lcaneed_party_constituency","lcaneed_ethnicity","lcaneed_identity","lcaneed_other_cov"),
###    exportlib=c("base",lib,"poLCA"),
###    outfile=paste0(dataset_file_directory,"rdata",slash,"parallel_handling_process-",t_sessioninfo_running,".txt"),
###    firstlcaneed=lcaneed_independence_attitude,
###    secondlcaneed=lcaneed_party_constituency
###  )
###}

#測試調整參數用
lcaneed_party_constituency[["2010overall"]] <- c("v87a1r","v85v86v87sumup") #可用 v84 v86 v87a1r v93 v94(多類):v88,"v85v86v87sumup","v93v94sumup","v85v88sumup"
cov_vars <- c("v84", "v86", "v87a1r", "v93", "v94", "v88", "v85v86v87sumup", "v93v94sumup", "v85v88sumup")
cov_vars_combns <- unlist(
  lapply(1:length(cov_vars),
         function(i)combn(1:length(cov_vars),i,simplify=FALSE)
  )
  ,recursive=FALSE) %>%
  lapply(FUN=function(X,var) extract(var,X), var=cov_vars)
need_in_lcaneed_party_constituency_combn_i<-258
LCAmodel_with_indp_covparty_combn<-list()
for (lcaneed_party_constituency_combn_item in cov_vars_combns[need_in_lcaneed_party_constituency_combn_i:length(cov_vars_combns)]) {
  need_in_lcaneed_party_constituency_combn <- list()
  need_in_lcaneed_party_constituency_combn[['2010overall']] <- lcaneed_party_constituency_combn_item
  #先測試 degree of freedom 是否為負數不然白忙一場
  LCAmodel_with_indp_covparty_testfor_resid_df <- custom_parallel_lapply(
    X=t_survey_data_test,
    FUN=custom_generate_LCA_model,
    exportvar=c("t_survey_data_test","custom_parallel_lapply","lcaneed_independence_attitude","lcaneed_party_constituency","lcaneed_ethnicity","lcaneed_identity","lcaneed_other_cov"),
    exportlib=c("base",lib,"poLCA","parallel"),
    outfile=paste0(dataset_file_directory,"rdata",slash,"parallel_handling_process-",t_sessioninfo_running,".txt"),
    n_rep=1,
    firstlcaneed=lcaneed_independence_attitude,
    secondlcaneed=need_in_lcaneed_party_constituency_combn
  ) #,secondlcaneed=lcaneed_party_constituency,thirdlcaneed=lcaneed_ethnicity,fourthlcaneed=lcaneed_identity,fifthlcaneed=lcaneed_other_cov
  LCAmodel_with_indp_covparty_test_correct_classes <- lapply(LCAmodel_with_indp_covparty_testfor_resid_df, function(X) {
    #,getElement,"resid.df"
    if (class(X)=="list") {
      class_assigned<-sapply(X,function(Z) {Z$nclass})
      wheredfbiggerthanzero<-which(sapply(X,getElement,"resid.df")>0)
      correct_class_to_assign<-class_assigned[wheredfbiggerthanzero]
      return(correct_class_to_assign)
    } else {
      return(NULL)
    }
  }) %>%
    setNames(stringi::stri_replace(names(.),replacement="",regex=".sav"))
  LCAmodel_with_indp_covparty<-custom_parallel_lapply(
    X=t_survey_data_test,
    FUN=custom_generate_LCA_model,
    exportvar=c("t_survey_data_test","custom_parallel_lapply","lcaneed_independence_attitude","lcaneed_party_constituency","lcaneed_ethnicity","lcaneed_identity","lcaneed_other_cov"),
    exportlib=c("base",lib,"poLCA","parallel"),
    n_latentclasses=LCAmodel_with_indp_covparty_test_correct_classes,
    outfile=paste0(dataset_file_directory,"rdata",slash,"parallel_handling_process-",t_sessioninfo_running,".txt"),
    firstlcaneed=lcaneed_independence_attitude,
    secondlcaneed=need_in_lcaneed_party_constituency_combn
  )
  LCAmodel_with_indp_covparty_combn[[need_in_lcaneed_party_constituency_combn_i]] <- list(
    "formula"=paste(need_in_lcaneed_party_constituency_combn,collapse="+"),
    "correctclasses"=LCAmodel_with_indp_covparty_test_correct_classes,
    "model"=LCAmodel_with_indp_covparty
    )
  need_in_lcaneed_party_constituency_combn_i <- need_in_lcaneed_party_constituency_combn_i+1
  if ((need_in_lcaneed_party_constituency_combn_i %% 15)==0) {
    save(LCAmodel_with_indp_covparty_combn,file=paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_covparty_combn.RData"))
  }
}

LCAresult_to_sheet <- function(str) {
  str %<>% customgsub(pattern = '[ ]{2,}', replacement = "[", perl = TRUE) %>%
    customgsub(pattern = '\\[\\[', replacement = '\\[') %>%
    stringi::stri_split_lines() %>%
    unlist() %>%
    lapply(unlist) %>%
    lapply(stringi::stri_split,regex = "\\[") %>%
    lapply(unlist) %>%
    lapply(t) %>%
    lapply(as.data.frame) %>%
    plyr::rbind.fill()
  View(str)
}
LCAresult_to_sheet()

save(LCAmodel_with_indp_covparty,file=paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_covparty",t_sessioninfo_running,".RData"))
#levels(t_survey_data_test[[1]]$myown_atti_ind)[levels(t_survey_data_test[[1]]$myown_atti_ind)=="1"] <- "統一"

# 第六-2部份：LCA latent variables 潛在類別模式政黨傾向 ====================
t_survey_data_test<-survey_data_test

LCAmodel_with_partyconstituency_nocov <- custom_parallel_lapply(
  X=t_survey_data_test,
  FUN=custom_generate_LCA_model,
  exportvar=c("t_survey_data_test","lcaneed_independence_attitude","lcaneed_party_constituency","lcaneed_ethnicity","lcaneed_identity","lcaneed_other_cov"),
  exportlib=c("base",lib,"poLCA"),
  outfile=paste0(dataset_file_directory,"rdata",slash,"parallel_handling_process-",t_sessioninfo_running,".txt"),
  firstlcaneed=lcaneed_party_constituency,
  secondlcaneed=lcaneed_independence_attitude,
  mc.set.seed = TRUE,
  mc.cores=parallel::detectCores()
) #,secondlcaneed=lcaneed_party_constituency,thirdlcaneed=lcaneed_ethnicity,fourthlcaneed=lcaneed_identity,fifthlcaneed=lcaneed_other_cov


save(LCAmodel_with_partyconstituency_nocov,file=paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_partyconstituency_nocov",t_sessioninfo_running,".RData"))


# 第六-3部份：潛在類別分析：將分析結果整併入dataset --------------------------------------------------

#load(paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_covpartyUbuntu18.04.1LTS_do_not_delete.RData"))
load(paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_covpartyUbuntu18.04.2LTS.RData"))

new_LCAmodel_with_indp_covparty_2004citizen_3_classes<-poLCA::poLCA(
  data=t_survey_data_test[[1]],
  formula=as.formula(paste0(
    "cbind(",
    paste(magrittr::extract2(lcaneed_independence_attitude,"2004citizen"),collapse=","),
    ") ~ ",
    paste0(lcaneed_party_constituency[[1]],collapse="+"),
    collapse=""
  )),
  nclass = 3,
  #graphs = TRUE,
  maxiter = 1000,
  nrep=30,
  probs.start=poLCA::poLCA.reorder(
    LCAmodel_with_indp_covparty[[1]][[2]]$probs.start,
    c(2,3,1)
  )
)
table(LCAmodel_with_indp_covparty[[1]][[2]]$predclass)
survey_data_test[[1]]$myown_indp_atti<-factor(
  LCAmodel_with_indp_covparty[[1]][[2]]$predclass,
  levels = c(1,2,3), labels = c("[2] 中立", "[1] 統一", "[3] 獨立") #needs interpretation and modify here
) %>% table()
new_LCAmodel_with_indp_covparty_2010overall_3_classes<-poLCA::poLCA(
  data=t_survey_data_test[[3]],
  formula=as.formula(paste0(
    "cbind(",
    paste(magrittr::extract2(lcaneed_independence_attitude,"2010overall"),collapse=","),
    ") ~ ",
    paste0(lcaneed_party_constituency[[3]],collapse="+"),
    collapse=""
  )),
  nclass = 3,
  #graphs = TRUE,
  maxiter = 1000,
  nrep=30,
  probs.start=poLCA::poLCA.reorder(
    LCAmodel_with_indp_covparty[[3]][[2]]$probs.start,
    c(3,1,2)
  )
)
LCAmodel_with_indp_covparty[[3]][[2]]<-new_LCAmodel_with_indp_covparty_3_3
survey_data_test[[1]]$myown_indp_atti<-factor(
  LCAmodel_with_indp_covparty[[1]][[2]]$predclass,
  levels = c(1,2,3), labels = c("[1] 統一", "[2] 中立", "[3] 獨立")
)#save(survey_data_test,file=paste0(filespath,"data",slash,"survey_data_test.RData"))
survey_data_test[[3]]$myown_indp_atti<-factor(
  LCAmodel_with_indp_covparty[[3]][[2]]$predclass,
  levels = c(1,2,3), labels = c("[1] 統一", "[2] 中立", "[3] 獨立")
)
table(LCAmodel_with_indp_covparty[[3]][[2]]$predclass)
survey_data_test[[3]]$myown_indp_atti<-factor(
  LCAmodel_with_indp_covparty[[3]][[2]]$predclass,
  levels = c(1,2,3), labels = c("[2] 中立", "[1] 統一", "[3] 獨立") #needs interpretation and modify here
) %>% table()
survey_data_test[[4]]$myown_indp_atti<-LCAmodel_with_indp_covparty[[4]]$h10r
#save(survey_data_test,file=paste0(filespath,"data",slash,"survey_data_test.RData"))


# 第七部份：把問卷資料變形以便串連及行政區、選舉資料 ---------------------------------
load(paste0(filespath,"data",slash,"survey_data_test.RData"))
#library(reshape2)

survey_oldq_id<-list(
  "2004citizen"=c("v25","v26","v27","v41","v42","v43","v44","v45","v46","v60","v61","v62","v65","v74","v91a","v91b","v92_1","v92_2","v92_3","v92_4","v92_5","v93a","v93b","v95","v96","v97","v105a","v105b","v105c","v106a","v106b","v106c","v107a","v107b","v107c","v114","v118a","v118b","v118c","v118d"),
  "2010env"=c("v39a", "v39b", "v39c", "v40", "v78a", "v78b", "v78c", "v78d", "v78e", "v78f", "v78g", "v78h", "v78i", "v90", "v91", "v92"),
  "2010overall"=c("kv21c_0", "kv31_0", "kv67_0", "v14a", "v14b", "v15a", "v15b", "v16a", "v16b", "v19", "v20a", "v20b", "v21c", "v22a", "v22b", "v22c", "v23a", "v23b", "v23c", "v24a", "v24b", "v24c", "v25a", "v25b", "v25c", "v26a", "v26b", "v26c", "v26d", "v26e", "v26f", "v26g", "v27a", "v27b", "v27c", "v27d", "v27e", "v27f", "v27g", "v28a", "v28b", "v29", "v30a", "v30b", "v31", "v32a", "v32b", "v32c", "v36a", "v36b", "v37a", "v37b", "v37c", "v37d", "v37e", "v37f", "v37g", "v37h", "v37i", "v38a1", "v38a2", "v38b1", "v38b2", "v38c1", "v38c2", "v38d1", "v38d2", "v38e1", "v38e2", "v39a", "v39b", "v39c", "v40", "v57", "v58", "v59", "v63", "v66c", "v66f", "v67", "v68", "v69", "v70b", "v70c", "v70d", "v70e", "v70f"),
  "2016citizen"=c("c1a",	"c1b",	"c1c",	"c1d",	"c1e",	"c2",	"c3",	"c4",	"c5",	"c6",	"c10",	"c11",	"c12",	"c13",	"c14",	"d1",	"d2a",	"d2b",	"d3a",	"d3b",	"d4",	"d5a",	"d5b",	"d5c",	"d5d",	"d5e",	"d5f",	"d6a",	"d6b",	"d6c",	"d6d",	"d6e",	"d6f",	"d6g",	"d6h",	"d7a",	"d7b",	"d7c",	"d7d",	"d7e",	"d7f",	"d7g",	"d7h",	"d7i",	"d7j",	"d7k",	"d8a",	"d8b",	"d8c",	"d11a",	"d11b",	"d12",	"d13a",	"d13b",	"d14a",	"d14b",	"d14c",	"d17a",	"d17b",	"d17c",	"e2a",	"e2b",	"e2c",	"e2d",	"e2e",	"e2f",	"e2g",	"e2h",	"e2i",	"f3",	"f4",	"f5",	"f8",	"f9",	"h10")
)
survey_q_id<-sapply(survey_data_title,function(X,df,oldvec=c()) {
  topickeyword<-c("議題","議題（或民主價值與公民意識牽涉群體）","民主價值與公民意識")
  if(identical(oldvec,c())) {
    oldvec[[X]]=c()
  }
  needq<-dplyr::filter(df,SURVEY==X,CATEGORY %in% topickeyword) %>%
    dplyr::select(ID) %>%
    unlist() %>%
    as.character() %>%
    union(oldvec[[X]])
  return(needq)
},df=survey_imputation_and_measurement)

#有些資料在轉換過程中內容會變成label而非coding的資料，要把他變回來
covert_label_according_to_xls_codebook<-FALSE
if(covert_label_according_to_xls_codebook) {
  mistakinglevelvars<-list(
    "2004citizen"=c('myown_indp_atti','v61','v62','v74','v91a','v91b','v93a','v93b','v95'),
    "2010env"=c('v14a','v14b','v16a','v16b','v20a','v20b','v28a','v28b','v31','v40','v57','v58','v59'),
    "2010overall"=c('myown_indp_atti','v61','v62','v80','v89'),
    "2016citizen"=c('myown_indp_atti','c1a','c1b','c1c','c1d','c1e','c2','c3','c6','c8','c8r','c9','c9r','c11','c14','d1','d4','d8a','d8b','d8c','d9a','d9b','d10','h10','h10r')
  )
  #以下部分是從先前問卷舊標籤factor方式而來，如果要使用還需要重新修改
  prepare_for_label_adj_df<-prepare_for_label_adj_df
  mistakinglevelvars<-mistakinglevelvars
  dfcodebook<-survey_codebook
  prepare_for_label_adj_df <- lapply(mistakinglevelvars,function(mistakinglevelvar,...) {
    dfcodebook<-dfcodebook[
      (dfcodebook$SURVEY==survey_data_title) & (dfcodebook$ID == mistakinglevelvar),
      c('SURVEY','ID','VALUE','LABEL')]#ID %in% Y
    dedf_keyvalues<-as.list(getElement(dfcodebook,'VALUE'))
    names(dedf_keyvalues)<-getElement(dfcodebook,'LABEL')
    #result<-filter(dfcodebook,SURVEY==X,ID %in% Y)
    dedf_keyvalues
  },MoreArgs=list(survey_data_title=survey_data_title,dfcodebook=dfcodebook),
  SIMPLIFY=FALSE)
  names(prepare_for_label_adj_df)<-mistakinglevelvars
  for (recodevar in mistakinglevelvars) {
    tplistforrecode <- getElement(prepare_for_label_adj_df,recodevar)
    X[[recodevar]] <- dplyr::recode(getElement(X,recodevar),!!!tplistforrecode)
    #message("recodevar is ", recodevar," and length is ",length(X$recodevar)," and list is ",tplistforrecode," and names of list is",names(tplistforrecode))
  }
  #X
}

#survey_data_melted
complete_survey_dataset<-mapply(function(X,Y) {
  survey_data_title<-X$SURVEY[1]
  
  other_var_as_id<-setdiff(names(X),Y)
  X<-mutate_at(X,Y,as.character)
  reshape2::melt(X, id.vars = other_var_as_id, variable.name = "SURVEYQUESTIONID", value.name = "SURVEYANSWERVALUE") %>%
    dplyr::mutate_at("SURVEYANSWERVALUE",funs(as.character)) %>%
    dplyr::group_by( SURVEYQUESTIONID ) %>%
    dplyr::mutate("all_pos_on_same_q_by_nation"=n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SURVEYQUESTIONID,SURVEYANSWERVALUE) %>%
    dplyr::mutate("same_pos_on_same_q_by_nation"=n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate("same_pos_to_all_ratio_by_nation"=same_pos_on_same_q_by_nation/all_pos_on_same_q_by_nation*100)
},
X=survey_data_test,
Y=survey_q_id)
#View(filter(complete_survey_dataset[[1]],SURVEYQUESTIONID=='myown_indp_atti'))
#dplyr::recode(survey_data_test[[1]]$v61,!!!getElement(getElement(prepare_for_label_adj_df,"2004citizen"),"v61"))

#以下是要把四份問卷合一，但這應該要放棄
common_var<-Reduce(intersect, lapply(complete_survey_dataset, names )) %>%
  setdiff(c("sd"))


#survey_data_melted 沒有節省欄位直接合併
#complete_survey_dataset<-Reduce(plyr::rbind.fill,complete_survey_dataset) %>%
#  extract(common_var)
#vhead(complete_survey_dataset)
#survey_data_melted_names<-lapply(survey_data_melted,names)


#factor to numeric method
#survey_data_melted<-lapply(survey_data_melted,function(X) {
#  X<-mutate_at(c(),as.numeric(levels(f))[f]

#節省欄位合併
complete_survey_dataset<-lapply(complete_survey_dataset,select_and_fill_nonexistcol,common_var) %>%
  plyr::rbind.fill(.) %>%
  rename(ansv_and_label=SURVEYANSWERVALUE)#%>%
#reshape2::melt(id.vars = setdiff(colnames(.),c("term1","term2")), variable.name = "variable_on_term", value.name = "term")
vhead(complete_survey_dataset %>% filter(SURVEYQUESTIONID=='myown_indp_atti'))
withoutlabelansv <- unique(complete_survey_dataset$ansv_and_label)[c(31,104:116,135:144,167:173,180:188,209:222,285:287,293:299)]
filter(complete_survey_dataset, ansv_and_label %in% withoutlabelansv) %>%
  distinct(SURVEY,SURVEYQUESTIONID,ansv_and_label) %>%
  View()
#c(NA,"","以上皆非等待發明  不知道何種替代能源","用垃圾科技轉換能源",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
#save(complete_survey_dataset,file=paste0(filespath,"vote_record",slash,"complete_survey_dataset.RData"))
##針對調查問卷資料處理變形，以便合併

#"c1a","c1b","c1c","c1d","c1e","c2","c3","c4","c5","c6","c10","c11","c12","c13","c14","d1","d2a","d2b","d3a","d3b","d4","d5a","d5b","d5c","d5d","d5e","d5f","d6a","d6b","d6c","d6d","d6e","d6f","d6g","d6h","d7a","d7b","d7c","d7d","d7e","d7f","d7g","d7h","d7i","d7j","d7k","d8a","d8b","d8c","d11a","d11b","d12","d13a","d13b","d14a","d14b","d14c","d17a","d17b","d17c","e2a","e2b","e2c","e2d","e2e","e2f","e2g","e2h","e2i","f3","f4","f5","f8","f9","h10","kh10"

# 第八部份：問卷資料串連立委資料、選舉資料 ---------------------------------

#load(paste0(filespath,"data",slash,"elections_df_test.RData"))

#直接讀取分析立法通過的資料集
#as glmdata_pass_on_bill
#distinct(glmdata,)
#load(file=paste0(dataset_file_directory,"rdata",slash,"pass_on_bill.RData"))
load(paste0(filespath,"data",slash,"complete_survey_dataset.RData"))
load(paste0(filespath,"data",slash,"mergedf_votes_bills_election_surveyanswer.RData"))
load(paste0(filespath,"data",slash,"legislator_additional_attributes.RData"))
load(paste0(filespath,"data",slash,"legislators_with_election.RData"))
#load(paste0(filespath,"vote_record",slash,"complete_survey_dataset.RData"))
#admincity admindistrict adminvillage

#legislator_age待修

#only_bill_to_survey_information<-distinct(mergedf_votes_bills_election_surveyanswer,stdbilldate,term,period,meetingno,temp_meeting_no,billn,billresult,billid_myown,SURVEY,variable_on_q,value_on_q_variable,SURVEYQUESTIONID,SURVEYANSWERVALUE,LABEL,QUESTION,opinionfromconstituent,opinionfrombill,issue_field1,issue_field2,opinionstrength,opiniondirectionfromconstituent,opiniondirectionfrombill,success_on_bill) %>%
#  mutate_at("SURVEYANSWERVALUE", funs(as.character))
#save(only_bill_to_survey_information,file=paste0(filespath,"data",slash,"only_bill_to_survey_information.RData"))
#load(paste0(filespath,"data",slash,"only_bill_to_survey_information.RData"))
complete_survey_dataset %<>% select(-zip,-wave,-qtype,-myown_industry,-myown_job,-villagefullname,-myown_family_income_ingroup)
filter(mergedf_votes_bills_election_surveyanswer[1:1000,], respondopinion==1) %>% View()
legislators_with_election %<>% select(-ename,-onboardDate,-committee,-degree,-experience,-picUrl,-leaveFlag,-leaveDate,-leaveReason,-ballotid,-birthplace,-education,-wonelection) %>%
  left_join(term_to_survey)
#test_tiny_legislators_with_election <- distinct(legislators_with_election, term, legislator_name, legislator_sex, legislator_party, partyGroup, areaName, birthday, legislator_age, incumbent, election_party, electionarea, admincity)
#term, legislator_name, legislator_sex, legislator_party, partyGroup, areaName, birthday, legislator_age, incumbent, election_party, electionarea, admincity, admindistrict, adminvillage, plranking, elec_dist_type
##注意有遺漏的部分委員
list(
  complete_survey_dataset,
  mergedf_votes_bills_election_surveyanswer,
  legislators_with_election
  #inner_join(mergedf_votes_bills_election_surveyanswer, distinct(survey_time_range[['2004citizen']],yrmonth) ),
  #inner_join(mergedf_votes_bills_election_surveyanswer, distinct(survey_time_range[['2004citizen']],yrmonth) ) %>%
  #left_join(test_tiny_legislators_with_election, by=c("term","legislator_name"))
) %>%
  sapply(nrow)
#有多個村里會重複所以join時會膨脹
testdf <- filter(complete_survey_dataset, SURVEY=='2016citizen')%>%
  left_join(legislators_with_election) %>%
  left_join(legislators_additional_attr) %>%
  select(-education,-experience) %>%
  left_join(mergedf_votes_bills_election_surveyanswer) %>%
  mutate_at("SURVEYANSWERVALUE", funs(as.character))

#只有針對議案的決定，而非有無代理
testdf <- mutate_at(complete_survey_dataset,"term", funs(as.numeric)) %>%
  inner_join(only_bill_to_survey_information)

#沒有投票權也會串到立委，也就是只串選區的串法
testdf <- inner_join(complete_survey_dataset, testdf, by = c("term", "electionarea", "SURVEY", "SURVEYQUESTIONID", "SURVEYANSWERVALUE"))
#只串到支持的候選人的串法
#testdf <- inner_join(complete_survey_dataset, testdf, by = c("term", "electionarea", "SURVEY", "SURVEYQUESTIONID", "SURVEYANSWERVALUE", "myown_constituency_party_vote"="election_party"))
#串全國，不限選區
#testdf <- inner_join(complete_survey_dataset, testdf, by = c("term", "SURVEY", "SURVEYQUESTIONID", "SURVEYANSWERVALUE", "myown_constituency_party_vote"="election_party"))

# only observe if bills are passed
testdf<-inner_join(complete_survey_dataset, only_bill_to_survey_information,by = c("SURVEY", "term", "SURVEYQUESTIONID", "SURVEYANSWERVALUE"))
