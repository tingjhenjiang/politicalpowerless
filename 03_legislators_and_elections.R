# 第Ｏ部份：環境設定 --------------------------------
if (!("benchmarkme" %in% rownames(installed.packages()))) install.packages("benchmarkme")
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
t_sessioninfo_running_with_cpu_locale<-gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2]))
source(file = "shared_functions.R", encoding="UTF-8")
#選舉資料
overall_elec_dist_types<-c('district','ab_m','ab_plain','partylist')
supplement_election_termseven<-c('supp2009miaoli1','supp2009nantou1','supp2009yunlin2','supp2009taipei6','supp2010taichungs3','supp2010hualian','supp2010taoyuan2','supp2010taoyuan3','supp2009hsinchus','supp2010chiayi2','supp2010taitung','supp2011tainan4','supp2011kaoshiung4')
supplement_election_termsix<-c('supp2006chiayi')
#supplement_election_termeight<-c('supp2013taichung2') 研究不需要
terms<-c(5,6,7,8,9)


if ({process_for_supp2006chiayi<-TRUE; process_for_supp2006chiayi}) {
  path_prefix_of_supp2006chiayi <- paste0(dataset_file_directory,"cec_vote_dataset",slash,"term",6,slash,"supp2006chiayi",slash)
  supp2006chiayi_dataset_file <- "term6_supp2006chiayi_process_for_raw.xlsx" %>%
    paste0(path_prefix_of_supp2006chiayi, .)
  tabs <- c("elprof","elpaty","electks","elcand","elbase")
  for (loop_over_sheet_i in 1:length(tabs)) {
    supp2006chiayi_temp_df <- openxlsx::read.xlsx(xlsxFile = supp2006chiayi_dataset_file, sheet = loop_over_sheet_i) %>%
      dplyr::select(-contains("CHI_NAME")) %>%
      dplyr::mutate_all(as.character) %>%
      readr::write_csv(path=paste0(path_prefix_of_supp2006chiayi, tabs[loop_over_sheet_i], ".csv"))
  }
}


# 第一部份：立委及選區資料 -------------------------------------------
library(parallel)
library(future)
library(future.apply)
reset_multi_p()
#for (term in terms) {
load(file=paste0(dataset_in_scriptsfile_directory, "elections_df.RData"), verbose=TRUE)
if ({process_elections_df<-FALSE; process_elections_df}) {
  elections_df <- future.apply::future_lapply(terms, function(term, ...) {
    elections_df <- elections_df_onekind <- data.frame()
    message("term=",term)
    term_character<-paste0("0",term)
    elec_types<-overall_elec_dist_types
    if (term==7) {
      elec_types<-c(overall_elec_dist_types,supplement_election_termseven)
    }
    if (term==6) {
      elec_types<-c(overall_elec_dist_types,supplement_election_termsix)
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
      election_admin_county<-filter(elections_df_dist,鄉鎮市區!="000" & 鄉鎮市區!=0 ,村里別!="0000" & 村里別!=0,!customgrepl(名稱,"選區|選舉區|全國|政黨")) %>%
        rename("村里名稱"="名稱")
      #省市別  縣市別  選區別  鄉鎮市區  村里別  村里名稱
      election_admin_dist<-filter(elections_df_dist,鄉鎮市區!="000",村里別=="0000" | 村里別==0,!customgrepl(名稱,"選區|選舉區|全國|政黨")) %>%
        rename("鄉鎮市區名稱"="名稱") %>%
        select("省市別","縣市別","選區別","鄉鎮市區","鄉鎮市區名稱")
      election_admin_city<-filter(elections_df_dist,鄉鎮市區=="000" | 鄉鎮市區==0,選區別=="00" | 選區別==0, !customgrepl(名稱,"選區|選舉區|全國|政黨")) %>%
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
        elections_df_onekind<-dplyr::left_join(elections_df_plcan,elections_df_onekind,by = c("政黨代號")) #%>%
        #rename(鄉鎮市區.y=鄉鎮市區.x,村里別.y=村里別.x)
        #pick_vector<-c( "省市別",       "縣市別",       "選區別",       "鄉鎮市區.x",  
        #                "村里別.x",     "號次",         "名字",         "政黨代號",    
        #                "性別",         "出生日期",     "年齡",         "出生地",      
        #                "學歷",         "現任",         "當選註記",     "副手",        
        #                "X17",          "政黨名稱",     "選舉區名稱",   "縣市名稱",    
        #                "鄉鎮市區.y",   "鄉鎮市區名稱", "村里別.y",     "村里名稱")
        #名字 名字.x 號次 排名 性別 性別.x 出生日期 出生日期.x 年齡 年齡.x 出生地 出生地.x 
      }
      elections_df_onekind <- cbind(term = term_character, elections_df_onekind, elec_dist_type = elec_dist_type) %>%
        mutate_cond(term=="05" && elec_dist_type=="district", 年齡=91-as.integer(出生日期)) %>%
        mutate_cond(term=="05" && elec_dist_type=="ab_m", 年齡=91-as.integer(出生日期)) %>%
        mutate_cond(term=="05" && elec_dist_type=="ab_plain", 年齡=91-as.integer(出生日期)) %>%
        mutate_cond(term=="06" && elec_dist_type=="district", 年齡=93-as.integer(出生日期)) %>%
        mutate_cond(term=="06" && elec_dist_type=="ab_m", 年齡=93-as.integer(出生日期)) %>%
        mutate_cond(term=="06" && elec_dist_type=="ab_plain", 年齡=93-as.integer(出生日期))
      if (elections_df_onekind$年齡[1]==99 | is.na(elections_df_onekind$年齡[1])) {
        message("term = ", term, " and elec_dist_type = ", elec_dist_type, " and birthday format = ", elections_df_onekind$出生日期[1])
        stop()
      }
      elections_df <- dplyr::bind_rows(elections_df,elections_df_onekind) #結合參選人以及選區的資料
    } #分區、全國區結束
    return(elections_df)
    #check: filter(elections_df,is.na(選舉區名稱)) %>% View()
    #check: distinct(legislators_needed,areaName,選舉區名稱) %>% View()
  },
  exportvar=c("supplement_election_termseven","supplement_election_termsix","dataset_file_directory","terms","slash","filespath","overall_elec_dist_types","custompaste0","customgsub","customgrep","customgrepl","mutate_cond"), #,"error_leave_and_attend_legislators","error_vote_record_from_name","replace_troublesome_names","anti_join_with_nrow_zero"
  exportlib=c("base",lib)) %>%
    dplyr::bind_rows() %>%
    .[, c("term", "號次", "名字", "性別", "出生日期", "年齡", "出生地", "學歷", "現任", "當選註記", "政黨名稱", "選舉區名稱", "縣市名稱", "鄉鎮市區名稱", "村里名稱", "排名", "elec_dist_type")] %>%
    rename(ballotid = 號次, name = 名字, sex = 性別, birthday = 出生日期, age = 年齡, birthplace = 出生地, education = 學歷, incumbent = 現任, wonelection = 當選註記, party = 政黨名稱, electionarea = 選舉區名稱, admincity = 縣市名稱, admindistrict = 鄉鎮市區名稱, adminvillage = 村里名稱, plranking = 排名) %>%
    mutate_at(c("sex"), dplyr::recode_factor, `2`="女", `1`="男") %>%
    mutate_cond(elec_dist_type %in% supplement_election_termseven,elec_dist_type="district") %>%
    mutate_at(c("term"), .funs = list(term = ~customgsub(term, "0(\\d)+", "\\1", perl = TRUE)) ) %>% #funs(customgsub(term, "0(\\d{1})", "\\1", perl = TRUE))
    mutate_at(c("term"), as.character) #%>%
  #left_join(all_admin_dist_with_zip)
  #elections_df_test <- elections_df
}

##從選區資料抓出舊制全國縣市鄉鎮市區
all_admin_dist_with_zip <- dplyr::distinct(elections_df, term, admincity, admindistrict) %>%
  dplyr::filter(!is.na(admincity)) %>%
  cbind(., "fullcountyname" = .$admindistrict) %>%
  dplyr::mutate_at(c("admindistrict"), .funs = list(admindistrict = ~stri_sub(admindistrict, from = 1, to = -2))) %>% #funs(stri_sub(admindistrict, from = 1, to = -2))
  dplyr::mutate_at("fullcountyname",as.character) %>%
  dplyr::left_join({ #透過全國行政區的行政區名稱，比對不完整鄉鎮市區名稱的郵遞區號行政區，組裝出行政區郵遞區號
    paste0(dataset_file_directory,"zip3.xlsx") %>%
      openxlsx::read.xlsx(sheet = 1) %>%
      dplyr::rename(admincity = 縣市名稱, admindistrict = 鄉鎮市區名稱) %>%
      dplyr::mutate_at(c("admindistrict"), .funs = list(admindistrict = ~customgsub(admindistrict, "區", ""))) %>% ##鄉鎮市區名稱還沒有統一 #funs(customgsub(admindistrict, "區", ""))
      dplyr::mutate_at("term",as.character)
    }) %>%
  dplyr::select(term, admincity, fullcountyname, zip, zip3rocyear) %>%
  dplyr::rename(admindistrict = fullcountyname)
#all_admin_dist <- distinct(elections_df, term, admincity, admindistrict) %>%
#  filter(!is.na(admincity))
#all_admin_dist_try <- cbind(all_admin_dist, "fullcountyname" = all_admin_dist$admindistrict) %>%
#  mutate_at(c("admindistrict"), .funs = list(admindistrict = ~stri_sub(admindistrict, from = 1, to = -2))) %>% #funs(stri_sub(admindistrict, from = 1, to = -2))
#  mutate_at("fullcountyname",as.character)
#all_admin_dist_with_zip <- left_join(all_admin_dist_try, zipcode_df) %>%
#  select(term, admincity, fullcountyname, zip, zip3rocyear) %>%
#  rename(admindistrict = fullcountyname)

legislators_ethicity_df <- paste0(dataset_file_directory, "legislators_ethicity_originalcollection.txt") %>%
  jsonlite::fromJSON() %>%
  mapply(c, list(
    'aboriginal'='谷辣斯．尤達卡Kolas．Yotaka|高潞．以用．巴魕剌Kawlo．Iyun．Pacidal',
    'foreignstates'='尹伶瑛|段宜康|趙麗雲|吳育昇|丁守中|朱鳳芝|周守訓|邱毅|帥化民|洪秀柱|孫大千|李慶安|李慶華|潘維剛|蔣孝嚴|賴士葆|費鴻泰|盧秀燕|王榮璋|顧立雄|段宜康|王定宇|趙天麟|梁文傑|王鍾渝|李永萍|江綺雯|沈智慧',
    'fulo'='尤清|王拓|王幸男|王金平|王政中|王昱婷|王雪峰|江丙坤|江昭儀|何金松|何敏豪|余政道|吳東昇|吳敦義|呂新民|李文忠|李全教|蔡正元|李嘉進|林豐正|郭素春|邱毅|李明憲|李俊毅|李鴻鈞|杜文卿|沈富雄|邱永仁',
    'hakka'='羅文嘉|林郁方|羅志明|彭添富|邱垂貞|張昌財|邱創良|鄭金玲|張學舜|邱鏡淳|陳進興|呂學樟|何智輝|徐耀昌|林豐喜|邱太三|郭俊銘|鍾紹和|傅崐萁|饒穎奇|李桐豪|鍾榮吉|徐中雄|吳志揚|彭紹瑾|鄭金玲|葉芳雄|管碧玲|張慶惠|劉盛良|廖正井|趙麗雲|邱志偉|呂玉玲|徐欣瑩|邱文彥|陳碧涵|吳宜臻|李應元|陳賴素美|徐志榮|鍾佳濱|鍾孔炤|林為洲|陳明真|馬文君|劉建國|黃昭順|蘇震清',
    #from 圖解客家政治與經濟 馬文君|劉建國|黃昭順|邱議瑩|邱志偉|管碧玲|蘇震清|
    "newresident"="",
    'other'='吳成典'
  ), ., SIMPLIFY=FALSE) %>% 
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
  dplyr::bind_rows()

#資料更正：是否現任
#for (checkterm in terms) {
#  previousterm <- checkterm-1
#  legislators_check_incumbent <- openxlsx::read.xlsx(paste0(dataset_file_directory, "legislators.xlsx"), sheet = 1) %>%
#    filter(term==previousterm) %>%
#    getElement("name")
#  legislators_correct_csv <- read_csv(file = paste0(dataset_file_directory, "cec_vote_dataset", slash, "term", checkterm, slash, "partylist", slash, "elrepm.csv")) %>%
#    mutate_cond(名字 %in% get("legislators_check_incumbent"),現任="Y") %>%
#    mutate_cond(!名字 %in% get("legislators_check_incumbent"),現任="N")
#  if (checkterm==5) {
#    legislators_correct_csv %<>% select(-13)
#  }
#  write_csv(legislators_correct_csv, path=paste0(dataset_file_directory, "cec_vote_dataset", slash, "term", checkterm, slash, "partylist", slash, "elrepm_new.csv"), na = "")
#}

#save(elections_df,file=paste0(dataset_in_scriptsfile_directory, "elections_df.RData"))
#test result: filter(legislators_needed,is.na(zip)) %>% View()

#立委資料與選區資料合併
#legislators <- read_csv(file = paste0(dataset_file_directory, "legislators.csv"))

#load(paste0(dataset_in_scriptsfile_directory, "legislators_with_elections.RData"), verbose=TRUE)
legislators_with_elections <- ret_std_legislators_data(legislatorsxlsxpath = paste0(dataset_file_directory, "legislators.xlsx"), terms=terms, elections_df=elections_df) %>%
  dplyr::select(-ename,-onboardDate,-picUrl,-leaveFlag,-leaveDate,-leaveReason,-ballotid,-committee,-birthday,-birthplace,-plranking)#inner_join目的是要排除沒有當選也沒有遞補進來的立法委員

legislators_additional_attr <- legislators_with_elections %>% #[!is.na(legislators_with_elections$wonelection),]
  #distinct(term, legislator_name, legislator_sex, legislator_party, partyGroup, areaName,
  #         committee, degree, experience, birthday,
  #         legislator_age, birthplace, education, incumbent, wonelection,
  #         election_party, electionarea, plranking, elec_dist_type) %>%
  dplyr::distinct(term,legislator_name,electionarea,degree,experience,education) %>%
  dplyr::mutate(legislator_eduyr=NA,legislator_occp=NA,legislator_ses=NA) %>%
  dplyr::mutate_at("legislator_occp",as.character) %>%
  mutate_cond(is.na(education), education=degree) %>%
  dplyr::left_join(legislators_ethicity_df,by=c("legislator_name")) %>%
  dplyr::mutate_at(c("legislator_ethnicity"), dplyr::recode, `fulo`="[1] 台灣閩南人",`hakka`="[2] 台灣客家人",`aboriginal`="[3] 台灣原住民",`foreignstates`="[4] 大陸各省市(含港澳金馬)",`newresident`="[5] 新移民",`other`="[6] 其他臺灣人") %>%
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
  mutate_cond(customgrepl(legislator_name,"王廷升|張顯耀|費鴻泰|林郁方|孫國華|李全教|陳碧涵|詹滿容"), experience=paste0(experience,"副教授 助理教授"), education="博士") %>%
  dplyr::mutate_at("legislator_eduyr", as.numeric) %>%
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
  mutate_cond(customgrepl(legislator_name,"李鎮楠|李雅景|李明憲|王雪峰|王幸男|江玲君|江連福|吳清池|邱創良|邱鏡淳|邱議瑩|林益世|林淑芬|余政道|呂學樟|翁重鈞|郭玟成|陳明文|陳杰|陳啟昱|陳瑩|馬文君|康世儒|黃昭順|楊瓊瓔|蔡煌瑯|鄭汝芬|鄭金玲|鄭麗文|劉銓忠|潘孟安|潘維剛|盧嘉辰|蕭景田|羅明才|王定宇|何欣純|蘇震清|吳思瑤|吳琪銘|呂孫綾|李俊俋|李彥秀|李應元|周陳秀霞|林俊憲|林為洲|林德福|段宜康|徐榛蔚|陳超明|張宏陸|黃秀芳|許淑華|鄭麗君|蕭美琴|蘇治芬|蘇嘉全|王昱婷|朱星羽|何智輝|李和順|杜文卿|沈智慧|邱垂貞|邱創進|卓榮泰|卓伯源|周雅淑|周慧瑛|林文郎|林育生|柯淑敏|唐碧娥|徐志明|郭俊銘|陳宗義|陳志彬|陳茂男|陳金德|陳健治|陳進丁|陳景峻|陳朝龍|陳麗惠|張秀珍|張蔡美|張學舜|章仁香|許舒博|彭添富|曾華德|廖本煙|蔡啟芳|蔡鈴蘭|鄭余鎮|鄭美蘭|鄭朝明|鄭貴蓮|劉文雄|劉松藩|劉俊雄|劉政鴻|盧博基|賴勁麟|藍美津|謝明源|謝章捷|尹伶瑛|朱俊曉|林耘生|林國慶|陳東榮|陳朝容|陳憲中|曹來旺|葉芳雄|楊宗哲|蔡錦隆|顏文章|林國正|林奕華|張嘉郡|楊應雄"),
              experience=paste0(experience,"職業民意代表")) %>%
  mutate_cond(customgrepl(legislator_name,"余天|高金素梅"), experience=paste0(experience,"藝人")) %>%
  mutate_cond(customgrepl(legislator_name,"林滄敏"), experience=paste0(experience,"商店售貨")) %>%
  mutate_cond(customgrepl(legislator_name,"柯建銘|涂醒哲|沈富雄|林進興|洪奇昌|陳其邁|侯水盛"), experience=paste0(experience,"醫師")) %>%
  mutate_cond(customgrepl(legislator_name,"孫大千"), experience=paste0(experience,"化工研究員")) %>%
  mutate_cond(customgrepl(legislator_name,"吳成典|黃劍輝|童惠珍"), experience=paste0(experience,"總經理")) %>%
  mutate_cond(customgrepl(legislator_name,"簡東明"), experience=paste0(experience,"國小教師")) %>%
  mutate_cond(customgrepl(legislator_name,"徐少萍|林正二|林春德|許榮淑|楊仁福"), experience=paste0(experience,"國中教師")) %>%
  mutate_cond(customgrepl(legislator_name,"劉盛良|謝鈞惠|顏錦福"), experience=paste0(experience,"高中教師")) %>%
  mutate_cond(customgrepl(legislator_name,"陳宗仁"), experience=paste0(experience,"商專教師")) %>%
  mutate_cond(customgrepl(legislator_name,"吳清池"), experience=paste0(experience,"固定攤販與市場售貨")) %>%
  mutate_cond(customgrepl(legislator_name,"何金松"), experience=paste0(experience,"金屬機械技術工")) %>%
  mutate_cond(customgrepl(legislator_name,"林惠官"), experience=paste0(experience,"金屬機械技術工 鐵道工人")) %>%
  mutate_cond(customgrepl(legislator_name,"李昆澤"), experience=paste0(experience,"電器維修工")) %>%
  mutate_cond(customgrepl(legislator_name,"林炳坤|郭素春|張花冠|王金平|許毓仁|林國華|陳建銘|湯火聖|何敏豪"),
              experience=paste0(experience,"總經理 創業主管")) %>%
  mutate_cond(customgrepl(legislator_name,"徐耀昌|張慶忠|薛凌|顏清標|余宛如|呂玉玲|林南生|陳宏昌|梁牧養|許登宮|程振隆|楊文欣|蔡豪|鍾金江|羅世雄|黃良華|葉津鈴|詹凱臣|陳飛龍"),
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
  mutate_cond(customgrepl(legislator_name,"瓦歷斯．貝林"),
              experience=paste0(experience,"宗教專業人員")) %>%
  mutate_cond(customgrepl(legislator_name,"田秋堇|陳節如|黃淑英|王育敏|王榮璋|吳玉琴|李麗芬|林麗蟬|陳曼麗|高潞|鍾孔炤"),
              experience=paste0(experience,"NGO理事長 NGO執行長 NGO秘書長 工會理事長")) %>%
  dplyr::mutate_at("legislator_ses", as.numeric) %>%
  dplyr::mutate_at("legislator_occp", as.character) %>%
  mutate_cond(customgrepl(experience,"漁民|討海人"), legislator_occp="620", legislator_ses=65.9) %>%
  mutate_cond(customgrepl(experience,"固定攤販與市場售貨"), legislator_occp="532", legislator_ses=67.3) %>%
  mutate_cond(customgrepl(experience,"商店售貨"), legislator_occp="531", legislator_ses=71.8) %>%
  mutate_cond(customgrepl(experience,"營建採礦技術工|水泥公司工人"), legislator_occp="710", legislator_ses=72.0) %>%
  mutate_cond(customgrepl(experience,"電器維修工|金屬機械技術工|鐵道工人"), legislator_occp="720", legislator_ses=74.2) %>%
  mutate_cond(customgrepl(experience,"辦公室事務性工作|公所秘書|事務工作公務員"), legislator_occp="410", legislator_ses=76.5) %>%
  mutate_cond(customgrepl(experience,"職業選手"), legislator_occp="322", legislator_ses=77.5) %>%
  mutate_cond(customgrepl(experience,"補習班教師|訓練班教師"), legislator_occp="303", legislator_ses=78.4) %>%
  mutate_cond(customgrepl(experience,"社會工作員|輔導員|社工"), legislator_occp="312", legislator_ses=74.5) %>%
  mutate_cond(customgrepl(experience,"護理師|醫藥專業人員"), legislator_occp="223", legislator_ses=79.1) %>%
  mutate_cond(customgrepl(experience,"記者|主播|採訪中心主任|作家|編輯"), legislator_occp="212", legislator_ses=80.0) %>%
  mutate_cond(customgrepl(experience,"藝人|主唱"), legislator_occp="213", legislator_ses=80.0) %>%
  mutate_cond(customgrepl(experience,"國會助理|省議員助理|政治人物幕僚"), legislator_occp="311", legislator_ses=80.1) %>%
  mutate_cond(customgrepl(experience,"高中教師|中學教師|中學教員|國中教師|國小教師|國中小教師|商工教師|商專教師|補校教師"), legislator_occp="202", legislator_ses=81.1) %>%
  mutate_cond(customgrepl(experience,"股長|襄理|課長|科長|副理|環保署資深科學主管") | customgrepl(legislator_name,"吳光訓"), legislator_occp="370", legislator_ses=81.9) %>%
  mutate_cond(customgrepl(experience,"專案經理"), legislator_occp="120", legislator_ses=81.4) %>%
  mutate_cond(customgrepl(experience,"牧師|宗教專業人員"), legislator_occp="214", legislator_ses=80.0) %>%
  mutate_cond(customgrepl(experience,"商學專業人員"), legislator_occp="230", legislator_ses=85.1) %>%
  mutate_cond(customgrepl(experience,"測量技士|土木技師|化工研究員|工程師|建築師|水利技師"), legislator_occp="250", legislator_ses=83.2) %>%
  mutate_cond(customgrepl(experience,"(基金會){0}(集團){0,1}(托兒所){0,1}董事長|總經理|監察人|(托兒所){0,1}負責人"), legislator_occp="110", legislator_ses=83.3) %>%
  mutate_cond(customgrepl(experience,"會計師"), legislator_occp="230", legislator_ses=85.1) %>%
  mutate_cond(customgrepl(experience,"法官|律師|地政士|土地登記代理人") | customgrepl(legislator_name,"吳志揚") & !customgrepl(legislator_name,"鄭天財Sra．Kacaw"), legislator_occp="211", legislator_ses=86.0) %>%
  mutate_cond(customgrepl(experience,"(兼任){0}副?教授|學系主任|系主任|學術科研機構研究員|大專講師"), legislator_occp="201", legislator_ses=87.9) %>%
  mutate_cond(customgrepl(experience,"醫師|產科主任"), legislator_occp="221", legislator_ses=86.0) %>%
  mutate_cond(customgrepl(experience,"旅長|軍總司令|國防管理學院院長"), legislator_occp="012", legislator_ses=81.4) %>%
  mutate_cond(customgrepl(experience,"NGO理事長|NGO執行長|NGO秘書長|產業總工會理事長|主管級公務員|職業民意代表") | customgrepl(legislator_name,"劉建國"), legislator_occp="140", legislator_ses=81.4) %>%
  mutate_cond(!is.na(legislator_ses), legislator_ses=(legislator_ses-55)*3) %>%
  dplyr::select(term,legislator_name,legislator_eduyr,legislator_ses,legislator_ethnicity) %>%
  dplyr::mutate_at(c("legislator_name","legislator_ethnicity"),as.factor)

legislators_with_elections %<>% dplyr::select(-education,-degree,-experience,-wonelection,-servingdayslong_in_this_term) %>%
  dplyr::mutate_at(c("legislator_name","electionarea","admincity","admindistrict","adminvillage"), as.factor) %>%
  dplyr::select(-areaName,-election_party,-electionarea)
#legislators_with_elections %<>% data.table::as.data.table()
#save(legislators_with_elections, file=paste0(dataset_in_scriptsfile_directory, "legislators_with_elections.RData"))

#legislators_additional_attr %<>% data.table::as.data.table()
#save(legislators_additional_attr,file=paste0(dataset_in_scriptsfile_directory, "legislators_additional_attr.RData"))

#陳東榮 no degree
#孫國華 僑選
#write.xlsx(legislators_additional_attr,file=paste0(dataset_file_directory,"legislator_additional_attributes.xlsx"))
#filter(legislators_additional_attr,is.na(legislator_ses)|is.na(legislator_eduyr)) %>%
#  select(name,experience,term,legislator_occp,legislator_ses,legislator_eduyr,education,legislator_ethnicity,electionarea) %>%
#  View()