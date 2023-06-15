lyterm56votes_class <- R6::R6Class("lyterm56votes", public = list(
  filespath = NULL,
  dataset_in_scriptsfile_directory = NULL,
  myown_vote_record_detailed_part_df_filepath = NULL,
  initialize = function(dataset_in_scriptsfile_directory="/mnt",filespath="/mnt") {
    self$dataset_in_scriptsfile_directory <- dataset_in_scriptsfile_directory
    self$filespath <- filespath
    self$myown_vote_record_detailed_part_df_filepath <- file.path(dataset_in_scriptsfile_directory,"myown_vote_record_detailed_part_df.rds")
  },
  get_term56_voting_records = function(loadExisted=TRUE,save=FALSE) {
    if (loadExisted==TRUE) {
      myown_vote_record_detailed_part_df <- readRDS(self$myown_vote_record_detailed_part_df_filepath)
    } else {
      myown_vote_record_detailed_part_df <- self$parse_term56_votingrecords(save)
      return (myown_vote_record_detailed_part_df)
    }
  },
  parse_term56_votingrecords = function(save=FALSE) {
    filespath <- self$filespath
    ly_meeting_path <- file.path(filespath, "2004_meeting", "original")
    myown_vote_record_detailed_part_df_filepath <- self$myown_vote_record_detailed_part_df_filepath
    meetingrecords_filenames<-c(#"立法院第5屆第5會期全院委員談話會紀錄.html",
      "立法院第5屆第5會期第1次臨時會第1次會議紀錄.html", #OK 九十三年八月十一日 立法院第93卷第36期 (3370)公報 https://lci.ly.gov.tw/LyLCEW/communique/final/pdf/93/36/LCIDC01_933601.pdf
      "立法院第5屆第5會期第1次臨時會第3次會議紀錄.html", #OK 九十三年八月二十三日 公報 93卷37期公報總號 3371上 https://lci.ly.gov.tw/LyLCEW/communique/final/pdf/93/37/LCIDC01_933701.pdf
      "立法院第5屆第5會期第1次臨時會第4次會議紀錄.html", #OK 九十三年八月二十四日 公報 93卷37期公報總號 3371上 https://lci.ly.gov.tw/LyLCEW/communique/final/pdf/93/37/LCIDC01_933701.pdf
      "立法院第5屆第6會期第1次會議紀錄.html", #第93卷 第38期(3372) 公報 九十三年九月十四日 https://lci.ly.gov.tw/LyLCEW/communique/final/pdf/93/38/LCIDC01_933801.pdf
      "立法院第5屆第6會期第16次會議紀錄.html", #94卷07期 3389號上 九十四年一月二十日（星期四）上午十時、一月二十一日（星期五）上午二時十四分 https://lci.ly.gov.tw/LyLCEW/communique/final/pdf/94/07/LCIDC01_940701.pdf
      "立法院第6屆第1會期第13次會議紀錄.html", #6
      "立法院第6屆第1會期第14次會議紀錄.html", #7
      "立法院第6屆第2會期第3次會議紀錄.html", #8
      "立法院第6屆第2會期第5次會議紀錄.html", #9
      "立法院第6屆第2會期第7次會議紀錄.html", #10
      "立法院第6屆第2會期第9次會議紀錄.html", #11
      "立法院第6屆第2會期第16次會議紀錄.html", #12
      "立法院第6屆第2會期第19次會議紀錄.html", #13
      "立法院第6屆第3會期第8次會議紀錄.html", #14
      "立法院第6屆第3會期第10次會議紀錄.html") #15
    term<-stringr::str_match(meetingrecords_filenames,pattern="立法院第([[:digit:]]+)屆")[,2] %>% as.integer()
    period<-stringr::str_match(meetingrecords_filenames,pattern="立法院第[[:digit:]]+屆第([[:digit:]]+)會期")[,2] %>% as.integer()
    meetingno<-stringr::str_match(meetingrecords_filenames,pattern="第([[:digit:]]+)次會議紀錄")[,2] %>% as.integer()
    temp_meeting_no<-stringr::str_match(meetingrecords_filenames,pattern="會期第([[:digit:]]+)次臨時會")[,2] %>%
      replace(.,is.na(.),0) %>% as.integer()
    meetingdate<-c("093/08/11","093/08/23","093/08/24","093/09/14","094/01/20,094/01/21","094/05/20,094/05/24","094/05/24,094/05/31","094/09/23,094/09/27","094/10/07,094/10/11","094/10/21","094/11/04,094/11/08","094/12/23","095/01/11","095/04/07","095/04/21,095/04/25")
    meeting_ad_date <- lapply(meetingdate, function(chi_date) {
      firstmeetingdate<-stri_split(chi_date,regex=",") %>%
        unlist() %>%
        getElement(1)
      dateparts<-stri_split(chi_date,regex="/") %>%
        unlist()
      dateparts[1]<-as.integer(dateparts[1])+1911
      return(as.Date(paste(dateparts,collapse="/"),format="%Y/%m/%d"))
    })
    term56_filepaths <- file.path(ly_meeting_path,meetingrecords_filenames)
    html<-sapply(term56_filepaths,custom_read_file)
    
    all_legislators <- openxlsx::read.xlsx(file.path(dataset_file_directory, "legislators.xlsx"), sheet = 1, detectDates = TRUE) %>%
      data.table::as.data.table() %>%
      .[, term := as.integer(term)] %>%
      .[, c("onboardDate", "leaveDate") := lapply(.SD, as.Date),
        .SDcols = c("onboardDate", "leaveDate")] %>%
      mutate_cond(term==5 & is.na(leaveDate), leaveDate=as.Date("2005/01/31",format="%Y/%m/%d") ) %>%
      mutate_cond(term==6 & is.na(leaveDate), leaveDate=as.Date("2008/01/31",format="%Y/%m/%d")) %>%
      mutate_cond(term==5 & customgrepl(name,"王雪峯"), name="王雪峰") %>%
      .[term %in% c(5,6),] %>%
      unique(., by = c("term","name","onboardDate","party","leaveDate"))
    #termfivelegislators <- all_legislators %>%  #mutate_at(c("term"), .funs = list(term = ~customgsub(term, "0(\\d{1})", "\\1", perl = TRUE))) %>% 
    #  filter(term==5) %>%
    #filter(!customgrepl(name,"(邱彰|劉世芳|謝鈞惠|周伯倫|陳唐山|卓榮泰|邱太三|陳其邁|賴勁麟|羅文嘉|張旭成|陳忠信)")) %>% #問卷調查前就離職了
    #termsixlegislators <- all_legislators %>%  #mutate_at(c("term"), .funs = list(term = ~customgsub(term, "0(\\d{1})", "\\1", perl = TRUE))) %>% 
    #  filter(term==6)
    #attend_and_leave_legislators_raw <- data.frame(
    #  "term"=c(6,6,6,6,6,6,6,6,6,6), "period"=c(1,1,2,2,2,2,2,2,3,3), "meetingno"=c(13,14,3,5,7,9,16,19,8,10), "temp_meeting_no"=c(0,0,0,0,0,0,0,0,0,0),
    #)
    #要改 傅崐萁 高金
    attend_legislators<-c("","","","","",
                          "吳清池　　劉盛良　　鄭國忠　　鄭朝明　　孔文吉　　林滄敏　　洪奇昌　　陳銀河　　王塗發　　張慶惠　　謝欣霓　　徐國勇　　賴清德　　羅志明　　何敏豪　　尹伶瑛　　陳重信　　賴士葆　　江昭儀　　賴幸媛　　陳進丁　　黃適卓　　郭林勇　　黃政哲　　費鴻泰　　白添枝　　杜文卿　　徐中雄　　吳育昇　　邱永仁　　廖國棟　　侯彩鳳　　周錫瑋　　邱　毅　　徐少萍　　鍾榮吉　　郭素春　　李明憲　　蔡勝佳　　張碩文　　楊瓊瓔　　黃志雄　　帥化民　　李昆澤　　謝文政　　吳英毅　　林國慶　　李　敖　　莊和子　　吳松柏　　林鴻池　　江丙坤　　莊碩漢　　顏文章　　潘孟安　　林春德　　雷　倩　　柯俊雄　　林樹山　　林德福　　江連福　　彭紹瑾　　李紀珠　　陳　杰　　林炳坤　　林耘生　　李全教　　孫大千　　蔡錦隆　　陳憲中　　林惠官　　林建榮　　丁守中　　曹壽民　　楊麗環　　盧天麟　　王世勛　　黃義交　　陳秀卿　　李復興　　梅長錡　　趙良燕　　林郁方　　傅崐萁　　王金平　　徐耀昌　　蔡正元　　沈智慧　　李嘉進　　翁重鈞　　張昌財　　黃偉哲　　李和順　　何智輝　　林益世　　呂學樟　　劉憶如　　吳敦義　　高金素梅　　蔡　豪　　黃德福　　羅世雄　　曾永權　　潘維剛　　邱鏡淳　　羅明才　　李慶安　　章孝嚴　　吳志揚　　廖本煙　　林岱樺　　張花冠　　曾燦燈　　劉銓忠　　趙永清　　陳朝容　　劉寬平　　管碧玲　　郭玟成　　楊仁福　　伍錦霖　　張麗善　　李鎮楠　　黃劍輝　　林濁水　　郭正亮　　曹爾忠　　廖婉汝　　尤　清　　林為洲　　吳光訓　　吳秉叡　　蔡其昌　　謝明源　　湯火聖　　王榮璋　　蔡煌瑯　　余政道　　蔡英文　　蕭美琴　　藍美津　　田秋堇　　王淑慧　　黃昭輝　　高志鵬　　黃淑英　　蔡啟芳　　林進興　　黃敏惠　　林重謨　　陳啟昱　　吳富貴　　薛　凌　　柯建銘　　陳　瑩　　李俊毅　　沈發惠　　葉宜津　　盧博基　　林正峰　　蔡同榮　　林淑芬　　邱創進　　王世堅　　彭添富　　陳秀惠　　蘇　起　　鄭運鵬　　朱鳳芝　　葉芳雄　　陳明真　　周守訓　　曹來旺　　林正二　　魏明谷　　張川田　　王　拓　　陳朝龍　　郭俊銘　　李永萍　　侯水盛　　林育生　　黃健庭　　陳景峻　　陳金德　　高建智　　謝國樑　　唐碧娥　　郭榮宗　　徐志明　　李文忠　　洪玉欽　　顏清標　　朱俊曉　　卓伯源　　高思博　　李鴻鈞　　黃宗源　　李顯榮　　陳志彬　　鄭金玲　　鍾紹和　　黃昭順　　章仁香　　張顯耀　　張慶忠　　吳成典　　蔡家福　　柯淑敏　　楊宗哲　　王昱婷　　洪秀柱　　顧崇廉　　劉文雄　　盧秀燕　　陳根德　　李慶華　　紀國棟",
                          "盧天麟　　李鎮楠　　王榮璋　　曾燦燈　　謝文政　　吳英毅　　吳松柏　　陳銀河　　陳志彬　　陳進丁　　侯彩鳳　　何敏豪　　李明憲　　羅志明　　尹伶瑛　　曹爾忠　　邱創進　　白添枝　　周錫瑋　　黃政哲　　劉寬平　　林重謨　　吳秉叡　　曹壽民　　郭素春　　郭林勇　　林進興　　徐國勇　　林濁水　　吳成典　　李紀珠　　章仁香　　陳重信　　賴清德　　徐少萍　　吳清池　　尤　清　　江昭儀　　黃志雄　　藍美津　　邱　毅　　邱永仁　　顏文章　　王塗發　　劉盛良　　陳　杰　　黃德福　　黃昭順　　郭正亮　　黃適卓　　蔡勝佳　　王幸男　　鄭朝明　　王金平　　鍾榮吉　　周守訓　　梅長錡　　江丙坤　　趙良燕　　葉芳雄　　黃義交　　柯俊雄　　林正峰　　雷　倩　　卓伯源　　馮定國　　顏清標　　林炳坤　　張昌財　　李　敖　　朱俊曉　　徐耀昌　　楊瓊瓔　　傅崐萁　　吳敦義　　李復興　　丁守中　　魏明谷　　蔡錦隆　　柯淑敏　　陳朝容　　紀國棟　　江連福　　吳志揚　　林郁方　　蔡正元　　林惠官　　顧崇廉　　陳根德　　翁重鈞　　陳秀卿　　孫大千　　楊麗環　　林鴻池　　張麗善　　李永萍　　劉憶如　　廖婉汝　　劉文雄　　張慶忠　　張碩文　　李慶安　　張顯耀　　李顯榮　　楊宗哲　　高金素梅　　何智輝　　蔡家福　　呂學樟　　李慶華　　曾永權　　曾華德　　林春德　　廖國棟　　孔文吉　　帥化民　　林岱樺　　黃宗源　　莊和子　　田秋堇　　張俊雄　　蔡其昌　　趙永清　　張慶惠　　王淑慧　　葉宜津　　賴幸媛　　廖本煙　　黃淑英　　楊仁福　　蔣孝嚴　　蔡啟芳　　張花冠　　彭添富　　郭玟成　　潘維剛　　高志鵬　　鄭金玲　　郭俊銘　　王世勛　　謝國樑　　李鴻鈞　　徐志明　　黃劍輝　　沈發惠　　王　拓　　吳光訓　　彭紹瑾　　費鴻泰　　蔡同榮　　蔡英文　　吳富貴　　劉銓忠　　林淑芬　　薛　凌　　高建智　　謝欣霓　　管碧玲　　林國慶　　莊碩漢　　陳　瑩　　洪玉欽　　盧博基　　林耘生　　侯水盛　　林建榮　　張川田　　陳景峻　　王世堅　　李昆澤　　鄭國忠　　李俊毅　　鄭運鵬　　謝明源　　潘孟安　　唐碧娥　　黃敏惠　　陳憲中　　吳育昇　　林為洲　　黃昭輝　　陳明真　　陳啟昱　　林樹山　　曹來旺　　陳秀惠　　余政道　　柯建銘　　黃偉哲　　蔡煌瑯　　郭榮宗　　洪奇昌　　蕭美琴　　陳金德　　伍錦霖　　湯火聖　　羅明才　　劉政鴻　　黃健庭　　盧秀燕　　王昱婷　　林益世　　李文忠　　林育生　　許舒博　　杜文卿　　高思博　　朱鳳芝　　林德福　　洪秀柱　　林南生　　李全教　　賴士葆　　蘇　起　　李嘉進　　徐中雄　　鍾紹和　　邱鏡淳　　蔡　豪　　林滄敏　　羅世雄　　沈智慧　　林正二",
                          "徐國勇　　侯彩鳳　　羅志明　　曾燦燈　　何敏豪　　李明憲　　侯水盛　　陳進丁　　吳英毅　　謝文政　　劉寬平　　吳成典　　盧天麟　　潘維剛　　孔文吉　　李鎮楠　　蘇　起　　王榮璋　　曹壽民　　江昭儀　　李俊毅　　廖國棟　　郭林勇　　黃政哲　　王塗發　　林重謨　　黃志雄　　黃昭順　　郭素春　　李顯榮　　王淑慧　　邱永仁　　曾永權　　黃德福　　帥化民　　許榮淑　　尤　清　　邱　毅　　田秋堇　　劉盛良　　李紀珠　　王世勛　　趙永清　　徐少萍　　賴清德　　陳銀河　　郭玟成　　陳根德　　李昆澤　　王　拓　　蔡啟芳　　蔡正元　　黃義交　　高志鵬　　章仁香　　朱鳳芝　　顏清標　　王幸男　　黃劍輝　　卓伯源　　林建榮　　李慶安　　張花冠　　林淑芬　　曹來旺　　徐中雄　　謝國樑　　黃昭輝　　吳松柏　　李嘉進　　林耘生　　洪玉欽　　沈智慧　　楊仁福　　林惠官　　柯淑敏　　馮定國　　丁守中　　洪秀柱　　周錫瑋　　黃適卓　　許舒博　　黃健庭　　鄭國忠　　周守訓　　陳　杰　　林鴻池　　劉銓忠　　盧秀燕　　林育生　　唐碧娥　　陳憲中　　謝明源　　蔡　豪　　賴士葆　　林岱樺　　黃宗源　　吳志揚　　林濁水　　郭正亮　　伍錦霖　　林為洲　　林德福　　湯火聖　　邱創進　　郭俊銘　　張俊雄　　藍美津　　白添枝　　柯建銘　　陳明真　　陳秀惠　　林進興　　尹伶瑛　　管碧玲　　顏文章　　吳秉叡　　莊碩漢　　陳景峻　　蔡同榮　　莊和子　　沈發惠　　蔡錦隆　　葉宜津　　張慶惠　　鄭運鵬　　陳啟昱　　徐志明　　蔡英文　　魏明谷　　薛　凌　　黃淑英　　費鴻泰　　蕭美琴　　陳　瑩　　黃偉哲　　林益世　　吳富貴　　張川田　　林國慶　　余政道　　高建智　　王世堅　　吳育昇　　郭榮宗　　彭添富　　洪奇昌　　陳金德　　林正峰　　雷　倩　　呂學樟　　楊麗環　　李永萍　　林滄敏　　黃敏惠　　李全教　　鍾榮吉　　林正二　　張顯耀　　張麗善　　趙良燕　　劉文雄　　賴幸媛　　孫大千　　柯俊雄　　高思博　　陳志彬　　鄭朝明　　蔡勝佳　　彭紹瑾　　羅明才　　王金平　　張碩文　　王昱婷　　曹爾忠　　葉芳雄　　梅長錡　　蔣孝嚴　　翁重鈞　　顧崇廉　　劉政鴻　　邱鏡淳　　何智輝　　蔡家福　　李鴻鈞　　李復興　　謝欣霓　　羅世雄　　潘孟安　　盧博基　　杜文卿　　朱俊曉　　徐耀昌　　蔡其昌　　楊瓊瓔　　鄭金玲　　林郁方　　廖婉汝　　陳朝龍　　林炳坤　　林春德　　張昌財　　江連福　　傅崐萁　　張慶忠　　鍾紹和　　陳朝容　　江丙坤　　吳敦義　　劉憶如　　吳光訓　　紀國棟　　林南生　　李和順　　陳秀卿　　吳清池　　李文忠　　李慶華",
                          "張花冠　　謝明源　　陳銀河　　王榮璋　　謝文政　　吳英毅　　吳松柏　　蘇　起　　楊瓊瓔　　張慶惠　　李明憲　　李俊毅　　曾燦燈　　侯水盛　　王世勛　　曹壽民　　徐國勇　　林國慶　　黃政哲　　潘維剛　　黃宗源　　莊和子　　吳清池　　許榮淑　　郭素春　　吳成典　　林鴻池　　陳明真　　林濁水　　孔文吉　　徐少萍　　林重謨　　侯彩鳳　　王塗發　　江昭儀　　黃淑英　　黃志雄　　賴清德　　吳育昇　　黃德福　　羅志明　　邱永仁　　林德福　　趙永清　　田秋堇　　費鴻泰　　邱　毅　　黃適卓　　章仁香　　何敏豪　　陳重信　　楊仁福　　劉盛良　　鄭朝明　　蔣孝嚴　　帥化民　　廖本煙　　黃偉哲　　王金平　　賴幸媛　　雷　倩　　黃義交　　柯俊雄　　周守訓　　李鴻鈞　　劉憶如　　林春德　　林正峰　　馮定國　　羅世雄　　蔡啟芳　　林岱樺　　郭林勇　　曹來旺　　林育生　　林進興　　柯建銘　　邱創進　　彭添富　　陳　瑩　　林耘生　　邱鏡淳　　林益世　　白添枝　　曾永權　　蔡錦隆　　洪玉欽　　沈智慧　　周錫瑋　　吳富貴　　江連福　　黃健庭　　陳朝容　　廖國棟　　黃昭順　　吳敦義　　林樹山　　李文忠　　劉銓忠　　盧博基　　管碧玲　　謝國樑　　王幸男　　吳志揚　　鄭運鵬　　郭玟成　　李紀珠　　顧崇廉　　陳秀惠　　伍錦霖　　鄭國忠　　薛　凌　　張慶忠　　余政道　　潘孟安　　杜文卿　　賴士葆　　彭紹瑾　　丁守中　　王淑慧　　高建智　　張俊雄　　葉宜津　　吳秉叡　　李鎮楠　　顏文章　　黃昭輝　　陳朝龍　　莊碩漢　　湯火聖　　陳金德　　林為洲　　沈發惠　　唐碧娥　　陳憲中　　蔡同榮　　郭正亮　　曹爾忠　　蔡英文　　高志鵬　　王　拓　　黃劍輝　　陳啟昱　　藍美津　　蕭美琴　　林淑芬　　尤　清　　張川田　　蔡其昌　　郭俊銘　　鄭金玲　　王世堅　　羅明才　　李昆澤　　陳景峻　　洪奇昌　　郭榮宗　　李顯榮　　徐耀昌　　楊宗哲　　林郁方　　黃敏惠　　吳光訓　　陳秀卿　　李復興　　鍾榮吉　　王昱婷　　張昌財　　蔡家福　　盧天麟　　高思博　　林正二　　呂學樟　　江丙坤　　傅崐萁　　張碩文　　葉芳雄　　紀國棟　　洪秀柱　　翁重鈞　　蔡勝佳　　陳志彬　　李慶安　　柯淑敏　　趙良燕　　李永萍　　鍾紹和　　陳根德　　李慶華　　梅長錡　　何智輝　　李　敖　　孫大千　　朱俊曉　　卓伯源　　楊麗環　　張麗善　　朱鳳芝　　張顯耀　　蔡正元　　林建榮　　李全教　　曾華德　　顏清標　　林炳坤　　劉文雄　　高金素梅　　劉政鴻　　尹伶瑛　　林滄敏　　林惠官　　李嘉進　　廖婉汝　　魏明谷　　林南生　　徐中雄　　盧秀燕　　許舒博　　蔡　豪　　陳　杰",
                          "彭紹瑾　　李鎮楠　　王榮璋　　吳英毅　　吳松柏　　何敏豪　　曾燦燈　　羅志明　　尹伶瑛　　邱創進　　王世勛　　曹爾忠　　徐國勇　　侯彩鳳　　帥化民　　黃宗源　　吳成典　　黃政哲　　黃適卓　　王塗發　　莊和子　　林德福　　江昭儀　　曹壽民　　黃昭順　　李俊毅　　蔡啟芳　　孔文吉　　陳銀河　　章仁香　　潘維剛　　曾永權　　白添枝　　吳志揚　　邱　毅　　黃淑英　　陳明真　　楊仁福　　林岱樺　　許榮淑　　郭素春　　黃志雄　　廖本煙　　鄭朝明　　謝明源　　賴清德　　費鴻泰　　趙永清　　田秋堇　　劉盛良　　郭林勇　　郭正亮　　邱永仁　　王金平　　紀國棟　　朱鳳芝　　顧崇廉　　李和順　　林鴻池　　李鴻鈞　　李慶華　　黃義交　　鍾榮吉　　楊宗哲　　蔡正元　　李顯榮　　周守訓　　張花冠　　高思博　　楊麗環　　邱鏡淳　　張碩文　　丁守中　　廖國棟　　張慶忠　　蔡勝佳　　張昌財　　蔡家福　　雷　倩　　葉芳雄　　蔡錦隆　　吳敦義　　馮定國　　趙良燕　　林正峰　　李全教　　翁重鈞　　羅明才　　李永萍　　張顯耀　　孫大千　　林春德　　呂學樟　　林郁方　　顏清標　　卓伯源　　廖婉汝　　楊瓊瓔　　唐碧娥　　陳啟昱　　李復興　　徐耀昌　　江丙坤　　柯淑敏　　張麗善　　何智輝　　賴幸媛　　陳朝容　　柯俊雄　　林正二　　劉文雄　　劉憶如　　吳富貴　　蔣孝嚴　　彭添富　　沈發惠　　陳重信　　葉宜津　　李昆澤　　魏明谷　　林濁水　　賴士葆　　鄭運鵬　　吳秉叡　　李明憲　　陳秀惠　　蔡其昌　　盧天麟　　羅世雄　　林進興　　林育生　　張俊雄　　莊碩漢　　王淑慧　　王幸男　　林重謨　　張慶惠　　陳朝龍　　林為洲　　蕭美琴　　陳景峻　　蔡英文　　侯水盛　　李文忠　　管碧玲　　蔡　豪　　伍錦霖　　潘孟安　　黃昭輝　　郭榮宗　　洪奇昌　　高志鵬　　張川田　　陳憲中　　高建智　　黃劍輝　　黃健庭　　陳　瑩　　柯建銘　　李紀珠　　黃偉哲　　曹來旺　　余政道　　林淑芬　　林滄敏　　蘇　起　　吳育昇　　林南生　　王　拓　　劉政鴻　　鄭金玲　　鍾紹和　　尤　清　　李嘉進　　謝文政　　陳志彬　　林樹山　　郭玟成　　梅長錡　　林建榮　　周錫瑋　　江連福　　洪秀柱　　王世堅　　徐少萍　　王昱婷　　鄭國忠　　洪玉欽　　郭俊銘　　謝國樑　　薛　凌　　黃德福　　盧博基　　顏文章　　蔡同榮　　李慶安　　陳秀卿　　許舒博　　朱俊曉　　林惠官　　陳金德　　曾華德　　吳清池　　林國慶　　湯火聖　　沈智慧　　杜文卿　　林益世　　盧秀燕　　藍美津　　徐中雄　　林耘生　　傅崐萁　　吳光訓　　陳　杰　　林炳坤　　劉銓忠　　黃敏惠",
                          "李俊毅　　陳進丁　　曹爾忠　　王榮璋　　謝文政　　王世勛　　吳清池　　陳銀河　　劉寬平　　莊和子　　黃政哲　　潘維剛　　羅志明　　徐國勇　　楊仁福　　林岱樺　　曾燦燈　　李鎮楠　　盧天麟　　邱永仁　　侯水盛　　林重謨　　蕭美琴　　鄭朝明　　尤　清　　黃志雄　　林濁水　　李紀珠　　邱　毅　　劉盛良　　鄭運鵬　　何敏豪　　郭素春　　趙永清　　吳志揚　　陳重信　　黃德福　　廖本煙　　許榮淑　　郭林勇　　黃宗源　　吳富貴　　林德福　　彭添富　　伍錦霖　　陳明真　　莊碩漢　　蔡啟芳　　陳秀惠　　帥化民　　張俊雄　　彭紹瑾　　蔣孝嚴　　李永萍　　林建榮　　王幸男　　賴幸媛　　李鴻鈞　　陳根德　　李嘉進　　鍾紹和　　林淑芬　　羅明才　　洪奇昌　　蘇　起　　盧博基　　紀國棟　　盧秀燕　　邱鏡淳　　沈發惠　　郭榮宗　　黃敏惠　　管碧玲　　吳育昇　　費鴻泰　　高金素梅　　洪玉欽　　顏文章　　葉宜津　　郭玟成　　曹壽民　　吳成典　　邱創進　　丁守中　　鄭國忠　　柯建銘　　李昆澤　　侯彩鳳　　李明憲　　謝明源　　江連福　　郭俊銘　　謝欣霓　　湯火聖　　徐少萍　　陳　瑩　　王　拓　　陳憲中　　林炳坤　　蔡勝佳　　林郁方　　王昱婷　　張麗善　　朱俊曉　　孫大千　　顏清標　　李復興　　黃昭順　　柯淑敏　　王金平　　江丙坤　　孔文吉　　黃淑英　　陳　杰　　雷　倩　　唐碧娥　　余政道　　薛　凌　　魏明谷　　蔡英文　　潘孟安　　蔡其昌　　郭正亮　　林國慶　　賴清德　　吳秉叡　　王淑慧　　賴士葆　　王塗發　　張慶惠　　藍美津　　黃昭輝　　林為洲　　陳景峻　　高建智　　江昭儀　　田秋堇　　蔡同榮　　高志鵬　　周錫瑋　　黃劍輝　　林育生　　曹來旺　　陳金德　　黃適卓　　林進興　　鄭金玲　　李顯榮　　黃偉哲　　林南生　　李文忠　　朱鳳芝　　張昌財　　林正峰　　蔡正元　　鍾榮吉　　李全教　　李慶安　　張顯耀　　黃義交　　翁重鈞　　蔡家福　　陳志彬　　柯俊雄　　張碩文　　楊麗環　　馮定國　　張慶忠　　周守訓　　曾永權　　卓伯源　　李　敖　　趙良燕　　葉芳雄　　林春德　　吳英毅　　張川田　　呂學樟　　林正二　　何智輝　　曾華德　　洪秀柱　　王世堅　　陳啟昱　　尹伶瑛　　楊瓊瓔　　吳松柏　　謝國樑　　吳光訓　　劉政鴻　　杜文卿　　張花冠　　林滄敏　　黃健庭　　徐中雄　　羅世雄　　陳朝龍　　林益世　　章仁香　　白添枝　　林鴻池　　林耘生　　林樹山　　蔡　豪　　陳秀卿　　吳敦義　　陳朝容　　廖婉汝　　劉文雄　　高思博　　劉憶如　　林惠官　　李慶華　　蔡錦隆　　李和順　　楊宗哲　　徐耀昌",
                          "劉寬平　　曾燦燈　　侯水盛　　何敏豪　　羅志明　　曹爾忠　　帥化民　　蔡啟芳　　尤　清　　張俊雄　　白添枝　　潘維剛　　徐少萍　　孔文吉　　盧天麟　　林岱樺　　陳重信　　林重謨　　蕭美琴　　邱永仁　　侯彩鳳　　李俊毅　　邱　毅　　蘇　起　　楊瓊瓔　　賴清德　　黃志雄　　鄭朝明　　郭素春　　黃淑英　　黃政哲　　丁守中　　徐國勇　　伍錦霖　　吳松柏　　劉盛良　　吳英毅　　謝文政　　林正峰　　王塗發　　李鎮楠　　莊和子　　王榮璋　　吳成典　　黃昭順　　王世堅　　張慶惠　　陳景峻　　林建榮　　王世勛　　洪奇昌　　高建智　　王淑慧　　高志鵬　　林德福　　費鴻泰　　楊仁福　　陳秀惠　　田秋堇　　尹伶瑛　　李嘉進　　郭林勇　　馮定國　　林鴻池　　林濁水　　張花冠　　蔡家福　　鍾榮吉　　柯俊雄　　葉宜津　　李全教　　周守訓　　楊宗哲　　顏清標　　高金素梅　　林郁方　　張昌財　　吳育昇　　李　敖　　李永萍　　高思博　　李復興　　葉芳雄　　蔡勝佳　　徐耀昌　　張顯耀　　楊麗環　　梅長錡　　呂學樟　　陳朝容　　張碩文　　羅明才　　蔡正元　　李和順　　劉憶如　　柯淑敏　　黃義交　　許舒博　　翁重鈞　　林益世　　顧崇廉　　吳清池　　吳敦義　　陳　杰　　劉文雄　　張麗善　　陳根德　　李慶華　　李鴻鈞　　蔣孝嚴　　孫大千　　蔡錦隆　　傅崐萁　　王金平　　王幸男　　彭紹瑾　　雷　倩　　曹壽民　　朱俊曉　　黃宗源　　蔡同榮　　趙永清　　郭玟成　　陳銀河　　彭添富　　魏明谷　　邱創進　　紀國棟　　莊碩漢　　盧博基　　朱鳳芝　　江丙坤　　洪玉欽　　江昭儀　　林滄敏　　廖本煙　　黃適卓　　柯建銘　　林炳坤　　許榮淑　　張慶忠　　江連福　　林南生　　杜文卿　　李昆澤　　林惠官　　李明憲　　唐碧娥　　吳志揚　　謝明源　　吳富貴　　章仁香　　黃健庭　　何智輝　　賴士葆　　林進興　　林育生　　林樹山　　李顯榮　　沈發惠　　陳朝龍　　蔡英文　　蔡　豪　　余政道　　鄭國忠　　顏文章　　謝欣霓　　林耘生　　林國慶　　藍美津　　洪秀柱　　吳秉叡　　陳憲中　　林淑芬　　黃偉哲　　黃劍輝　　陳　瑩　　黃昭輝　　潘孟安　　邱鏡淳　　鄭運鵬　　郭榮宗　　曹來旺　　管碧玲　　鄭金玲　　郭俊銘　　徐中雄　　廖國棟　　張川田　　蔡其昌　　李紀珠　　郭正亮　　趙良燕　　李慶安　　陳金德　　謝國樑　　廖婉汝　　王昱婷　　盧秀燕　　劉銓忠　　黃德福　　林為洲　　鍾紹和　　王　拓　　羅世雄　　陳秀卿　　賴幸媛　　湯火聖　　曾永權　　李文忠　　吳光訓　　陳啟昱　　陳明真　　薛　凌",
                          "彭紹瑾　　吳英毅　　林建榮　　陳進丁　　郭俊銘　　張麗善　　盧天麟　　李鎮楠　　曾燦燈　　蘇　起　　楊瓊瓔　　徐少萍　　徐國勇　　吳清池　　曹壽民　　潘維剛　　孔文吉　　高建智　　王塗發　　吳成典　　郭素春　　許榮淑　　鄭朝明　　侯彩鳳　　吳富貴　　劉盛良　　邱永仁　　謝文政　　沈發惠　　黃偉哲　　楊仁福　　李紀珠　　王世勛　　李明憲　　劉寬平　　伍錦霖　　鄭運鵬　　李全教　　陳銀河　　潘孟安　　林重謨　　莊和子　　王榮璋　　張慶忠　　陳秀惠　　王淑慧　　黃健庭　　黃淑英　　何敏豪　　郭榮宗　　陳朝龍　　李俊毅　　帥化民　　蔡啟芳　　盧博基　　林為洲　　林進興　　江連福　　白添枝　　丁守中　　鄭國忠　　吳秉叡　　賴清德　　張俊雄　　趙永清　　郭林勇　　蕭美琴　　陳志彬　　梅長錡　　呂學樟　　吳松柏　　尤　清　　柯俊雄　　陳　杰　　張碩文　　陳根德　　楊麗環　　何智輝　　李永萍　　吳敦義　　趙良燕　　江丙坤　　鄭金玲　　鍾紹和　　張昌財　　陳金德　　陳憲中　　廖婉汝　　曹爾忠　　余政道　　林淑芬　　高志鵬　　黃宗源　　李嘉進　　蔡同榮　　曾永權　　劉銓忠　　章仁香　　林滄敏　　王　拓　　謝國樑　　張花冠　　蔣孝嚴　　林濁水　　林德福　　彭添富　　邱創進　　王幸男　　李復興　　雷　倩　　葉芳雄　　蔡勝佳　　馮定國　　賴士葆　　林正峰　　王金平　　鍾榮吉　　張慶惠　　林耘生　　李鴻鈞　　李昆澤　　薛　凌　　黃劍輝　　朱鳳芝　　蔡英文　　田秋堇　　羅志明　　陳　瑩　　侯水盛　　魏明谷　　洪奇昌　　費鴻泰　　黃昭輝　　莊碩漢　　郭正亮　　郭玟成　　林育生　　黃昭順　　盧秀燕　　張川田　　陳秀卿　　葉宜津　　王世堅　　林樹山　　林國慶　　謝明源　　陳重信　　杜文卿　　陳景峻　　李顯榮　　高思博　　洪玉欽　　曹來旺　　林鴻池　　孫大千　　羅明才　　吳志揚　　林正二　　高金素梅　　李文忠　　李和順　　翁重鈞　　王昱婷　　吳光訓　　曾華德　　蔡正元　　林炳坤　　吳育昇　　蔡　豪　　蔡家福　　黃德福　　朱俊曉　　蔡錦隆　　黃義交　　林岱樺　　陳朝容　　徐耀昌　　紀國棟　　顏清標　　林春德　　柯淑敏　　李慶華　　劉憶如　　劉文雄　　傅崐萁　　顧崇廉　　林郁方　　林南生　　楊宗哲　　張顯耀　　洪秀柱　　沈智慧　　李　敖　　藍美津　　謝欣霓　　陳明真　　黃志雄　　李慶安　　陳啟昱　　林惠官　　管碧玲　　顏文章　　蔡其昌　　唐碧娥　　邱鏡淳　　廖國棟　　羅世雄　　邱　毅　　徐中雄　　湯火聖　　廖本煙　　尹伶瑛　　賴幸媛　　黃適卓　　黃政哲　　周守訓　　林益世　　許舒博　　柯建銘　　江昭儀",
                          "陳志彬　　許榮淑　　彭紹瑾　　邱永仁　　陳重信　　湯火聖　　盧天麟　　白添枝　　李鎮楠　　羅志明　　何敏豪　　陳進丁　　李明憲　　賴清德　　吳清池　　蕭美琴　　王塗發　　雷　倩　　蘇　起　　潘維剛　　侯彩鳳　　張慶惠　　曾燦燈　　劉憶如　　劉盛良　　黃政哲　　徐國勇　　黃昭輝　　楊瓊瓔　　徐少萍　　鄭朝明　　鄭國忠　　莊和子　　薛　凌　　曹壽民　　黃志雄　　邱　毅　　藍美津　　黃適卓　　曹爾忠　　謝文政　　林滄敏　　陳秀惠　　陳明真　　廖本煙　　黃宗源　　莊碩漢　　劉銓忠　　郭林勇　　吳志揚　　葉宜津　　林重謨　　江丙坤　　顏清標　　顧崇廉　　李顯榮　　柯淑敏　　張碩文　　林春德　　蔡家福　　王金平　　李鴻鈞　　吳敦義　　林正峰　　呂學樟　　蔡正元　　朱鳳芝　　楊宗哲　　吳育昇　　洪秀柱　　李嘉進　　江義雄　　李文忠　　鄭金玲　　洪玉欽　　蔡啟芳　　蔡其昌　　費鴻泰　　廖國棟　　陳憲中　　馮定國　　蔡勝佳　　李　敖　　趙良燕　　朱俊曉　　徐耀昌　　林郁方　　王昱婷　　高思博　　蔣孝嚴　　葉芳雄　　陳朝容　　王幸男　　丁守中　　張昌財　　蔡錦隆　　鍾榮吉　　李復興　　楊麗環　　張麗善　　紀國棟　　張慶忠　　李慶華　　吳光訓　　孫大千　　林炳坤　　林鴻池　　梅長錡　　孔文吉　　郭正亮　　余政道　　黃德福　　曾永權　　彭添富　　吳明敏　　林德福　　尹伶瑛　　李紀珠　　柯建銘　　謝明源　　吳英毅　　陳銀河　　趙永清　　鄭運鵬　　郭榮宗　　郭俊銘　　黃淑英　　帥化民　　黃劍輝　　賴士葆　　王榮璋　　王世勛　　尤　清　　許舒博　　賴幸媛　　田秋堇　　李昆澤　　張俊雄　　黃偉哲　　王淑慧　　洪奇昌　　管碧玲　　李俊毅　　高志鵬　　林濁水　　陳金德　　陳景峻　　廖婉汝　　謝欣霓　　高建智　　林淑芬　　林進興　　羅明才　　沈發惠　　唐碧娥　　陳　瑩　　張川田　　曹來旺　　林國慶　　杜文卿　　林南生　　周守訓　　柯俊雄　　楊仁福　　翁重鈞　　林建榮　　陳秀卿　　吳松柏　　江昭儀　　張花冠　　吳成典　　盧秀燕　　陳朝龍　　郭玟成　　黃健庭　　吳富貴　　侯水盛　　章仁香　　顏文章　　李永萍　　江連福　　李慶安　　郭素春　　吳秉叡　　伍錦霖　　潘孟安　　林岱樺　　黃昭順　　李全教　　林耘生　　王　拓　　陳啟昱　　曾華德　　羅世雄　　林樹山　　魏明谷　　邱創進　　林育生　　何智輝　　王世堅　　沈智慧　　蔡　豪　　林益世　　蔡同榮　　劉文雄　　傅崐萁　　鍾紹和　　林正二　　黃義交　　盧博基　　邱鏡淳　　徐中雄　　林為洲　　林惠官　　張顯耀　　陳根德　　高金素梅　　李和順　　陳　杰",
                          "蔡勝佳　　謝明源　　李鎮楠　　彭紹瑾　　陳重信　　吳明敏　　陳進丁　　羅志明　　何敏豪　　李明憲　　吳清池　　黃政哲　　曾燦燈　　徐少萍　　顏文章　　鄭運鵬　　林濁水　　楊仁福　　莊碩漢　　江昭儀　　潘維剛　　劉寬平　　黃淑英　　莊和子　　黃適卓　　李紀珠　　湯火聖　　曹壽民　　張慶惠　　陳秀惠　　孔文吉　　邱　毅　　侯彩鳳　　白添枝　　鄭朝明　　藍美津　　廖本煙　　尹伶瑛　　黃宗源　　洪奇昌　　郭林勇　　田秋堇　　張花冠　　彭添富　　吳富貴　　林德福　　黃昭輝　　陳銀河　　林耘生　　王塗發　　李昆澤　　洪玉欽　　江丙坤　　呂學樟　　葉芳雄　　王幸男　　鍾榮吉　　賴幸媛　　雷　倩　　林南生　　李顯榮　　柯俊雄　　李慶華　　陳朝容　　林正二　　林鴻池　　王世勛　　林育生　　李文忠　　沈智慧　　黃志雄　　吳志揚　　柯建銘　　李復興　　何智輝　　吳敦義　　柯淑敏　　傅崐萁　　陳　杰　　楊麗環　　洪秀柱　　張顯耀　　孫大千　　江連福　　王世堅　　陳志彬　　朱俊曉　　紀國棟　　楊瓊瓔　　蔣孝嚴　　林正峰　　李鴻鈞　　顧崇廉　　張麗善　　蔡錦隆　　吳光訓　　張碩文　　邱鏡淳　　林春德　　蔡正元　　劉銓忠　　張昌財　　張慶忠　　吳育昇　　黃義交　　劉文雄　　劉盛良　　蔡家福　　楊宗哲　　朱鳳芝　　劉憶如　　鄭金玲　　徐耀昌　　王金平　　黃德福　　陳朝龍　　賴士葆　　蔡其昌　　盧博基　　帥化民　　魏明谷　　黃健庭　　曹爾忠　　許榮淑　　林滄敏　　陳金德　　杜文卿　　趙永清　　林重謨　　吳秉叡　　蔡啟芳　　郭正亮　　郭俊銘　　蔡同榮　　王榮璋　　許舒博　　鄭國忠　　郭榮宗　　蘇　起　　張俊雄　　陳景峻　　尤　清　　徐中雄　　張川田　　高金素梅　　曾永權　　鍾紹和　　賴清德　　盧天麟　　王淑慧　　林淑芬　　江義雄　　陳憲中　　謝國樑　　王　拓　　唐碧娥　　王昱婷　　黃偉哲　　曹來旺　　薛　凌　　陳　瑩　　林為洲　　高志鵬　　吳英毅　　廖國棟　　林進興　　蔡　豪　　陳秀卿　　葉宜津　　林惠官　　吳松柏　　郭玟成　　謝文政　　沈發惠　　邱創進　　管碧玲　　林國慶　　徐國勇　　陳根德　　高建智　　潘孟安　　余政道　　侯水盛　　林岱樺　　蕭美琴　　林樹山　　謝欣霓　　羅明才　　黃劍輝　　陳啟昱　　李俊毅　　費鴻泰　　伍錦霖　　廖婉汝　　李嘉進　　趙良燕　　林炳坤　　盧秀燕　　顏清標　　章仁香　　林益世　　丁守中　　李慶安　　黃昭順　　陳明真　　李永萍　　吳成典　　林建榮　　林郁方　　高思博"
    )
    leavelegislators<-c("","","","","",
                        "王幸男　　林南生　　張俊雄　　許舒博　　曾華德　　馮定國　　劉政鴻",
                        "李和順　　陳朝龍",
                        "李　敖　　林樹山　　高金素梅　　陳重信　　曾華德　　楊宗哲　　廖本煙",
                        "李和順　　陳進丁　　劉寬平　　謝欣霓　　徐志明",
                        "李　敖　　高金素梅　　陳根德　　陳進丁　　劉寬平　　謝欣霓",
                        "沈智慧　　梅長錡　　許舒博　　傅崐萁　　廖國棟　　劉銓忠　　顧崇廉",
                        "沈智慧　　林正二　　林春德　　陳志彬　　陳進丁　　曾華德",
                        "劉寬平　　謝國樑",
                        "李全教　　李和順　　李　敖　　周守訓　　邱永仁　　翁重鈞　　梅長錡　　郭素春　　曾華德　　馮定國　　羅世雄"
    )
    
    #---- 開始掃過檔案迴圈執行 ----
    #pattern<-"[\n\r]{1,3}.+贊成者：.+[\n\r]{1,3}(.+)[\n\r]{1,3}.+反對者.+[\n\r]{1,3}(.+)[\n\r]{1,3}([一二三四五六七八九、棄權者：人]+[\n\r]{1,3}(.+)){0,1}"
    error_from_name<-openxlsx::read.xlsx(file.path(filespath, "2004_meeting", "errors_processing_data.xlsx"), sheet=1) %>%
      data.table::as.data.table()
    votepattern<-"[\n\r]{1,3}([贊成者一二三四五六七八九零○百十、：人。\\d]*贊成者[贊成者一二三四五六七八九零○百十、：人。\\d]+[\n\r]{1,3}([\u4e00-\u9fa5　．\\s]*))[\n\r]{1,3}([反對者一二三四五六七八九零○百十、：人。\\d]*反對者[反對者一二三四五六七八九零○百十、：人。\\d]+[\n\r]{0,3}([\u4e00-\u9fa5　．\\s]*)){0,1}[\n\r]{1,3}([棄權者一二三四五六七八九零○百十、：人。\\d]*棄權者[棄權者一二三四五六七八九零○百十、：人。\\d]+[\n\r]{0,3}([\u4e00-\u9fa5　．\\s]*)){0,1}[\n\r]{1}"
    myown_vote_record_detailed_part_df<-list()#data.table::data.table()
    #regular exp online check 用[\n\r]{1,3}([贊成者一二三四五六七八九零○百十、：人。\d]*贊成者[贊成者一二三四五六七八九零○百十、：人。\d]+[\n\r]{1,3}([\u4e00-\u9fa5　．\s]*))[\n\r]{1,3}([反對者一二三四五六七八九零○百十、：人。\d]*反對者[反對者一二三四五六七八九零○百十、：人。\d]+[\n\r]{0,3}([\u4e00-\u9fa5　．\s]*)){0,1}[\n\r]{1,3}([棄權者一二三四五六七八九零○百十、：人。\d]*棄權者[棄權者一二三四五六七八九零○百十、：人。\d]+[\n\r]{0,3}([\u4e00-\u9fa5　．\s]*)){0,1}[\n\r]{1}
    for (i in 1:length(term56_filepaths)) {#length(filename) 1:length(filename)
      #if (i!=2) {
      #  next
      #}
      meetingdataurl<-term56_filepaths[i]
      #urln<-paste("p_",i,sep="",collapse="")
      urln<-i
      this_meeting_ad_date<-meeting_ad_date[[i]]
      #從名單中把實際上已經離職或是未到職的委員剔除，依據為會議日期以及資料計算
      exact_onboard_legislators<- dplyr::filter(all_legislators, term==UQ(term[i])) %>%
        dplyr::mutate(diff_meeting_leave_days=as.numeric(difftime(leaveDate,UQ(meeting_ad_date[[i]]),units="days"))) %>%
        dplyr::mutate(diff_onboard_meeting_days=as.numeric(difftime(onboardDate,UQ(meeting_ad_date[[i]]),units="days"))) %>%
        dplyr::filter(diff_meeting_leave_days>=0, diff_onboard_meeting_days<=0) %>%
        getElement("name")
      #switch(as.character(term[i]==5),
      #  "TRUE"={
      #    mutate(termfivelegislators, diff_meeting_leave_days=as.numeric(difftime(leaveDate,UQ(meeting_ad_date[[i]],units="days")))) %>%
      #      mutate(diff_onboard_meeting_days=as.numeric(difftime(onboardDate,UQ(meeting_ad_date[[i]],units="days"))))
      #      filter(diff_meeting_leave_days>=0, diff_onboard_meeting_days<=0) %>%
      #      extract("name")
      #  },
      #  "FALSE"={
      #    #paste(attend_legislators[i],"　　",leavelegislators[i], sep="") %>%
      #    #  stri_split(regex="　　") %>% unlist() %>% stri_replace_all_fixed("　","")
      #  },
      #)
      #未出席委員?
      exact_leave_legislators <- switch(as.character(term[i]==5),
                                        "TRUE"="undetermined",
                                        "FALSE"={
                                          stri_split(leavelegislators, regex="　　") %>% unlist() %>% stri_replace_all_fixed("　","")
                                        },
      ) %>%
        setdiff(exact_onboard_legislators) %>%
        .[.!=""]
      content<-html[i]
      doc <- xml2::read_html(content,encoding="UTF-8")
      xpath<-"//p"
      paragraph_list <- xml2::xml_find_all(doc, xpath) %>%
        xml2::xml_text() %>%
        customgsub("\\n\\r","") %>%
        customgsub("\\r\\n","") %>%
        customgsub("王雪.","王雪峰") %>%
        customgsub("傅.萁","傅崐萁") %>%
        customgsub("瓦歷斯.貝林","瓦歷斯．貝林") %>%
        customgsub("陳　杰","陳杰") %>%
        customgsub("王　拓","王拓") %>%
        customgsub("尤　清","尤清") %>%
        customgsub("邱　毅","邱毅") %>%
        customgsub("蘇　起","蘇起") %>%
        customgsub("李　敖","李敖") %>%
        customgsub("雷　倩","雷倩") %>%
        customgsub("薛　凌","薛凌") %>%
        customgsub("蔡　豪","蔡豪") %>%
        customgsub("陳　瑩","陳瑩") %>%
        trimws()
      if (i==3) {
        paragraph_list<-c(
          paragraph_list[1:314],
          "許登宮程振隆錢林慧君何敏豪　羅志明　周慧瑛李俊毅　柯建銘　湯金全蔡煌瑯　陳建銘　黃宗源廖本煙　林志隆　黃政哲洪奇昌　顏錦福　郭正亮沈富雄　吳東昇　趙永清林豐喜　蔡同榮　鍾金江邱垂貞　許榮淑　林濁水王　拓　湯火聖　周清玉尤　清　林國華　梁牧養王雪峰　曹啟鴻　鄭朝明張清芳　張秀珍　劉俊雄陳景峻　林重謨　張川田江昭儀　蘇嘉富　邱永仁陳茂男　周雅淑　葉宜津唐碧娥　張學舜　李文忠賴清德　林進興　陳勝宏陳朝龍　蘇治芬　郭俊銘李鎮楠　彭添富　謝明源林文郎　侯水盛　盧博基李明憲　陳道明　藍美津魏明谷　陳金德　陳宗義高孟定　蔡啟芳　林育生邱創進　鄭國忠　王淑慧郭玟成　張花冠　郭榮宗簡肇棟　杜文卿　林忠正林岱樺　邱議瑩　蕭美琴段宜康　高志鵬　張俊宏",
          "二、反對者：一百零五人",
          paragraph_list[316:length(paragraph_list)]
        )
      }
      
      if (i==14) {
        paragraph_list<-c(
          paragraph_list[1:1155],
          "主席：報告表決結果：在場委員101人，贊成者101人，反對者0人，多數，本條照國民黨黨團、親民黨黨團提案條文通過。",
          paragraph_list[1157:length(paragraph_list)]
        )
      }
      paragraph_list<-customgsub(paragraph_list,"\n","")
      bill_list<-customgrep(paragraph_list,"報告表決結果|報告重付表決結果|報告本案表決結果",value=TRUE)
      if (i==5) {
        #移除未記名表決
        bill_list<-bill_list[c(1:69,72:76)]
      }
      if (i==8) {
        #移除未記名表決
        bill_list<-bill_list[c(2:12)]
      }
      if (i==15) {
        #移除未記名表決
        bill_list<-bill_list[c(2:11)]
      }
      if (length(bill_list)<1) {
        next
      }
      
      bill_list<-stringi::stri_trim_both(bill_list)
      pure_html<-paste(paragraph_list,sep="",collapse="\n\r")
      match<-stringr::str_match_all(pure_html,votepattern)
      #testmatch<-stringr::str_match_all(teststr,votepattern)
      scan_area<-match[[1]][,1]
      #檢查抓到的前半部詳細案由是否和後半部表決紀錄筆數是否對得上
      if (length(scan_area)!=length(bill_list))
        stop("Error at ", term56_filepaths[i], "   bill_list and scan_area does not match!")
      
      agree_votes_list<-match[[1]][,3] %>%
        customgsub("\n","")
      dissent_votes_list<-match[[1]][,5]
      giveup_votes_list<-match[[1]][,7]
      testing_for_check_bill_result_df<-cbind.fill( #rowr::cbind.fill(
        "billn"=seq(1:length(bill_list)),
        "scanarea"=stringi::stri_trim_both(scan_area),
        "billlist"=bill_list
        # fill = NA
      ) %>% data.table::as.data.table()
      names(testing_for_check_bill_result_df)=c("billn","scanarea","billlist")
      testing_for_check_bill_result_df %<>% dplyr::mutate("billresult"=NA)
      #testing_for_check_bill_result_df<-data.frame()
      for (billn in 1:length(bill_list)) {
        message("i=",i," & filename=",meetingrecords_filenames[i]," & billn=",billn)
        #if (i %in% 6:15) {
        #i=7開始正常
        #exact_agree_voter<-stri_split(agree_votes_list[billn],regex="　{2}") %>% unlist()
        #exact_dissent_voter<-stri_split(dissent_votes_list[billn],regex="　{2}") %>% unlist()
        #exact_giveup_voter<-stri_split(giveup_votes_list[billn],regex="　{2}") %>% unlist()
        #message(paste(agree_vote_names,sep=" ",collapse=" "))
        #%>% stri_extract_all(regex="[\u4e00-\u9fa5　．\\s]{3,}")
        #} else {
        for (error_from_name_n in 1:nrow(error_from_name)) {
          agree_votes_list[billn]<-customgsub(agree_votes_list[billn],
                                              error_from_name$name[error_from_name_n],
                                              error_from_name$replace[error_from_name_n])
          exact_agree_voter<-stringr::str_match_all(agree_votes_list[billn],"[\u4e00-\u9fa5．]{2,6}") %>% unlist()
          dissent_votes_list[billn]<-customgsub(dissent_votes_list[billn],
                                                error_from_name$name[error_from_name_n],
                                                error_from_name$replace[error_from_name_n])
          exact_dissent_voter<-stringr::str_match_all(dissent_votes_list[billn],"[\u4e00-\u9fa5．]{2,6}") %>% unlist()
          giveup_votes_list[billn]<-customgsub(giveup_votes_list[billn],
                                               error_from_name$name[error_from_name_n],
                                               error_from_name$replace[error_from_name_n])
          exact_giveup_voter<-stringr::str_match_all(giveup_votes_list[billn],"[\u4e00-\u9fa5．]{2,6}") %>% unlist()
          #test_list<-c(test_list,agree_votes_list[billn])
        }
        #exact_leavelegislators_list<-exact_leave_legislators
        #message(paste(agree_votes_list[billn],sep=" ",collapse=" "))
        #}
        #exact_onboard_legislators,exact_leave_legislators
        billresult<-ifelse(length(exact_agree_voter)>length(exact_dissent_voter),"Passed","NotPassed")
        testing_for_check_bill_result_df$billresult[billn] <- billresult
        #data.frame("billn"=billn,"billresult"=billresult)
        #testing_for_check_bill_result_df_one<-rowr::cbind.fill(
        #  data.frame("scan_area"=scan_area[billn]),
        #  data.frame("bill_list"=bill_list[billn]),
        #  data.frame("agree_voter"=length(exact_agree_voter)),
        #  data.frame("dissent_voter"=length(exact_dissent_voter)),
        #  data.frame("billresult"=billresult)
        #) %>% as.data.frame() %>% mutate_all(stringi::stri_trim_both)
        #testing_for_check_bill_result_df %<>% rbind(testing_for_check_bill_result_df_one)
        
        other_not_attend_or_not_vote_voter <- base::setdiff(exact_onboard_legislators, exact_giveup_voter) %>%
          base::setdiff(exact_agree_voter) %>%
          base::setdiff(exact_dissent_voter)# %>%
        #setdiff(exact_leave_legislators)
        
        voters_status_list <- list(
          "棄權"=exact_giveup_voter,
          "贊成"=exact_agree_voter,
          "反對"=exact_dissent_voter,
          "請假"=exact_leave_legislators,
          "未投票"=other_not_attend_or_not_vote_voter
        )
        voters_voting_status <- lapply(names(voters_status_list), function(list_title) {
          nlist<-voters_status_list[[list_title]]
          tf_decision <- (length(nlist)==0 | all(is.na(nlist)))
          if (list_title=="請假" & term[i]==5) { tf_decision<-TRUE }
          #第五屆的未投票可能包含未出席（因為沒有出席資料）
          if (tf_decision) {
            return (data.table::data.table())
          } else {
            cbind("votedecision"=list_title,
                  "legislator_name"=nlist,
                  "term"=term[i],
                  "period"=period[i],
                  "meetingno"=meetingno[i],
                  "temp_meeting_no"=temp_meeting_no[i],
                  "billn"=billn,
                  "billcontent"=bill_list[billn],
                  "billresult"=billresult,
                  "url"=meetingdataurl,
                  "urln"=urln,
                  "date"=meetingdate[i]
            ) %>% data.table::as.data.table() %>%
              return()
          }
        })
        myown_vote_record_detailed_part_df <- c(myown_vote_record_detailed_part_df, voters_voting_status)
        # exact_giveup_voter_df<-if (length(exact_giveup_voter)==0 | all(is.na(exact_giveup_voter)) ) {
        #   data.frame()
        # } else {
        #   cbind("votedecision"="棄權",
        #         "legislator_name"=exact_giveup_voter,
        #         "term"=term[i],
        #         "period"=period[i],
        #         "meetingno"=meetingno[i],
        #         "temp_meeting_no"=temp_meeting_no[i],
        #         "billn"=billn,
        #         "billcontent"=bill_list[billn],
        #         "billresult"=billresult,
        #         "url"=meetingdataurl,
        #         "urln"=urln,
        #         "date"=meetingdate[i]
        #   ) %>% as.data.frame()
        # }
        # exact_agree_voter_df<-if (length(exact_agree_voter)==0 | all(is.na(exact_agree_voter))) {
        #   data.frame()
        # } else {
        #   cbind("votedecision"="贊成",
        #         "legislator_name"=exact_agree_voter,
        #         "term"=term[i],
        #         "period"=period[i],
        #         "meetingno"=meetingno[i],
        #         "temp_meeting_no"=temp_meeting_no[i],
        #         "billn"=billn,
        #         "billcontent"=bill_list[billn],
        #         "billresult"=billresult,
        #         "url"=meetingdataurl,
        #         "urln"=urln,
        #         "date"=meetingdate[i]
        #   ) %>% as.data.frame()
        # }
        # exact_dissent_voter_df<-if (length(exact_dissent_voter)==0 | all(is.na(exact_dissent_voter))) {
        #   data.frame()
        # } else {
        #   cbind("votedecision"="反對",
        #         "legislator_name"=exact_dissent_voter,
        #         "term"=term[i],
        #         "period"=period[i],
        #         "meetingno"=meetingno[i],
        #         "temp_meeting_no"=temp_meeting_no[i],
        #         "billn"=billn,
        #         "billcontent"=bill_list[billn],
        #         "billresult"=billresult,
        #         "url"=meetingdataurl,
        #         "urln"=urln,
        #         "date"=meetingdate[i]
        #   ) %>% as.data.frame()
        # }
        # exact_leave_voter_df <- switch(as.character(term[i]==5 | length(exact_leave_legislators)==0 | all(is.na(exact_leave_legislators))), #exact_leave_legislators
        #   "TRUE"=data.frame(),
        #   "FALSE"=cbind("votedecision"="請假",
        #                 "legislator_name"=exact_leave_legislators,
        #                 "term"=term[i],
        #                 "period"=period[i],
        #                 "meetingno"=meetingno[i],
        #                 "temp_meeting_no"=temp_meeting_no[i],
        #                 "billn"=billn,
        #                 "billcontent"=bill_list[billn],
        #                 "billresult"=billresult,
        #                 "url"=meetingdataurl,
        #                 "urln"=urln,
        #                 "date"=meetingdate[i]
        #   ) %>% as.data.frame()
        # )
        # exact_other_not_attend_or_not_vote_voter_df <-if (length(other_not_attend_or_not_vote_voter)==0 | all(is.na(other_not_attend_or_not_vote_voter))) {
        #   data.frame()
        # } else {
        #   cbind("votedecision"="未投票", #第五屆的未投票可能包含未出席（因為沒有出席資料）
        #         "legislator_name"=other_not_attend_or_not_vote_voter,
        #         "term"=term[i],
        #         "period"=period[i],
        #         "meetingno"=meetingno[i],
        #         "temp_meeting_no"=temp_meeting_no[i],
        #         "billn"=billn,
        #         "billcontent"=bill_list[billn],
        #         "billresult"=billresult,
        #         "url"=meetingdataurl,
        #         "urln"=urln,
        #         "date"=meetingdate[i]
        #   ) %>% as.data.frame()
        # }
        # myown_vote_record_detailed_part_df<-rbind(
        #   myown_vote_record_detailed_part_df,
        #   exact_agree_voter_df,
        #   exact_dissent_voter_df,
        #   exact_giveup_voter_df,
        #   exact_leave_voter_df,
        #   exact_other_not_attend_or_not_vote_voter_df
        # )
        
        
        #exact_giveup_voter
        #check_not_replaced_pattern<-"(([\u4e00-\u9fa5]{2,4}?)(　{1,3}?)([\u4e00-\u9fa5]{2,4}?))"
        #if (identical(agree_votes_list[billn], character(0))) {
        #  next
        #}
        #if (customgrepl(agree_votes_list[billn],check_not_replaced_pattern)) {
        #not_replaced<-customgsub(agree_votes_list[billn],check_not_replaced_pattern,"\\1")
        #not_replaced<-str_match_all(agree_votes_list[billn],check_not_replaced_pattern)
        #not_replaced
        #writeClipboard(agree_votes_list[billn])
        #stop("Error at ", filepath[i], "  and error is ",not_replaced[[1]][,1], "   and list is", agree_votes_list[billn])
        #}
        
        
      }
      #write.xlsx(testing_for_check_bill_result_df,paste0(ly_meeting_path,"testnewinputpart",i,".xlsx"))
      #test_list_chr<-paste(test_list,sep="",collapse="")
      #pure_html
      #write_file(test_list_chr, paste(ly_meeting_path,"tmp.html",sep="",collapse=""))
      
    }
    myown_vote_record_detailed_part_df %<>%
      data.table::rbindlist() %>%
      dplyr::filter(!is.na(legislator_name)) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate_at(c("term","period","meetingno","temp_meeting_no","billn","urln"),as.integer)
    if (save==TRUE) {
      #save(myown_vote_record_detailed_part_df,file=paste0(dataset_in_scriptsfile_directory,  "myown_vote_record_detailed_part_df.RData"))
      saveRDS(myown_vote_record_detailed_part_df, file=myown_vote_record_detailed_part_df_filepath)
    }
    return (myown_vote_record_detailed_part_df)
  }
))

# setwd(filespath)

#load(file=paste0(dataset_file_directory, "rdata", slash,  "myown_vote_record_df.RData"))
#distinct(myown_vote_record_detailed_part_df,billcontent,url,date,term,period,meetingno,temp_meeting_no,billn,billresult) %>%
#  write.xlsx(paste0(dataset_file_directory,"rdata",slash,"myown_vote_record_detailed_part_df_2.xlsx"))
#write_file(as.character(pure_html), path=paste(dataset_file_directory, "rdata", slash,  "checkcontent.txt", sep = ""), append = FALSE)
#myown_vote_record_df<-bind_rows(myown_vote_record_df,myown_vote_record_detailed_part_df)
#regexp=
#表決結果名單：[\n\r]{1,3}.+贊成者：.+[\n\r]{1,3}.+[\n\r]{1,3}.+反對者.+[\n\r]{1,3}.+[\n\r]{1,3}([一二三四五六七八九、棄權者：人]+[\n\r]{1,3}.+){0,1}
#n_occur <- data.frame(table(test_final$ID))
#customgsub(test,"([\u4e00-\u9fa5]{3})([\u4e00-\u9fa5]{3})","\\1　\\2") %>% customgsub("([\u4e00-\u9fa5]{4})([\u4e00-\u9fa5]{3})","\\1　\\2")