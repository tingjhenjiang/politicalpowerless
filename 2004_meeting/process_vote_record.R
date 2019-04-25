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
ly_meeting_path <- paste0(filespath, "2004_meeting", slash, "original", slash, sep="", collapse="")
filename<-c(#"立法院第5屆第5會期全院委員談話會紀錄.html",
            "立法院第5屆第5會期第1次臨時會第1次會議紀錄.html",
            "立法院第5屆第5會期第1次臨時會第3次會議紀錄.html",
            "立法院第5屆第5會期第1次臨時會第4次會議紀錄.html",
            "立法院第5屆第6會期第1次會議紀錄.html",
            "立法院第5屆第6會期第16次會議紀錄.html",
            "立法院第6屆第1會期第13次會議紀錄.html",
            "立法院第6屆第1會期第14次會議紀錄.html",
            "立法院第6屆第2會期第3次會議紀錄.html",
            "立法院第6屆第2會期第5次會議紀錄.html",
            "立法院第6屆第2會期第7次會議紀錄.html",
            "立法院第6屆第2會期第9次會議紀錄.html",
            "立法院第6屆第2會期第16次會議紀錄.html",
            "立法院第6屆第2會期第19次會議紀錄.html",
            "立法院第6屆第3會期第8次會議紀錄.html",
            "立法院第6屆第3會期第10次會議紀錄.html")
term<-c(5,5,5,5,5,6,6,6,6,6,6,6,6,6,6)
period<-c(5,5,5,6,6,1,1,2,2,2,2,2,2,3,3)
temp_meeting_no<-c(1,1,1,1,0,0,0,0,0,0,0,0,0,0,0)
meetingno<-c(1,3,4,1,16,13,14,3,5,7,9,16,19,8,10)
date<-c("093/08/21","093/08/23","093/08/24","093/09/14","094/01/20,094/01/21","094/05/20,094/05/24","094/05/24,094/05/31","094/09/23,094/09/27","094/10/07,094/10/11","094/10/21","094/11/04,094/11/08","094/12/23","095/01/11","095/04/07","095/04/21,095/04/25")
filepath <- paste(ly_meeting_path,filename,sep="")
html<-sapply(filepath,custom_read_file)
myown_vote_record_detailed_part_df<-data.frame()
#pattern<-"[\n\r]{1,3}.+贊成者：.+[\n\r]{1,3}(.+)[\n\r]{1,3}.+反對者.+[\n\r]{1,3}(.+)[\n\r]{1,3}([一二三四五六七八九、棄權者：人]+[\n\r]{1,3}(.+)){0,1}"
error_from_name<-read.xlsx(paste0(filespath, slash, "2004_meeting", slash, "errors_processing_data.xlsx"), sheet=1) #read_csv(paste0(filespath,"vote_record",slash,"2004_meeting",slash,"error_names_replace_complete_record.csv"))
votepattern<-"[\n\r]{1,3}([贊成者一二三四五六七八九零○百十、：人。\\d]*贊成者[贊成者一二三四五六七八九零○百十、：人。\\d]+[\n\r]{1,3}([\u4e00-\u9fa5　．\\s]*))[\n\r]{1,3}([反對者一二三四五六七八九零○百十、：人。\\d]*反對者[反對者一二三四五六七八九零○百十、：人。\\d]+[\n\r]{0,3}([\u4e00-\u9fa5　．\\s]*)){0,1}[\n\r]{1,3}([棄權者一二三四五六七八九零○百十、：人。\\d]*棄權者[棄權者一二三四五六七八九零○百十、：人。\\d]+[\n\r]{0,3}([\u4e00-\u9fa5　．\\s]*)){0,1}[\n\r]{1}"
#regular exp online check 用[\n\r]{1,3}([贊成者一二三四五六七八九零○百十、：人。\d]*贊成者[贊成者一二三四五六七八九零○百十、：人。\d]+[\n\r]{1,3}([\u4e00-\u9fa5　．\s]*))[\n\r]{1,3}([反對者一二三四五六七八九零○百十、：人。\d]*反對者[反對者一二三四五六七八九零○百十、：人。\d]+[\n\r]{0,3}([\u4e00-\u9fa5　．\s]*)){0,1}[\n\r]{1,3}([棄權者一二三四五六七八九零○百十、：人。\d]*棄權者[棄權者一二三四五六七八九零○百十、：人。\d]+[\n\r]{0,3}([\u4e00-\u9fa5　．\s]*)){0,1}[\n\r]{1}
for (i in 1:length(filename)) {#length(filename) 1:length(filename)
  #if (i!=2) {
  #  next
  #}
  url<-filepath[i]
  #urln<-paste("p_",i,sep="",collapse="")
  urln<-i
  content<-html[i]
  doc <- read_html(content,encoding="UTF-8")
  xpath<-"//p"
  paragraph_list<-xml_find_all(doc, xpath) %>%
    xml_text() %>%
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
  #write_file(paste0(paragraph_list,collapse="\n"), path=paste(dataset_file_directory, "rdata", slash,  "checkcontent.txt", sep = ""), append = FALSE)
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
    stop("Error at ", filepath[i], "   bill_list and scan_area does not match!")
  
  agree_votes_list<-match[[1]][,3] %>%
    customgsub("\n","")
  dissent_votes_list<-match[[1]][,5]
  giveup_votes_list<-match[[1]][,7]
  testing_for_check_bill_result_df<-rowr::cbind.fill(
    "billn"=seq(1:length(bill_list)),
    "scanarea"=stringi::stri_trim_both(scan_area),
    "billlist"=bill_list,
    fill = NA
  ) %>% as.data.frame()
  names(testing_for_check_bill_result_df)=c("billn","scanarea","billlist")
  testing_for_check_bill_result_df %<>% mutate("billresult"=NA)
  #testing_for_check_bill_result_df<-data.frame()
  for (billn in 1:length(bill_list)) {
    message("i=",i," & filename=",filename[i]," & billn=",billn)
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
      
      #message(paste(agree_votes_list[billn],sep=" ",collapse=" "))
    #}
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

    exact_giveup_voter_df<-if (length(exact_giveup_voter)==0) {
      data.frame()
    } else {
      cbind("votedecision"="棄權",
            "legislator_name"=exact_giveup_voter,
            "term"=term[i],
            "period"=period[i],
            "meetingno"=meetingno[i],
            "temp_meeting_no"=temp_meeting_no[i],
            "billn"=billn,
            "billcontent"=bill_list[billn],
            "billresult"=billresult,
            "url"=url,
            "urln"=urln,
            "date"=date[i]
      ) %>% as.data.frame()
    }
    exact_agree_voter_df<-if (length(exact_agree_voter)==0) {
      data.frame()
    } else {
      cbind("votedecision"="贊成",
            "legislator_name"=exact_agree_voter,
            "term"=term[i],
            "period"=period[i],
            "meetingno"=meetingno[i],
            "temp_meeting_no"=temp_meeting_no[i],
            "billn"=billn,
            "billcontent"=bill_list[billn],
            "billresult"=billresult,
            "url"=url,
            "urln"=urln,
            "date"=date[i]
      ) %>% as.data.frame()
    }
    exact_dissent_voter_df<-if (length(exact_dissent_voter)==0) {
      data.frame()
    } else {
      cbind("votedecision"="反對",
            "legislator_name"=exact_dissent_voter,
            "term"=term[i],
            "period"=period[i],
            "meetingno"=meetingno[i],
            "temp_meeting_no"=temp_meeting_no[i],
            "billn"=billn,
            "billcontent"=bill_list[billn],
            "billresult"=billresult,
            "url"=url,
            "urln"=urln,
            "date"=date[i]
      ) %>% as.data.frame()
    }
    myown_vote_record_detailed_part_df<-rbind(
      myown_vote_record_detailed_part_df,
      exact_agree_voter_df,
      exact_dissent_voter_df,
      exact_giveup_voter_df
    )
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
myown_vote_record_detailed_part_df<-filter(myown_vote_record_detailed_part_df,!is.na(legislator_name)) %>%
  mutate_all(as.character) %>%
  mutate_at(c("term","period","meetingno","temp_meeting_no","billn","urln"),as.integer)
#save(myown_vote_record_detailed_part_df,file=paste0(dataset_in_scriptsfile_directory,  "myown_vote_record_detailed_part_df.RData"))
load(file=paste0(dataset_file_directory, "rdata", slash,  "myown_vote_record_df.RData"))
#distinct(myown_vote_record_detailed_part_df,billcontent,url,date,term,period,meetingno,temp_meeting_no,billn,billresult) %>%
#  write.xlsx(paste0(dataset_file_directory,"rdata",slash,"myown_vote_record_detailed_part_df_2.xlsx"))
#write_file(as.character(pure_html), path=paste(dataset_file_directory, "rdata", slash,  "checkcontent.txt", sep = ""), append = FALSE)
#myown_vote_record_df<-bind_rows(myown_vote_record_df,myown_vote_record_detailed_part_df)
#regexp=
#表決結果名單：[\n\r]{1,3}.+贊成者：.+[\n\r]{1,3}.+[\n\r]{1,3}.+反對者.+[\n\r]{1,3}.+[\n\r]{1,3}([一二三四五六七八九、棄權者：人]+[\n\r]{1,3}.+){0,1}
#n_occur <- data.frame(table(test_final$ID))
#customgsub(test,"([\u4e00-\u9fa5]{3})([\u4e00-\u9fa5]{3})","\\1　\\2") %>% customgsub("([\u4e00-\u9fa5]{4})([\u4e00-\u9fa5]{3})","\\1　\\2")