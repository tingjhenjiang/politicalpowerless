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
no_rollcall<-c()
urlarr<-as.character(meetingdata$url)
error_vote_record_from_name<-read_csv("error_vote_record_from_name.csv")
error_leave_and_attend_legislators<-read_csv("leave_and_attend_legislators.csv") %>%
  mutate_cond(is.na(replace_with),replace_with="")

myown_vote_record_df<-data.frame()
replace_troublesome_names<-function(df) {
  df <- df %>%
    mutate_cond(customgrepl(legislator_name,"邱　毅"),legislator_name="邱毅") %>%
    mutate_cond(customgrepl(legislator_name,"薛　凌"),legislator_name="薛凌") %>%
    mutate_cond(customgrepl(legislator_name,"陳　瑩"),legislator_name="陳瑩") %>%
    mutate_cond(customgrepl(legislator_name,"余　天"),legislator_name="余天") %>%
    mutate_cond(customgrepl(legislator_name,"陳　杰"),legislator_name="陳杰") %>%
    mutate_cond(term==9 & customgrepl(legislator_name,"簡東明"),legislator_name="簡東明Uliw．Qaljupayare") %>%
    mutate_cond(term==9 & customgrepl(legislator_name,"廖國棟"),legislator_name="廖國棟Sufin．Siluko") %>%
    mutate_cond(term==9 & customgrepl(legislator_name,"鄭天財"),legislator_name="鄭天財Sra．Kacaw") %>%
    mutate_cond(term==9 & customgrepl(legislator_name,"高潞"),legislator_name="高潞．以用．巴魕剌Kawlo．Iyun．Pacidal") %>%
    mutate_cond(term==9 & customgrepl(legislator_name,"陳秀霞"),legislator_name="周陳秀霞")
  return(df)
}
anti_join_with_nrow_zero<-function(X,Y,by=c()) {
  if (nrow(Y)>0) {
    return(anti_join(X,Y,by))
  } else {
    return(X)
  }
}

#mutate(leave_and_attend_legislators,chichrcount=stri_count(legislator_name,regex="[\u4e00-\u9fa5A-aZ-z]{1}") ) %>% View()
filter(leave_and_attend_legislators,stri_count(legislator_name,regex="[\u4e00-\u9fa5A-aZ-z]{1}")>3 ) %>%
  filter(!is.element(legislator_name, c("高金素梅","周陳秀霞","張廖萬堅","陳賴素美","鄭天財Sra．Kacaw","簡東明Uliw．Qaljupayare","廖國棟Sufin．Siluko","鄭天財Sra．Kacaw","高潞．以用．巴魕剌Kawlo．Iyun．Pacidal","Kolas Yotaka")) | 
           customgrepl(legislator_name,"(　|　|　| | )")) %>%
  View()
#  write_csv(path="leave_and_attend_legislators.csv")
#第九會期從377開始 //problem:108
myown_vote_record_df<-data.frame()
leave_and_attend_legislators<-data.frame()
for (urln in 1:length(urlarr)) { #length(urlarr)
  url<-urlarr[urln]
  if (is.na(url) | urln==478) {
    next
  }
  # | !(urln %in% c(469,475))
  #urln=478是一堆有表決結果名單附(n)至(n)形式的，暫存函式如下:
  #customgsub(test, "(.+?[表決]{0,2}[結果]{0,2}[名單]{0,2}附{1}[後件]{0,1}。{0,1}[(（]{0,1}\\s{0,1}([—○一二三四五六七八九十\\d]{0,})\\s{0,1}[)）]{0,1}至{1}[(（]{0,1}([—○一二三四五六七八九十\\d]{0,})[)）]{0,1}[〕】）]{0,}[；。]{0,1}).+?", "\\2:\\3;", perl = TRUE)
  
  term<-meetingdata$term[urln]
  period<-meetingdata$period[urln]
  meetingno<-meetingdata$meetingno[urln]
  temp_meeting_no<-meetingdata$temp_meeting_no[urln]
  date<-as.character(meetingdata$date[urln])
  if (is.na(temp_meeting_no)) {
    temp_meeting_no<-0
  }
  #content<-read_file(url)
  content<-meetingdata$content[urln]
  
  #直接跳過不妨礙（又因為直接跳過省去篩選麻煩
  if (term==6 & period==4 & meetingno==17) {
    #尾部沒有討論事項記名表決，但是有其他事項表決，卻沒有詳細有人名的名單
    next
  }
  
  #clean data 特別處理
  if (term==7 & period==1 & meetingno==19) {
    #有一次的表決少了贊成者和棄權者的文字，補上
    #content<-read_file("E:\Software\scripts\R\vote_record\processed_ly_meeting_record\LCEWC03_070119.htm")
    content<-read_file(paste(filespath,"vote_record",slash,"processed_ly_meeting_record",slash,"LCEWC03_070119.htm", sep = ""))
  }
  if (term==7 & period==2 & meetingno==17) {
    #有一次的表決少了贊成者和棄權者的文字，補上
    #content<-read_file("E:\Software\scripts\R\vote_record\processed_ly_meeting_record\LCEWC03_070119.htm")
    content<-read_file(paste(filespath,"vote_record",slash,"processed_ly_meeting_record",slash,"LCEWC03_070119.htm", sep = ""))
  }
  if (term==7 & period==3 & meetingno==5) {
    #表決案數字亂碼修正
    #content<-read_file("E:\Software\scripts\R\vote_record\processed_ly_meeting_record\LCEWC03_070119.htm")
    content<-read_file(paste(filespath,"vote_record",slash,"processed_ly_meeting_record",slash,"LCEWC03_070119.htm", sep = ""))
  }
  if (term==7 & period==3 & meetingno==8) {
    #立法院第7屆第3會期第8次會議議事錄有二個表決紀錄的敘述被合併
    #content<-read_file("E:\Software\scripts\R\vote_record\processed_ly_meeting_record\LCEWC03_070119.htm")
    content<-read_file(paste(filespath,"vote_record",slash,"processed_ly_meeting_record",slash,"LCEWC03_070119.htm", sep = ""))
  }
  
  #paragraph_list[roll_call_list_block_sp:paragraph_list_length]<-customgsub(paragraph_list[roll_call_list_block_sp:paragraph_list_length],"高金素梅　","高金素梅　　")
  content<-customgsub(content,"高金素梅　","高金素梅　　") %>%
    customgsub("Kolas Yotaka　　　　　","Kolas Yotaka　　") %>%
    customgsub("許舒博（尚未報到）","許舒博") %>%
    customgsub("傅.萁","傅崐萁") %>%
    customgsub("瓦歷斯.貝林","瓦歷斯．貝林") %>%
    customgsub("王雪.","王雪峰") %>%
    customgsub("陳賴素美　","陳賴素美　　") %>%
    customgsub("張廖萬堅　","張廖萬堅　　") %>%
    customgsub("周陳秀霞　","周陳秀霞　　") %>%
    customgsub("廖國棟Sufin．Siluko　","廖國棟Sufin．Siluko　　") %>%
    customgsub("鄭天財Sra Kacaw　　　","鄭天財Sra Kacaw　　") %>%
    customgsub("簡東明Uliw．Qaljupayare　　　　","簡東明Uliw．Qaljupayare　　") %>%
    customgsub("高潞‧以用‧巴魕剌Kawlo．Iyun．Pacidal　","高潞‧以用‧巴魕剌Kawlo．Iyun．Pacidal　　") %>%
    customgsub(" ","")
  
  ##立法院第6屆第6會期第16次會議議事錄
  if (term==6 & period==6 & meetingno==16) {
    content<-customgsub(content,
                        '決議：離島建設條例第十條之一、第十七條之一及第十七條之二條文，均不予增訂；第十條、第十六條、第十七條條文，均不予修正。〔其中第十條之一經表決不予增訂，表決結果名單附後',
                        '決議：離島建設條例第十條之一、第十七條之一及第十七條之二條文，均不予增訂；第十條、第十六條、第十七條條文，均不予修正。〔其中第十條之一經記名表決不予增訂，表決結果名單附後'
    )
  }
  if (term==7 & period==2 & meetingno==1 & temp_meeting_no==1) {
    content<-customgsub(content,
                        '】本次會議表決結果名單：',
                        '】</p><p>本次會議表決結果名單：'
    )
  }
  if (term==9 & period==4 & meetingno==14) {
    #立法院第9屆第4會期第14次會議議事錄
    #決議表決結果附後(8)至(12)】
    content<-customgsub(content,
                        '(<p{1}.+?決議：)(交通部鐵道局組織法)(草案修正通過。)(〔二讀時，名稱及條文均照審查會名稱及條文通過【其中)第二條至第六條均經記名表決結果，予以通過，表決結果附後{1}.+?>】；',
                        '\\1\\2\\3\\4\\2第二條照審查會條文經記名表決結果予以通過，表決結果名單附後(8)；\\2第三條照審查會條文經記名表決結果予以通過，表決結果名單附後(9)；\\2第四條照審查會條文經記名表決結果予以通過，表決結果名單附後(10)；\\2第五條照審查會條文記名表決結果予以通過，表決結果名單附後(11)；\\2第六條照審查會條文記名表決結果予以通過，表決結果名單附後(12)】；'
    )
  }
  if (term==9 & period==4 & meetingno==11) {
    content<-customgsub(content,
                        '「討論事項第一案促進轉型正義條例於二讀後繼續進行三讀予以通過」',
                        '「討論事項第一案促進轉型正義條例於二讀後繼續進行三讀予以通過」部分：'
    )
  }
  if (term==9 & period==4 & meetingno==15) {
    #立法院第9屆第4會期第15次會議議事錄
    #表決結果附後(1)、(2)
    content<-customgsub(content,
                        "(<p [\\w\\W]+決議：)(金融科技發展與創新實驗條例)(草案修正通過。)(〔二讀時，第六條及第十六條均照民進黨黨團修正動議條文通過【)經記名表決結果，均予以通過，表決結果附後([\\w\\W]+?)>】；",
                        "\\1\\2\\3\\4\\2第六條照民進黨黨團修正動議經記名表決結果予以通過，表決結果名單附後(1)；\\2第十六條照民進黨黨團修正動議經記名表決結果予以通過，表決結果名單附後(2)】；",
                        perl=TRUE)
  }
  if (term == 9 & period == 1 & meetingno == 1 & temp_meeting_no == 1) {
    content <- customgsub(content,
                          "多數通過；並採記表決方式",
                          "多數通過；並採記名表決方式"
    )
  }
  if (term == 9 & period == 3 & meetingno == 3 & temp_meeting_no == 1) {
    content <- customgsub(content,
                          "多數通過；並採記表決方式",
                          "多數通過；並採記名表決方式"
    )
  }
  
  #doc <- read_xml(content,as_html = TRUE)
  doc <- read_html(content,encoding="UTF-8")
  #xpath<-"//p[(contains(.,'次會議議事錄'))]"
  #meetingname<-xml_find_all(doc, xpath) %>%
  #  xml_text() %>% getElement(1)
  meetingname<-as.character(meetingdata$termmeetingtime[urln])
  #xpath<-"//p[(contains(.,'時　　間'))]"
  #meetingtimeanddata<-xml_find_all(doc, xpath) %>%
  #  xml_text() %>% getElement(1)
  #xpath<-"//p[(contains(.,'請假委員　'))]"
  message("urln=",urln," | 1", meetingname, url)
  
  xpath<-"//p"
  paragraph_list<-xml_find_all(doc, xpath) %>%
    xml_text()
  #paragraph_list_length<-length(paragraph_list)
  
  check_leave_and_attend_legislator_chr_paragraph<-paragraph_list[1:15] %>%
    customgsub("(Siluko){1} {0,1}　{1} {0,1}","Siluko　　") %>%
    customgsub("(Kacaw){1} {0,1}　{1} {0,1}","Kacaw　　") %>%
    customgsub("(Yotaka){1} {0,1}　{1} {0,1}","Yotaka　　") %>%
    customgsub("(Pacidal|Pacida){1} {0,1}　{1} {0,1}","Pacidal　　")
  replace_leave_and_attend_legislator_pattern<-filter(error_leave_and_attend_legislators,term==UQ(term),period==UQ(period),meetingno==UQ(meetingno),temp_meeting_no==UQ(temp_meeting_no))
  if (nrow(replace_leave_and_attend_legislator_pattern)>0) {
    check_leave_and_attend_legislator_chr_paragraph<- stri_replace_all_fixed(check_leave_and_attend_legislator_chr_paragraph,replace_leave_and_attend_legislator_pattern$legislator_name, replace_leave_and_attend_legislator_pattern$replace_with,vectorize_all=FALSE)
  }
  
  leavelegislator<-customgrep(check_leave_and_attend_legislator_chr_paragraph,"請假委員",value=TRUE)
  if (identical(leavelegislator,as.character())) {
    leavelegislator<-data.frame()
  } else {
    leavelegislator<-leavelegislator %>%
      strsplit('請假委員　') %>%
      unlist() %>%
      strsplit('　　') %>%
      unlist() %>%
      customgrep("[\u4e00-\u9fa5A-aZ-z]",value=TRUE) %>%
      stri_replace_all_fixed("　","") %>%
      trimws()
    leavelegislator<-data.frame(
      "legislator_name"=leavelegislator,
      "term"=term,
      "period"=period,
      "temp_meeting_no"=temp_meeting_no,
      "meetingno"=meetingno,
      "url"=url,
      "urln"=urln,
      "date"=date
    )
  } %>%
    replace_troublesome_names()
  
  attendlegislator<-customgrep(check_leave_and_attend_legislator_chr_paragraph,"出席委員",value=TRUE) %>%
    strsplit('出席委員　') %>%
    unlist() %>%
    strsplit('　　') %>%
    unlist() %>%
    customgrep("[\u4e00-\u9fa5]",value=TRUE) %>%
    stri_replace_all_fixed("　","") %>%
    trimws()
  attendlegislator<-data.frame(
    "legislator_name"=attendlegislator,
    "term"=term,
    "period"=period,
    "temp_meeting_no"=temp_meeting_no,
    "meetingno"=meetingno,
    "url"=url,
    "urln"=urln,
    "date"=date
  ) %>%
    replace_troublesome_names()
  
  leave_and_attend_legislators<-bind_rows(leave_and_attend_legislators,leavelegislator,attendlegislator)
  #message(leavelegislator,"\n\r",attendegislator,"\n\r")
  #attendegislator
  #next()
  
  #特別處理
  ##立法院第6屆第2會期第19次會議議事錄
  if (term==6 & period==2 & meetingno==19) {
    paragraph_list<-c(paragraph_list[1:3161],"各項記名表決結果名單",paragraph_list[3162:length(paragraph_list)])
    #(customgrepl(paragraph_list,'贊成者：'))-2 %>% which.min()
    #取代模式
    #paragraph_list<-sapply(paragraph_list,
    #                       customgsub,
    #                       '一、本院無黨團結聯盟黨團提議，將無黨團結聯盟黨團擬具之「公教人員保險法增訂第十五條之一條文草案」由法制委員會抽出，逕付二讀，並由無黨團結聯盟黨團負責召集協商，經表決未獲通過。【在場委員',
    #                       '一、本院無黨團結聯盟黨團提議，將無黨團結聯盟黨團擬具之「公教人員保險法增訂第十五條之一條文草案」由法制委員會抽出，逕付二讀，並由無黨團結聯盟黨團負責召集協商，經記名表決未獲通過。表決結果名單附後(十)【在場委員'
    #)
  }
  
  message("urln=",urln," | 2 RegularExpression 處理前面的投票表決議案詳細說明 ", meetingname, url)
  #xpath<-"//p[(contains(.,'記名表決')) and (contains(.,'表決結果名單附後') or contains(.,'表決結果名單附件') or contains(.,'表決結果附後') or contains(.,'表決結果名單及時程表附後'))   ]"
  #bill_list<-xml_find_all(doc, xpath) %>%
  #xml_text() %>%
  bill_list<-customgrep(paragraph_list,'記名表決',value=TRUE) %>%
    customgrep("表決結果名單附後|表決結果名單附件|表決結果附後|表決結果名單及時程表附後",value=TRUE) %>%
    customgsub("(.+?[表決]{0,2}[結果]{0,2}[名單]{0,2}附{1}[後件]{0,1}。{0,1}[(（]{0,1}\\s{0,1}[—○一二三四五六七八九十\\d]{0,}\\s{0,1}[)）]{0,1}[〕】）]{0,}[；。]{0,1})","\\1 </endofp>",perl=TRUE) %>% 
    strsplit('</endofp>') %>% unlist() %>%
    customgrep("[表決]{0,1}[結果]{0,1}[名單]{0,1}附{1}[後|件]{1}",perl=TRUE,value=TRUE)
  #立法院第9屆第3會期第3次臨時會第2次會議議事錄
  #https://lci.ly.gov.tw/LyLCEW/html/agendarec1/03/09/03/03/02/LCEWC03_09030302.htm
  #前瞻計畫紀錄的案由正規表達式還要再修
  #0302: 筆數可以對上但是文字被切割太多的版本
  #(.+?[表決]{0,1}[結果]{0,1}[名單]{0,1}附{0,1}[後|件]{0,1}。{0,1}[(（]{0,1}\\s{0,1}[—○一二三四五六七八九十\\d]{0,}\\s{0,1}[)）]{0,1}[〕】）]{0,}[；。]{0,1})
  
  #反覆測試regexp
  #customgsub(test,"(.+[表決]{0,1}[結果]{0,1}[名單]{0,1}附{0,1}[後|件]{0,1}。{0,1}[(（]{0,1}\\s{0,1}[—○一二三四五六七八九十\\d]{0,}\\s{0,1}[)）]{0,1}[〕】）]{0,}[；。]{0,1})","\\1 </endofp>",perl=TRUE)
  #特別處理：立法院第6屆第2會期第9次會議議事錄
  if (term==6 & period==2 & meetingno==9) {
    merge<-list(c(2,3),c(4,5),c(6,7),c(8,9),c(10,11),c(12,13),c(14,15))
    new_bill_list<-c()
    for (merge_n in 1:length(merge)) {
      new_bill_list<-c(new_bill_list,paste0(bill_list[merge[[merge_n]]],sep="",collapse=""))
    }
    bill_list<-c(bill_list[c(1,2)],new_bill_list)
  }
  #特別處理：立法院第6屆第3會期第10次會議議事錄
  if (term==6 & period==3 & meetingno==10) {
    merge<-list(c(10,11,12))
    new_bill_list<-c()
    for (merge_n in 1:length(merge)) {
      new_bill_list<-c(new_bill_list,paste0(bill_list[merge[[merge_n]]],sep="",collapse=""))
    }
    bill_list<-c(bill_list[c(1:9)],new_bill_list)
  }
  
  ##立法院第9屆第3會期第3次臨時會第2次會議議事錄
  if (term==9 & period==3 & temp_meeting_no==3 & meetingno==2) {
    #stop("test if skip 立法院第9屆第3會期第3次臨時會第2次會議議事錄")
    paragraph_list<-c(paragraph_list[1:876],
                      "棄權者：0人",
                      "(97)「討論事項第一案通案部分黨團、委員提案第96項不予通過」部分：",
                      paragraph_list[878:2090],
                      "棄權者：0人",
                      "(298)「討論事項第一案歲出部分第3款第4項黨團、委員提案第725項復議不通過」部分：",
                      paragraph_list[2092:2725],
                      "棄權者：0人",
                      "(399)「討論事項第一案歲出部分第7款第4項黨團、委員提案第559項予以通過」部分：",
                      paragraph_list[2727:length(paragraph_list)]
    )
  }
  #特別處理：立法院第7屆第1會期第19次會議議事錄, 這裡很奇怪, https 和 http 版本不一樣
  if (term==7 & period==1 & temp_meeting_no==0 & meetingno==19) {
    paragraph_list <- c(paragraph_list[1:1449],
                        "二、上開決定除國民年金法部分條文修正草案，提出於本（第19）次會議處理外，其餘均照原決定通過。",
                        "各項記名表決結果名單",
                        paragraph_list[1451:1637],
                        "反對者：0人",
                        #"棄權者：0人",
                        paragraph_list[1639:length(paragraph_list)]
    )
  }
  if (term == 9 & period == 1 & temp_meeting_no == 1 & meetingno == 1) {
    paragraph_list <- c(paragraph_list[1:1458],
                        "棄權者：0人",
                        "(141)「討論事項第二案營業部分台灣中油股份有限公司有黨團、委員提案第十九項不予通過」部分：",
                        paragraph_list[1460:length(paragraph_list)]
    )
  }
  
  message("urln=",urln," | 3 處理記名表決區域 ", meetingname, url)
  roll_call_list_block_sp<-customgrep(paragraph_list,'各項記名表決結果名單|本次會議記名表決結果名單|本次會議表決結果名單|本次會議各項記名表決名單')
  if (length(roll_call_list_block_sp)<1) {##沒有表決名單的區域
    no_rollcall<-c(no_rollcall,url)
    next
  } else if (length(roll_call_list_block_sp)>2) {
    stop("Error at ", meetingname, url)
  }
  #roll_call_list_block<-paragraph_list[roll_call_list_block_sp:length(paragraph_list)]
  
  search_agree_pattern<-'贊成者[:：]{0,1}'
  agree_record_times<-count_value_times_in_vector(paragraph_list[roll_call_list_block_sp:length(paragraph_list)],search_agree_pattern)
  
  if (is.null(bill_list) | agree_record_times<1) {
    next
  }
  
  #檢查抓到的前半部詳細案由是否和後半部表決紀錄筆數是否對得上
  if (agree_record_times!=length(bill_list))
    stop("Error at ", meetingname, url, "bill lists and agree times does not match!")
  
  #定位投票紀錄區域
  for (locate_vote_record in 1:2) {
    #抓到 贊成者： 下一行
    agree_voters<-grep(search_agree_pattern,paragraph_list[roll_call_list_block_sp:length(paragraph_list)])+roll_call_list_block_sp
    #預防投票人數等於零，加上空白元素
    if (locate_vote_record==1) {
      paragraph_list<-insert.at(paragraph_list,agree_voters-1,rep("",agree_record_times))
    }
    #抓到bill short title, 有時候會因為只有一個表決案，沒有列案由，抓不到
    vote_bill_short_title<-grep('」部分',paragraph_list[roll_call_list_block_sp:length(paragraph_list)])+roll_call_list_block_sp-1
    if (length(vote_bill_short_title)<1) {
      vote_bill_short_title<-agree_voters-2
    }
    #抓到 反對者： 下一行
    dissent_voters<-grep('反對者[:：]',paragraph_list[roll_call_list_block_sp:length(paragraph_list)])+roll_call_list_block_sp
    #預防投票人數等於零，加上空白元素
    if (locate_vote_record==1) {
      paragraph_list<-insert.at(paragraph_list,dissent_voters-1,rep("",length(dissent_voters)))
    }
    #抓到 棄權者： 下一行
    giveup_voters<-grep('棄權者[:：]',paragraph_list[roll_call_list_block_sp:length(paragraph_list)])+roll_call_list_block_sp
    #giveup_voters<-setdiff(grep('棄權者：',roll_call_list_block),grep('棄權者：0人',roll_call_list_block))+roll_call_list_block_sp
    #預防投票人數等於零，加上空白元素
    if (locate_vote_record==1) {
      paragraph_list<-insert.at(paragraph_list,giveup_voters-1,rep("",length(giveup_voters)))
    }
  }
  
  #paragraph_list_length<-length(paragraph_list)
  
  
  
  for (billn in (1:length(agree_voters))) {
    #每一案掃描的範圍
    scan_area_start<-vote_bill_short_title[billn]+1
    scan_area_end<-if (is.na(vote_bill_short_title[billn+1])) {
      length(paragraph_list)
    } else {vote_bill_short_title[billn+1]-1}
    scan_area<-seq(from=scan_area_start,to=scan_area_end)
    #贊成者區域
    agree_voter_area_start<-intersect(agree_voters,scan_area)
    agree_voter_area_end<-intersect(dissent_voters-2,scan_area)
    agree_voter_area<-seq(from=agree_voter_area_start, to=agree_voter_area_end )
    #反對者區域
    dissent_voter_area_start<-intersect(dissent_voters,scan_area)
    dissent_voter_area_end<-intersect(giveup_voters-2,scan_area)
    dissent_voter_area<-seq(from=dissent_voter_area_start,to=dissent_voter_area_end)
    #棄權者區域
    giveup_voter_area_start<-intersect(giveup_voters,scan_area)
    giveup_voter_area_end<-scan_area_end
    
    #特別處理沒有適當組織的議事錄
    if (term==6 & period==3 & meetingno==10 & billn %in% c(3,4)) {
      #立法院第6屆第3會期第10次會議議事錄有二個表決紀錄名單空白只有一個
      #test:myown_vote_record_df[myown_vote_record_df$billn==4,]
      paragraph_list[scan_area]<-customgsub(paragraph_list[scan_area],"薛　凌\\s{1,2}田秋堇","薛　凌　田秋堇") %>%
        customgsub("　{1}([\u4e00-\u9fa5]{1}　{0,1}[\u4e00-\u9fa5]{1,2})","　　\\1")
    }
    #更改沒有適當組織的議事錄：從已經建立好的除錯資料集
    #modify_wrong_record_target<-filter(error_vote_record_from_name,term==term)
    #trace("filter", edit=T)
    #dplyr 會有問題
    #modify_wrong_record_target<-dplyr::filter(error_vote_record_from_name,
    #  term==as.character(term),
    #  period==as.character(period),
    #  meetingno==as.character(meetingno),
    #  temp_meeting_no==as.character(temp_meeting_no),
    #  billn==as.character(billn))
    #modify_wrong_record_target<-error_vote_record_from_name[error_vote_record_from_name$term==term,]
    #modify_wrong_record_target<-modify_wrong_record_target[modify_wrong_record_target$period==period,]
    #modify_wrong_record_target<-modify_wrong_record_target[modify_wrong_record_target$meetingno==meetingno,]
    #modify_wrong_record_target<-modify_wrong_record_target[modify_wrong_record_target$temp_meeting_no==temp_meeting_no,]
    #modify_wrong_record_target<-modify_wrong_record_target[modify_wrong_record_target$billn==billn,]
    modify_wrong_record_target<-filter(error_vote_record_from_name,term==term,period==period,meetingno==meetingno,temp_meeting_no==temp_meeting_no,billn==billn)
    nrow_modify_wrong_record_target<-nrow(modify_wrong_record_target)
    if (nrow_modify_wrong_record_target>0) {
      ### for (modify_wrong_record_target_i in 1:nrow_modify_wrong_record_target) {
      ### message("urln=",urln," | 4 除錯：原議事錄投票區塊第",billn,"案有文字結構錯誤處 modify ",modify_wrong_record_target_i," times. ", meetingname, url)
      ### modify_wrong_record_target_wrongname<-modify_wrong_record_target$legislator_name[modify_wrong_record_target_i]
      ### modify_wrong_record_target_correctname<-modify_wrong_record_target$correct_legislator_name[modify_wrong_record_target_i]
      ### paragraph_list[scan_area]<-customgsub(paragraph_list[scan_area],modify_wrong_record_target_wrongname,modify_wrong_record_target_correctname)
      ### }
      message("urln=",urln," | 4 除錯：原議事錄投票區塊第",billn,"案有文字結構錯誤處 modify ",nrow_modify_wrong_record_target," times. ", meetingname, url)
      paragraph_list[scan_area]<-stri_replace_all_fixed(paragraph_list[scan_area],modify_wrong_record_target$legislator_name,modify_wrong_record_target$correct_legislator_name, vectorize_all=FALSE)
    }
    paragraph_list[scan_area]<-customgsub(paragraph_list[scan_area],"(Kolas Yotaka).+?([\u4e00-\u9fa5]{3})","\\1　　\\2")
    paragraph_list[scan_area]<-customgsub(paragraph_list[scan_area],"(高潞)(.+)?(Pacidal{1}).+?([\u4e00-\u9fa5]{3})","\\1\\2\\3　　\\4")
    #if (term==9 & period==4 & meetingno %in% c(8,10) & billn %in% c(7,9)) {
    #}
    #if (term==9 & period==3 & meetingno==2 & temp_meeting_no %in% c(1,3) ) {
    
    #}
    #search Kolas Yotaka　　      洪宗熠
    
    if (length(giveup_voter_area_start)==0) {
      exact_giveup_voter<-c()
    } else {
      giveup_voter_area<-seq(from=giveup_voter_area_start,to=giveup_voter_area_end)
      message("urln=",urln," | 5 有棄權者 ", meetingname, url)
      exact_giveup_voter<-paragraph_list[giveup_voter_area] %>%
        strsplit('　　') %>% unlist() %>% trimws() %>%
        customgsub("[\r\n]+","") %>%
        customgrep("[\u4e00-\u9fa5]",value=TRUE) %>%
        stri_replace_all_fixed("　","") %>%
        trimws()
    }
    message("urln=",urln," | 6 抓取同意者及反對者（第", billn, "案）", meetingname, url)
    exact_agree_voter<-paragraph_list[agree_voter_area] %>%
      strsplit('　　') %>% unlist() %>% trimws() %>%
      customgsub("[\r\n]+","") %>%
      customgrep("[\u4e00-\u9fa5]",value=TRUE) %>%
      stri_replace_all_fixed("　","") %>%
      trimws()
    
    exact_dissent_voter<-paragraph_list[dissent_voter_area] %>%
      strsplit('　　') %>% unlist() %>% trimws() %>%
      customgsub("[\r\n]+","") %>%
      customgrep("[\u4e00-\u9fa5]",value=TRUE) %>%
      stri_replace_all_fixed("　","") %>%
      trimws()
    #exact_giveup_voter<-intersect(giveup_voters,checkarea) #沒有交集會傳回integer(0)
    #exact_dissent_voter<-dissent_voters[n]
    #exact_giveup_voter<-ifelse(is.integer(exact_giveup_voter),
    #                           FALSE,
    #                           str_split(paragraph_list[exact_giveup_voter],'　　')
    #)
    billresult<-ifelse(length(exact_agree_voter)>length(exact_dissent_voter),"Passed","NotPassed")
    exact_giveup_voter_df<-if (length(exact_giveup_voter)==0) {
        data.frame()
      } else {
        data.frame("votedecision"="棄權",
              "legislator_name"=exact_giveup_voter,
              "term"=term,
              "period"=period,
              "meetingno"=meetingno,
              "temp_meeting_no"=temp_meeting_no,
              "billn"=billn,
              "billcontent"=bill_list[billn],
              "billresult"=billresult,
              "url"=url,
              "urln"=urln,
              "date"=date
        ) %>%
          mutate_at(c("term","period","meetingno","temp_meeting_no","billn"),funs(as.integer)) %>%
          mutate_at(c("legislator_name","billcontent","url","date"),funs(as.character)) %>%
          replace_troublesome_names()
      }
    
    exact_agree_voter_df<-if (length(exact_agree_voter)==0) {
      data.frame()
    } else {
      data.frame("votedecision"="贊成",
            "legislator_name"=exact_agree_voter,
            "term"=term,
            "period"=period,
            "meetingno"=meetingno,
            "temp_meeting_no"=temp_meeting_no,
            "billn"=billn,
            "billcontent"=bill_list[billn],
            "billresult"=billresult,
            "url"=url,
            "urln"=urln,
            "date"=date
      )  %>%
        mutate_at(c("term","period","meetingno","temp_meeting_no","billn"),funs(as.integer)) %>%
        mutate_at(c("legislator_name","billcontent","url","date"),funs(as.character)) %>%
        replace_troublesome_names()
    }
    exact_dissent_voter_df<-if (length(exact_dissent_voter)==0) {
      data.frame()
    } else {
      data.frame("votedecision"="反對",
            "legislator_name"=exact_dissent_voter,
            "term"=term,
            "period"=period,
            "meetingno"=meetingno,
            "temp_meeting_no"=temp_meeting_no,
            "billn"=billn,
            "billcontent"=bill_list[billn],
            "billresult"=billresult,
            "url"=url,
            "urln"=urln,
            "date"=date
      ) %>%
        mutate_at(c("term","period","meetingno","temp_meeting_no","billn"),funs(as.integer)) %>%
        mutate_at(c("legislator_name","billcontent","url","date"),funs(as.character)) %>%
        replace_troublesome_names()
    }
    
    #exact_agree_voter<-paragraph_list[from] %>%
    #  strsplit('　　') %>% getElement(1)
    #exact_dissent_voter<-paragraph_list[exact_dissent_voter] %>%
    #  strsplit('　　') %>% getElement(1)
    attend_but_not_vote_df<-data.frame(
      "term"=term,
      "period"=period,
      "meetingno"=meetingno,
      "temp_meeting_no"=temp_meeting_no,
      "billn"=billn,
      "billcontent"=bill_list[billn],
      "billresult"=billresult
    ) %>%
      right_join(attendlegislator) %>%
      mutate("votedecision"="未投票") %>%
      anti_join_with_nrow_zero(exact_giveup_voter_df,by=c("term","period","meetingno","temp_meeting_no","billn","legislator_name")) %>%
      anti_join_with_nrow_zero(exact_agree_voter_df,by=c("term","period","meetingno","temp_meeting_no","billn","legislator_name")) %>%
      anti_join_with_nrow_zero(exact_dissent_voter_df,by=c("term","period","meetingno","temp_meeting_no","billn","legislator_name"))

    leavelegislator_df<-if(nrow(leavelegislator)>0) {
      data.frame(
        "term"=term,
        "period"=period,
        "meetingno"=meetingno,
        "temp_meeting_no"=temp_meeting_no,
        "billn"=billn,
        "billcontent"=bill_list[billn],
        "billresult"=billresult
      ) %>%
        right_join(leavelegislator) %>%
        mutate("votedecision"="未出席") %>%
        anti_join_with_nrow_zero(exact_giveup_voter_df,by=c("term","period","meetingno","temp_meeting_no","billn","legislator_name")) %>%
        anti_join_with_nrow_zero(exact_agree_voter_df,by=c("term","period","meetingno","temp_meeting_no","billn","legislator_name")) %>%
        anti_join_with_nrow_zero(exact_dissent_voter_df,by=c("term","period","meetingno","temp_meeting_no","billn","legislator_name"))
    } else {
      data.frame()
    }
    myown_vote_record_df<-bind_rows(
      myown_vote_record_df,
      exact_agree_voter_df,
      exact_dissent_voter_df,
      exact_giveup_voter_df,
      attend_but_not_vote_df,
      leavelegislator_df
    )

  }
  
  
}

myown_vote_record_df<-filter(myown_vote_record_df,!is.na(legislator_name))
#save(myown_vote_record_df,file="myown_vote_record_df.RData")



##debug area
url_urln_df<-data.frame("urln"=1:nrow(meetingdata),
                        "url"=as.character(meetingdata$url),
                        "termmeetingtime"=meetingdata$termmeetingtime,
                        "term"=as.integer(meetingdata$term),
                        "period"=as.integer(meetingdata$period),
                        "meetingno"=as.integer(meetingdata$meetingno)
)
#處理中文字間隔有問題的部分
#廖國棟 Sufin．Siluko
#劉銓忠費鴻泰
#testdf<-distinct(complete_vote_record,term,period,meetingno,temp_meeting_no,billn,billcontent,billresult,url,date)
#write_excel_csv(testdf,path="myownvotebill.csv")



#errorposition<-customgrep(myown_vote_record_df$legislator_name,"　")
errorposition<-distinct(myown_vote_record_df,legislator_name)
#errorposition<-errorposition[c(377:384),]
#testerrorrows<-myown_vote_record_df[errorposition,] %>%
#  distinct(errorrows,legislator_name)
testerrorrows<-as.character(errorposition[c(229:231),])
#  
error_vote_record_from_name_append<-filter(myown_vote_record_df,legislator_name %in% testerrorrows)
error_vote_record_from_name_append<-error_vote_record_from_name_append[,c(2:7)] %>%
  cbind(correct_legislator_name="")
error_vote_record_from_name<-rbind(error_vote_record_from_name,error_vote_record_from_name_append)
#error_vote_record_from_name<-error_vote_record_from_name[,c()]
#error_vote_record_from_name$term<-as.integer(as.character(error_vote_record_from_name$term))
#error_vote_record_from_name$period<-as.integer(as.character(error_vote_record_from_name$period))
#error_vote_record_from_name$meetingno<-as.integer(as.character(error_vote_record_from_name$meetingno))
#error_vote_record_from_name$urln<-as.integer(error_vote_record_from_name$urln)
#where_error_occurs<-left_join(error_vote_record_from_name,url_urln_df)
#write_csv(error_vote_record_from_name,"error_vote_record_from_name.csv")
#
#輸出純案由資料集
if (FALSE) {
  vote_bills_df<-distinct(myown_vote_record_df,term,period,meetingno,temp_meeting_no,billn,billcontent,url,billresult,date)
  #vote_bills_df$term<-as.integer(vote_bills_df$term)
  #vote_bills_df$period<-as.integer(vote_bills_df$period)
  #vote_bills_df$meetingno<-as.integer(vote_bills_df$meetingno)
  #vote_bills_df$temp_meeting_no<-as.integer(vote_bills_df$temp_meeting_no)
  #vote_bills_df$billn<-as.integer(vote_bills_df$billn)
  #vote_bills_df$date<-as.character(vote_bills_df$date)
  #vote_bills_df$billcontent<-as.character(vote_bills_df$billcontent)
  write.foreign(vote_bills_df,"myownvotebill.datafile.txt","myownvotebill.codefile.sps",package="SPSS")
  write_csv(vote_bills_df,path="myownvotebill.csv")
}
#
#
#
#
#
#
#
#

testdf<-read.xlsx(file="votingdf_datafile_myown.xlsx",sheetIndex=1,startRow = 1,endRow = 3574,header = T,encoding = "UTF-8")

#save(myown_vote_record_df,file="myown_vote_record_df.RData")
#save(myown_vote_record_df,file="/mnt/e/Software/scripts/R/vote_record/myown_vote_record_df.RData")