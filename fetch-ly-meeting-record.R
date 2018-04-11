filespath<-"E:\\Software\\scripts\\R\\"
filespath <-"/mnt/e/Software/scripts/R/"
source(file=paste(filespath,"shared_functions.R",sep=""))
meetingurldata<-read_csv(
  ifelse(check_if_windows(),
         "D:\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\vote_record\\meetingrecord.csv",
         "/mnt/d/OneDrive/OnedriveDocuments/NTU/Work/thesis/vote_record/meetingrecord.csv"
         )
)
#html1data<-sapply(meetingurldata$HTML1,custom_read_file)
#html2data<-sapply(meetingurldata$HTML2,custom_read_file)
#html3data<-sapply(meetingurldata$HTML3,custom_read_file)
#html4data<-sapply(meetingurldata$HTML4,custom_read_file)
meetingurldata_urlrange<-4:13

content<-c()
meetingurl<-c()
error_record<-c()
for (i in 1:nrow(meetingurldata)) {
   if (i %in% c(473)) {
        #415,416,417,428,429,445,447,448,469,470,471,472,473,474,475
    meetingurl<-c(meetingurl,NA)
    content<-c(content,NA)
    next
  }
  term<-meetingurldata$term[i]
  #tmpcontent<-c(
  #  (html1data[i]),
  #  (html2data[i]),
  #  (html3data[i]),
  #  (html4data[i])
  #)
  #tmphtmllink<-c(
  #  meetingurldata$HTML1,
  #  meetingurldata$HTML2,
  #  meetingurldata$HTML3,
  #  meetingurldata$HTML4
  #)
  tmpcontent<-sapply((meetingurldata[i,meetingurldata_urlrange]),custom_read_file) %>%
    as.character()
  big5checkresult<-customgrep(tmpcontent,"text/html; charset=big5")
  if (length(big5checkresult)>0) {
    #tmpurl<-not_empty_filter(meetingurldata[i,meetingurldata_urlrange]) %>%
    #  sapply(custompaste0,"php7.0 /mnt/e/Software/scripts/R/vote_record/utf8out.php curl=TRUE target=",reverse=TRUE)
    #tmpcontent<-sapply(tmpurl,system,intern=TRUE) %>%
    #  sapply(custompaste0) %>% unlist() %>%
    #  customgsub("text/html; charset=big5","text/html; charset=utf-8")
    for (big5checkresult_i in 1:length(big5checkresult)) {
      tmpcontent[big5checkresult[big5checkresult_i]]<-stri_encode(tmpcontent[big5checkresult[big5checkresult_i]],"Big5","UTF-8") 
    }
  }
  filter_result<-customgrep(tmpcontent,"html|body|span",value=FALSE)
  length_filter_result<-length(filter_result)
  tmpmeetingtitle<-c()
  if (length_filter_result<1) {
    stop("Error at ", i, meetingurldata[i,1], meetingurldata[i,2])
  } else if (length_filter_result>1 | length_filter_result==1) {
    need_compare_content<-tmpcontent[filter_result] %>% stri_length() %>% unique()
    if (length(need_compare_content)>1) {
      error<-c(error,paste0(as.character(meetingurldata[i,1:2]),sep="",collapse=""))
      #stop("Error at ", i, meetingurldata[i,1], meetingurldata[i,2])
    }
    filter_result<-filter_result[1]
  } else {
    stop("Error at ", i, meetingurldata[i,1], meetingurldata[i,2])
  }
  
  #for (ti in 1:6) {
  #  if ( !(ti %in% filter_result)) {
  #    tmpmeetingtitle<-c(tmpmeetingtitle,NA)
  #    next
  #  }
  #  doc <- read_html(tmpcontent[[ti]][1],encoding="UTF-8")
  #  xpath<-"//p[(contains(.,'次會議議事錄'))]"
  #  meetingname<-xml_find_all(doc, xpath) %>%
  #    xml_text() %>% getElement(1) %>%
  #    customgsub(" ","") %>% customgsub("\\r","") %>% customgsub("\\n","")
  #  tmpmeetingtitle<-c(tmpmeetingtitle,meetingname)
  #} 
  #filter_result<-customgrep(tmpmeetingtitle,meetingurldata[i,11],value=FALSE) %>%
  #  getElement(1)
  #filter_result<-customgrepl(tmpcontent,"html",value=FALSE) %>%
  #  which.max()
  needcontent<-tmpcontent[filter_result]
  needurl<-getElement(meetingurldata[i,meetingurldata_urlrange],filter_result)
  meetingurl<-c(meetingurl,needurl)
  content<-c(content,needcontent)
}

meetingdata<-data.frame(
  "kind"=meetingurldata$kind,
  "termmeetingtime"=meetingurldata$termmeetingtime,
  "date"=meetingurldata$date,
  "url"=meetingurl,
  "term"=meetingurldata$term,
  "period"=meetingurldata$period,
  "meetingno"=meetingurldata$meetingno,
  "temp_meeting_no"=meetingurldata$temp_meeting_no,
  "content"=content
  )

#出錯處 at 312 臨時會 第08屆 第04會期 第01次臨時會 第01次會議 or 313
save(meetingdata, file = paste(filespath, "vote_record", ifelse(check_if_windows(),"\\","/"),  "meetingdata.RData", sep = "") )
#save(meetingdata,file="/mnt/e/Software/scripts/R/vote_record/meetingdata.RData")