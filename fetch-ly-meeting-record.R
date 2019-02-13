t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
filespath<-switch(
  paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model),
  "Windows8x64build9200Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="E:\\Software\\scripts\\R\\",
  "Windows10x64build17763Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="E:\\Software\\scripts\\R\\",
  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="/mnt/e/Software/scripts/R/",
  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"="/mnt/d/Software/scripts/",
  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\"
)
source(file = paste(filespath, "shared_functions.R", sep = ""))
meetingurldata<-paste0(filespath,"vote_record",slash,"meetingrecord.xlsx") %>%
  read.xlsx(sheet = 1) %>%
  filter(kind!="談話會")
meetingurldata_urlrange<-4:13 #需要的欄位
meetingdata_range<-19:28

fetchmeetingdata<-custom_parallel_lapply(
  data=meetingurldata[,meetingurldata_urlrange],
  f=function (X) {
    returnX<-sapply(X,custom_read_file) %>%
      as.character()
    message(" ｜ ")
    return(returnX)
  },
  exportvar=c("meetingurldata","meetingurldata_urlrange","custom_read_file"),
  exportlib=c("base",lib),
  outfile=paste0(dataset_file_directory,"rdata",slash,"parallel_handling_process-",t_sessioninfo_running,".txt"),
  firstlcaneed=lcaneed_independence_attitude,
  secondlcaneed=lcaneed_party_constituency,
  mc.set.seed = TRUE,
  mc.cores=parallel::detectCores()
) %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  (function(X) {
    names(X)<-paste(names(X),"DATA",sep="")
    return(X)
  })

#load(paste(dataset_file_directory,slash, "rdata", slash,  "fetchmeetingdata.RData", sep = "")) #476 obs at 1233 now 481


adplydata<-bind_cols(meetingurldata,fetchmeetingdata)
message(needdatanum)
meetingdata<-apply(adplydata,1,function (X) {
  #if (i %in% c(473)) {
  #      #415,416,417,428,429,445,447,448,469,470,471,472,473,474,475
  #  meetingurl<-c(meetingurl,NA)
  #  content<-c(content,NA)
  #  next
  #}
  term<-X["term"]
  error<-""
  message("starting! term=",X["term"],"; cols=",ncol(X),"; names=",names(X))
  #tmpcontent<-sapply(X[meetingurldata_urlrange],custom_read_file) %>%
  #  as.character()
  #tmpcontent<-fetchmeetingdata[needdatanum,]
  tmpcontent<-X[meetingdata_range]
  big5checkresult<-customgrep(tmpcontent,"text/html; charset=big5")
  if (length(big5checkresult)>0) {
    #tmpurl<-not_empty_filter(meetingurldata[i,meetingurldata_urlrange]) %>%
    #  sapply(custompaste0,"php7.0 /mnt/e/Software/scripts/R/vote_record/utf8out.php curl=TRUE target=",reverse=TRUE)
    #tmpcontent<-sapply(tmpurl,system,intern=TRUE) %>%
    #  sapply(custompaste0) %>% unlist() %>%
    #  customgsub("text/html; charset=big5","text/html; charset=utf-8")
    for (big5checkresult_i in 1:length(big5checkresult)) {
      message(big5checkresult_i)
      tmpcontent[big5checkresult[big5checkresult_i]] %<>% as.character() %>%
        stri_encode("Big5","UTF-8") 
    }
  }
  filter_result<-customgrep(tmpcontent,"html|body|span",value=FALSE,perl=TRUE)
  length_filter_result<-length(filter_result)
  tmpmeetingtitle<-c()
  if (length_filter_result<1) {
    stop("Error at ", X[1], X[2], "length<1")
  } else if (length_filter_result>1 | length_filter_result==1) {
    need_compare_content<-tmpcontent[filter_result] %>% stri_length() %>% unique()
    if (length(need_compare_content)>1) {
      error<-paste0("Error at ", X[1], X[2], "length>2")
      #stop("Error at ", i, meetingurldata[i,1], meetingurldata[i,2])
    }
    filter_result<-filter_result[1]
  } else {
    stop("Error at ", i, X[1], X[2])
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
  needurl<-X[meetingurldata_urlrange][filter_result]
  needelement<-c("kind","termmeetingtime","date","term","period","meetingno","temp_meeting_no")
  needdf<-c(X[needelement],needurl,needcontent,error) %>%
    trimws()
  names(needdf)<-c(needelement,"url","content","fetchcontenterror")
  return(needdf)
}) %>% t() %>% as.data.frame(stringsAsFactors=FALSE)


#出錯處 at 312 臨時會 第08屆 第04會期 第01次臨時會 第01次會議 or 313
save(meetingdata, file = paste(dataset_file_directory,slash, "rdata", slash,  "meetingdata.RData", sep = "") )
save(fetchmeetingdata, file = paste(dataset_file_directory,slash, "rdata", slash,  "fetchmeetingdata.RData", sep = "") )
#save(meetingdata,file="/mnt/e/Software/scripts/R/vote_record/meetingdata.RData")