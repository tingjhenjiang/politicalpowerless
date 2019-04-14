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
  "Windows8x64build9200Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\Downloads\\vote_record\\",
  "Windows10x64build17134Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\Downloads\\vote_record\\"
)
source(file = paste(filespath, "shared_functions.R", sep = ""))
meetingurldata<-paste0(filespath, "data", slash, "meetingrecord.xlsx") %>%
  read.xlsx(sheet = 1) %>%
  filter(kind!="談話會")
meetingurldata_urlrange<-4:13 #需要的欄位
meetingdata_range<-19:28
big5headpattern<-'text/html; charset=big5'
utf8headpattern<-'text/html; charset=utf-8'

library(parallel)
fetchmeetingdata<-meetingurldata[,meetingurldata_urlrange] %>%
  split(.,seq(nrow(.))) %>%
  custom_parallel_lapply(
  FUN=function (X,...) {
    returnX<-sapply(X,custom_read_file) %>%
      as.character() %>%
      custom_detect_and_transform_utf8()
    message(" ｜ ")
    names(returnX)<-paste(names(X),"DATA",sep="")
    return(returnX)
  },
  exportvar=c("meetingurldata","meetingurldata_urlrange","custom_read_file","customgrepl","custom_detect_and_transform_utf8"),
  exportlib=c("base",lib),
  outfile=paste0(dataset_file_directory, "rdata", slash, "parallel_handling_process-", t_sessioninfo_running, ".txt"),
  mc.set.seed = TRUE,
  mc.cores=parallel::detectCores()
)  %>%
  list_of_vec_asmanyrows_to_df() %>%
  mutate_all(as.character)
#check: utf8::utf8_valid(fetchmeetingdata$HTML5DATA[10])
#save(fetchmeetingdata,file=paste0(filespath, "data", slash, "fetchmeetingdata.RData"))
load(file=paste0(filespath, "data", slash, "fetchmeetingdata.RData"))
#load(paste0(filespath, "vote_record", slash, "fetchmeetingdata.RData", sep = "")) #476 obs at 1233 now 481


#I think there are two variables here: the sequence of bytes and the declared encoding.
##If the declared encoding is 'UTF-8'and the sequence of bytes is valid UTF-8 (see stri_enc_isutf8), then it's valid
##        and the sequence of bytes is not valid UTF-8, then it's invalid
#If the declared encoding is 'unknown'
##and all bytes in the sequence are less than 127 (see stri_enc_mark), then we can assume it's ASCII and thus embeddable in UTF-8 and valid
##        and not all bytes in the sequence are less than 127, then it could either be UTF-8 or some non-UTF-8 extended ASCII, so it's invalid
#If the declared encoding is anything else, then it's invalid


#meetingdata_df<-data.frame()
meetingdata<-bind_cols(meetingurldata,fetchmeetingdata) %>%
  split(.,seq(nrow(.))) %>%
  custom_parallel_lapply(FUN=function (X,...) {
  #for (split_i in 1:length(meetingdata)) {  #debug專用，可以有螢幕輸出 #length(meetingdata)
    #X<-meetingdata[[split_i]] #debug專用，可以有螢幕輸出
    term<-X["term"]
    error<-""
    #message("starting! split_i=",split_i,"; term=",X["term"],"; cols=",length(X),"; termmeetingtime=",X["termmeetingtime"],"; names=",paste0(names(X),sep=","))
    tmpcontent<-X[meetingdata_range]
    #big5checkresult<-customgrep(as.character(tmpcontent),big5headpattern)
    #if (length(big5checkresult)>0) {
    #  for (big5checkresult_i in 1:length(big5checkresult)) {
    #    #message(big5checkresult_i)
    #    tmpcontent[big5checkresult[big5checkresult_i]] %<>% customgsub(big5headpattern,utf8headpattern)
    #  }
    #}
    filter_result<-sapply(tmpcontent,function(X) {
      as.integer(stri_locate_first( X, regex="html|body|span")[,'start'])
    }) %>% {which(!is.na(.))}
    length_filter_result<-length(filter_result) #檢查有幾個欄位的網址有抓到資料
    tmpmeetingtitle<-c()
    if (length_filter_result<1) {
      stop("Error at ", X[1], X[2], "length<1")
    } else if (length_filter_result>1 | length_filter_result==1) {
      need_compare_content<-tmpcontent[filter_result] %>% stringr::str_length() %>% unique()
      if (length(need_compare_content)>1) {
        error<-paste0("Error at ", X[1], X[2], "length>2")
        #stop("Error at ", i, meetingurldata[i,1], meetingurldata[i,2])
      }
      filter_result<-filter_result[1]
    } else {
      stop("Error at ", i, X[1], X[2])
    }
    needcontent<-tmpcontent[1,filter_result]
    needurl<-X[meetingurldata_urlrange][filter_result]
    needelement<-c("kind","termmeetingtime","date","term","period","meetingno","temp_meeting_no")
    needcvec<-c(X[needelement],needurl,needcontent,error) %>%
      stri_trim_both()
    names(needcvec)<-c(needelement,"url","content","fetchcontenterror")
    needcvec['content'] %<>% as.character()
    #meetingdata_df %<>% rbind(needcvec)
    #colnames(meetingdata_df)<-names(needcvec)
    return(needcvec)
  } ,
  exportvar=c("meetingurldata","meetingurldata_urlrange","custom_read_file","fetchmeetingdata"),
  exportlib=c("base",lib),
  meetingdata_range=meetingdata_range,
  outfile=paste0(dataset_file_directory, "rdata", slash, "parallel_handling_process-",t_sessioninfo_running,".txt"),
  mc.set.seed = TRUE,
  mc.cores=parallel::detectCores()
  ) %>%
  list_of_vec_asmanyrows_to_df()
#check html content
#write_file(as.character(meetingdata[1,c('content')]), path=paste(dataset_file_directory, "rdata", slash,  "checkcontent.txt", sep = ""), append = FALSE)
#write_file(content, path=paste(dataset_file_directory, "rdata", slash,  "checkcontent.txt", sep = ""), append = FALSE)

#出錯處 at 312 臨時會 第08屆 第04會期 第01次臨時會 第01次會議 or 313
save(meetingdata, file = paste0(filespath, "data", slash, "meetingdata.RData", sep = "") )
