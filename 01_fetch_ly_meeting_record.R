if (!("benchmarkme" %in% rownames(installed.packages()))) try(install.packages("benchmarkme"))
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-try(paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model))
t_sessioninfo_running_with_cpu_locale<-try(gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2])))
source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/shared_functions.R"), encoding="UTF-8")
meetingurldata<-paste0(dataset_in_scriptsfile_directory, "meetingrecord.xlsx") %>%
  openxlsx::read.xlsx(sheet = 1) %>%
  dplyr::filter(!kind %in% c("談話會","選舉會議"))
meetingurldata_urlrange<-4:13 #需要的欄位
meetingdata_range<-19:28
big5headpattern<-'text/html; charset=big5'
utf8headpattern<-'text/html; charset=utf-8'
#parallel_method<-"socks"

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
  method=parallel_method,
  exportvar=c("meetingurldata","meetingurldata_urlrange","custom_read_file","customgrepl","custom_detect_and_transform_utf8"),
  exportlib=c("base",lib),
  outfile=paste0(dataset_file_directory, "rdata", slash, "parallel_handling_process-", t_sessioninfo_running_with_cpu, ".txt"),
  mc.set.seed = TRUE
)  %>%
  list_of_vec_asmanyrows_to_df() %>%
  dplyr::mutate_all(as.character)
#check: utf8::utf8_valid(fetchmeetingdata$HTML5DATA[10])
#save(fetchmeetingdata,file=paste0(dataset_in_scriptsfile_directory, "fetchmeetingdata.RData"))
load(file=paste0(dataset_in_scriptsfile_directory, "fetchmeetingdata.RData"), verbose=TRUE)
#load(paste0(dataset_in_scriptsfile_directory, "fetchmeetingdata.RData", sep = "")) #476 obs at 1233 now 481


#I think there are two variables here: the sequence of bytes and the declared encoding.
##If the declared encoding is 'UTF-8'and the sequence of bytes is valid UTF-8 (see stri_enc_isutf8), then it's valid
##        and the sequence of bytes is not valid UTF-8, then it's invalid
#If the declared encoding is 'unknown'
##and all bytes in the sequence are less than 127 (see stri_enc_mark), then we can assume it's ASCII and thus embeddable in UTF-8 and valid
##        and not all bytes in the sequence are less than 127, then it could either be UTF-8 or some non-UTF-8 extended ASCII, so it's invalid
#If the declared encoding is anything else, then it's invalid


#meetingdata_df<-data.frame()
meetingdata<-dplyr::bind_cols(meetingurldata,fetchmeetingdata) %>%
  lapply(1:nrow(.), FUN=function (split_i, dataX,...) {
    #for (split_i in 1:length(meetingdata)) {  #debug專用，可以有螢幕輸出 #length(meetingdata)
    #dataX<-meetingdata[[split_i]] #debug專用，可以有螢幕輸出
    term<-dataX[split_i,"term"]
    error<-""
    message("starting! split_i=",split_i,"; term=",dataX[split_i,"term"],"; cols=",length(dataX[split_i,]),"; termmeetingtime=",dataX[split_i,"termmeetingtime"],"; names=",paste0(names(dataX),sep=","))
    tmpcontent<-dataX[split_i,meetingdata_range]
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
    if (dataX$kind[split_i]=="選舉會議") { #會沒有內容，隨便指定
      filter_result<-1
    } else if (length_filter_result<1) {
      stop("Error at ", dataX[split_i,1], dataX[split_i,2], "length<1")
    } else if (length_filter_result>1 | length_filter_result==1) {
      need_compare_content<-tmpcontent[filter_result] %>% stringr::str_length() %>% unique()
      if (length(need_compare_content)>1) {
        error<-paste0("Error at ", dataX[1], dataX[2], "length>2")
        #stop("Error at ", i, meetingurldata[i,1], meetingurldata[i,2])
      }
      filter_result<-filter_result[1]
    } else {
      stop("Error at ", i, dataX[split_i,1], dataX[split_i,2])
    }
    needcontent<-tmpcontent[1,filter_result]
    needurl<-dataX[split_i,meetingurldata_urlrange][filter_result]
    needelement<-c("kind","termmeetingtime","date","term","period","meetingno","temp_meeting_no")
    needcvec<-c(dataX[split_i,needelement],needurl,needcontent,error) %>%
      stringi::stri_trim_both() %>%
      magrittr::set_names(c(needelement,"url","content","fetchcontenterror"))
    needcvec['content'] %<>% as.character()
    #meetingdata_df %<>% rbind(needcvec)
    #colnames(meetingdata_df)<-names(needcvec)
    return(needcvec)
  },
  dataX=.
  #meetingurldata=meetingurldata,
  #meetingurldata_urlrange=meetingurldata_urlrange,
  #custom_read_file=custom_read_file,
  #fetchmeetingdata=fetchmeetingdata,
  #meetingdata_range=meetingdata_range
  ) %>%
  list_of_vec_asmanyrows_to_df() %>%
  dplyr::mutate_at("content", as.character)
#check html content
#write_file(as.character(meetingdata[1,c('content')]), path=paste(dataset_file_directory, "rdata", slash,  "checkcontent.txt", sep = ""), append = FALSE)
#write_file(content, path=paste(dataset_file_directory, "rdata", slash,  "checkcontent.txt", sep = ""), append = FALSE)

#出錯處 at 312 臨時會 第08屆 第04會期 第01次臨時會 第01次會議 or 313
save(meetingdata, file = paste0(dataset_in_scriptsfile_directory, "meetingdata.RData", sep = "") )


#會議記錄文件連結（非議事錄）
sapply(1:20, function(X) {paste0("http://data.ly.gov.tw:80/odw/openDatasetJson.action?id=41&selectTerm=all&page=",X)}) %>%
  lapply(jsonlite::fromJSON) %>%
  lapply(magrittr::extract2, 1) %>%
  {
    magrittr::extract(., which(sapply(., class)=="data.frame"))
  } %>%
  dplyr::bind_rows() %>%
  dplyr::rename(period=sessionPeriod, meetingno=sessionTimes, temp_meeting_no=meetingTimes) %>%
  dplyr::select(-temp_meeting_no) %>%
  dplyr::left_join(., {
    paste0(dataset_file_directory, "votingdf_datafile_myown_englished.xlsx", sep="") %>%
      openxlsx::read.xlsx(sheet = 1) %>%
      dplyr::distinct(temp_meeting_no, date) %>%
      dplyr::mutate_at("date", ~gsub("/", "", .) ) %>%
      dplyr::mutate_at("date", ~gsub("[\\s]", "", ., perl=TRUE) )
  }, by=c("meetingDate"="date")) %>%
  dplyr::mutate_at(c("term","period","temp_meeting_no","meetingno"), as.character) %>%
  dplyr::mutate_at(c("term","period","temp_meeting_no","meetingno"), as.integer) %>%
  mutate_cond(is.na(temp_meeting_no), temp_meeting_no=0) %>%
  dplyr::mutate(meetingid=paste0(term,"-",period,"-",temp_meeting_no,"-",meetingno)) %>%
  dplyr::select(term, period, meetingno, temp_meeting_no, meetingid, everything()) %>%
  dplyr::filter(!grepl("(本期委員發言紀錄索引|本會召集委員|國是論壇|委員會會議|委員會聯席會議|選舉本院院長|選舉本院副院長|談話會|議事錄|勘誤)", subject, perl=TRUE)) %>%
  openxlsx::write.xlsx(paste0(dataset_in_scriptsfile_directory, "fullmeetingrecordlinks.xlsx"))
