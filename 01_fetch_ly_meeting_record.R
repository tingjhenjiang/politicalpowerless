lymeetingfetcher_class <- R6::R6Class("lymeetingfetcher", public = list(
  dataset_in_scriptsfile_directory = NULL,
  meetingurldata_filepath = NULL,
  fetchmeetingdata_filepath = NULL,
  meetingdata_filepath = NULL,
  fullmeetingrecordlinks_filepath = NULL,
  meetingdata_range = NULL,
  meetingurldata_urlrange = NULL,
  initialize = function(dataset_in_scriptsfile_directory="/mnt") {
    self$dataset_in_scriptsfile_directory <- dataset_in_scriptsfile_directory
    self$meetingurldata_filepath<-file.path(dataset_in_scriptsfile_directory, "meetingrecord.xlsx")
    self$fetchmeetingdata_filepath <- file.path(dataset_in_scriptsfile_directory, "fetchmeetingdata.rds")
    self$meetingdata_filepath <- file.path(dataset_in_scriptsfile_directory, "meetingdata.rds")
    self$fullmeetingrecordlinks_filepath <- file.path(dataset_in_scriptsfile_directory, "fullmeetingrecordlinks.xlsx")
    self$meetingdata_range <- 19:28
    self$meetingurldata_urlrange <- 4:13
  },
  get_filtered_meetingurldata = function() {
    filtered_meetingurldata<-self$meetingurldata_filepath %>%
      openxlsx::read.xlsx(sheet = 1) %>%
      dplyr::filter(!kind %in% c("談話會","選舉會議"))
    filtered_meetingurldata
  },
  get_fetchmeetingdata = function(loadExisted=TRUE,save=FALSE) {
    if (loadExisted==FALSE) {
      fetchmeetingdata <- self$parse_fetchmeetingdata()
    } else {
      fetchmeetingdata <- readRDS(file=self$fetchmeetingdata_filepath)
    }
    if (save==TRUE) {
      saveRDS(fetchmeetingdata, file=self$fetchmeetingdata_filepath)
    }
  },
  parse_fetchmeetingdata = function() {
    meetingurldata_urlrange<-self$meetingurldata_urlrange #需要的欄位
    big5headpattern<-'text/html; charset=big5'
    utf8headpattern<-'text/html; charset=utf-8'
    meetingurldata<-self$get_filtered_meetingurldata()
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
        outfile=file.path(dataset_file_directory, "rdata", paste0("parallel_handling_process-", t_sessioninfo_running_with_cpu, ".txt") ),
      )  %>%
      list_of_vec_asmanyrows_to_df() %>%
      dplyr::mutate_all(as.character)
    fetchmeetingdata
  },
  get_meetingdata = function(loadExisted=TRUE,preparedata=TRUE,save=FALSE,meetingurldata=NA,fetchmeetingdata=NA) {
    if (loadExisted==TRUE) {
      meetingdata <- readRDS(file=self$meetingdata_filepath)
    } else {
      meetingdata <- self$parse_meetingdata(preparedata=preparedata,meetingurldata=meetingurldata,fetchmeetingdata=fetchmeetingdata)
    }
    if (save==TRUE) {
      saveRDS(meetingdata, file = self$meetingdata_filepath )
    }
    meetingdata
  },
  parse_meetingdata = function(preparedata=TRUE,meetingurldata=NA,fetchmeetingdata=NA) {
    if (preparedata==TRUE) {
      meetingurldata <- self$get_filtered_meetingurldata()
      fetchmeetingdata <- self$get_fetchmeetingdata(loadExisted=TRUE,save=FALSE)
    }
    meetingurldata_urlrange<-self$meetingurldata_urlrange
    meetingdata_range<-self$meetingdata_range
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
    meetingdata
  },
  #會議記錄文件連結（非議事錄）
  get_meeting_links = function(save=FALSE) {
    df <- sapply(1:20, function(X) {paste0("http://data.ly.gov.tw:80/odw/openDatasetJson.action?id=41&selectTerm=all&page=",X)}) %>%
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
      dplyr::filter(!grepl("(本期委員發言紀錄索引|本會召集委員|國是論壇|委員會會議|委員會聯席會議|選舉本院院長|選舉本院副院長|談話會|議事錄|勘誤)", subject, perl=TRUE))
    if (save==TRUE) {
      openxlsx::write.xlsx(df,self$fullmeetingrecordlinks_filepath)
    }
    df
  }
))

#check: utf8::utf8_valid(fetchmeetingdata$HTML5DATA[10])
#save(fetchmeetingdata,file=paste0(dataset_in_scriptsfile_directory, "fetchmeetingdata.RData"))
#load(paste0(dataset_in_scriptsfile_directory, "fetchmeetingdata.RData", sep = "")) #476 obs at 1233 now 481


#I think there are two variables here: the sequence of bytes and the declared encoding.
##If the declared encoding is 'UTF-8'and the sequence of bytes is valid UTF-8 (see stri_enc_isutf8), then it's valid
##        and the sequence of bytes is not valid UTF-8, then it's invalid
#If the declared encoding is 'unknown'
##and all bytes in the sequence are less than 127 (see stri_enc_mark), then we can assume it's ASCII and thus embeddable in UTF-8 and valid
##        and not all bytes in the sequence are less than 127, then it could either be UTF-8 or some non-UTF-8 extended ASCII, so it's invalid
#If the declared encoding is anything else, then it's invalid


#meetingdata_df<-data.frame()


#check html content
#write_file(as.character(meetingdata[1,c('content')]), path=paste(dataset_file_directory, "rdata", slash,  "checkcontent.txt", sep = ""), append = FALSE)
#write_file(content, path=paste(dataset_file_directory, "rdata", slash,  "checkcontent.txt", sep = ""), append = FALSE)

#出錯處 at 312 臨時會 第08屆 第04會期 第01次臨時會 第01次會議 or 313



