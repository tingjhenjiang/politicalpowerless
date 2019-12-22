#setwd("E:/Software/scripts/R")
#setwd("/mnt/e/Software/scripts/R")
#Sys.setlocale(category = "LC_ALL", locale = "zh_TW.UTF-8")
chooseCRANmirror(ind=which(grepl("cran.csie.ntu.edu.tw",getCRANmirrors()$URL)))
#reposurl<-getCRANmirrors()$URL
#for (i in reposurl) {
#  install.packages('feather', repo=i)
#}
#Sys.setenv(R_INSTALL_STAGED = FALSE)
#lib<-c("stringi","stringr","XML","xml2","rvest","htmltidy","curl","RCurl","gdata","readr","DBI","lazyeval","dplyr","rmarkdown","rticles","knitr","data.table","ggplot2","scales","reshape2","janitor","stargazer","xtable","apa","tesseract","pdftools","tiff","schoolmath","jsonlite","foreign","MASS","class","caret","tm","kernlab","jiebaR","RTextTools","tmcn","text2vec","RODBC","xlsx")
lib<-c("stringi","XML","xml2","readr","plyr","dplyr","magrittr","openxlsx","data.table","dtplyr")
#install.packages("xml2", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
#,"Rcmdr"
sapply(lib,function (X) {
  if(!require(X,character.only=TRUE)) {
    install.packages(X)
    require(X,character.only=TRUE)
  }
})
check_if_windows<-function () {
  backgroundinfo<-sessionInfo()
  running<-backgroundinfo$running
  return(grepl("Windows", backgroundinfo$running))
}
slash<-ifelse(check_if_windows(),"\\","/")

driverletter_prefixes <- c("C","D","E","F","G","V","X","Y","Z")
filespath <- ifelse(check_if_windows(),
                           paste(driverletter_prefixes, ":\\Software\\scripts\\R\\vote_record\\",sep="", collapse=","),
                           "/home/j/rscripts/vote_record/"
                           ) %>%
                  stri_split(regex=",") %>% unlist() %>% {.[sapply(.,dir.exists)]}
dataset_file_directory <- ifelse(check_if_windows(),
                            paste0(driverletter_prefixes, ":\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\",sep="", collapse=","),
                            paste0(c(
                              paste0("/mnt/", tolower(driverletter_prefixes), "/OneDrive/OnedriveDocuments/NTU/Work/thesis/dataset(2004-2016)/",sep=""),
                              paste0("/mnt/", tolower(driverletter_prefixes), "/Users/dowba/OneDrive/OnedriveDocuments/NTU/Work/thesis/dataset(2004-2016)/",sep="")
                            ),collapse=",")
                            ) %>%
                          stri_split(regex=",") %>% unlist() %>% {.[sapply(.,dir.exists)]} #
dataset_file_directory_rdata <- paste0(dataset_file_directory,"rdata",slash)
#filespath<-switch(
#  t_sessioninfo_running_with_cpu,
#  "Windows8x64build9200Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="E:\\Software\\scripts\\R\\vote_record\\",
#  "Windows10x64build17763Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="E:\\Software\\scripts\\R\\vote_record\\",
#  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="/mnt/e/Software/scripts/R/vote_record/",
#  "Ubuntu18.04.2LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="/mnt/e/Software/scripts/R/vote_record/",
#  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"="/home/j/rscripts/vote_record/",
#  "Ubuntu18.04.2LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"="/home/j/rscripts/vote_record/",
#  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"=paste0(correct_driveletter,":\\Software\\scripts\\R\\vote_record\\"),
#  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2660 v4 @ 2.00GHz"=paste0(correct_driveletter,":\\Software\\scripts\\R\\vote_record\\"),
#  "Windows8x64build9200Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"=paste0(correct_driveletter,":\\Software\\scripts\\R\\vote_record\\"),
#  "Windows10x64build17134Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"=paste0(correct_driveletter,":\\Software\\scripts\\R\\vote_record\\")
#)
dataset_in_scriptsfile_directory <- switch(
  t_sessioninfo_running_with_cpu,
  "Ubuntu18.04.3LTSIntel(R) Core(TM) i7-9750H CPU @ 2.60GHz"=paste0(filespath, "data", slash),
  "Windows8x64build9200Intel(R) Core(TM) i7-9750H CPU @ 2.60GHz"=paste0(filespath, "data", slash),
  "Windows8x64build9200Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz"=paste0(filespath, "data", slash),
  "Windows10x64build17763Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz"=paste0(filespath, "data", slash),
  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"=paste0(filespath, "data", slash),
  "Ubuntu18.04.2LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"=paste0(filespath, "data", slash),
  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"=paste0(filespath, "data", slash),
  "Ubuntu18.04.2LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"=paste0(filespath, "data", slash),
  "Ubuntu18.04.3LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"=paste0(filespath, "data", slash),
  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\vote_record\\data\\",
  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2660 v4 @ 2.00GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\vote_record\\data\\",
  "Windows8x64build9200Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\vote_record\\data\\",
  "Windows10x64build17134Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\vote_record\\data\\"
)
#dataset_file_directory <- switch(
#  t_sessioninfo_running_with_cpu,
#  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\",
#  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2660 v4 @ 2.00GHz"="C:\\Users\\r03a21033\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\",
#  "Windows8x64build9200Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="Y:\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\",
#  "Windows10x64build17134Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="Z:\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\",
#  "Windows8x64build9200Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz" = "D:\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\",
#  "Windows10x64build17763Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz" = "D:\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\",
#  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="/mnt/d/OneDrive/OnedriveDocuments/NTU/Work/thesis/dataset(2004-2016)/",
#  "Ubuntu18.04.2LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="/mnt/d/OneDrive/OnedriveDocuments/NTU/Work/thesis/dataset(2004-2016)/",
#  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"="/mnt/g/Users/dowba/OneDrive/OnedriveDocuments/NTU/Work/thesis/dataset(2004-2016)/",
#  "Ubuntu18.04.2LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"="/mnt/g/Users/dowba/OneDrive/OnedriveDocuments/NTU/Work/thesis/dataset(2004-2016)/"
#)
if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = "/usr/local/spark")
}
survey_data_title<-c("2004citizen","2010env","2010overall","2016citizen") %>% sort()
imputation_sample_i_s <- seq(1,5)
Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-8-oracle")
if (FALSE && t_sessioninfo_running_with_cpu=="Ubuntu18.04.2LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz" && FALSE) {
  sparkpackage<-"sparklyr"
  sparkconfiglist<-merge(sparklyr::spark_config(), list(
    sparklyr.log.console = TRUE,
    spark.driver.memory = "2g",
    spark.blockManager.port='59933',
    spark.broadcast.port='59934',
    spark.driver.port='59935',
    spark.executor.port='59936',
    spark.fileserver.port='59937',
    spark.replClassServer.port='59938',
    spark.shuffle.service.port='59939'
  ))
  switch (
    sparkpackage,
    "SparkR"={
      library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
      sparkR.session(master = "local[*]", sparkConfig=sparkconfiglist)
      #sparkR.stop()
    },
    "sparklyr"={
      library("sparklyr")
      ip<-grep("inet 192.168.",system("ifconfig", intern=TRUE))
      sc<-spark_connect(master = "spark://192.168.10.202:7077", version='2.4.0', config=sparkconfiglist, spark_home=Sys.getenv("SPARK_HOME"))
      #spark_web(sc)
      #spark_log(sc)
      #sparklyr::spark_disconnect(sc)
    }
  )
}

ntuspace_file_directory <- switch(
  t_sessioninfo_running_with_cpu,
  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\NTUSpace\\",
  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2660 v4 @ 2.00GHz"="C:\\NTUSpace\\",
  "Windows8x64build9200Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\NTUSpace\\",
  "Windows8x64build9200Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz" = "D:\\NTUSpace\\",
  "Windows10x64build17763Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz" = "D:\\NTUSpace\\",
  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="/mnt/d/NTUSpace/",
  "Ubuntu18.04.2LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="/mnt/d/NTUSpace/",
  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"="/mnt/g/NTUSpace/"
)
customloadsurveys <- function(X, path_prefix=dataset_file_directory_rdata) {
  path<-paste0(path_prefix, X, '_', survey_data_title,".feather")
  ret_list <- lapply(path, feather::read_feather)
  return(ret_list)
}
customsavesurveys <- function(X_list, prefix, path_prefix=dataset_file_directory_rdata) {
  path<-paste0(path_prefix, prefix, '_', survey_data_title,".feather")
  ret_list <- mapply(function(X,Y) {
    feather::write_feather(X,Y)
    message(Y)
  }, X=X_list, Y=path)
  return(ret_list)
}

custompaste0<-function(str,connect=c(),reverse=FALSE,sep="",collapse="") {
  if (reverse) {
    str<-c(connect,str)
  } else {
    str<-c(str,connect)
  }
  return(paste(str,sep=sep,collapse=collapse))
}
customtidyhtml<-function(str) {
  opts <- list(
    TidyDocType="auto",
    TidyMakeClean=TRUE,
    TidyHideComments=TRUE,
    TidyIndentContent=TRUE,
    TidyWrapLen=200,
    TidyXhtmlOut=TRUE,
    TidyHtmlOut=FALSE,
    TidyDropEmptyElems=TRUE,
    TidyDropEmptyParas=TRUE,
    TidyFixBackslash=TRUE
  )
  return(tidy_html(str, option=opts))
}
customcurl<-function(url,async=length(url),encoding="UTF-8") {
  opt<-curlOptions(followlocation=TRUE,verbose=TRUE,autoreferer=TRUE,maxredirs=999,timeout=3000,ssl.verifypeer=FALSE)
  return(getURI(url, .opts=opt, async=async, .encoding=encoding, curl=getCurlHandle(), .mapUnicode = TRUE))
}
customrecursivecurl<-function(url,failedpattern="NA",async=length(url),encoding="UTF-8") {
  firstresult<-customcurl(url)
  failedresultpos_one<-grepl(failedpattern,firstresult,perl=TRUE)
  failedresultpos_two<-(firstresult=="")
  failedresultpos<-(failedresultpos_one | failedresultpos_two)
  correctresultpos<-!failedresultpos
  if (length(firstresult[failedresultpos])>0) {
    continuefetchresult<-customrecursivecurl(url[failedresultpos],failedpattern=failedpattern,async=async,encoding=encoding)
    return(c(firstresult[correctresultpos],continuefetchresult))
  } else {
    return(firstresult[correctresultpos])
  }
}
customcurluntilnoerror<-function(url,fetchencoding="BIG-5",curl=curl) {
  #recursive fetch webpages umtil there is no error
  results<-customcurl(url,fetchencoding=fetchencoding) %>%
    sapply(strsplit,"<div class=\"leg03_body\">") %>%
    sapply(`[`,2) %>%
    sapply(custompaste0,"<div class=\"leg03_body\">",reverse=TRUE) %>%
    sapply(strsplit,"<td class=\"contentbgtop06\">") %>%
    sapply(`[`,1) %>%
    sapply(customgsub,pattern="&nbsp;",replacement="") %>%
    #sapply(customgsub,pattern="排版用表格",replacement="test_arrange") %>%
    sapply(customtidyhtml)
  wrongitems<-(grepl("NA",results))
  correctitems<-!wrongitems
  correctresults<-results[correctitems]
  fetchagainurl<-url[wrongitems]
  message(custompaste0(c("fetching following URL...",url)))
  if (length(fetchagainurl)>0) {
    adjresults<-customcurluntilnoerror(fetchagainurl,fetchencoding)
    return(c(correctresults,adjresults))
  } else {
    return(correctresults)
  }
}
custom_strsplit<-function(str, pattern, returnnum=1) {
  resultlist<-strsplit(as.character(str),pattern)
  target<-sapply(resultlist,`[[`,returnnum)
  return(target)
}
customgrep<-function(x,pattern,ignore.case=FALSE,perl=FALSE,value=FALSE,fixed=FALSE,useBytes=FALSE,invert=FALSE) {
  return(grep(pattern,x,ignore.case,perl,value,fixed,useBytes,invert))
}
customgrepl<-function(x,pattern,ignore.case=FALSE,perl=FALSE,value=FALSE,fixed=FALSE) {
  return(grepl(pattern,x,ignore.case,perl,value,fixed))
}
customgsub<-function(x,pattern,replacement,ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  return(gsub(pattern, replacement, x, ignore.case, perl, fixed, useBytes))
}
customreadfile<-function(targetfile,encoding="UTF-8") {
  connection<-file(targetfile,encoding=encoding)
  result<-sapply(connection,readLines) %>%
    sapply(custompaste0)
  return(result)
}
custominsertRow <- function(newrow, existingDF, r=nrow(existingDF)) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}
custommessage<-function(v,existingDF=data.frame()) {
  message(v)
  message("-----------\n")
}
count_value_times_in_vector<-function(arr,checkv,completematch=FALSE) {
  length_one<-grepl(checkv,arr)
  length_two<-(arr==checkv)
  length<-ifelse(completematch,sum(length_two),sum(length_one))
  return(length)
}
custom_read_file<-function(url, locale = default_locale()) {
  if (is.na(url)) {
    return(NULL)
  } else {
    message(URLdecode(url))
    retryterm<-'(Timeout|403)'
    content<-tryCatch({
      read_file(url,locale) #要執行的指令放這裡
    },warning = function(war){
      message("MY_WARNING:  ",war) #如果有warning則輸出warning,"MY_WARNING:  "這一行可以自己改
      if (customgrepl(war$message,retryterm)) {
        war<-custom_read_file(url)
      }
      return(war)
    },error = function(err) {
      message("MY_ERROR:  ",err)   #如果有error則輸出error,"MY_Error:  "這一行可以自己改
      if (customgrepl(err$message,retryterm)) {
        err<-custom_read_file(url)
      }
      return(err)
    })
    return(content)
  }
}
not_empty_filter <- function(x) {
  x<-as.character(x)
  notna<-!is.na(x)
  notnull<-!is.null(x)
  notempty<-!(x=="")
  not_length_zero<-x[length(x)>0]
  x<-x[notna]
  x<-x[notnull]
  return(x)
}
insert.at <- function(vect, pos, elems){
  #dots <- list(...)
  #dots<-inputlist
  stopifnot(length(elems)==length(pos))
  #result <- vector("list",2*length(pos)+1)
  #result[c(TRUE,FALSE)] <- split(a, cumsum(seq_along(a) %in% (pos+1)))
  #result[c(FALSE,TRUE)] <- dots
  #unlist(result)
  l = length(vect)
  j = 0
  for (i in 1:length(pos)){
    if (pos[i]==1) {
      vect = c(vect[1], elems[j+1], vect[2:l])
    } else if (pos[i] == l) {
      vect = c(vect, elems[j+1])
    } else {
      vect = c(vect[1:(pos[i]+j)], elems[j+1], vect[(pos[i]+j+1):(l+j)])
    }
    j = j+1
  }
  return(vect)
  #insert.at(c(1,2,3,4,5),c(1,2,3,5),c("a","b","c","d"))
}
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  condition[is.na(condition)] = FALSE
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}
#DF %>% mutate_cond(measure == 'exit', qty.exit = qty, cf = 0, delta.watts = 13)
mutate_last <- function(.data, ...) {
  n <- n_groups(.data)
  indices <- attr(.data, "indices")[[n]] + 1
  .data[indices, ] <- .data[indices, ] %>% mutate(...)
  .data
}
#DF %>% 
#  group_by(is.exit = measure == 'exit') %>%
#  mutate_last(qty.exit = qty, cf = 0, delta.watts = 13) %>%
#  ungroup() %>%
#  select(-is.exit)
path_to_survey_imputation_and_measurement_file<-paste0(dataset_file_directory,"merger_survey_dataset",slash,"imputationcomputingbasis.xlsx")

gcreset<-function() {
  gc(reset = TRUE)
}

findduplicatedrowsindf<-function(df,vard=c()) {
  if (length(var)>0) {
    df[duplicated(df[,vard]), ]
  } else {
    df
  }
}

#rJava安裝前要R CMD javareconf
if (check_if_windows()) {
  Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre-9.0.4')
  Sys.setenv(JAVA_OPTS='-Xms128M -Xmx512M')
  #library("rJava")
  #library("xlsx")
  library("openxlsx")
} else {
  #export LD_LIBRARY_PATH=/usr/lib/jvm/java-9-oracle/lib/server/
  Sys.setenv(JAVA_HOME='/usr/lib/jvm/java-9-oracle/bin/java')
  Sys.setenv(JAVA_OPTS='-Xms128M -Xmx512M')
  #library("rJava")
  #library("xlsx")
  library("openxlsx")
}
as_factor_to_integer<-function(f) {
  as.numeric(levels(f))[f]
}

t_sessioninfo_running_platformcore<-customgsub(sessionInfo()$running," ","") %>%
  customgsub("[>=()]","") %>%
  customgsub("Ubuntu16.04.4LTS|Ubuntu16.04.5LTS|Ubuntu18.04.1LTS","Linux") %>%
  customgsub("Windows7x64build7601ServicePack1|Windows10x64build17763|Windows8x64build9200","Windows")

custom_parallel_lapply<-function(X=list(), FUN=FUN, ..., method="socks", exportlib=c("base","magrittr","parallel"), exportvar=c(), outfile="", verbose=TRUE, mc.cores=parallel::detectCores()  ) {
  result<-switch(method,
    #as.character(grepl("Ubuntu",sessionInfo()$running)),
    "fork"=#function (X=list(), FUN, ..., mc.cores=parallel::detectCores() ) {
      mclapply(X=X, FUN=FUN, ..., mc.cores=mc.cores,mc.preschedule=FALSE )
    #  return(result)
    #}
    ,
    "socks" = {#function ( X=list(), FUN, ... ) {
      argumentstopass<-list(...)
      tryCatch({stopCluster(cl)},
        # 遇到 warning 時的自訂處理函數
        warning = function(msg) {
          message("tryCatch Original warning message while stopCluster:")
          message(paste0(msg,"\n"))
        },
        # 遇到 error 時的自訂處理函數
        error = function(msg) {
          message("tryCatch Original error message while stopCluster:")
          message(paste0(msg,"\n"))
        }
      )
      #library(MASS)
      if (verbose) {
        message("<===== at custom_parallel_lapply exportlib is ", exportlib, " and exportvar is ", exportvar, " and outfile is ", outfile, "=====>")
      }
      cl <- makeCluster(parallel::detectCores(),outfile=outfile)
      sapply(exportlib,function(needlib,cl) {
        if (verbose) {message("cluster calling ", needlib, " at ", substr(as.character(cl),6,13) )}
        clusterCall(cl=cl, library, needlib, character.only=TRUE)
      },cl=cl)
      clustersessioninfopkgs<-clusterCall(cl=cl, sessionInfo) %>%
        sapply(function(X) {
          return(dplyr::union(X$basePkgs,names(X$otherPkgs)) %>% paste0(collapse=" "))
        })
      if (verbose) {message("overall libraries called are ", clustersessioninfopkgs, " at ",substr(as.character(cl),6,13) )    }
      if (length(exportvar)>0) {
        if (verbose) {message("cluster exporting: ", paste0(exportvar, collapse=" "), " at ", substr(as.character(cl),6,13) )}
        clusterExport(cl, varlist=c("exportlib",exportvar,"exportvar","outfile"), envir=environment())#
      }
      if (length(argumentstopass)>0) {
        #lapply(argumentstopass,function(needvar,cl) {
        #  clusterExport(cl, needvar, envir=environment())
        #},cl=cl)#
      }
      returndata<-parLapply(
        cl,
        X=X,
        fun=FUN,
        ...)
      stopCluster(cl)
      #return(returndata)
      returndata
    }
  )
}

vhead<- function(X) {
  View(head(X))
}

select_and_fill_nonexistcol <- function(fundf,colVec) {
  fundf <- fundf[intersect(colVec, names(fundf))]
  fundf[setdiff(colVec, names(fundf))] <- NA
  fundf
}

list_of_vec_asmanyrows_to_df <- function(X) {
  needdf <- as.data.frame(
          matrix(
            data=unlist( X ), nrow=length(X), ncol=length(X[[1]]), byrow=T,
            dimnames=list(NULL, names( X[[1]] ) )
          )
        ) 
  return(needdf)
}

custom_detect_and_transform_utf8<-Vectorize(FUN=function(srcstr) {
  if ((identical(srcstr,NA)) | (identical(srcstr,NULL))) {
    #do nothing
  } else {
    checkvaludutf8result<-utf8::utf8_valid(srcstr)
    if (isFALSE(checkvaludutf8result)) {
      srcstr <- rvest::repair_encoding(srcstr,from = 'Big5')
    }
    #srcstr<-stri_split_lines(srcstr)# %>%
      #lapply(function(srcstr_lines) {
      #  checkvaludutf8result<-utf8::utf8_valid(srcstr_lines)
      #  needtorepairlines<-which(isFALSE(checkvaludutf8result))
      #  srcstr_lines[needtorepairlines] %<>% rvest::repair_encoding(from = 'Big5')
      #  srcstr<-paste0(srcstr_lines,collapse="\n")
      #}) %>%
      #unlist()
    #ifdeclareutf8<-stri_enc_mark(srcstr)
    #ifconsistsutf8<-all(stri_enc_isutf8(srcstr))
    #if ((identical(ifdeclareutf8,"ASCII")) & (identical(ifconsistsutf8,TRUE))) {
    #  Encoding(srcstr)<-"UTF-8"
    #} else if(identical(ifconsistsutf8,FALSE)) {
    #  srcstr<-stri_enc_toutf8(srcstr)
    #}
  }
  return(srcstr)
})

list_allcombn_of_model <- function(vars,prefix="1~") {
  n <- length(vars)
  id <- unlist(
    lapply(1:n,
           function(i)combn(1:n,i,simplify=FALSE)
    )
    ,recursive=FALSE)
  Formulas <- sapply(id,function(i)
    paste(prefix, paste(vars[i],collapse="+"))
  )
  return(Formulas)
  #gregmisc::combinations
  #MuMIn::dredge
}

preserve_warning_tryCatch <- function(expr)
{
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}

custom_parallel_expr<-function(expr, name, mc.set.seed = TRUE, silent = FALSE,
                  mc.affinity = NULL, mc.interactive = FALSE,
                  detached = FALSE,
                  wait = TRUE, timeout = 0, intermediate = FALSE) {
  do_job<-parallel::mcparallel(expr, name, mc.set.seed, silent,
                     mc.affinity, mc.interactive,
                     detached)
  return(parallel::mccollect(do_job, wait, timeout, intermediate))
}

reset_multi_p <- function(t_sessioninfo_running = gsub("[>=()]","",gsub(" ","",sessionInfo()$running)) ) {
  future::plan(sequential)
  switch(as.character(customgrepl(t_sessioninfo_running, "Windows")),
         "TRUE"=future::plan(multisession),
         "FALSE"=future::plan(multicore) 
         #"FALSE"=plan(multisession)
  )
}

#research_odbc_file<-"E:\\Software\\scripts\\R\\vote_record\\votingdf.sqlite.dsn"
#research_odbc<-"Research"
#research_odbc_ch <- odbcConnect(research_odbc, believeNRows = FALSE, rows_at_time = 1, DBMSencoding="UTF-8")
#df<-sqlQuery(research_ch,"SELECT * from bill")SELECT * from bill")a