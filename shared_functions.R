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

lib<-c("stringi","XML","xml2","readr","plyr","dplyr","magrittr","openxlsx","rvest","parallel") #,"data.table","dtplyr"
#install.packages("xml2", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
#,"Rcmdr"
load_lib_or_install<-function (libs) {
  sapply(libs,function (X) {
    if(!require(X,character.only=TRUE)) {
      install.packages(X)
      require(X,character.only=TRUE)
    }
  })
}
#shared install.packages(c("DBI", "DescTools", "dplyr", "feather", "future", "ggplot2", "ggsci", "gtools", "here", "magrittr", "openxlsx", "rvest", "sparklyr", "SparkR", "survey", "utf8"))
#04 install.packages(c("benchmarkme", "DBI", "dplyr", "factoextra", "getPass", "Gifi", "homals", "magrittr", "MCMCpack", "mirt", "openxlsx", "psych", "random.polychor.pa", "reshape2", "RMariaDB", "stringr"))
#03 install.packages("future.apply")
#05 install.packages("labelled")
#06 install.packages(c("BaylorEdPsych", "Hmisc", "mice", "micemd", "VIM"))
load_lib_or_install(lib)
check_if_windows<-function () {
  backgroundinfo<-sessionInfo()
  running<-backgroundinfo$running
  return(grepl("Windows", backgroundinfo$running))
}
slash<-ifelse(check_if_windows(),"\\","/")
parallel_method<-ifelse(check_if_windows(),"socks","fork")

driverletter_prefixes <- c("C","D","E","F","G","V","X","Y","Z")
filespath <- ifelse(check_if_windows(),
  paste(driverletter_prefixes, ":\\Software\\scripts\\R\\vote_record\\",sep="", collapse=","),
  "/home/j/rscripts/vote_record/"
  ) %>%
  stri_split(regex=",") %>% unlist() %>% {.[sapply(.,dir.exists)]}
dataset_file_directory <- ifelse(
  check_if_windows(),
  paste0(driverletter_prefixes, ":\\Users\\dowba\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\",sep="", collapse=","),
  paste0(c(
    paste0("/mnt/", tolower(driverletter_prefixes), "/OneDrive/OnedriveDocuments/NTU/Work/thesis/dataset(2004-2016)/",sep=""),
    paste0("/mnt/", tolower(driverletter_prefixes), "/Users/dowba/OneDrive/OnedriveDocuments/NTU/Work/thesis/dataset(2004-2016)/",sep="")
    ),collapse=",")
  ) %>%
  stri_split(regex=",") %>% unlist() %>% {.[sapply(.,dir.exists)]} %>%
  ifelse(gtools::invalid(.), paste0(here::here(),slash,"data",slash), .)
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
  "Windows10x64build19041Intel(R) Core(TM) i7-9750H CPU @ 2.60GHz"=paste0(filespath, "data", slash),
  "Windows8x64build9200Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz"=paste0(filespath, "data", slash),
  "Windows10x64build17763Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz"=paste0(filespath, "data", slash),
  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"=paste0(filespath, "data", slash),
  "Ubuntu18.04.2LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"=paste0(filespath, "data", slash),
  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"=paste0(filespath, "data", slash),
  "Ubuntu18.04.2LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"=paste0(filespath, "data", slash),
  "Ubuntu18.04.3LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"=paste0(filespath, "data", slash),
  "Ubuntu18.04.4LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"=paste0(filespath, "data", slash),
  "Ubuntu16.04.6LTSIntel(R) Xeon(R) Platinum 8124M CPU @ 3.00GHz"=paste0(here::here(),slash,"data",slash),
  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\vote_record\\data\\",
  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2660 v4 @ 2.00GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\vote_record\\data\\",
  "Windows8x64build9200Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\vote_record\\data\\",
  "Windows10x64build17134Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\vote_record\\data\\"
) %>%
  {ifelse((gtools::invalid(.) | !dir.exists(.) | !file.exists(paste0(., "shared_functions.R") )), paste0(here::here(),slash,"data",slash), .)}

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
myremoteip <- tryCatch({read_html('https://www.myip.com/') %>%
      html_nodes(xpath = "//span[@id='ip']//text()") %>%
      html_text()},error=function (e) {
        read_html('https://www.whatismyip.com.tw/') %>%
          html_nodes(xpath = "//body/b/span//text()") %>%
          html_text()
      })
mysqldbhost <- if (t_sessioninfo_running_with_cpu_locale=="Ubuntu18.04.3LTSIntel(R)Core(TM)i7-9750HCPU@2.60GHzzh_TW.UTF-8") {
  "tjhome.crabdance.com" #gsub("nameserver ","", x=system("cat /etc/resolv.conf", intern=TRUE)[4])
} else if (t_sessioninfo_running_with_cpu_locale=="Ubuntu18.04.3LTSIntel(R)Core(TM)i5-7400CPU@3.00GHzzh_TW.UTF-8") {
  "192.168.10.200"
} else if (myremoteip=="140.112.7.192") {
  "tjhome.crabdance.com"
} else if (t_sessioninfo_running_with_cpu_locale=='Windows8x64build9200Intel(R)Core(TM)i7-9750HCPU@2.60GHzChinese(Traditional)_Taiwan.950') {
  "localhost"
} else {
  "tjhome.crabdance.com"
}
#localhostip_for_wsl<-gsub("nameserver ","", x=system("cat /etc/resolv.conf", intern=TRUE)[4])
#"192.168.10.202" #"140.112.7.192" #"192.168.16.1" localhost

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
ggplotapatheme=ggplot2::theme_bw()+
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        text=ggplot2::element_text(family='Arial'),
        legend.title=ggplot2::element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = ggplot2::element_line(color='black'),
        axis.line.y = ggplot2::element_line(color='black'))
ggplotscreeplot <- function(data, dimensionvar, eigenvaluevar) {
  dimensions <- magrittr::extract2(data, dimensionvar)
  breakv <- seq.int(from=min(dimensions), to=max(dimensions))
  #
  if (TRUE) {
    p = ggplot2::ggplot(data, ggplot2::aes(x=.data[[dimensionvar]], y=.data[[eigenvaluevar]])) + #, shape=type
      #Add lines connecting data points
      ggplot2::geom_line()+
      #Add the data points.
      ggplot2::geom_point(size=4)+
      #Label the y-axis 'Eigenvalue'
      ggplot2::scale_y_continuous(name='Eigenvalue')+
      #Label the x-axis 'Factor Number', and ensure that it ranges from 1-max # of factors, increasing by one with each 'tick' mark.
      ggplot2::scale_x_continuous(name='Factor Number', breaks=breakv )+
      #Manually specify the different shapes to use for actual and simulated data, in this case, white and black circles.
      ggplot2::scale_shape_manual(values=c(16,1)) +
      #Add vertical line indicating parallel analysis suggested max # of factors to retain
      #geom_vline(xintercept = parallel$nfact, linetype = 'dashed')+
      #Apply our apa-formatting theme
      ggplotapatheme
    #Call the plot. Looks pretty!
    print(p)
  }
}
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
  target<-unlist(extract(resultlist,returnnum))
  return(resultlist)
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
custom_shift_sqrt<-function(x) {
  minx<-min(x)
  if (minx<0) {
    x<-x-minx
  }
  y<-sqrt(x)
  midpoint<-mean(y)
  y<-y-midpoint
  return(y)
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
custom_plot<-function(df, fvar, weightvar="", usingsurveypkg=FALSE, fillcolor="", ...) {
  n_bins<-350
  singleclass<-class(extract2(df, fvar))
  randomcolors<-ggsci::pal_npg("nrc", alpha = 0.7)(10)
  randomcolors<-c(randomcolors,"lightblue","lightgreen")#,"lightred"
  if (fillcolor=="") fillcolor<-sample(randomcolors,1)
  if (weightvar!="") {
    if (class(weightvar)!="character") {
      ggplotweight<-weightvar
    } else {
      df_weight<-extract2(df,weightvar)
      sum_df_weight<-sum(df_weight)
      samplerepresents<-df_weight*sum_df_weight
      ggplotweight<-df_weight/sum_df_weight
    }
  }
  #https://cran.r-project.org/web/packages/srvyr/index.html
  #https://rpubs.com/mcchadreque/survey10
  if (weightvar!="" & usingsurveypkg==TRUE) {
    svdesi<-survey::svydesign(ids = ~1, data = df, weights = df_weight)
    switch(singleclass,
           "numeric"=survey::svyhist(as.formula(paste0("~",fvar)), svdesi, breaks=n_bins, col=fillcolor),
           "factor"={
             library(survey)
             a<- survey::svymean(as.formula(paste0("~",fvar)), svdesi)
             barplot(a, col=fillcolor)
           })
  } else {
    #mappingopt<-list(x=fvar, alpha=0.5)
    #if (weightvar!="") mappingopt<-rlist::list.append(mappingopt, weight = ggplotweight)
    mapping <-aes(
      x=.data[[fvar]],
      weight={
        if (weightvar=="") {
          1
        } else {
          ggplotweight #.data[[weightvar]]
        }
      })
    ggplotinit<-ggplot(df)
    statlayeropt<-list(mapping=mapping,
                       fill = fillcolor,
                       bins=n_bins,
                       alpha=0.5)
    statlayer<-do.call(switch(singleclass,
                              "numeric"=stat_bin,
                              "integer"=stat_bin,
                              "factor"=stat_count),
                       statlayeropt)
    if (FALSE) {
      geom_func<-switch(singleclass,
                        "numeric"=geom_histogram(mapping=mapping, stat="bin"), #, fill="lightblue"
                        "factor"=geom_bar(mapping=mapping, stat="identity")#fill="white", 
      )
    }
    ggplotlab<-labs(x = paste0("ggplot ",fvar))
    ggp_using_color<-scale_colour_brewer()#scale_color_brewer(palette="Pastel2")
    ggp_scale<-switch(singleclass,
           "numeric"=scale_fill_distiller(palette = "Pastel2"),#scale_fill_continuous(type = "gradient"),
           "factor"=scale_fill_brewer(palette = "Pastel2")
    )
    ggplotinit+statlayer+ggplotlab#+geom_func+scale_colour_brewer()+ggp_scale
  }
}
custom_notdfplot<-function(srcvector, weight=NULL, usingsurveypkg=FALSE, fillcolor="", ...) {
  df<-data.frame(
    "targetvector"=srcvector
  )
  if (!identical(weight,NULL)) {
    df<-cbind(df, "targetvectorweight"=weight)
    weight<-"targetvectorweight"
  } else {
    weight<-""
  }
  return(custom_plot(df, fvar="targetvector", weightvar=weight, usingsurveypkg=usingsurveypkg, fillcolor=fillcolor, ...))
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

custom_df_replaceinto_db<-function(dbconnect_info, db_table_name, with_df, columns=c()) {
  if (length(columns)==0) {
    columns<-names(with_df)
  }
  qmarks<-rep('?', times=length(columns)) %>%
    paste0(collapse=',')
  con <- do.call(DBI::dbConnect, dbconnect_info)
  replacesql <- paste0('`',columns,'`') %>%
    paste0(collapse=',') %>%
    paste0(
    'REPLACE INTO `',db_table_name,'` ',
    '(', ., ') VALUES (',
    qmarks,
    ')') %>%
    DBI::dbSendQuery(con, .)
  with_df %>% as.list() %>% unname() %>% DBI::dbBind(replacesql, .)
  DBI::dbClearResult(replacesql)  # release the prepared statement
  DBI::dbDisconnect(con)
}

custom_parallel_lapply<-function(X=list(), FUN=FUN, ..., method="socks", exportlib=c("base","magrittr","parallel"), exportvar=c(), outfile="", verbose=TRUE, mc.cores=parallel::detectCores()  ) {
  if (mc.cores==1) {
    return(lapply(X, FUN=FUN, ...))
  } else {
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

cn2num<-function(string){
  if(is.numeric(string)){
    return(string)
  }
  if(!is.na(as.numeric(string))) {
    return(as.numeric(string))
  }
  # '仟' => '千','佰' => '百','拾' => '十',
  string = gsub('仟', '千', string)
  string = gsub('佰', '百', string)
  string = gsub('拾', '十', string)
  num = 0
  wan = unlist(strsplit(string, '萬'))
  if (length(wan) > 1) {
    num = num+cn2num(wan[1]) * 10000
    string = wan[2]
  }
  qian = unlist(strsplit(string, '千'))
  if (length(qian) > 1) {
    num = num+cn2num(qian[1]) * 1000
    string = qian[2]
  }
  bai = unlist(strsplit(string, '百'))
  if (length(bai) > 1) {
    num = num+cn2num(bai[1]) * 100
    string = bai[2]
  }
  shi = unlist(strsplit(string, '十'))
  if (length(shi) > 1) {
    #num = num+cn2num(shi[1] ? shi[1] : '一') * 10
    num = num+cn2num(
      ifelse(gtools::invalid(shi[1]), '一', shi[1])
    )*10
    #string = shi[2] ? shi[2] : '零'
    string = ifelse(gtools::invalid(shi[2]), '零', shi[2])
  }
  ling = unlist(strsplit(string,'零'))
  if (length(ling) > 1) {
    string = ling[2]
  }
  d <- list(
    "一" = 1,"二" = 2,"三" = 3,"四" = 4,"五" = 5,"六" = 6,"七" = 7,"八" = 8,"九" = 9,
    "壹" = 1,"貳" = 2,"參" = 3,"肆" = 4,"伍" = 5,"陸" = 6,"柒" = 7,"捌" = 8,"玖" = 9,
    "贰" = 2,"叁" = 3,"陆" = 6,"两" = 2,
    "零" = 0,"0" = 0,"O" = 0,"o" = 0,"兩" = 2
  )
  return(num + d[[string]])
}

generate_weight_repeated_data<-function(single_survey_df, weight_reptimes_n_integer=0.5, surveyweightvar="myown_wr", needvars="") {
  #message(survey)
  min_myownwr<-min(extract2(single_survey_df, surveyweightvar))
  min_repeat_times<-1/min_myownwr #要讓最權重最小的觀察值出現一次的重複row倍數
  min_rep_weighted_myownwr<-extract2(single_survey_df, surveyweightvar)*min_repeat_times #所有觀察值的重複倍數
  #因為要讓最權重最小的觀察值出現一次的重複row倍數 套用在其他觀察值上 會出現小數，所以現在要計算重複row倍數的最小公倍數
  n_digits<-max(sapply(min_rep_weighted_myownwr,function (x) nchar(sub('^0+','',sub('\\.','',x)))))
  #n_digits<-9 #大於9似乎會有問題
  repeat { #為了防止求最小公倍數時出現問題所以往下求整數
    #message("n_digits is now ", n_digits)
    continue_to_minus<-FALSE
    ten_multiplier<-10^n_digits 
    tryCatch({
      integer_min_repeat_times<-min_rep_weighted_myownwr*ten_multiplier
      integer_min_repeat_times<-round(integer_min_repeat_times,digits = 0)
      integer_min_repeat_times<-as.integer(integer_min_repeat_times)
      lcm<-Reduce(f=DescTools::LCM, x=integer_min_repeat_times)
    }, warning=function (war) {
      #message(war)
      continue_to_minus<-TRUE
      #return(TRUE)
    }, error=function (err) {
      #message(err)
      continue_to_minus<-TRUE
      #return(TRUE)
    })
    n_digits<-n_digits-1
    if (continue_to_minus==TRUE) next
    if (is.numeric(lcm) | is.integer(lcm)) break
  }
  #message("continue_to_minus is ", continue_to_minus, " and lcm is ", class(lcm))
  lcm<-abs(lcm)
  adj_min_repeat_times<-lcm/min_myownwr
  adj_all_sample_rep_times<-extract2(single_survey_df, surveyweightvar)*adj_min_repeat_times
  single_survey_df$repeat_sample_times<-adj_all_sample_rep_times
  #message("adj_all_sample_rep_times is now ", length(adj_all_sample_rep_times))
  #message("uniq adj_all_sample_rep_times is now ", length(unique(adj_all_sample_rep_times)))
  weighted_adj_survey_data<-lapply(unique(adj_all_sample_rep_times), function(repeattimes, needvars, weight_reptimes_n_integer=1) {
    needrows<-which(adj_all_sample_rep_times==repeattimes)
    log_part<-log10(repeattimes)
    integers_of_log_part<-trunc(log_part)
    reptimes<-10^(log_part-integers_of_log_part+weight_reptimes_n_integer)
    reptimes<-round(reptimes,digits = 0)
    return(dplyr::slice(single_survey_df[,needvars], rep(needrows,reptimes)))
  },needvars=needvars, weight_reptimes_n_integer=weight_reptimes_n_integer) %>% dplyr::bind_rows()
  #needvars=c(clustering_var[[survey]],".imp")
  #message("weighted_adj_survey_data has ", nrow(weighted_adj_survey_data), " rows")
  return(weighted_adj_survey_data)
}

ret_std_legislators_data<-function(legislatorsxlsxpath = paste0(dataset_file_directory, "legislators.xlsx"), terms=5:9, elections_df=elections_df) {
  openxlsx::read.xlsx(legislatorsxlsxpath, sheet = 1, detectDates = TRUE) %>%
    dplyr::mutate(term=as.integer(term)) %>%  #mutate_at(c("term"), .funs = list(term = ~customgsub(term, "0(\\d{1})", "\\1", perl = TRUE))) %>% 
    dplyr::mutate(onboardDate=as.Date(onboardDate)) %>%
    dplyr::mutate(leaveDate=as.Date(leaveDate)) %>%
    mutate_cond(customgrepl(name,"簡東明") & term %in% c(8,9), name="簡東明Uliw．Qaljupayare") %>%
    mutate_cond(customgrepl(name,"Kolas"), name="谷辣斯．尤達卡Kolas．Yotaka") %>%
    mutate_cond(customgrepl(name,"王雪."), name="王雪峰") %>%
    mutate_cond(is.na(leaveDate) & term==2, leaveDate=as.Date(c("1996/1/31"))) %>% 
    mutate_cond(is.na(leaveDate) & term==3, leaveDate=as.Date(c("1999/1/31"))) %>% 
    mutate_cond(is.na(leaveDate) & term==4, leaveDate=as.Date(c("2002/1/31"))) %>% 
    mutate_cond(is.na(leaveDate) & term==5, leaveDate=as.Date(c("2005/1/31"))) %>% 
    mutate_cond(is.na(leaveDate) & term==6, leaveDate=as.Date(c("2008/1/31"))) %>% 
    mutate_cond(is.na(leaveDate) & term==7, leaveDate=as.Date(c("2012/1/31"))) %>% 
    mutate_cond(is.na(leaveDate) & term==8, leaveDate=as.Date(c("2016/1/31"))) %>% 
    mutate_cond(is.na(leaveDate) & term==9, leaveDate=as.Date(c("2016/6/1"))) %>%  #調查開始日當作年資起算日
    dplyr::mutate(servingdayslong_in_this_term=difftime(leaveDate, onboardDate, units = c("days"))) %>% 
    dplyr::mutate_at("servingdayslong_in_this_term", as.integer) %>% 
    dplyr::group_by(name) %>%    #calculate overall service time in previous periods
    dplyr::mutate(seniority=cumsum(servingdayslong_in_this_term)-servingdayslong_in_this_term) %>%
    dplyr::ungroup() %>%
    dplyr::filter(term %in% terms) %>%
    dplyr::mutate_at(c("term"), as.character) %>%
    dplyr::inner_join(elections_df, by = c("name", "term", "sex"))  %>%
    dplyr::rename(legislator_sex=sex,
           legislator_party=party.x,
           election_party=party.y,
           legislator_age=age,
           legislator_name=name
    ) %>%
    dplyr::mutate_at(c("term"), as.numeric) %>%
    dplyr::mutate_at(c("legislator_sex","legislator_party","partyGroup","areaName","leaveFlag","incumbent","wonelection","election_party","elec_dist_type"), as.factor) %>%
    return()
}

#research_odbc_file<-"E:\\Software\\scripts\\R\\vote_record\\votingdf.sqlite.dsn"
#research_odbc<-"Research"
#research_odbc_ch <- odbcConnect(research_odbc, believeNRows = FALSE, rows_at_time = 1, DBMSencoding="UTF-8")
#df<-sqlQuery(research_ch,"SELECT * from bill")SELECT * from bill")a