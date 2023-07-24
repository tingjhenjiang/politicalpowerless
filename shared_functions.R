#setwd("E:/Software/scripts/R")
#setwd("/mnt/e/Software/scripts/R")
#Sys.setlocale(category = "LC_ALL", locale = "zh_TW.UTF-8")
#chooseCRANmirror(ind=which(grepl("cran.csie.ntu.edu.tw",getCRANmirrors()$URL)))
#reposurl<-getCRANmirrors()$URL
#for (i in reposurl) {
#  install.packages('feather', repo=i)
#}
#Sys.setenv(R_INSTALL_STAGED = FALSE)
#lib<-c("stringi","stringr","XML","xml2","rvest","htmltidy","curl","RCurl","gdata","readr","DBI","lazyeval","dplyr","rmarkdown","rticles","knitr","data.table","ggplot2","scales","reshape2","janitor","stargazer","xtable","apa","tesseract","pdftools","tiff","schoolmath","jsonlite","foreign","MASS","class","caret","tm","kernlab","jiebaR","RTextTools","tmcn","text2vec","RODBC","xlsx")
#conda install -c conda-forge r-bestnormalize r-sandwich r-desctools r-feather r-rcpparmadillo r-gtools r-here r-hmisc r-mice r-miceadds r-mirt r-openxlsx r-psych r-quantreg r-sparklyr r-survey r-benchmarkme
lib<-c("stringi","readr","plyr","dplyr","magrittr","parallel") #,"openxlsx","XML","xml2","rvest","data.table","dtplyr"
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
  file.path(paste0(driverletter_prefixes, ":"), "Software","scripts","R","vote_record"),
  "/home/j/rscripts/vote_record/"
  ) %>%
  stri_split(regex=",") %>% unlist() %>% c(.,here::here()) %>% {.[sapply(.,dir.exists)]}
dataset_file_directory <- ifelse(
  check_if_windows(),
  file.path(paste0(driverletter_prefixes, ":"), "Users","dowba","OneDrive","OnedriveDocuments","NTU","Work","thesis","dataset(2004-2016)"),
  paste0(c(
    file.path("/mnt",tolower(driverletter_prefixes),"OneDrive","OnedriveDocuments","NTU","Work","thesis","dataset(2004-2016)"),
    file.path("/mnt",tolower(driverletter_prefixes),"Users","dowba","OneDrive","OnedriveDocuments","NTU","Work","thesis","dataset(2004-2016)")
    ),collapse=",")
  ) %>%
  stri_split(regex=",") %>% unlist() %>% {.[sapply(.,dir.exists)]} %>%
  ifelse(gtools::invalid(.), file.path(here::here(),"data"), .)
dataset_file_directory_rdata <- file.path(dataset_file_directory,"rdata")
dataset_in_scriptsfile_directory <- switch(
  t_sessioninfo_running_with_cpu,
  "Ubuntu18.04.3LTSIntel(R) Core(TM) i7-9750H CPU @ 2.60GHz"=file.path(filespath, "data"),
  "Windows8x64build9200Intel(R) Core(TM) i7-9750H CPU @ 2.60GHz"=file.path(filespath, "data"),
  "Windows10x64build19041Intel(R) Core(TM) i7-9750H CPU @ 2.60GHz"=file.path(filespath, "data"),
  "Windows8x64build9200Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz"=file.path(filespath, "data"),
  "Windows10x64build17763Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz"=file.path(filespath, "data"),
  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"=file.path(filespath, "data"),
  "Ubuntu18.04.2LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"=file.path(filespath, "data"),
  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"=file.path(filespath, "data"),
  "Ubuntu18.04.2LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"=file.path(filespath, "data"),
  "Ubuntu18.04.3LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"=file.path(filespath, "data"),
  "Ubuntu18.04.4LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"=file.path(filespath, "data"),
  "Ubuntu16.04.6LTSIntel(R) Xeon(R) Platinum 8124M CPU @ 3.00GHz"=file.path(here::here(),"data"),
  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\vote_record\\data\\",
  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2660 v4 @ 2.00GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\vote_record\\data\\",
  "Windows8x64build9200Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\vote_record\\data\\",
  "Windows10x64build17134Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\vote_record\\data\\"
) %>%
  {
    returnhere<-FALSE
    if (gtools::invalid(.)) {
      returnhere<-TRUE
    } else if (!dir.exists(.)) {
      returnhere<-TRUE
    } else if (!file.exists(paste0(., "shared_functions.R") )) {
      returnhere<-TRUE
    } else if(is(., 'try-error')) {
      returnhere<-TRUE
    }
    if (returnhere==TRUE) file.path(here::here(),"data") else NA
  }
if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = "/usr/local/spark")
}
survey_data_title<-c("2004citizen","2010env","2010overall","2016citizen") %>% sort()
imputation_sample_i_s <- seq(1,5)
imputation_sample_i_s <- 1:24
myremoteip <- tryCatch({xml2::read_html('https://www.myip.com/') %>%
    rvest::html_nodes(xpath = "//span[@id='ip']//text()") %>%
    rvest::html_text()},error=function (e) {
      try(xml2::read_html('https://www.whatismyip.com.tw/') %>%
        rvest::html_nodes(xpath = "//body/b/span//text()") %>%
        rvest::html_text())
      })
save_dataset_in_scriptsfile_directory<-if (grepl(pattern="140.110.148", myremoteip)) "/work1/dowbatw1133/" else dataset_in_scriptsfile_directory

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
ggplotapatheme <- ggplot2::theme_bw()+
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
customgrep<-function(x,pattern,ignore.case=FALSE,perl=FALSE,value=FALSE,fixed=FALSE,useBytes=FALSE,invert=FALSE) {
  return(grep(pattern,x,ignore.case,perl,value,fixed,useBytes,invert))
}
customgrepl<-function(x,pattern,ignore.case=FALSE,perl=FALSE,value=FALSE,fixed=FALSE) {
  return(grepl(pattern,x,ignore.case,perl,value,fixed))
}
customgsub<-function(x,pattern,replacement,ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  return(gsub(pattern, replacement, x, ignore.case, perl, fixed, useBytes))
}
custom_read_file<-function(url, locale = default_locale()) {
  if (is.na(url)) {
    return(NULL)
  } else {
    message(URLdecode(url))
    retryterm<-'(Timeout|403)'
    content<-tryCatch({
      readr::read_file(url,locale) #要執行的指令放這裡
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
custom_plot<-function(df, fvar, weightvar="", usingsurveypkg=FALSE, fillcolor="", fillvar="", n_bins="", ...) {
  n_bins<-if(n_bins=="") 500 else n_bins
  singleclass<-class(magrittr::extract2(df, fvar))
  discri_class<-paste0(singleclass,collapse="")
  randomcolors<-ggsci::pal_npg("nrc", alpha = 0.7)(10)
  randomcolors<-c(randomcolors,"lightblue","lightgreen")#,"lightred"
  if (fillcolor=="") fillcolor<-sample(randomcolors,1)
  if (weightvar!="") {
    if (class(weightvar)!="character") {
      ggplotweight<-weightvar
    } else {
      df_weight<-magrittr::extract2(df,weightvar)
      sum_df_weight<-sum(df_weight)
      samplerepresents<-df_weight*sum_df_weight
      ggplotweight<-df_weight/sum_df_weight
    }
  }
  #https://cran.r-project.org/web/packages/srvyr/index.html
  #https://rpubs.com/mcchadreque/survey10
  if (weightvar!="" & usingsurveypkg==TRUE) {
    svdesi<-survey::svydesign(ids = ~1, data = df, weights = df_weight)
    if (base::is.element(discri_class,c("factor", "orderedfactor")) ) {
      a<-survey::svymean(as.formula(paste0("~",fvar)), svdesi)
      survey:::barplot.svystat(a, col=fillcolor)
    } else if (discri_class=="numeric") {
      survey::svyhist(as.formula(paste0("~",fvar)), svdesi, breaks=n_bins, col=fillcolor)
    }
  } else {
    #mappingopt<-list(x=fvar, alpha=0.5)
    #if (weightvar!="") mappingopt<-rlist::list.append(mappingopt, weight = ggplotweight)
    mapping <-ggplot2::aes(
      x=.data[[fvar]],
      fill={if(fillvar=="") fillcolor else .data[[fillvar]]},
      weight={if (weightvar=="") 1 else ggplotweight} #.data[[weightvar]]
    )
    ggplotinit<-ggplot2::ggplot(df)
    statlayeropt<-list(mapping=mapping,
                       bins=n_bins,
                       alpha=0.5)
    statlayer<-do.call(switch(discri_class,
                              "numeric"=ggplot2::stat_bin,
                              "integer"=ggplot2::stat_bin,
                              "factor"=ggplot2::stat_count,
                              "orderedfactor"=ggplot2::stat_count),
                       statlayeropt)
    if (FALSE) {
      geom_func<-switch(singleclass,
                        "numeric"=geom_histogram(mapping=mapping, stat="bin"), #, fill="lightblue"
                        "factor"=geom_bar(mapping=mapping, stat="identity")#fill="white", 
      )
    }
    ggplotlab<-ggplot2::labs(x = paste0("ggplot ",fvar))
    ggp_using_color<-ggplot2::scale_colour_brewer()#scale_color_brewer(palette="Pastel2")
    ggp_scale<-switch(discri_class,
                      "numeric"=ggplot2::scale_fill_distiller(palette = "Pastel2"),#scale_fill_continuous(type = "gradient"),
                      "factor"=ggplot2::scale_fill_brewer(palette = "Pastel2")
    )
    # if (histogramlegend=="") {
    #   return(ggplotinit+statlayer+ggplotlab) #+geom_func+scale_colour_brewer()+ggp_scale
    # } else {
    #   ggplot_scalelab<-do.call(ggplot2::scale_fill_manual, args=histogramlegend)
    #   return(ggplotinit+statlayer+ggplotlab+ggplot_scalelab) #+geom_func+scale_colour_brewer()+ggp_scale
    # }
    return(ggplotinit+statlayer+ggplotlab)
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

ret_merged_for_idealpoint_and_pp_df_list<-function(survey_data_imputed, dataset_in_scriptsfile_directory, directlyload=FALSE, transform_pp_data_to_normal=FALSE, minuspolicy=FALSE, ...) {
  needimps<-custom_ret_appro_kamila_clustering_parameters()
  doneimps<-unique(survey_data_imputed$`2016citizen`$.imp)
  need_svytitle<-names(survey_data_imputed)
  merged_acrossed_surveys_list_with_normality_filepath<-paste0(dataset_in_scriptsfile_directory,"merged_acrossed_surveys_list_with_normality.RData")
  if (directlyload==FALSE) {
    merged_acrossed_surveys_list_with_normality<-lapply(doneimps, function(imp, normalize=FALSE, ...) {
      needdf<-survey_data_imputed[need_svytitle] %>%
        custom_parallel_lapply(FUN=function(X,nimp) {dplyr::filter(X, .imp==!!nimp) %>% dplyr::select(-myown_indp_atti)}, nimp=imp, method = parallel_method) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate_at("SURVEY", as.factor) %>%
        dplyr::mutate_at("cluster_kamila", as.ordered)  %>%
        dplyr::mutate(id=as.factor(paste0(SURVEY,id))) %>%
        dplyr::mutate_at("adminvillage", ~paste0(admincity,admindistrict,adminvillage)) %>%
        dplyr::mutate_at("admindistrict", ~paste0(admincity,admindistrict)) %>%
        dplyr::mutate_at(c("myown_areakind","admincity","admindistrict","adminvillage"), as.factor) %>%
        dplyr::mutate(myown_factoredses_overallscaled=as.numeric(scale(myown_factoredses)) ) %>%
        dplyr::mutate(myown_factoredparticip_overallscaled=as.numeric(scale(myown_factoredparticip)) ) %>%
        dplyr::mutate(myown_age_overallscaled=as.numeric(scale(myown_age)) ) %>%
        dplyr::mutate(myown_factoredparticip_ordinal=cut(myown_factoredparticip,breaks=c(-10,-2,-1.5,-1.3,-0.65,-0.4,-0.15,0.15,0.7,1.3,1.8,10),right=TRUE,include.lowest=TRUE,ordered_result=TRUE)) %>%
        dplyr::rename(policyidealpoint_cos_similarity_to_median=policyidealpoint_cos_similarity_to_median_policy_idealpoint,policyidealpoint_euclid_distance_to_median=policyidealpoint_euclid_distance_to_median_policy_idealpoint) %>%
        dplyr::mutate(policyidealpoint_cos_similarity_to_median_ordinal=cut(policyidealpoint_cos_similarity_to_median,breaks=17,right=TRUE,include.lowest=TRUE,ordered_result=TRUE)) %>%
        dplyr::mutate_if(is.factor, droplevels)
      if (minuspolicy==TRUE) {
        needdf %<>% dplyr::select(-dplyr::contains("policy"))
      }
      #C L Q E4 E5
      #dplyr::mutate_at("cluster_kamila", ~dplyr::recode_factor(., `1` = "A", `2` = "B", `3` = "C", `4` = "D", `5` = "E", `6` = "F", .ordered =TRUE) ) %>%
      if (normalize==TRUE) {
        transform_normality<-bestNormalize::bestNormalize(needdf$myown_factoredparticip)
        needdf$myown_factoredparticip_normalized<-transform_normality$x.t
      } else {
        transform_normality<-NA
      }
      return(list(needdf, transform_normality))
    }, survey_data_imputed=survey_data_imputed, need_svytitle=need_svytitle, normalize=transform_pp_data_to_normal, minuspolicy=minuspolicy)
    merged_acrossed_surveys_list<-lapply(merged_acrossed_surveys_list_with_normality, function(X) {X[[1]]})
    save(merged_acrossed_surveys_list, merged_acrossed_surveys_list_with_normality,file=merged_acrossed_surveys_list_with_normality_filepath)
  } else {
    load(merged_acrossed_surveys_list_with_normality_filepath,verbose=TRUE)
  }
  return(merged_acrossed_surveys_list)
}
count_value_times_in_vector<-function(arr,checkv,completematch=FALSE) {
  length_one<-grepl(checkv,arr)
  length_two<-(arr==checkv)
  length<-ifelse(completematch,sum(length_two),sum(length_one))
  return(length)
}
cbind.fill<-function(...){
  nm <- list(...) 
  nm<-lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
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
debugprint <- function(ob) {
  objname <- deparse(substitute(ob))
  message("object is: ",objname)
  tryCatch({
    message("  class is: ",class(ob))
  }, error = function(e) {
    message("  error in getting class: ",conditionMessage(e))
  })
  tryCatch({
    message("  typeof is: ",typeof(ob))
  }, error = function(e) {
    message("  error in typeof: ",conditionMessage(e))
  })
  tryCatch({
    message("  sloop ftype is: ",sloop::ftype(ob))
  }, error = function(e) {
    message("  error in getting sloop ftype: ",conditionMessage(e))
  })
  tryCatch({
    message("  length of is: ",length(ob))
  }, error = function(e) {
    message("  error in getting length: ",conditionMessage(e))
  })
  tryCatch({
    message("  names of is: ",names(ob))
  }, error = function(e) {
    message("  error in getting names: ",conditionMessage(e))
  })
  tryCatch({
    message("  print is: ")
    print(ob)
  }, error = function(e) {
    message("  error in printing: ",conditionMessage(e))
  })
  tryCatch({
    message("  content is: ")
    cat(ob)
  }, error = function(e) {
    message("  error in catting: ",conditionMessage(e))
  })
}






mutate_cond <- function(data, condition, ..., envir = parent.frame()) {
  if (inherits(data, what=c("data.table"))) {
    # message("mutate_cond in data.table mode")
    # https://stackoverflow.com/questions/70969830/how-do-i-correctly-use-the-env-variable-for-data-tables-within-a-function
    # dots <- eval(substitute(alist(...)), envir = envir)
    subsetcondition <- eval(substitute(condition), data, envir)
    subsetcondition[is.na(subsetcondition)] <- FALSE
    j_call <- substitute(`:=`(...))
    targetEnv <- envir
    targetEnv$data <- data
    targetEnv$.i <- subsetcondition
    targetEnv$.j <- j_call
    dtq <- substitute(data[.i, .j],targetEnv)
    eval(dtq)
    data
  } else if (inherits(data, what=c("dtplyr_step"))) {
    # message("mutate_cond in dtplyr_step mode")
    # message("condition is ",ls(envir))
    # .data <- dtplyr::mutate.dtplyr_step(RNrow_number=dplyr::row_number())
    # newcond <- dtplyr::filter.dtplyr_step(.data, condition) %>%
    #   dtplyr::select.dtplyr_step(RNrow_number)
    #   dtplyr::slice.dtplyr_step	
    #   #eval(substitute(condition), .data, envir)
    # .data[newcond, ]
  } else {
    condition <- eval(substitute(condition), data, envir)
    condition[is.na(condition)] = FALSE
    data[condition, ] <- data[condition, ] %>% dplyr::mutate(...)
    data
  }
}
#DF %>% mutate_cond(measure == 'exit', qty.exit = qty, cf = 0, delta.watts = 13)

#DF %>% 
#  group_by(is.exit = measure == 'exit') %>%
#  mutate_last(qty.exit = qty, cf = 0, delta.watts = 13) %>%
#  ungroup() %>%
#  select(-is.exit)
paths_to_survey_imputation_and_measurement_file<-c(
  paste0(dataset_file_directory,"merger_survey_dataset",slash,"imputationcomputingbasis.xlsx"),
  paste0(dataset_in_scriptsfile_directory,"imputationcomputingbasis.xlsx")
) %>%
  .[sapply(., file.exists)]
custom_ret_survey_imputation_and_measurement<-function(paths) {
  survey_imputation_and_measurement<-try(openxlsx::read.xlsx(paths[1],sheet = 1))
  if(is(survey_imputation_and_measurement, 'try-error')) {
    survey_imputation_and_measurement<- paths[2] %>%
      openxlsx::read.xlsx(sheet = 1)
  }
  return(survey_imputation_and_measurement)
}

kamila_clustering_parameters_path<-paste0(dataset_in_scriptsfile_directory, "kamila_clustering_parameters.Rdata")
custom_ret_appro_kamila_clustering_parameters<-function(path=kamila_clustering_parameters_path, intact=FALSE) {
  message(paste("now loading", path))
  load(file=path, verbose=TRUE)
  if (intact==TRUE) {
    return(kamila_clustering_parameters)
  }
  needimps<-dplyr::distinct(kamila_clustering_parameters,survey,imp,totlclusters) %>%
    dplyr::filter( (survey=="2010overall" & totlclusters==4) | (survey=="2016citizen" & totlclusters==6)) %>%
    dplyr::select(-totlclusters)
  n_of_imps<-dplyr::group_by(needimps, survey) %>%
    dplyr::summarise(nimps=dplyr::n()) %>%
    magrittr::use_series(nimps) %>%
    min()
  t<-dplyr::group_by(needimps, survey) %>%
    dplyr::slice_head(n=n_of_imps) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.imp=imp) %>%
    cbind(newimp=rep.int(1:n_of_imps, 2))
  return(t)
}

gcreset<-function() {
  gc(reset = TRUE)
}

base_r6_class <- R6::R6Class("base_r6_class", public = list(
  dataset_file_directory = NULL,
  dataset_in_scriptsfile_directory = NULL,
  debug_func_mode = NULL,
  error_vote_record_from_name_filepath = NULL,
  error_leave_and_attend_legislators_filepath = NULL,
  fetchmeetingdata_filepath = NULL,
  filespath = NULL,
  fullmeetingrecordlinks_filepath = NULL,
  legislatorsxlsxpath = NULL,
  ly_meeting_path = NULL,
  meetingdata_filepath = NULL,
  meetingdata_range = NULL,
  meetingurldata_filepath = NULL,
  meetingurldata_urlrange = NULL,
  myown_vote_record_df_filepath = NULL,
  myown_vote_record_detailed_part_df_filepath = NULL,
  term56_filepaths = NULL,
  term56_meetingrecords_filenames = NULL,
  mccores = 1,
  initialize = function(dataset_in_scriptsfile_directory="/mnt",filespath="/mnt", dataset_file_directory="/mnt", debug_func_mode=TRUE) {
    self$dataset_file_directory <- dataset_file_directory
    self$dataset_in_scriptsfile_directory <- dataset_in_scriptsfile_directory
    self$debug_func_mode <- debug_func_mode
    self$error_vote_record_from_name_filepath <- file.path(dataset_in_scriptsfile_directory, "error_vote_record_from_name.xlsx")
    self$error_leave_and_attend_legislators_filepath <- file.path(dataset_in_scriptsfile_directory, "leave_and_attend_legislators.xlsx")
    self$filespath <- filespath
    self$fetchmeetingdata_filepath <- file.path(dataset_in_scriptsfile_directory, "fetchmeetingdata.rds")
    self$fullmeetingrecordlinks_filepath <- file.path(dataset_in_scriptsfile_directory, "fullmeetingrecordlinks.xlsx")
    self$legislatorsxlsxpath <- file.path(dataset_file_directory, "legislators.xlsx")
    self$ly_meeting_path <- file.path(filespath, "2004_meeting", "original")
    self$mccores <- base::ifelse(self$debug_func_mode==TRUE, 1, parallel::detectCores())
    self$meetingurldata_filepath<-file.path(dataset_in_scriptsfile_directory, "meetingrecord.xlsx")
    self$meetingdata_filepath <- file.path(dataset_in_scriptsfile_directory, "meetingdata.rds")
    self$myown_vote_record_df_filepath <- file.path(dataset_in_scriptsfile_directory,"myown_vote_record_df.rds")
    self$myown_vote_record_detailed_part_df_filepath <- file.path(dataset_in_scriptsfile_directory,"myown_vote_record_detailed_part_df.rds")
    self$term56_meetingrecords_filenames<-c(#"立法院第5屆第5會期全院委員談話會紀錄.html",
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
    self$term56_filepaths <- file.path(self$ly_meeting_path,self$term56_meetingrecords_filenames)
  }
  
))

#rJava安裝前要R CMD javareconf
if (check_if_windows()) {
  Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre-9.0.4')
  Sys.setenv(JAVA_OPTS='-Xms128M -Xmx512M')
  #library("rJava")
  #library("xlsx")
  try(library("openxlsx"))
} else {
  #export LD_LIBRARY_PATH=/usr/lib/jvm/java-9-oracle/lib/server/
  Sys.setenv(JAVA_HOME='/usr/lib/jvm/java-9-oracle/bin/java')
  Sys.setenv(JAVA_OPTS='-Xms128M -Xmx512M')
  #library("rJava")
  #library("xlsx")
  try(library("openxlsx"))
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
data.table::setDTthreads(parallel::detectCores())
custom_parallel_lapply<-function(X=list(), FUN=FUN, ..., method="socks", exportlib=c("base","magrittr","parallel"), exportvar=c(), outfile="", verbose=TRUE, mc.cores=parallel::detectCores()  ) {
  if (mc.cores==1) {
    return(lapply(X, FUN=FUN, ...))
  } else {
    result<-switch(method,
                   "fork"=
                     mclapply(X=X, FUN=FUN, ..., mc.cores=mc.cores,mc.preschedule=FALSE ),
                   "socks" = {
                     argumentstopass<-list(...)
                     tryCatch(
                       {stopCluster(cl)},
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
                     cl <- parallel::makeCluster(parallel::detectCores(),outfile=outfile)
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
                     returndata<-parallel::parLapply(
                       cl,
                       X=X,
                       fun=FUN,
                       ...)
                     parallel::stopCluster(cl)
                     #return(returndata)
                     returndata
                   }
    )
  }
}
reset_multi_p <- function(t_sessioninfo_running = gsub("[>=()]","",gsub(" ","",sessionInfo()$running)) ) {
  future::plan(sequential)
  switch(as.character(customgrepl(t_sessioninfo_running, "Windows")),
         "TRUE"=future::plan(multisession),
         "FALSE"=future::plan(multicore) 
         #"FALSE"=plan(multisession)
  )
}
custom_apply_thr_argdf <- function(argdf, loopstorekey, customfunc, method="fork", datadf=NA, ...) {
  loopkeys<-as.character(argdf[,loopstorekey])
  retlist<-custom_parallel_lapply(loopkeys, FUN=customfunc, loopargdf=argdf, method=method, datadf=datadf, ...)
  retlist<-magrittr::set_names(retlist, loopkeys)
  return(retlist)
}

vhead<- function(X) {
  View(head(X))
}

select_and_fill_nonexistcol <- function(fundf,colVec) {
  fundf <- fundf[base::intersect(colVec, names(fundf))]
  fundf[base::setdiff(colVec, names(fundf))] <- NA
  fundf
}

list_of_vec_asmanyrows_to_df <- function(X) {
  needdf <- data.table::data.table(
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
  }
  return(srcstr)
})

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




custom_ret_survey_book_file<-function(dataset_file_directory, subject="issue") {
  survey_codebook_file<-paste0(dataset_file_directory,"all_survey_questions_englished.xlsx")
  survey_keys <- c("2010overall","2016citizen")
  survey_question_category_df<-lapply(c(1,3), function(fi,...) {
    openxlsx::read.xlsx(survey_codebook_file,sheet = fi)
  },survey_codebook_file=survey_codebook_file) %>%
    dplyr::bind_rows()
  if (subject=="pp") {
    survey_question_category_df %<>% dplyr::filter(grepl(pattern="參與", x=CATEGORY, perl=TRUE) | CATEGORY=="參與")
  } else {
    survey_question_category_df %<>% dplyr::filter(grepl(pattern="民主價值", x=CATEGORY, perl=TRUE) | CATEGORY=="議題")
  }
  survey_question_category_df %<>% dplyr::filter(SURVEY %in% !!survey_keys) %>%
    dplyr::filter(MEASUREMENT %in% c("nominal","ordinal")) %>%
    dplyr::filter(IMPUTATION!="ignore" & ID!="myown_indp_atti") %>%
    dplyr::mutate(itemtype=NA) %>%
    mutate_cond(MEASUREMENT=="nominal", itemtype="2PL") %>%
    mutate_cond((grepl(pattern=";3", x=ANSWER) & itemtype=="2PL"), itemtype="nominal") %>%
    mutate_cond(MEASUREMENT=="ordinal", itemtype="graded") %>%
    mutate_cond(grepl(pattern="(1分;22分|22分;33分)", x=ANSWER), itemtype="grsm") %>%
    dplyr::arrange(SURVEYCOMPLETEID) %>%
    lapply(survey_keys, function(k, data) {
      return(dplyr::filter(data, SURVEY==!!k))
    }, data=.) %>%
    magrittr::set_names(survey_keys)
  return(survey_question_category_df)
}

custom_mirt_coef_to_df <- function(mirtmodel, rotate="varimax", printSE = FALSE, ...) {
  coefdf <- mirt::coef(mirtmodel, rotate=rotate, as.data.frame=TRUE, printSE=printSE)
  firsttry <- try({coefdf %>%
      .[grepl("Group",rownames(.))==FALSE,] %>%
      data.frame(value=., attr={
        #names(.)
        stringr::str_extract_all(names(.), "\\..+$") %>%
          unlist() %>%
          gsub(pattern="\\.",replacement="",x=.)
      }, rowvar={
        stringr::str_extract_all(names(.), ".+\\.") %>%
          unlist() %>%
          gsub(pattern="\\.",replacement="",x=.)
      }) %>%
      reshape2::dcast(rowvar ~ attr, value.var="value")})
  if(!is(firsttry, 'try-error')) {coefdf<-firsttry} else {
    coefdf <- data.frame(coefdf,
                         par_type=rownames(coefdf) %>%
                           stringr::str_extract_all(pattern="\\..+$") %>%
                           unlist() %>%
                           gsub(pattern="\\.",replacement="",x=.),
                         rowvar=rownames(coefdf) %>%
                           stringr::str_extract_all(pattern=".+\\.") %>%
                           unlist() %>%
                           gsub(pattern="\\.",replacement="",x=.)
    ) %>%
      reshape2::melt(id.vars=c("rowvar","par_type"))
  }
  return(coefdf)
}

custom_pickcolnames_accordingtoclass<-function(df,needclass="factor") {
  colnames(df)[which(grepl(pattern=needclass, x=sapply(df,function(X) paste0(class(X),collapse=""))) )] %>%
    return()
}




dummycode_of_a_dataframe<-function(df,catgvars=c()) {
  detectedcatgvars<-custom_pickcolnames_accordingtoclass(df,needclass="factor")
  detectedcatgvars<-detectedcatgvars[(sapply(dplyr::select(df,!!detectedcatgvars), nlevels ))>=2]
  catgvars<-if (length(catgvars)==0) detectedcatgvars else base::intersect(catgvars, detectedcatgvars)
  if (length(catgvars)==0) return(df)
  dplyr::bind_cols(dplyr::select(df, -!!catgvars), custom_parallel_lapply(catgvars, function(factorvar,df,...) {
    psych::dummy.code(dplyr::pull(df,!!factorvar)) %>%
      {.[,gtools::mixedsort(colnames(.))]} %>%
      {magrittr::set_colnames(., paste0(factorvar,colnames(.)))} %>%
      {(.[,2:ncol(.),drop=FALSE])} %>%
      return()
  }, df=df, method=parallel_method) %>%
    data.frame() ) %>%
    return()
}

inflate_df_from_weight<-function(needdf, weightvar="myown_wr", rate=10000, parallel_method="fork") {
  custom_parallel_lapply(1:nrow(needdf), function(fi, ...) {
    needrow<-needdf[fi,]
    weight<-needrow[, weightvar]
    weight_times<-round(weight*rate)
    dplyr::slice(needrow, rep(1, each = weight_times))
  }, needdf=needdf, weightvar=weightvar, rate=rate, method=parallel_method) %>% plyr::rbind.fill() %>%
    return()
}

##use
# nullmodels<-custom_apply_thr_argdf(nullmodel_args, "formula", function(fikey, loopargdf, datadf, ...) {
#   dplyr::filter(nullmodel_args, formula==!!fikey) %>%
#     magrittr::use_series("formula") %>%
#     as.character() %>%
#     as.formula() %>%
#     lme4::lmer(data=datadf) %>%
#     return()
# }, datadf=merged_acrossed_surveys_list[[1]])

#https://stats.stackexchange.com/questions/233800/how-can-i-get-confidence-intervals-for-fixed-effects-using-the-rlmer-function-r






##pooling functions for ordinal::clmm, from ordinalimputation
if (TRUE) {
  prof_ci <- function(fits, alpha = 0.05, index, data){
    # Function to calculate the profile confidence interval around the
    # variance of the random effect,
    x_est   <- unlist(fits[[1]]$ST)[index]
    xmax    <- 5 * x_est
    ci.lb   <- optimize(f = score_prog_ll, interval = c(0, x_est),
                        alpha = alpha/2, fits = fits, data = data,
                        index = index)
    ci.ub   <- optimize(f = score_prog_ll, interval = c(x_est, xmax),
                        alpha = 1 - alpha/2, fits = fits, data = data, index = index)
    
    return(c(ci.lb$minimum, ci.ub$minimum))
  }
  
  
  score_prog_ll <- function (x, fits, index, alpha, data)
  {
    n_x      <- length(x)
    score    <- rep(NA, n_x)
    
    for(j in 1:n_x){
      ll_value <- rep(NA , length(fits))
      for(i in 1:length(fits)){
        fitted <- fits[[i]]
        
        start             <- list(fitted$coefficients, unlist(fitted$ST))
        gamma             <- start[[2]][index]
        start[[2]][index] <- x[j]
        
        fit_prof <- update(fitted, start = start, eval.max = 1,
                           data = complete(data, i))
        ll_value[i] <- pnorm(sign(x[j] - gamma[index]) * sqrt(-2 * (fit_prof$logLik -
                                                                      fitted$logLik)))
      }
      
      avg_ll   <- mean(ll_value)
      score[j] <- (avg_ll - alpha)^2
    }
    return(score)
  }
  
  
  pooling <- function(x,...){
    UseMethod("pooling",x)
  }
  
  
  get_re_std <- function(x){
    res <- x$ST
    return(res)
  }
  
  pool_rubin <- function(coefs, variance){
    qbar <- colMeans(coefs)
    
    m            <- length(variance)
    
    bw_imp_var   <- var(coefs)
    wi_imp_var   <- Reduce("+", variance)/m
    
    totalVar     <- wi_imp_var + (1 + 1/m) * bw_imp_var
    qbar         <- colMeans(coefs)
    
    res <- list(estimate = qbar, variance = totalVar)
  }
  
  pool_re <- function(re_mode, condvar){
    n_cluster <- ncol(re_mode)
    m         <- nrow(re_mode)
    
    res <- data.frame(cluster = 1:n_cluster,
                      mode = rep(NA, n_cluster),
                      cond_var = rep(NA, n_cluster))
    for(i in 1:n_cluster){
      
      
      res$mode[i] <- mean(re_mode[, i])
      
      bw_imp_var <- var(re_mode[, i])
      wi_imp_var <- mean(condvar[, i])
      
      res$cond_var[i] <- wi_imp_var + (1 + 1/m) * bw_imp_var
    }
    return(res)
  }
  
  print.pooled.clmm <- function(x){
    cat("Random effect parameters:\n")
    print(x$random_dist)
    cat("\n\nFixed effect estimates:\n")
    print(x$fixed_effects)
    
  }
  
  get_vcov <- function(x, coefs){
    res <- vcov(x)
    res <- res[coefs, coefs]
    return(res)
  }
  
  plot.pooled.clmm <- function(x, xlim = NULL, ylim = NULL, xlab = "Cluster effect", ylab = "Cluster", refline = 0,...){
    index <- order(x$random_effects$mode)
    x$random_effects <- x$random_effects[index, ]
    y      <- 1:nrow(x$random_effects)
    
    
    x_plot <- x$random_effects$mode
    
    if(is.null(xlim)){
      xmax   <- with(x$random_effects, mode + qnorm(0.975) * sqrt(cond_var))
      xmin   <- with(x$random_effects, mode - qnorm(0.975) * sqrt(cond_var))
      
      xlim <- c(floor(min(xmin) + 0.1), ceiling(max(xmax) + 0.1))
    }
    if(is.null(ylim)){
      ylim <- c(0, nrow(x$random_effects) + 1)
    }
    
    plot(x_plot, y, xlim = xlim, ylim = ylim, pch = 19, xlab = xlab, ylab = ylab,...)
    segments(x0 = xmin, x1 = xmax, y0 = y, y1 = y)
    abline(v = refline, lty = 3)
  }
  
  
  pooling.clmm <- function(x, conf.int.re = c("none", "profile"), data = NULL){
    conf.int.re <- match.arg(conf.int.re)
    
    # Pool fixed effects and standard deviation random effect.
    coefs        <- sapply(X = x, FUN = coefficients)
    std_re       <- sapply(X = lapply(X = x, FUN = get_re_std),
                           FUN = unlist)
    coefs_fit    <- t(coefs)
    
    vcov_fits  <- lapply(X = x, FUN = get_vcov, coefs = rownames(coefs))
    pool_fixed <- pool_rubin(coefs = coefs_fit, variance = vcov_fits)
    
    if(class(std_re)=="numeric"){
      n_re <- 1
    }else{
      n_re <- nrow(std_re)
    }
    
    mu_fixed   <- pool_fixed$estimate
    se_fixed   <- sqrt(diag(pool_fixed$variance))
    ci_l_fixed <- mu_fixed - 1.96 * se_fixed
    ci_u_fixed <- mu_fixed + 1.96 * se_fixed
    
    fixef      <- cbind(mu_fixed, ci_l_fixed, ci_u_fixed)
    
    rownames(fixef) <- names(pool_fixed$estimate)
    colnames(fixef) <- c("Estimate", "Lower 95% CI", "Upper 95% CI")
    if(n_re==1){
      std_dev <- sqrt(mean(std_re))
    }else{
      std_dev <- sqrt(rowMeans(std_re))
    }
    
    mor     <- exp(sqrt(2 * std_dev^2) * qnorm(0.75))
    std_re  <- data.frame(std_dev = std_dev, mor = mor)
    if(conf.int.re=="profile"){
      if(is.null(data)){
        stop("To calculate the profile likelihood please supply the data used when fitting the model.")
      }
      cat("Calculating profile likelihood, may take a very long time.\n")
      conf_int <- matrix(nrow = n_re, ncol = 2)
      for(i in 1:n_re){
        conf_int[i, ] <- prof_ci(fits = x, index = i, data = data)
        cat("Random effect variabce number ", i, "done\n")
      }
    }else{
      conf_int <- NULL
    }
    
    ranef_fits   <- t(sapply(X = lapply(X = x, FUN = ordinal::ranef), FUN = unlist))
    condVar_fits <- t(sapply(X = lapply(X = x, FUN = ordinal::condVar), FUN = unlist))
    
    random_effect <- pool_re(ranef_fits, condVar_fits)
    
    mu_random <- random_effect$mode
    se_random <- sqrt(random_effect$cond_var)
    
    ci_l_random <- mu_random - 1.96 * se_random
    ci_u_random <- mu_random + 1.96 * se_random
    
    random <- cbind(mu_random, ci_l_random, ci_u_random)
    
    
    rownames(random) <- 1:nrow(random)
    colnames(random) <- c("Estimate", "Lower 95% CI", "Upper 95% CI")
    
    res <- list(fixed_effects = fixef, random_dist = std_re,
                random_effects = random, se_fixed = se_fixed,
                se_random = se_random, conf_int_re = conf_int)
    class(res) <- c("pooled.clmm")
    return(res)
  }
}



if (FALSE) {
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
  custominsertRow <- function(newrow, existingDF, r=nrow(existingDF)) {
    existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
    existingDF[r,] <- newrow
    existingDF
  }
  custommessage<-function(v,existingDF=data.frame()) {
    message(v)
    message("-----------\n")
  }
  customreadfile<-function(targetfile,encoding="UTF-8") {
    connection<-file(targetfile,encoding=encoding)
    result<-sapply(connection,readLines) %>%
      sapply(custompaste0)
    return(result)
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
  mutate_last <- function(.data, ...) {
    n <- n_groups(.data)
    indices <- attr(.data, "indices")[[n]] + 1
    .data[indices, ] <- .data[indices, ] %>% mutate(...)
    .data
  }
  as_factor_to_integer<-function(f) {
    as.numeric(levels(f))[f]
  }
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
  custom_parallel_expr<-function(expr, name, mc.set.seed = TRUE, silent = FALSE,
                                 mc.affinity = NULL, mc.interactive = FALSE,
                                 detached = FALSE,
                                 wait = TRUE, timeout = 0, intermediate = FALSE) {
    do_job<-parallel::mcparallel(expr, name, mc.set.seed, silent,
                                 mc.affinity, mc.interactive,
                                 detached)
    return(parallel::mccollect(do_job, wait, timeout, intermediate))
  }
  custom_find_duplicated_rows<-function(targetdf, cols=c(), findall=FALSE, remove=FALSE) {
    if (length(cols)<=0) {
      cols<-names(targetdf)
    }
    targetdf %<>% data.table::as.data.table()
    if (remove==TRUE) {
      matcheddf<-targetdf[!duplicated(targetdf[,cols]),]
    } else {
      if (findall==FALSE) {
        matcheddf<-targetdf[duplicated(targetdf[,cols]),]
      } else {
        matcheddf<-targetdf[duplicated(targetdf[,cols]),] %>%
          dplyr::semi_join(targetdf)
      }
    }
    return(matcheddf)
  }
  
}
#install.packages("~/pbdDMAT_0.5-1.tar.gz", repos = NULL, type="source")

#research_odbc_file<-"E:\\Software\\scripts\\R\\vote_record\\votingdf.sqlite.dsn"
#research_odbc<-"Research"
#research_odbc_ch <- odbcConnect(research_odbc, believeNRows = FALSE, rows_at_time = 1, DBMSencoding="UTF-8")
#df<-sqlQuery(research_ch,"SELECT * from bill")SELECT * from bill")a