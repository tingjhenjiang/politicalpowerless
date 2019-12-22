# 第Ｏ部份：環境設定 --------------------------------
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
#.libPaths("C:/Users/r03a21033/Documents/R")
source(file = "shared_functions.R")

gc(verbose=TRUE)



t_sessioninfo_running_with_cpu_locale<-sessionInfo()$locale %>% stringi::stri_split(regex=";") %>% unlist() %>% getElement(1) %>% stringi::stri_split(regex="=") %>% unlist() %>% getElement(2) %>%
  paste0(t_sessioninfo_running_with_cpu, .) %>% gsub(pattern=" ",replacement = "", x=.)
# 第六部份：LCA latent variables 潛在類別模式資料清理  ================================= 
load(paste0(dataset_in_scriptsfile_directory,"miced_survey_9_Ubuntu18.04.3LTSdf_with_mirt.RData"), verbose=TRUE)
imps <- 1:5


library(poLCA)
library(parallel)
library(LCAvarsel)

lcaneed_independence_attitude<-list(
  "2004citizen"=c("v95r","v96","v97"),#公投選項共變
  "2010env"=c(),#沒有統獨
  "2010overall"=c("v90r","v91","v92"),
  "2016citizen"=c("h10r")#2016只有一題統獨傾向"
)
lcaneed_party_constituency<-list(
  "2004citizen"=c("v88","v89","v90","v98r","v99"),#可用 v88 v89 v90 v99 (多類別)"v98r"
  "2010env"=c("v103r"),#2010env只有一題政黨傾向
  "2010overall"=c("v84","v85v86v87sumup","v93v94sumup","v85v88sumup"), # ,"v86","v87a1r" 太多遺漏值無法填補; 可用 v84 v86 v87a1r v93 v94(多類):v88,"v85v86v87sumup","v93v94sumup","v85v88sumup"
  "2016citizen"=c("h5","h6r_recode_party_for_forgotten","h7","h8r","h9r")
)
#lapply(survey_data_test[[1]][,c(lcaneed_party_constituency[[1]],"v98r")], is.na) %>%
#  lapply(sum)
lcaneed_ethnicity<-list(
  "2004citizen"=c("myown_selfid","myown_dad_ethgroup","myown_mom_ethgroup"),#"myown_dad_ethgroup","myown_mom_ethgroup",
  "2010env"=c("myown_selfid"),#"myown_dad_ethgroup","myown_mom_ethgroup",
  "2010overall"=c("myown_selfid","myown_dad_ethgroup","myown_mom_ethgroup"),#,"myown_dad_ethgroup","myown_mom_ethgroup"
  "2016citizen"=c("myown_selfid") #到這裡，但還沒有輸出上述的v94r #"myown_dad_ethgroup","myown_mom_ethgroup",
)
lcaneed_identity<-list(
  "2004citizen"=c("v94r"),
  "2010env"=c(),
  "2010overall"=c("v89r"),
  "2016citizen"=c() #到這裡，但還沒有輸出上述的v94r
)
lcaneed_other_cov<-list(
  "2004citizen"=c("v87_1","v87_2","v87_3","v87_4","v87_5","v87_6","v91v91a_sumup","v91v91b_sumup"),#共變(公投投票選擇及參與政治活動例如凱道選擇),填補太多:"v91a","v91b","v92_1","v92_2","v92_3","v92_4"
  "2010env"=c(),
  "2010overall"=c(),
  "2016citizen"=c()
)
# 第六-1部份：LCA latent variables 潛在類別模式統獨傾向 =====================
#load(paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_eth_iden_othercovWindows8x64build9200.RData"))
#LCAmodel_with_indp

#自動變數選擇https://cran.r-project.org/web/packages/LCAvarsel/LCAvarsel.pdf

custom_generate_LCA_model<-function(X, n_latentclasses=3, nrep=30, maxiter=1000, modelformula=NA, firstlcaneed=c(), secondlcaneed=c(), ..., exportlib=c("base"), exportvar=c(), outfile="") {#,thirdlcaneed=c(),fourthlcaneed=c(),fifthlcaneed=c(), exportlib=c("base"), exportvar=c(), outfile=""
  #X此時就形同是一個問卷的dataset
  #lcaneed_independence_attitude
  #lcaneed_party_constituency
  #lcaneed_ethnicity
  #lcaneed_identity
  #lcaneed_other_cov
  #message("<===== at custom_generate_LCA_model exportlib is ", exportlib, " and exportvar is ", exportvar, " and outfile is ", outfile, "=====>")
  otherlcaneed<-unlist(list(...)) #testing: otherlcaneed<-NULL
  #needsurveyi<-X$SURVEY[1]
  cov_parameter_in_formula<-dplyr::union_all(
    secondlcaneed,otherlcaneed
  )
  #magrittr::extract2(secondlcaneed,needsurveyi),
  #magrittr::extract2(otherlcaneed,needsurveyi)
  #if (needsurveyi %in% names(n_latentclasses)) {
  #  #n_latentclasses <- magrittr::extract2(n_latentclasses,needsurveyi)
  #  n_latentclasses <- getElement(n_latentclasses,needsurveyi)
  #}
  if (identical(cov_parameter_in_formula,logical()) | gtools::invalid(cov_parameter_in_formula) ) {
    cov_parameter_in_formula<-"1"
  }
  #if (all(sapply(list(modelformula, firstlcaneed), gtools::invalid) )) {
  #  return(NULL)
  #}
  #if (!gtools::invalid(modelformula) & !gtools::invalid(firstlcaneed)) {
  #  return(NULL)
  #}
  workingmodelformula<-switch(as.character(modelformula),
    "NA"=paste0(
      "cbind(",
      paste(firstlcaneed,collapse=","),
      ") ~ ",
      paste0(cov_parameter_in_formula,collapse="+"),
      collapse=""
    ),
    modelformula)
  #paste(magrittr::extract2(firstlcaneed,needsurveyi),collapse=","),
  formulated_workingmodelformula <- as.formula(workingmodelformula)
  #library(poLCA) #for fixing stange situation that Windows does not fork well
  tmppoLCAresult<-#switch(
    #as.character(length(firstlcaneed)),
    #"0"=NULL,
    #"1"=NULL, #X[,magrittr::extract2(firstlcaneed,needsurveyi)],
    #{
      #custom_parallel_lapply(X=n_latentclasses,FUN=function(poXi,s_survey_data,modelformula,nrep,maxiter,...)
      #{
      #### lapply(2:7,function(poXi,s_survey_data) {
      switch(as.character(gtools::invalid(n_latentclasses) | (is.integer(n_latentclasses) && n_latentclasses<=0) ),
        "TRUE"=list("result"=NULL),
        "FALSE"=preserve_warning_tryCatch({
          poLCA(
            data = X,
            formula = formulated_workingmodelformula,
            nclass = n_latentclasses,#poXi,
            nrep = nrep,
            maxiter = maxiter
          )})#,
          ## 遇到 warning 時的自訂處理函數
          #warning = function(msg) {
          #  errmessage <- paste0("tryCatch Original warning message while ",modelformula," and ",n_latentclasses," and msg is", msg, "\n")
          #  message(errmessage)
          #  return(list("content"=errmessage))
          #},
          ## 遇到 error 時的自訂處理函數
          #error = function(msg) {
          #  errmessage <- paste0("tryCatch Original error message while ",modelformula," and ",n_latentclasses," and msg is", msg, "\n")
          #  message(errmessage)
          #  return(list("content"=errmessage))
          #})
      )
      #ret_lcamodelbuildtresult <- list()
      #if (gtools::invalid(lcamodelbuildtresult$content)) {
      #}
      #return(lcamodelbuildtresult)
      #ret_lcamodelbuildtresult
      #poXi
    #},s_survey_data = X,
    #modelformula = modelformula,
    #nrep = nrep,
    #maxiter = maxiter,
    #exportvar = exportvar,
    #exportlib = exportlib,
    #outfile = outfile
    #)
    #}
  #poLCAresult$result <- lcamodelbuildtresult$result
  if (class(tmppoLCAresult[[1]])=="poLCA") {
    poLCAresult <- tmppoLCAresult[[1]]
    poLCAresult$content <- paste0(as.character(tmppoLCAresult[[2]]), collapse="|") 
  } else {
    poLCAresult <- list("result"=NULL)
    poLCAresult$content <- paste0(tmppoLCAresult[[1]],as.character(tmppoLCAresult[[2]]), collapse="|") 
  }
  poLCAresult$nclass <- n_latentclasses#poXi
  poLCAresult$modelformula <- workingmodelformula
  poLCAresult$residdf <- poLCAresult$resid.df
  #poLCAresult$formulated_modelformula <- formulated_workingmodelformula
  #poLCAresult$inputformula <- modelformula
  poLCAresult$nrep <- nrep
  poLCAresult$maxiter <- maxiter
  #) #end of switch
  return(poLCAresult)
}

LCAresult_to_sheet <- function(LCAstr) {
  LCAstr <- customgsub(LCAstr, pattern = '[ ]{2,}', replacement = "[", perl = TRUE) %>%
    customgsub(pattern = '\\[\\[', replacement = '\\[') %>%
    unlist() %>%
    lapply(unlist) %>%
    lapply(stringi::stri_split,regex = "\\[") %>%
    lapply(unlist) %>%
    lapply(t) %>%
    lapply(as.data.frame) %>%
    plyr::rbind.fill()
  View(LCAstr)
  return(LCAstr)
}

###LCAmodel_with_indp_covparty <- list()
###for (lcamodelit in 1:1) {#length(t_survey_data_test)
###  LCAmodel_with_indp_covparty[[lcamodelit]]<-custom_parallel_lapply(
###    X=t_survey_data_test[[lcamodelit]],
###    FUN=custom_generate_LCA_model,
###    exportvar=c("t_survey_data_test","lcaneed_independence_attitude","lcaneed_party_constituency","lcaneed_ethnicity","lcaneed_identity","lcaneed_other_cov"),
###    exportlib=c("base",lib,"poLCA"),
###    outfile=paste0(dataset_file_directory,"rdata",slash,"parallel_handling_process-",t_sessioninfo_running_with_cpu,".txt"),
###    firstlcaneed=lcaneed_independence_attitude,
###    secondlcaneed=lcaneed_party_constituency
###  )
###}


repeatloadsavedestfile <- function(loadsavemode="load",destfile=paste0(dataset_file_directory,"rdata",slash,"list_of_degree_of_freedom.RData"), savedestobject=list_of_degree_of_freedom, breakafterntimes=10) {
  testloadprocess<-expression(load(file=destfile, envir = globalenv(), verbose=TRUE))
  testsaveprocess<-expression({
    assign("list_of_degree_of_freedom", savedestobject, .GlobalEnv)
    save(list_of_degree_of_freedom, file=destfile)
  })
  loadsaveexpr <- switch(loadsavemode,
    "load"=list(testloadprocess),
    "save"=list(testsaveprocess),
    "loadandsave"=list(testloadprocess,testsaveprocess)
  )
  message("destfile is ", destfile)
  loop_times <- 1
  repeat{
    message("now it is repeating #", loop_times, " to ", loadsavemode)
    testprocess<-tryCatch(
      {lapply(loadsaveexpr, eval, envir = environment() )},
      error=function(e) {
        message(e)
        return("failedonprocessing")
      })
    if ((loadsavemode=="save" & gtools::invalid(testprocess[[1]])) | breakafterntimes==loop_times) { # | testprocess!="failedonprocessing"
      message("done repeating ", loadsavemode)
      break
    }
    if (loadsavemode=="load") {
      if (testprocess[[1]]!="failedonprocessing" | breakafterntimes==loop_times) {
        assign("list_of_degree_of_freedom", list_of_degree_of_freedom, envir=globalenv())
        message("done repeating ", loadsavemode)
        break
      }
    }
    if (loadsavemode=="loadandsave") {
      if (length(testprocess)==2) {
        if (gtools::invalid(testprocess[[2]]) | breakafterntimes==loop_times) {
          message("done repeating ", loadsavemode)
          break
        }
      }
    }
    loop_times <- loop_times+1
  }
}
#repeatloadsavedestfile()
#repeatloadsavedestfile(loadsavemode="save")

repeat_connectdb_readtable_close <- function(drv, dbname, ..., tablename, breakafterntimes=10) { #function(dbname, tablename, dbtype=RSQLite::SQLite(), ..., breakafterntimes=10) {
  #, setting_synchronous = "off"
  loop_times <- 1
  othervar <- unlist(list(...))
  othervar_names <- names(list(...))
  #return(othervar_names)
  #if_id_pw_host <- all(sapply(c(username,password,host),gtools::invalid)) #TRUE代表完全沒設定
  repeat {
    testprocess<-tryCatch({
      con <- DBI::dbConnect(drv, dbname = dbname, ...)
      #con <- switch(as.character(if_id_pw_host),
      #              "TRUE"=DBI::dbConnect(drv = dbtype, dbname = dbname),
      #              "FALSE"=DBI::dbConnect(drv = dbtype, dbname = dbname, username=username, password=password, host=host),
      #              )
      tableresult <- DBI::dbReadTable(con, name=tablename)
      DBI::dbDisconnect(con)
      tableresult
    },
    error=function(e) {
      message(e,"\n"," dbname=",dbname,"; tablename=",tablename)
      DBI::dbDisconnect(con)
      return("failedonprocessing")
    })
    DBI::dbDisconnect(con)
    if (class(testprocess)=="data.frame" | breakafterntimes==loop_times) {
      DBI::dbDisconnect(con)
      return(testprocess)
      break
    }
    loop_times <- loop_times+1
    DBI::dbDisconnect(con)
  }
  DBI::dbDisconnect(con)
}
#ref http://adv-r.had.co.nz/Functions.html
#ref http://adv-r.had.co.nz/Computing-on-the-language.html#capturing-dots
#sqlite_dbname <- paste0(dataset_in_scriptsfile_directory, "list_of_degree_of_freedom.sqlite")
#list_of_degree_of_freedom_2004citizen_sqlite <- repeat_connectdb_readtable_close(dbtype=RSQLite::SQLite(), dbname=tmp_dest_sqlite_file, tablename="list_of_degree_of_freedom_2004citizen", synchronous=NULL, breakafterntimes=1)
#con <- DBI::dbConnect(drv=RSQLite::SQLite(), dbname=tmp_dest_sqlite_file, tablename="list_of_degree_of_freedom_2004citizen", synchronous=NULL)
#tableresult <- DBI::dbReadTable(con, name="list_of_degree_of_freedom_2004citizen")
#repeat_connectdb_readtable_close(sqlite_dbname, dbtype=RSQLite::SQLite(), "list_of_degree_of_freedom", tablename="list_of_degree_of_freedom_2004citizen")
#library(RMySQL)
#testret<-repeat_connectdb_readtable_close(dbtype=RMySQL::MySQL(), dbname="thesis", tablename="list_of_degree_of_freedom_2004citizen", host="127.0.0.1", username = "root", password = "")
#con <- DBI::dbConnect(RMySQL::MySQL(), dbname = "thesis", username="root", password="", host="127.0.0.1")

#fresh restart from here
library(future)
reset_multi_p()
library(future.apply)
t_survey_data_test<-survey_data_imputed
list_of_degree_of_freedom<-list()
#load(file=paste0(dataset_file_directory,"rdata",slash,"list_of_degree_of_freedom.RData"),verbose=TRUE)
#load(file=paste0(dataset_file_directory,"rdata",slash,"list_of_degree_of_freedom_2010overall.RData"))
#repeatloadsavedestfile(destfile=testdestfile)
#save(list_of_degree_of_freedom, file=paste0(dataset_file_directory,"rdata",slash,"list_of_degree_of_freedom.test.RData"))
#testdestfile=paste0(dataset_file_directory,"rdata",slash,"list_of_degree_of_freedom.test.RData")
# 測試連線 --------------------------------
dbtype <- RMariaDB::MariaDB() #RSQLite::SQLite()
dbhost <- "localhost"#"192.168.10.202" #"140.112.7.192" #"192.168.16.1" localhost
dbname <- "thesis"
dbusername <- "root"
dbpassword <- rstudioapi::askForPassword("input password")
dbport <- 3306
dbconnect_info <- list(
  "drv"=dbtype,
  "host"=dbhost,
  "dbname"=dbname,
  "username"=dbusername,
  "password"=dbpassword,
  "port"=dbport
)
#ssl_disabled='True',#TRUE,
#use_pure='True'
#group = "my-db"
con <- do.call(DBI::dbConnect, dbconnect_info)
DBI::dbDisconnect(con)

# 迴圈開始處 --------------------------------
reset_multi_p()
#基本上可以用兩次大迴圈跑，最外層大迴圈第一次可以generate list to filter out those whose degree of freedom<0，第二次則從resid.df>=0的模型開始處理
#list_of_degree_of_freedom <- lapply(t_survey_data_test, function(a_single_survey_dataset,...) {
for (index_of_testing_resid_df in c(1:2)) {
  general_nrep <- switch(as.character(index_of_testing_resid_df), "1"=1, "2"=35)
  general_maxiter <- switch(as.character(index_of_testing_resid_df), "1"=1, "2"=1200)
  general_everystep_n <- switch(as.character(index_of_testing_resid_df), "1"=900, "2"=20)
  #for win7 eng
  #t_sessioninfo_running_with_cpu_locale<-"Windows7x64build7601ServicePack1Intel(R)Xeon(R)CPUE5-2650v3@2.30GHzEnglish"
  general_start_iters_name <- c(
    "Windows8x64build9200Intel(R)Xeon(R)CPUE5-2650v3@2.30GHzChinese(Traditional)_Taiwan.950",
    t_sessioninfo_running_with_cpu_locale,
    "Windows8x64build9200Intel(R)Core(TM)i7-9750HCPU@2.60GHzChinese(Traditional)_Taiwan.950"
    #"Ubuntu18.04.3LTSIntel(R)Core(TM)i7-9750HCPU@2.60GHzzh_TW.UTF-8"
    #"Ubuntu18.04.2LTSIntel(R)Core(TM)i5-4210UCPU@1.70GHzzh_TW.UTF-8"
    #"Windows7x64build7601ServicePack1Intel(R)Xeon(R)CPUE5-2650v3@2.30GHzEnglish",
    #"Windows7x64build7601ServicePack1Intel(R)Xeon(R)CPUE5-2650v3@2.30GHzChinese(Traditional)_Taiwan.950",
    #"Windows7x64build7601ServicePack1Intel(R)Xeon(R)CPUE5-2660v4@2.00GHzChinese(Traditional)_Taiwan.950",
    #"Windows7x64build7601ServicePack1Intel(R)Xeon(R)CPUE5-2650v3@2.30GHzEnglish",
    #"Ubuntu18.04.2LTSIntel(R)Core(TM)i5-7400CPU@3.00GHzzh_TW.UTF-8"
  )
  general_start_iters <- seq(from=1, to=length(general_start_iters_name)) %>% as.list() %>%
    magrittr::set_names(general_start_iters_name)
  general_start_iter <- general_start_iters[[t_sessioninfo_running_with_cpu_locale]]
  general_setting_n_computers <- length(general_start_iters)
  tmp_dest_loadandsave_file <- paste0(dataset_file_directory,"rdata",slash,"list_of_degree_of_freedom.RData")
  #dbname <- paste0(dataset_file_directory,"rdata",slash,"list_of_degree_of_freedom.sqlite")

  for (a_single_survey_dataset in t_survey_data_test) {
    #a_single_survey_dataset<-t_survey_data_test[[1]]
    allImputations <- imps %>%
      lapply(function(subsetindex, df) {dplyr::filter(df, .imp==subsetindex)}, df=a_single_survey_dataset) %>%
      mitools::imputationList()
    needsurvey <- a_single_survey_dataset$SURVEY[1]
    db_table_name <- paste0("list_of_degree_of_freedom", "_", needsurvey)
    #if (needsurvey=="2004citizen" & index_of_testing_resid_df==1) {next}
    if (index_of_testing_resid_df==2) {
      #repeatloadsavedestfile(destfile=tmp_dest_loadandsave_file)
      #list_of_degree_of_freedom_of_a_survey <- repeat_connectdb_readtable_close(dbname = sqlite_dbname, tablename=db_table_name)
    } else {
      #tmp_backup_empty_list_of_degree_of_freedom <- sapply(t_survey_data_test, function(X) X$SURVEY[1], USE.NAMES = FALSE) %>%
      #  { set_names( lapply(., function(X) return(data.frame())) , .)   }
      #if (!file.exists(tmp_dest_loadandsave_file)) {
      #  list_of_degree_of_freedom<-tmp_backup_empty_list_of_degree_of_freedom
      #} else {
      #  repeatloadsavedestfile(destfile=tmp_dest_loadandsave_file)
      #}
      #list_of_degree_of_freedom_of_a_survey <- repeat_connectdb_readtable_close(dbname = sqlite_dbname, tablename=db_table_name)
    }
    #if (nrow(list_of_degree_of_freedom[[needsurvey]])==0 | gtools::invalid(list_of_degree_of_freedom[[needsurvey]]) ) {
    #if (nrow(list_of_degree_of_freedom_of_a_survey)==0 | gtools::invalid(list_of_degree_of_freedom_of_a_survey) ) {
      #list_of_degree_of_freedom[[needsurvey]]<-data.frame()
      #list_of_degree_of_freedom_of_a_survey <- data.frame()
    #}
    if (length(lcaneed_independence_attitude[[needsurvey]]) %in% c(0,1)) {
      #list_of_degree_of_freedom[[needsurvey]] <- data.frame()
      #list_of_degree_of_freedom_of_a_survey <- data.frame()
      next
    }
    cov_vars <- c(lcaneed_party_constituency[[needsurvey]],lcaneed_ethnicity[[needsurvey]],lcaneed_identity[[needsurvey]],lcaneed_other_cov[[needsurvey]]) %>%
      unique()
    cov_vars_combns <- unlist(
        lapply(1:length(cov_vars),
          function(i)combn(1:length(cov_vars),i,simplify=FALSE)
      )
      ,recursive=FALSE) %>%
      lapply(FUN=function(X,var) extract(var,X), var=cov_vars)
    cov_vars_combns_check_str <- future_lapply(cov_vars_combns, function(cov_vars_combn) {
      modelformula_prefix <- paste0(lcaneed_independence_attitude[[needsurvey]], collapse=",", sep="") %>%
        paste0("cbind(", ., ") ~ ")#"cbind(v90,v91,v92) ~"
      modelformula_suffix <- paste0(cov_vars_combn, collapse="+")
      modelformula_for_debug <- paste0(modelformula_prefix, modelformula_suffix, collapse="")
      return(modelformula_for_debug)
    }) %>%
      unlist()
    modelformula_df_for_debug <- future_lapply(cov_vars_combns_check_str, function(modelformula,imps) {
      cbind("modelformula"=modelformula,"nclass"=3:5) %>% cbind(., .imp=rep(imps, each=nrow(.))) %>% as.data.frame() %>%
        mutate(modelformula=as.character(modelformula), nclass=as.integer(as.character(nclass)  ) ) %>%
        mutate(.imp=as.integer(as.character(.imp)) ) %>%
        return()
    }, imps=imps) %>% dplyr::bind_rows()
    
    if (index_of_testing_resid_df==1) {
      tryCatch({
        con <- do.call(DBI::dbConnect, dbconnect_info)
        modelformula_df_for_debug[0,] %>%
          mutate(nclass=NA,residdf=NA,content=NA,llik=NA,aic=NA,bic=NA,Gsq=NA,Chisq=NA,Nobs=NA,nrep=NA,maxiter=NA,.imp=NA) %>%
          mutate_at(.vars=c("modelformula","content"), .funs=as.character) %>%
          mutate_at(.vars=c("llik","aic","bic","Gsq","Chisq"), .funs=as.numeric) %>%
          mutate_at(.vars=c("nclass","residdf","Nobs","nrep","maxiter",".imp"), .funs=as.integer) %>%
          DBI::dbWriteTable(con, db_table_name, .)
        DBI::dbDisconnect(con)}, error=function(e) {
          cat("Failed on dbWriteTable")
          cat(str(e))
          }
        )
    }
    
    repeat {
      list_of_degree_of_freedom_of_a_survey <- c(list(tablename=db_table_name), dbconnect_info) %>%
        do.call(repeat_connectdb_readtable_close, .)
      if (identical(list_of_degree_of_freedom_of_a_survey,"failedonprocessing")) {
        stop()
      }
      if (index_of_testing_resid_df==1 & nrow(modelformula_df_for_debug)<=nrow(list_of_degree_of_freedom_of_a_survey)) {
        break
      }
      #maxobservs<-max(Filter(function(X) !gtools::invalid(X),list_of_degree_of_freedom[[needsurvey]]$Nobs))
      maxobservs<-max(Filter(function(X) !gtools::invalid(X),list_of_degree_of_freedom_of_a_survey$Nobs))
      #if (nrow(list_of_degree_of_freedom[[needsurvey]])>0) {
      if (nrow(list_of_degree_of_freedom_of_a_survey)>0) {
        #qualified_lca_model <- dplyr::left_join(modelformula_df_for_debug, list_of_degree_of_freedom[[needsurvey]])
        qualified_lca_model <- dplyr::left_join(modelformula_df_for_debug, list_of_degree_of_freedom_of_a_survey, by=c("modelformula","nclass",".imp"))
        qualified_lca_model <- switch(as.character(index_of_testing_resid_df),
                                      "1"={
                                        filter(qualified_lca_model, gtools::invalid(residdf) | is.na(residdf))
                                      },
                                      "2"={
                                        filter(qualified_lca_model, nrep<general_nrep & residdf>=0 & Nobs==maxobservs)
                                      }
        )
      } else {
        qualified_lca_model <- modelformula_df_for_debug
      }
      if (nrow(qualified_lca_model)<=0) {next}
      split_factor <- rep(seq_len(ceiling(nrow(qualified_lca_model) / general_everystep_n)), each = general_everystep_n, length.out = nrow(qualified_lca_model))
      splitted_qualified_lca_models <- split(qualified_lca_model, split_factor)
      count_iter = 1
      need_splitted_qualified_lca_models <- seq.int(from=general_start_iter, to=length(splitted_qualified_lca_models), by=general_setting_n_computers)
      #for (splitted_qualified_lca_model in splitted_qualified_lca_models[need_splitted_qualified_lca_models]) {
      splitted_qualified_lca_model <- splitted_qualified_lca_models[need_splitted_qualified_lca_models][[1]]
      if (nrow(splitted_qualified_lca_model)>0) {
        message("now it's in table number ", count_iter, " and total number of tables are ", length(splitted_qualified_lca_models[need_splitted_qualified_lca_models]))
        message("models are ", splitted_qualified_lca_model$modelformula, " and nclass are ", splitted_qualified_lca_model$nclass)
        count_iter <- count_iter+1
        #用來決定哪些模型還需要繼續處理計算LCA的條件
        if (nrow(splitted_qualified_lca_model)<=0) {next}
        message("number of qualified models to process: ", nrow(splitted_qualified_lca_model))
        models_range <- seq(from=1, to=nrow(splitted_qualified_lca_model))
        for (imp in imps) {
          an_imputeddf_of_single_survey_dataset <- filter(a_single_survey_dataset,.imp==imp)
          temp_df_list_of_degree_of_freedom <- future_mapply(
            function(modelformula, n_latentclasses, nrep, maxiter, X, ...) {
              tryCatch({custom_generate_LCA_model(
                X=X,
                modelformula=modelformula,
                n_latentclasses=n_latentclasses,
                nrep=nrep,
                maxiter=maxiter
              )},error=function(e) {
                cat("Failed on modelformula = ", modelformula, "\n nclass = ", n_latentclasses, sep="") ## browser()
                stop(e)
              }
              )}
            ,modelformula=splitted_qualified_lca_model$modelformula[models_range], n_latentclasses=splitted_qualified_lca_model$nclass[models_range], nrep=rep(general_nrep,length(models_range)), maxiter=rep(general_maxiter,length(models_range)), SIMPLIFY=FALSE, MoreArgs = list(X=an_imputeddf_of_single_survey_dataset,custom_generate_LCA_model=custom_generate_LCA_model)
          ) %>% #end of future_mapply processing LCA #exportvar = c("custom_parallel_lapply","a_single_survey_dataset","custom_generate_LCA_model","lcaneed_independence_attitude","needsurvey","dataset_file_directory","slash","t_sessioninfo_running_with_cpu","modelformula_prefix"),  exportlib = c("base","poLCA","parallel","gtools","magrittr"),  outfile = paste0(dataset_file_directory,"rdata",slash,"parallel_handling_process-",t_sessioninfo_running_with_cpu,".txt")
            lapply(function(LCAresultmodel) {
              LCAdfresult <- cbind(
                "modelformula"=LCAresultmodel$modelformula,
                "nclass"=LCAresultmodel$nclass,
                "residdf"=LCAresultmodel$residdf,
                "content"=LCAresultmodel$content,
                "llik"=LCAresultmodel$llik,
                "aic"=LCAresultmodel$aic,
                "bic"=LCAresultmodel$bic,
                "Gsq"=LCAresultmodel$Gsq,
                "Chisq"=LCAresultmodel$Chisq,
                "Nobs"=LCAresultmodel$Nobs,
                "nrep"=LCAresultmodel$nrep,
                "maxiter"=LCAresultmodel$maxiter
              ) %>%  as.data.frame() %>%
                mutate(
                  "modelformula"=as.character(modelformula),
                  "nclass"=as.integer(as.character(nclass)),
                  "residdf"=as.integer(as.character(residdf)),
                  "content"=as.character(content),
                  "llik"=as.double(as.character(llik)),
                  "aic"=as.double(as.character(aic)),
                  "bic"=as.double(as.character(bic)),
                  "Gsq"=as.double(as.character(Gsq)),
                  "Chisq"=as.double(as.character(Chisq)),
                  "Nobs"=as.integer(as.character(Nobs)),
                  "nrep"=as.integer(as.character(nrep)),
                  "maxiter"=as.integer(as.character(maxiter))
                )
              return(LCAdfresult)
            }) %>%
            dplyr::bind_rows() %>%
            mutate(nclass=as.integer(as.character(nclass)), Nobs=as.integer(as.character(Nobs)), bic=as.double(as.character(bic)), residdf=as.integer(as.character(residdf)), nrep=as.integer(as.character(nrep)), maxiter=as.integer(as.character(maxiter)), aic=as.double(as.character(aic)), Gsq=as.double(as.character(Gsq)), Chisq=as.double(as.character(Chisq)), llik=as.double(as.character(llik)), .imp=imp )
          #if (file.exists(tmp_dest_loadandsave_file)) {
          repeat {
            #multi process hint: pragma journal_mode=wal;
            #https://blog.csdn.net/wql2rainbow/article/details/73650056
            #https://grokbase.com/t/perl/dbi-users/10cpmpbmsf/sqlite-concurrency-issue
            test_load_save_list_of_deg_freedom<-tryCatch({
              #repeatloadsavedestfile(destfile=tmp_dest_loadandsave_file)
              #list_of_degree_of_freedom[[needsurvey]] <- switch(as.character(nrow(list_of_degree_of_freedom[[needsurvey]])),
              #                                                    "0"=data.frame(),
              #                                                    dplyr::anti_join(list_of_degree_of_freedom[[needsurvey]],splitted_qualified_lca_model, by=c("modelformula", "nclass"))
              #                                                    )
              #list_of_degree_of_freedom[[needsurvey]] <- switch(as.character(nrow(list_of_degree_of_freedom[[needsurvey]])),
              #                                                    "0"=temp_df_list_of_degree_of_freedom,
              #                                                    dplyr::bind_rows(list_of_degree_of_freedom[[needsurvey]],temp_df_list_of_degree_of_freedom)
              #)
              #repeatloadsavedestfile(loadsavemode="save", destfile=tmp_dest_loadandsave_file, breakafterntimes=1)
              list_of_degree_of_freedom_already_in_db <- c(list(tablename=db_table_name), dbconnect_info) %>%
                do.call(repeat_connectdb_readtable_close, .) #repeat_connectdb_readtable_close(dbname = sqlite_dbname, tablename=db_table_name)
              #to_be_updated_rows_list_of_degree_of_freedom_in_db <- dplyr::anti_join(list_of_degree_of_freedom_already_in_db, temp_df_list_of_degree_of_freedom) %>% #把重複的刪掉
              #  dplyr::semi_join(list_of_degree_of_freedom_already_in_db, ., by=c("modelformula", "nclass"))
              to_be_updated_rows_list_of_degree_of_freedom_in_db_for_bindquery <- dplyr::select(list_of_degree_of_freedom_already_in_db, modelformula, nclass, .imp) %>%
                dplyr::filter(.imp==imp) %>%
                dplyr::semi_join(temp_df_list_of_degree_of_freedom, by=c("modelformula", "nclass", ".imp")) %>%
                dplyr::left_join(temp_df_list_of_degree_of_freedom, by=c("modelformula", "nclass", ".imp")) %>%
                dplyr::anti_join(list_of_degree_of_freedom_already_in_db) %>%
                dplyr::select(-.imp, everything())
                #dplyr::mutate("cond_modelformula"=modelformula, "cond_nclass"=nclass)
              to_be_inserted_rows_list_of_degree_of_freedom <- dplyr::select_at(to_be_updated_rows_list_of_degree_of_freedom_in_db_for_bindquery, vars(-starts_with("cond_")) ) %>%
                dplyr::anti_join(temp_df_list_of_degree_of_freedom, .)
              if (nrow(to_be_updated_rows_list_of_degree_of_freedom_in_db_for_bindquery)>0) {
                con <- do.call(DBI::dbConnect, dbconnect_info) #DBI::dbConnect(RSQLite::SQLite(), dbname = sqlite_dbname)
                #paste0('update ',db_table_name, ' set "modelformula"=?, "nclass"=?, "resid.df"=?, "content"=?, "llik"=?, "aic"=?, "bic"=?, "Gsq"=?, "Chisq"=?, "Nobs"=?, "nrep"=?, "maxiter"=? WHERE "modelformula"=? AND "nclass"=?')
                #updatesql <- DBI::dbSendQuery(con, paste0('update ',db_table_name, ' set modelformula=:modelformula, nclass=:nclass, resid.df=:resid.df, content=:content, llik=:llik, aic=:aic, bic=:bic, Gsq=:Gsq, Chisq=:Chisq, Nobs=:Nobs, nrep=:nrep, maxiter=:maxiter WHERE modelformula=:cond_modelformula AND nclass=:cond_nclass'))
                #updatesql <- DBI::dbSendQuery(con, paste0('UPDATE `',db_table_name, '` SET `modelformula`=$modelformula, `nclass`=$nclass, `residdf`=$residdf, `content`=$content, `llik`=$llik, `aic`=$aic, `bic`=$bic, `Gsq`=$Gsq, `Chisq`=$Chisq, `Nobs`=$Nobs, `nrep`=$nrep, `maxiter`=$maxiter WHERE `modelformula`=$cond_modelformula AND `nclass`=$cond_nclass AND `.imp`=$.imp'))
                #updatesql <- DBI::dbSendQuery(con, paste0('update ',db_table_name, ' set "modelformula"=?, "nclass"=?, "resid.df"=?, "content"=?, "llik"=?, "aic"=?, "bic"=?, "Gsq"=?, "Chisq"=?, "Nobs"=?, "nrep"=?, "maxiter"=? WHERE "modelformula"=? AND "nclass"=?'))
                #DBI::dbBind(updatesql, to_be_updated_rows_list_of_degree_of_freedom_in_db_for_bindquery)  # send the updated data
                updatesql <- DBI::dbSendQuery(con, paste0('UPDATE `',db_table_name, '` SET `modelformula`=?, `nclass`=?, `residdf`=?, `content`=?, `llik`=?, `aic`=?, `bic`=?, `Gsq`=?, `Chisq`=?, `Nobs`=?, `nrep`=?, `maxiter`=? WHERE `.imp`=? AND `modelformula`=? AND `nclass`=?'))
                
                to_be_updated_rows_list_of_degree_of_freedom_in_db_for_bindquery %>% mutate("cond_modelformula"=modelformula, "cond_nclass"=nclass) %>% as.list() %>% unname() %>%
                  DBI::dbBind(updatesql, .)
                DBI::dbClearResult(updatesql)  # release the prepared statement
              }
              #sqlwriteresult<-switch(as.character(nrow(list_of_degree_of_freedom[[needsurvey]])),
              #  "1"=DBI::dbWriteTable(con, db_table_name, to_be_inserted_rows_list_of_degree_of_freedom, append=TRUE),
              #  "2"=DBI::dbWriteTable(con, db_table_name, to_be_inserted_rows_list_of_degree_of_freedom, append=TRUE)
              #)
              if (nrow(to_be_inserted_rows_list_of_degree_of_freedom)>0) {
                con <- do.call(DBI::dbConnect, dbconnect_info)
                DBI::dbWriteTable(con, db_table_name, to_be_inserted_rows_list_of_degree_of_freedom, append=TRUE)
                DBI::dbDisconnect(con)
              }
              DBI::dbDisconnect(con)
            },
            error=function(e) {
              DBI::dbDisconnect(con)
              message(e)
              return("failedonprocessing")
            }
            )
            DBI::dbDisconnect(con)
            if (gtools::invalid(test_load_save_list_of_deg_freedom)) {
              DBI::dbDisconnect(con)
              break
            } else if (test_load_save_list_of_deg_freedom!="failedonprocessing" | isTRUE(test_load_save_list_of_deg_freedom)) {
              DBI::dbDisconnect(con)
              break
            }
          }
          #}
        } #end of procceed each imputed df

      } #end of check nrow(splitted_qualified_lca_model)>0
    } #end of repeat until same nrow in total check and in database
  }
}#,lcaneed_independence_attitude=lcaneed_independence_attitude,
#lcaneed_party_constituency=lcaneed_party_constituency,
#lcaneed_ethnicity=lcaneed_ethnicity,
#lcaneed_identity=lcaneed_identity,
#lcaneed_other_cov=lcaneed_other_cov)

#lcaneed_independence_attitude,lcaneed_party_constituency,lcaneed_ethnicity,lcaneed_identity,lcaneed_other_cov
# 迴圈結束處 --------------------------------

library(RSQLite)
library(DBI)
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sqlite_dbname)
DBI::dbWriteTable(con, "list_of_degree_of_freedom", list_of_degree_of_freedom[[1]])
DBI::dbDisconnect(con)
dbReadTable(con, "list_of_degree_of_freedom")
DBI::dbWriteTable(con, "mtcars", mtcars)

if ({testing_on_LCAvarsel<-FALSE; testing_on_LCAvarsel}) {
  
  needsurvey<-"2010overall"
  cov_parameter_in_formula<-mapply(function(party,ethnicity,identity,other) {
    return(union_all(party,ethnicity,identity,other))
  },party=lcaneed_party_constituency,
  ethnicity=lcaneed_ethnicity,
  identity=lcaneed_identity,
  other=lcaneed_other_cov, SIMPLIFY = FALSE)
  
  workingmodelformula<-paste0(
      "cbind(",
      paste(lcaneed_independence_attitude[[needsurvey]],collapse=","),
      ") ~ ",
      paste0(cov_parameter_in_formula[[needsurvey]],collapse="+"),
      collapse=""
    )
  survey_data_test %<>% set_names(names(cov_parameter_in_formula))
  needY<-survey_data_test[[needsurvey]][,lcaneed_independence_attitude[[needsurvey]]]
  needX<-survey_data_test[[needsurvey]][,cov_parameter_in_formula[[needsurvey]]]
  result<-LCAvarsel(Y=needY,
            G = 3:5,
            X = needX,
            search = c("forward"),
            independence = FALSE,
            swap = FALSE,
            bicDiff = 0,
            start = NULL,
            checkG = TRUE,
            parallel = TRUE,
            verbose = TRUE)#interactive()
  
  #ctrlLCA = controlLCA(),
  #ctrlReg = controlReg(),
  #ctrlGA = controlGA(),

}

if ({usingold_lca_search_method<-FALSE; usingold_lca_search_method}) {
  
  load(file=paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_covparty_combn_list_contains_2010overall.RData"))
  if ({usingLCAmodel2010overall<-TRUE; usingLCAmodel2010overall}) {
    message("NICE") #LCAmodel_with_indp_covparty_combn_2010overall
  }
  load(file=paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_covparty_combn.RData"))
  #save(LCAmodel_with_indp_covparty_combn,file=paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_covparty_combn.RData"))
  LCAmodel_with_indp_covparty_combn<-list("2004citizen"=LCAmodel_with_indp_covparty_combn[["2004citizen"]],"2010overall"=LCAmodel_with_indp_covparty_combn_2010overall)
  LCAmodel_with_indp_covparty_combn_2010overall<-NULL
  LCAmodel_with_indp_covparty_combn<-list("2004citizen"=list(),"2010overall"=LCAmodel_with_indp_covparty_combn)
  #save(LCAmodel_with_indp_covparty_combn,file=paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_covparty_combn_for_apply.RData"))
  load(file=paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_covparty_combn_for_apply.RData"))
  
  #測試調整參數用
  t_survey_data_test<-survey_data_test
  need_in_lcaneed_party_constituency_combn_i<-1
  prompt_for_lcamodel <- FALSE #prompt_for_lcamodel <- TRUE
  only_check_between_models <- FALSE #only_check_between_models <- TRUE
  #names(t_survey_data_test)<-c("2004citizen", "2010env", "2010overall", "2016citizen")
  #test <- lapply(t_survey_data_test, function(a_single_survey_dataset,...)  {
  for (key in names(t_survey_data_test[3])) {
    message("need_in_lcaneed_party_constituency_combn_i is ",need_in_lcaneed_party_constituency_combn_i)
    a_single_survey_dataset <- t_survey_data_test[[key]]
    needsurvey <- a_single_survey_dataset$SURVEY[1]
    cov_vars <- c(lcaneed_party_constituency[[needsurvey]],lcaneed_ethnicity[[needsurvey]],lcaneed_identity[[needsurvey]],lcaneed_other_cov[[needsurvey]])
    cov_vars_combns <- unlist(
      lapply(1:length(cov_vars),
             function(i)combn(1:length(cov_vars),i,simplify=FALSE)
      )
      ,recursive=FALSE) %>%
      lapply(FUN=function(X,var) extract(var,X), var=cov_vars)
    if (needsurvey=="2004citizen") {
      #cov_vars_combns %<>% rlist::list.filter("v98b" %in% .)
    }
    modelformula_prefix <- paste0(lcaneed_independence_attitude[[needsurvey]], collapse=",", sep="") %>%
      paste0("cbind(", ., ") ~")#"cbind(v90,v91,v92) ~"
    list_information_df_of_lca <- lapply(getElement(LCAmodel_with_indp_covparty_combn,needsurvey), function(X) {
      #X <- getElement(LCAmodel_with_indp_covparty_combn,needsurvey)[[183]]
      workable_model <- which(sapply(X$model,class)=="poLCA") 
      workable_model_len <- length(workable_model)
      if (workable_model_len>0) {
        ret_inf <- lapply(X[workable_model]$model, function(model) {
          data.frame(
            "nclass"=model$nclass,
            "modelformula"=model$modelformula,
            "n_predclass"=length(model$predclass),
            "bic"=model$bic,
            "aic"=model$aic,
            "residf"=model$residdf,
            "chisq"=model$Chisq,
            "Gsq"=model$Gsq,
            "llik"=model$llik
          )
        }) %>%
          plyr::rbind.fill()
      } else {
        if (is.null(X$formula)) X$formula<-NA
        ret_inf <- data.frame("nclass"=NA, "modelformula"=X$formula, "n_predclass" = NA, "bic"=NA,
                              "aic"=NA, "residf"=NA, "chisq"=NA, "Gsq"=NA, "llik"=NA)
      }
      return(ret_inf)
    }) %>%
      plyr::rbind.fill() #View(filter(list_information_df_of_lca,nclass>2) %>% arrange(bic))
    if (only_check_between_models) {
      if (!exists("list_information_df_of_lcas")) {list_information_df_of_lcas <- list()}
      list_information_df_of_lcas[[needsurvey]] <- list_information_df_of_lca
      next()
    }
    if (!is.null(list_information_df_of_lca)) {
      filter_and_arranged_inf_of_lca <- filter(list_information_df_of_lca,nclass>2) %>%
        arrange(desc(n_predclass),bic)
      if (prompt_for_lcamodel) {
        if (needsurvey=="2004citizen") {
          filter_and_arranged_inf_of_lca <- filter(filter_and_arranged_inf_of_lca,!stringr::str_detect(modelformula,"v92_"))
        }
        View(filter_and_arranged_inf_of_lca)
      }
    } else {
      filter_and_arranged_inf_of_lca <- NULL
    }
    if (length(LCAmodel_with_indp_covparty_combn)<1) {LCAmodel_with_indp_covparty_combn<-list()}
    for (lcaneed_party_constituency_combn_item in cov_vars_combns[need_in_lcaneed_party_constituency_combn_i:length(cov_vars_combns)]) {
      need_in_lcaneed_party_constituency_combn <- list()
      need_in_lcaneed_party_constituency_combn[[needsurvey]] <- lcaneed_party_constituency_combn_item
      if (prompt_for_lcamodel) {
        process_lca_formula_n <- as.integer(readline(prompt="Enter an integer: "))
        need_in_lcaneed_party_constituency_combn[[needsurvey]] <- 
          filter_and_arranged_inf_of_lca[[process_lca_formula_n,c("modelformula")]] %>% as.character() %>% stringi::stri_split(regex=" ~ ") %>% unlist() %>% getElement(2) %>%  stringi::stri_split(regex="\\+") %>% unlist()
      }
      lcaformula<-paste(need_in_lcaneed_party_constituency_combn[[needsurvey]],collapse = "+") %>%
        paste(modelformula_prefix, ., collapse = "")
      if (lcaformula %in% as.character(list_information_df_of_lca$modelformula)) {
        message(lcaformula," in!, now is ", Sys.time())
        if (prompt_for_lcamodel!=TRUE) {
          next()
        }
      } else {
        message(lcaformula," not in! to be processed, now is ", Sys.time())
      }
      #先測試 degree of freedom 是否為負數不然白忙一場
      LCAmodel_with_indp_covparty_testfor_resid_df <- custom_generate_LCA_model(
        X=a_single_survey_dataset,
        exportvar=c("t_survey_data_test","custom_parallel_lapply","lcaneed_independence_attitude","lcaneed_party_constituency","lcaneed_ethnicity","lcaneed_identity","lcaneed_other_cov"),
        exportlib=c("base","magrittr","dplyr","poLCA","parallel","gtools"),
        outfile=paste0(dataset_file_directory,"rdata",slash,"parallel_handling_process-",t_sessioninfo_running_with_cpu,".txt"),
        nrep = 1,
        maxiter = 1,
        firstlcaneed=lcaneed_independence_attitude,
        secondlcaneed=need_in_lcaneed_party_constituency_combn
      ) #,secondlcaneed=lcaneed_party_constituency,thirdlcaneed=lcaneed_ethnicity,fourthlcaneed=lcaneed_identity,fifthlcaneed=lcaneed_other_cov
      LCAmodel_with_indp_covparty_test_correct_classes <- lapply(LCAmodel_with_indp_covparty_testfor_resid_df, function(X) {
        return_classes <- switch(class(X),
          "list"={
            class_assigned<-sapply(X,function(Z) {Z$nclass})
            wheredfbiggerthanzero<-which(sapply(X,getElement,"residdf")>0)
            class_assigned[wheredfbiggerthanzero]
          },
          "poLCA"=if(X$residdf>0) X$nclass else NULL,
          NULL
        )
        return(return_classes)
      }) #%>%
      #setNames(stringi::stri_replace(names(.),replacement="",regex=".sav"))
      if (prompt_for_lcamodel) {LCAmodeling_again <- readline(prompt="Modeling again? (N=NO): ")}
      LCAmodel_with_indp_covparty<-switch(as.character(prompt_for_lcamodel && LCAmodeling_again=="N"),
        "TRUE"={
          for (lcamodelitem in LCAmodel_with_indp_covparty_combn[[needsurvey]]) {
            if (!identical(lcamodelitem$model,list())) {
              message(lcamodelitem$model[[1]]$modelformula)
              if(!gtools::invalid(lcamodelitem$model[[1]]$modelformula) && lcamodelitem$model[[1]]$modelformula == lcaformula) {
                message("FOUND ！")
                break
              }
            } else {
              next()
            }
          }
          lcamodelitem$model
        },
        tryCatch(
          {
            custom_generate_LCA_model(
              X=a_single_survey_dataset,
              exportvar=c("t_survey_data_test","custom_parallel_lapply","lcaneed_independence_attitude","lcaneed_party_constituency","lcaneed_ethnicity","lcaneed_identity","lcaneed_other_cov"),
              exportlib=c("base","magrittr","dplyr","poLCA","parallel","gtools"),
              n_latentclasses=LCAmodel_with_indp_covparty_test_correct_classes,
              nrep = 75,
              maxiter = 4500,
              outfile=paste0(dataset_file_directory,"rdata",slash,"parallel_handling_process-",t_sessioninfo_running_with_cpu,".txt"),
              firstlcaneed=lcaneed_independence_attitude,
              secondlcaneed=need_in_lcaneed_party_constituency_combn
            )
          },# warning=function(war) {
          #print(paste("MY_WARNING:  ",war))
          #return(NULL)      }, 
          error=function(err) {
            print(paste("MY_ERROR:  ",err))
            return(NULL)
          }, finally = {
            print(paste("End Try&Catch LCAmodel_with_indp_covparty"))
          }
        ))
      if (gtools::invalid(LCAmodel_with_indp_covparty)) {next()}
      if (prompt_for_lcamodel) {
        path_to_temp_xlsx_for_lca_result <- here::here("LCAModel.xlsx")
        cat("\014")
        stdout<-capture.output(LCAmodel_with_indp_covparty)
        openxlsx::write.xlsx(LCAresult_to_sheet(stdout), file=path_to_temp_xlsx_for_lca_result)
        #shell(path_to_temp_xlsx_for_lca_result)
        #shell.exec(path_to_temp_xlsx_for_lca_result)
        #system(shell.exec,path_to_temp_xlsx_for_lca_result)
        next()
      } else {
        LCAmodel_with_indp_covparty_combn[[needsurvey]] <- rlist::list.append(LCAmodel_with_indp_covparty_combn[[needsurvey]],list(
          "formula"=paste(unlist(need_in_lcaneed_party_constituency_combn),collapse="+") %>% paste(modelformula_prefix, .),
          "correctclasses"=LCAmodel_with_indp_covparty_test_correct_classes,
          "model"=LCAmodel_with_indp_covparty
        ))
        need_in_lcaneed_party_constituency_combn_i <- need_in_lcaneed_party_constituency_combn_i+1
        if ((need_in_lcaneed_party_constituency_combn_i %% 8)==0) {
          save(LCAmodel_with_indp_covparty_combn,file=paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_covparty_combn.RData"))
        }
      }
      #break
    }
    break
  }#,lcaneed_independence_attitude=lcaneed_independence_attitude,lcaneed_ethnicity=lcaneed_ethnicity,lcaneed_identity=lcaneed_identity,lcaneed_other_cov=lcaneed_other_cov)
  
  a_single_survey_dataset <- survey_data_test[[1]]
  need_in_lcaneed_party_constituency_combn<-list("2004citizen"=c("v98b"), "2010overall"=c("v87a1r","v94","v89r"))
  testmodel <- poLCA::poLCA( #custom_generate_LCA_model(
    data=a_single_survey_dataset, #X=a_single_survey_dataset
    #exportvar=c("t_survey_data_test","custom_parallel_lapply","lcaneed_independence_attitude","lcaneed_party_constituency","lcaneed_ethnicity","lcaneed_identity","lcaneed_other_cov"),
    #exportlib=c("base","magrittr","dplyr","poLCA","parallel","gtools"),
    nclass=3, #n_latentclasses=3,
    nrep = 75,
    maxiter = 4500,
    formula = as.formula("cbind(v95,v96,v97) ~ v98b")
    #outfile=paste0(dataset_file_directory,"rdata",slash,"parallel_handling_process-",t_sessioninfo_running_with_cpu,".txt"),
    #firstlcaneed=lcaneed_independence_attitude,
    #secondlcaneed=need_in_lcaneed_party_constituency_combn
  )
  #2004citizen cbind(v95,v96,v97) ~ v98b needs 1781
  #2010overall cbind(v90,v91,v92) ~ v87a1r+v94+v89r needs 2209
  #save(LCAmodel_with_indp_covparty_combn_2010overall,file=paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_covparty_combn_2010overall.RData"))
  #LCAmodel_with_indp_covparty_combn[["2010overall"]]<-lapply(LCAmodel_with_indp_covparty_combn[["2010overall"]], function(X) {
  #  X$model<-X$model[[1]]
  #})
  
  
  #LCAmodel_with_indp_covparty_combn <- lapply(LCAmodel_with_indp_covparty_combn, function(X,modelformula_prefix,...) {
  #  simformula <- customgsub(X$formula,pattern = '", "', replacement = "+") %>%
  #    customgsub(pattern = '[c()"]', replacement = '') %>%
  #    paste(modelformula_prefix, ., collapse="")
  #  X$formula <- simformula
  #  return(X)
  #},modelformula_prefix=modelformula_prefix)
}
#save(LCAmodel_with_indp_covparty,file=paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_covparty",t_sessioninfo_running,".RData"))
#levels(t_survey_data_test[[1]]$myown_atti_ind)[levels(t_survey_data_test[[1]]$myown_atti_ind)=="1"] <- "統一"

# 第六-2部份：LCA latent variables 潛在類別模式政黨傾向 ====================
t_survey_data_test<-survey_data_test

LCAmodel_with_partyconstituency_nocov <- custom_parallel_lapply(
  X=t_survey_data_test,
  FUN=custom_generate_LCA_model,
  exportvar=c("t_survey_data_test","lcaneed_independence_attitude","lcaneed_party_constituency","lcaneed_ethnicity","lcaneed_identity","lcaneed_other_cov"),
  exportlib=c("base",lib,"poLCA"),
  outfile=paste0(dataset_file_directory,"rdata",slash,"parallel_handling_process-",t_sessioninfo_running_with_cpu,".txt"),
  firstlcaneed=lcaneed_party_constituency,
  secondlcaneed=lcaneed_independence_attitude,
  mc.set.seed = TRUE,
  mc.cores=parallel::detectCores()
) #,secondlcaneed=lcaneed_party_constituency,thirdlcaneed=lcaneed_ethnicity,fourthlcaneed=lcaneed_identity,fifthlcaneed=lcaneed_other_cov

save(LCAmodel_with_partyconstituency_nocov,file=paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_partyconstituency_nocov",t_sessioninfo_running,".RData"))


# 第六-3部份：潛在類別分析：將分析結果整併入dataset --------------------------------------------------

library(parallel)
library(future)
library(future.apply)
reset_multi_p()
#load(paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_covpartyUbuntu18.04.1LTS_do_not_delete.RData"))
#load(paste0(dataset_file_directory,"rdata",slash,"LCAmodel_2004citizen_final.RData"), verbose=TRUE)
#load(paste0(dataset_file_directory,"rdata",slash,"LCAmodel_2010overall_final.RData"), verbose=TRUE)

orig_dest_load_file <- paste0(dataset_file_directory,"rdata",slash,"PRECIOUS_DO_NOT_DELETE_list_of_degree_of_freedom_for_LCA_on_indp_atti.RData")
tmp_dest_loadandsave_file <- paste0(dataset_file_directory,"rdata",slash,"list_of_degree_of_freedom.RData")
tmp_dest_sqlite_file <- paste0(dataset_file_directory,"rdata",slash,"list_of_degree_of_freedom_backup.sqlite")
repeatloadsavedestfile(destfile=orig_dest_load_file)
list_of_degree_of_freedom_2004citizen_sqlite <- repeat_connectdb_readtable_close(dbname=tmp_dest_sqlite_file, tablename="list_of_degree_of_freedom_2004citizen", synchronous=NULL)
list_of_degree_of_freedom_2010overall_sqlite <- repeat_connectdb_readtable_close(dbname=tmp_dest_sqlite_file, tablename="list_of_degree_of_freedom_2010overall", synchronous=NULL)
list_of_degree_of_freedom[["2004citizen"]] %<>% dplyr::select(modelformula, nclass, content, llik, aic, bic, Gsq, Chisq, Nobs, nrep, maxiter) %>%
  dplyr::left_join(dplyr::select(list_of_degree_of_freedom_2004citizen_sqlite, modelformula, nclass, residdf))
list_of_degree_of_freedom[["2010overall"]] %<>% dplyr::select(modelformula, nclass, content, llik, aic, bic, Gsq, Chisq, Nobs, nrep, maxiter) %>%
  dplyr::left_join(dplyr::select(list_of_degree_of_freedom_2010overall_sqlite, modelformula, nclass, residdf))

reset_multi_p()
needLCAmodels <- mapply(function(singlesurvey_degree_of_freedom, survey_data) {
  needrank <- 1:20
  elementreptimes <- length(needrank)
  #DBI::dbDisconnect(con)
  if (nrow(singlesurvey_degree_of_freedom)>0) {
    modelformulas<-filter(singlesurvey_degree_of_freedom, residdf>=0) %>% arrange(bic) %>% getElement("modelformula") %>% extract(needrank)
    nclasses<-filter(singlesurvey_degree_of_freedom, residdf>=0) %>% arrange(bic) %>% getElement("nclass") %>% extract(needrank)
    needmodels<-future_mapply(custom_generate_LCA_model, n_latentclasses=nclasses, modelformula=modelformulas, MoreArgs=list(X=survey_data, nrep=50, maxiter=2300), SIMPLIFY = FALSE )
    return(needmodels)
  } else {
    return(NULL)
  }
}, singlesurvey_degree_of_freedom=list_of_degree_of_freedom, survey_data=survey_data_test)

sapply(needLCAmodels[[1]], function(X) {message(X$modelformula, X$nclass, ",bic=", X$bic)})
save(needLCAmodels,file=paste0(dataset_file_directory,"rdata",slash,"needLCAmodels.RData"))
load(file=paste0(dataset_file_directory,"rdata",slash,"needLCAmodels.RData"))
path_to_temp_xlsx_for_lca_result <- here::here("LCAModel.xlsx")
cat("\014")
stdout<-capture.output(needLCAmodels[[1]][[1]])
openxlsx::write.xlsx(LCAresult_to_sheet(stdout), file=path_to_temp_xlsx_for_lca_result)


if({LCA_recoding_by_restarting_modeling<-FALSE;LCA_recoding_by_restarting_modeling}) {
  new_LCAmodel_with_indp_covparty_2004citizen_3_classes<-poLCA::poLCA(
    data=t_survey_data_test[[1]],
    formula=as.formula(paste0(
      "cbind(",
      paste(magrittr::extract2(lcaneed_independence_attitude,"2004citizen"),collapse=","),
      ") ~ ",
      paste0(lcaneed_party_constituency[[1]],collapse="+"),
      collapse=""
    )),
    nclass = 3,
    #graphs = TRUE,
    maxiter = 1000,
    nrep=30,
    probs.start=poLCA::poLCA.reorder(
      LCAmodel_with_indp_covparty[[1]][[2]]$probs.start,
      c(2,3,1)
    )
  )
  table(LCAmodel_with_indp_covparty[[1]][[2]]$predclass)
  survey_data_test[[1]]$myown_indp_atti<-factor(
    LCAmodel_with_indp_covparty[[1]][[2]]$predclass,
    levels = c(1,2,3), labels = c("[2] 中立", "[1] 統一", "[3] 獨立") #needs interpretation and modify here
  ) %>% table()
  new_LCAmodel_with_indp_covparty_2010overall_3_classes<-poLCA::poLCA(
    data=t_survey_data_test[[3]],
    formula=as.formula(paste0(
      "cbind(",
      paste(magrittr::extract2(lcaneed_independence_attitude,"2010overall"),collapse=","),
      ") ~ ",
      paste0(lcaneed_party_constituency[[3]],collapse="+"),
      collapse=""
    )),
    nclass = 3,
    #graphs = TRUE,
    maxiter = 1000,
    nrep=30,
    probs.start=poLCA::poLCA.reorder(
      LCAmodel_with_indp_covparty[[3]][[2]]$probs.start,
      c(3,1,2)
    )
  )
}

LCAmodel_2004citizen<-needLCAmodels[[1]][[1]]
LCAmodel_2010overall<-needLCAmodels[[3]][[1]]
survey_data_test[[1]]$myown_indp_atti <- LCAmodel_2004citizen$predclass %>%
  dplyr::recode_factor(`1`="[1] 統一", `3`="[2] 中立", `2`="[3] 獨立", .ordered = TRUE)
survey_data_test[[3]]$myown_indp_atti <- LCAmodel_2010overall$predclass %>%
  dplyr::recode_factor(`1`="[1] 統一", `2`="[2] 中立", `3`="[3] 獨立", .ordered = TRUE)
survey_data_test[[4]]$myown_indp_atti<-survey_data_test[[4]]$h10r

if ({record_myown_religion<-FALSE; record_myown_religion}) {
  survey_data_test[[3]]$myown_religion %<>% dplyr::recode_factor(
    `1`="[1] 佛教",
    `2`="[2] 道教",
    `3`="[3] 民間信仰",
    `4`="[4] 一貫道",
    `5`="[5] 回教(伊斯蘭教)",
    `6`="[6] 天主教",
    `7`="[7] 基督教",
    `8`="[8] 沒有宗教信仰",
    `9`="[9] 其他,請說明",
    .ordered = FALSE)
}
#save(survey_data_test,file=paste0(dataset_in_scriptsfile_directory, "survey_data_test.RData"))


