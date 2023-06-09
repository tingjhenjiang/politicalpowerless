
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
  workingmodelformula<-switch(
    as.character(modelformula),
    "NA"=paste0(
      "cbind(",
      paste(firstlcaneed,collapse=","),
      ") ~ ",
      paste0(cov_parameter_in_formula,collapse="+"),
      collapse=""
    ),modelformula)
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
    switch(
      as.character(gtools::invalid(n_latentclasses) | (is.integer(n_latentclasses) && n_latentclasses<=0) ),
      "TRUE"=list("result"=NULL),
      "FALSE"=preserve_warning_tryCatch({
        poLCA(
          data = X,
          formula = formulated_workingmodelformula,
          nclass = n_latentclasses,#poXi,
          nrep = nrep,
          maxiter = maxiter
        )}
      )#,
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

repeat_connectdb_readtable_close <- function(drv, dbname, ..., tablename, breakafterntimes=10) { #function(dbname, tablename, dbtype=RSQLite::SQLite(), ..., breakafterntimes=10) {
  #, setting_synchronous = "off"
  loop_times <- 1
  othervar <- unlist(list(...))
  othervar_names <- names(list(...))
  
  repeat {
    testprocess<-tryCatch({
      con <- DBI::dbConnect(drv, dbname = dbname, ...)
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


# general arguments df --------------------------------
general_indepence_arguments_df<-lapply(names(lcaneed_independence_attitude), function(needsurvey) {
  cov_vars <- c(lcaneed_party_constituency[[needsurvey]],lcaneed_ethnicity[[needsurvey]],lcaneed_identity[[needsurvey]],lcaneed_other_cov[[needsurvey]]) %>%
    unique()
  cov_vars_combns <- unlist(
    mclapply(1:length(cov_vars),
             function(i)combn(1:length(cov_vars),i,simplify=FALSE),
             mc.cores=detectedcores
    ),
    recursive=FALSE) %>%
    mclapply(FUN=function(X,var) extract(var,X), var=cov_vars, mc.cores=detectedcores)
  cov_vars_combns_check_str <- mclapply(cov_vars_combns, function(cov_vars_combn) {
    modelformula_prefix <- paste0(lcaneed_independence_attitude[[needsurvey]], collapse=",", sep="") %>%
      paste0("cbind(", ., ") ~ ")#"cbind(v90,v91,v92) ~"
    modelformula_suffix <- paste0(cov_vars_combn, collapse="+")
    modelformula_for_debug <- paste0(modelformula_prefix, modelformula_suffix, collapse="")
    return(modelformula_for_debug)
  }, mc.cores=detectedcores) %>%
    unlist()
  modelformula_df_for_debug <- data.frame("modelformula"=cov_vars_combns_check_str) %>%
    cbind(., nclass = rep(3:5, each = nrow(.))) %>%
    cbind(., .imp = rep(imps, each = nrow(.))) %>%
    dplyr::mutate(modelformula=as.character(modelformula), nclass=as.integer(as.character(nclass)  ) ) %>%
    dplyr::mutate(.imp=as.integer(as.character(.imp)) )
  
  return(modelformula_df_for_debug)
}) %>% set_names(names(lcaneed_independence_attitude))
  






# 迴圈開始處 --------------------------------
#check at http://localhost/scripts/phpscripts/list_of_degree_of_freedom_counts.php

if ({trynewiter<-TRUE;trynewiter}) {
  load_lib_or_install(c("itertools"))
  it <- ihasNext(product(index_of_testing_resid_df = 1:2, survey=1:length(survey_data_imputed) )) #, needimp = imps
  general_nreps <- c(1, 35)
  general_maxiters <- c(1, 1200)
  while(hasNext(it)) {
    iterx <- nextElem(it)
    #general_nrep <- general_nreps[iterx$index_of_testing_resid_df]
    #general_maxiter <- general_maxiters[iterx$index_of_testing_resid_df]
    needsurvey <- survey_data_imputed[[iterx$survey]]$SURVEY[1]
    db_table_name <- paste0("list_of_degree_of_freedom", "_", needsurvey)
    modelformula_df_for_debug <- general_indepence_arguments_df[[needsurvey]]
    if (length(lcaneed_independence_attitude[[needsurvey]]) %in% c(0,1)) next
    if (iterx$index_of_testing_resid_df==1) {
      tryCatch({
        con <- do.call(DBI::dbConnect, dbconnect_info)
        modelformula_df_for_debug[0,] %>%
          dplyr::mutate(nclass=NA,residdf=NA,content=NA,llik=NA,aic=NA,bic=NA,Gsq=NA,Chisq=NA,Nobs=NA,nrep=NA,maxiter=NA,.imp=NA) %>%
          dplyr::mutate_at(.vars=c("modelformula","content"), .funs=as.character) %>%
          dplyr::mutate_at(.vars=c("llik","aic","bic","Gsq","Chisq"), .funs=as.numeric) %>%
          dplyr::mutate_at(.vars=c("nclass","residdf","Nobs","nrep","maxiter",".imp"), .funs=as.integer) %>%
          DBI::dbWriteTable(con, db_table_name, .)
        DBI::dbDisconnect(con)}, error=function(e) {
          cat("Failed on dbWriteTable")
          cat(str(e))
        }
      )
    }
    
    #check progress
    con <- do.call(DBI::dbConnect, dbconnect_info)
    check_rows_results <- paste("SELECT COUNT(*) FROM", db_table_name) %>%
      DBI::dbGetQuery(con, .) %>%
      extract(1,1)
    DBI::dbDisconnect(con)
    if (iterx$index_of_testing_resid_df==1 & nrow(modelformula_df_for_debug)<=check_rows_results) {
      next
    } else {
      if (!exists("list_of_degree_of_freedom_of_a_survey") | needsurvey!="2004citizen") {
        list_of_degree_of_freedom_of_a_survey <- c(list(tablename=db_table_name), dbconnect_info) %>%
          do.call(repeat_connectdb_readtable_close, .)
      }
      if (identical(list_of_degree_of_freedom_of_a_survey,"failedonprocessing")) {
        stop()
      }
    }
    maxobservs<-max(Filter(function(X) !gtools::invalid(X),list_of_degree_of_freedom_of_a_survey$Nobs))
    if (nrow(list_of_degree_of_freedom_of_a_survey)>0) {
      if ({lazy_copy_to_fill_not_analyzed_data<-FALSE;lazy_copy_to_fill_not_analyzed_data}) {
        #t_sessioninfo_running_with_cpu_locale=="Windows8x64build9200Intel(R)Core(TM)i7-9750HCPU@2.60GHzChinese(Traditional)_Taiwan.950"
        #先從資料庫裡面找出已經至少分析過一次但還沒有完整分析的偷懶範本，忽略不同imp差異而填上degrees of freedom後寫入資料庫
        lazyjoin_list_of_degree_of_freedom_of_a_survey<-subset(list_of_degree_of_freedom_of_a_survey, select=-c(.imp)) %>%
          filter(nrep<2) %>%
          .[!duplicated(.[c("modelformula","nclass")]),] %>%
          cbind(., .imp = rep(imps, each = nrow(.))) %>%
          dplyr::anti_join(list_of_degree_of_freedom_of_a_survey,by=c("modelformula","nclass",".imp"))
        if (nrow(lazyjoin_list_of_degree_of_freedom_of_a_survey)>0) {
          con <- do.call(DBI::dbConnect, dbconnect_info)
          DBI::dbWriteTable(con, db_table_name, lazyjoin_list_of_degree_of_freedom_of_a_survey, append=TRUE)
          DBI::dbDisconnect(con)
          list_of_degree_of_freedom_of_a_survey %<>% dplyr::anti_join(lazyjoin_list_of_degree_of_freedom_of_a_survey, by=c("modelformula","nclass",".imp")) %>%
            dplyr::bind_rows(lazyjoin_list_of_degree_of_freedom_of_a_survey)
        }
      }
      
      #找出有曾經poLCA分析建檔過的資料並且串到目標分析對象
      qualified_lca_model <- dplyr::left_join(modelformula_df_for_debug, list_of_degree_of_freedom_of_a_survey, by=c("modelformula","nclass",".imp"))
      qualified_lca_model <- switch(
        as.character(iterx$index_of_testing_resid_df),
        "1"={
          dplyr::filter(qualified_lca_model, gtools::invalid(residdf) | is.na(residdf))
          }, #從目標分析對象找出連一次pcLCA分析都沒有過的
        "2"={
          dplyr::filter(qualified_lca_model, nrep<general_nreps[iterx$index_of_testing_resid_df] & residdf>=0 & Nobs==maxobservs)
          } #從目標分析對象找出還沒有完整pcLCA分析過的
      )
    } else {
      qualified_lca_model <- modelformula_df_for_debug
    }
    if (nrow(qualified_lca_model)<=0) {next}
    
    #idx_process_ratio<-0.001
    idx<-round(nrow(qualified_lca_model)/10*idx_process_ratio,0)
    if (idx<1) idx<-1
    modelrange <- seq(from=idx, to=nrow(qualified_lca_model))
    mclapply(modelrange, function(exec_row_i, qualified_lca_model_df, surveydf, ...) {
      tryCatch({
        singlerow_qualified_lca_model<-qualified_lca_model_df[exec_row_i,] %>%
          dplyr::rename(n_latentclasses=nclass)
        needimp<-singlerow_qualified_lca_model$.imp
        LCAresultmodel<-singlerow_qualified_lca_model %>%
          dplyr::select(modelformula, n_latentclasses, nrep, maxiter) %>%
          dplyr::mutate(nrep=general_nreps[iterx$index_of_testing_resid_df]) %>%
          dplyr::mutate(maxiter=general_maxiters[iterx$index_of_testing_resid_df]) %>%
          #dplyr::mutate(nrep=3) %>%
          #dplyr::mutate(maxiter=3) %>%
          as.list() %>%
          rlist::list.append(X={
            dplyr::filter(surveydf, .imp==needimp)
          }) %>%
          do.call(custom_generate_LCA_model, .)
        LCAdfresult <- cbind(
          "modelformula"=LCAresultmodel$modelformula,
          "nclass"=LCAresultmodel$nclass,
          ".imp"=needimp,
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
          dplyr::mutate(
            "modelformula"=as.character(modelformula),
            "nclass"=as.integer(as.character(nclass)),
            ".imp"=as.integer(as.character(.imp)),
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
        con <- do.call(DBI::dbConnect, dbconnect_info)
        custom_df_replaceinto_db(dbconnect_info, db_table_name, LCAdfresult, columns=names(LCAdfresult))
        DBI::dbDisconnect(con)
      }, #end of trycatch 1st argument
      error=function(err) {
        #message("temp_df_list_of_degree_of_freedom error at 1")
        message(err)
        #skip_to_next <- TRUE #cat("Failed on modelformula = ", modelformula, "\n nclass = ", n_latentclasses, sep="") ## browser()
      } ) #end of trycatch
      return(singlerow_qualified_lca_model$modelformula[1])
    }, #end of custom LCA model generating func
    qualified_lca_model_df=qualified_lca_model, surveydf=survey_data_imputed[[needsurvey]], mc.cores=detectedcores#1
    ) #end of mclapply
  } # end of iters
}


# 備份原有 有點冗長的方法 -----------------
if ({workablemethod<-FALSE;workablemethod}) {
  for (index_of_testing_resid_df in c(1:2)) {
    general_nrep <- switch(as.character(index_of_testing_resid_df), "1"=1, "2"=35)
    general_maxiter <- switch(as.character(index_of_testing_resid_df), "1"=1, "2"=1200)
    general_everystep_n <- switch(as.character(index_of_testing_resid_df), "1"=900, "2"=20)
    general_start_iters <- seq(from=1, to=length(general_start_iters_name)) %>% as.list() %>%
      magrittr::set_names(general_start_iters_name)
    general_start_iter <- general_start_iters[[t_sessioninfo_running_with_cpu_locale]]
    general_setting_n_computers <- length(general_start_iters)
    tmp_dest_loadandsave_file <- paste0(dataset_file_directory,"rdata",slash,"list_of_degree_of_freedom.RData")
    #dbname <- paste0(dataset_file_directory,"rdata",slash,"list_of_degree_of_freedom.sqlite")
    
    for (a_single_survey_dataset in t_survey_data_test) {
      #a_single_survey_dataset<-t_survey_data_test[[1]]
      allImputations <- imps %>%
        mclapply(function(subsetindex, df) {dplyr::filter(df, .imp==subsetindex)}, df=a_single_survey_dataset, mc.cores=detectedcores) %>%
        mitools::imputationList()
      needsurvey <- a_single_survey_dataset$SURVEY[1]
      db_table_name <- paste0("list_of_degree_of_freedom", "_", needsurvey)
      if (index_of_testing_resid_df==2) {
        #
      } else {
        #
      }
      
      if (length(lcaneed_independence_attitude[[needsurvey]]) %in% c(0,1)) {
        #list_of_degree_of_freedom[[needsurvey]] <- data.frame()
        #list_of_degree_of_freedom_of_a_survey <- data.frame()
        next
      }
      
      modelformula_df_for_debug <- general_indepence_arguments_df[[needsurvey]]
      
      if (index_of_testing_resid_df==1) {
        tryCatch({
          con <- do.call(DBI::dbConnect, dbconnect_info)
          modelformula_df_for_debug[0,] %>%
            dplyr::mutate(nclass=NA,residdf=NA,content=NA,llik=NA,aic=NA,bic=NA,Gsq=NA,Chisq=NA,Nobs=NA,nrep=NA,maxiter=NA,.imp=NA) %>%
            dplyr::mutate_at(.vars=c("modelformula","content"), .funs=as.character) %>%
            dplyr::mutate_at(.vars=c("llik","aic","bic","Gsq","Chisq"), .funs=as.numeric) %>%
            dplyr::mutate_at(.vars=c("nclass","residdf","Nobs","nrep","maxiter",".imp"), .funs=as.integer) %>%
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
        if (nrow(list_of_degree_of_freedom_of_a_survey)>0) {
          if (t_sessioninfo_running_with_cpu_locale=="Windows8x64build9200Intel(R)Core(TM)i7-9750HCPU@2.60GHzChinese(Traditional)_Taiwan.950") {
            #先從資料庫裡面找出已經至少分析過一次但還沒有完整分析的偷懶範本，忽略不同imp差異而填上degrees of freedom後寫入資料庫
            lazyjoin_list_of_degree_of_freedom_of_a_survey<-subset(list_of_degree_of_freedom_of_a_survey, select=-c(.imp)) %>%
              filter(nrep<2) %>%
              .[!duplicated(.[c("modelformula","nclass")]),] %>%
              cbind(., .imp = rep(imps, each = nrow(.))) %>%
              dplyr::anti_join(list_of_degree_of_freedom_of_a_survey,by=c("modelformula","nclass",".imp"))
            if (nrow(lazyjoin_list_of_degree_of_freedom_of_a_survey)>0) {
              con <- do.call(DBI::dbConnect, dbconnect_info)
              DBI::dbWriteTable(con, db_table_name, lazyjoin_list_of_degree_of_freedom_of_a_survey, append=TRUE)
              DBI::dbDisconnect(con)
              list_of_degree_of_freedom_of_a_survey %<>% dplyr::anti_join(lazyjoin_list_of_degree_of_freedom_of_a_survey, by=c("modelformula","nclass",".imp")) %>%
                dplyr::bind_rows(lazyjoin_list_of_degree_of_freedom_of_a_survey)
            }
          }
          
          #找出有曾經poLCA分析建檔過的資料並且串到目標分析對象
          qualified_lca_model <- dplyr::left_join(modelformula_df_for_debug, list_of_degree_of_freedom_of_a_survey, by=c("modelformula","nclass",".imp"))
          qualified_lca_model <- switch(as.character(index_of_testing_resid_df),
                                        "1"={
                                          dplyr::filter(qualified_lca_model, gtools::invalid(residdf) | is.na(residdf))
                                        }, #從目標分析對象找出連一次pcLCA分析都沒有過的
                                        "2"={
                                          dplyr::filter(qualified_lca_model, nrep<general_nrep & residdf>=0 & Nobs==maxobservs)
                                        } #從目標分析對象找出還沒有完整pcLCA分析過的
          )
        } else {
          qualified_lca_model <- modelformula_df_for_debug
        }
        if (nrow(qualified_lca_model)<=0) {next}
        split_factor <- rep(seq_len(ceiling(nrow(qualified_lca_model) / general_everystep_n)), each = general_everystep_n, length.out = nrow(qualified_lca_model))
        splitted_qualified_lca_models <- split(qualified_lca_model, split_factor)
        count_iter = 1
        need_splitted_qualified_lca_models <- seq.int(from=general_start_iter, to=length(splitted_qualified_lca_models), by=general_setting_n_computers)
        #每次都只處理序列的第一個目標
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
            skip_to_next <- FALSE
            an_imputeddf_of_single_survey_dataset <- dplyr::filter(a_single_survey_dataset,.imp==imp)
            repeat {
              temp_df_list_of_degree_of_freedom <- tryCatch({mcmapply(
                function(modelformula, n_latentclasses, nrep, maxiter, X, ...) {
                  custom_generate_LCA_model(
                    X=X,
                    modelformula=modelformula,
                    n_latentclasses=n_latentclasses,
                    nrep=nrep,
                    maxiter=maxiter
                  )}, #end of custom LCA model generating func
                modelformula=splitted_qualified_lca_model$modelformula[models_range], n_latentclasses=splitted_qualified_lca_model$nclass[models_range], nrep=rep(general_nrep,length(models_range)), maxiter=rep(general_maxiter,length(models_range)), SIMPLIFY=FALSE, MoreArgs = list(X=an_imputeddf_of_single_survey_dataset,custom_generate_LCA_model=custom_generate_LCA_model), mc.cores=detectedcores
              )}, #end of futuremapply
              error=function(e) {
                message("temp_df_list_of_degree_of_freedom error at 1")
                message(e)
                skip_to_next <- TRUE #cat("Failed on modelformula = ", modelformula, "\n nclass = ", n_latentclasses, sep="") ## browser()
              })
              if (class(temp_df_list_of_degree_of_freedom)!="logical") {
                break
              } else {
                message("output of model is a logical value error and retry")
              }
            }
            if(skip_to_next) { next }
            temp_df_list_of_degree_of_freedom1.5<-temp_df_list_of_degree_of_freedom[sapply(temp_df_list_of_degree_of_freedom,class)=="poLCA"]
            #end of future_mapply processing LCA #exportvar = c("custom_parallel_lapply","a_single_survey_dataset","custom_generate_LCA_model","lcaneed_independence_attitude","needsurvey","dataset_file_directory","slash","t_sessioninfo_running_with_cpu","modelformula_prefix"),  exportlib = c("base","poLCA","parallel","gtools","magrittr"),  outfile = paste0(dataset_file_directory,"rdata",slash,"parallel_handling_process-",t_sessioninfo_running_with_cpu,".txt")
            temp_df_list_of_degree_of_freedom2 <- tryCatch({mclapply(temp_df_list_of_degree_of_freedom1.5, function(LCAresultmodel) {
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
                dplyr::mutate(
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
            }, mc.cores=detectedcores) #end of cbind and mutate
            }, #end of lapply
            error=function(e) {
              message("temp_df_list_of_degree_of_freedom error at 2")
              skip_to_next <- TRUE #cat("Failed on modelformula = ", modelformula, "\n nclass = ", n_latentclasses, sep="") ## browser()
            }) #end of tryCatch
            if(skip_to_next) { next }
            temp_df_list_of_degree_of_freedom2.5<-temp_df_list_of_degree_of_freedom2[sapply(temp_df_list_of_degree_of_freedom2,class)=="data.frame"]
            tryCatch({temp_df_list_of_degree_of_freedom3 <- temp_df_list_of_degree_of_freedom2.5 %>%
              dplyr::bind_rows() %>%
              dplyr::mutate(nclass=as.integer(as.character(nclass)), Nobs=as.integer(as.character(Nobs)), bic=as.double(as.character(bic)), residdf=as.integer(as.character(residdf)), nrep=as.integer(as.character(nrep)), maxiter=as.integer(as.character(maxiter)), aic=as.double(as.character(aic)), Gsq=as.double(as.character(Gsq)), Chisq=as.double(as.character(Chisq)), llik=as.double(as.character(llik)), .imp=imp )},
              error=function(e) {
                message("temp_df_list_of_degree_of_freedom error at 3")
                message(e)
                skip_to_next <- TRUE
              }
            )
            if(skip_to_next) { next }
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
                #list_of_degree_of_freedom_already_in_db<-list_of_degree_of_freedom_of_a_survey
                list_of_degree_of_freedom_already_in_db <- c(list(tablename=db_table_name), dbconnect_info) %>%
                  do.call(repeat_connectdb_readtable_close, .) #repeat_connectdb_readtable_close(dbname = sqlite_dbname, tablename=db_table_name)
                #to_be_updated_rows_list_of_degree_of_freedom_in_db <- dplyr::anti_join(list_of_degree_of_freedom_already_in_db, temp_df_list_of_degree_of_freedom) %>% #把重複的刪掉
                #  dplyr::semi_join(list_of_degree_of_freedom_already_in_db, ., by=c("modelformula", "nclass"))
                to_be_updated_rows_list_of_degree_of_freedom_in_db_for_bindquery <- dplyr::select(list_of_degree_of_freedom_already_in_db, modelformula, nclass, .imp) %>%
                  dplyr::filter(.imp==imp) %>%
                  dplyr::semi_join(temp_df_list_of_degree_of_freedom3, by=c("modelformula", "nclass", ".imp")) %>%
                  dplyr::left_join(temp_df_list_of_degree_of_freedom3, by=c("modelformula", "nclass", ".imp")) %>%
                  dplyr::anti_join(list_of_degree_of_freedom_already_in_db) %>%
                  dplyr::select(-.imp, everything())
                #dplyr::mutate("cond_modelformula"=modelformula, "cond_nclass"=nclass)
                to_be_inserted_rows_list_of_degree_of_freedom <- dplyr::select_at(to_be_updated_rows_list_of_degree_of_freedom_in_db_for_bindquery, vars(-starts_with("cond_")) ) %>%
                  dplyr::anti_join(temp_df_list_of_degree_of_freedom3, .)
                if (nrow(to_be_updated_rows_list_of_degree_of_freedom_in_db_for_bindquery)>0) {
                  con <- do.call(DBI::dbConnect, dbconnect_info) #DBI::dbConnect(RSQLite::SQLite(), dbname = sqlite_dbname)
                  #paste0('update ',db_table_name, ' set "modelformula"=?, "nclass"=?, "resid.df"=?, "content"=?, "llik"=?, "aic"=?, "bic"=?, "Gsq"=?, "Chisq"=?, "Nobs"=?, "nrep"=?, "maxiter"=? WHERE "modelformula"=? AND "nclass"=?')
                  #updatesql <- DBI::dbSendQuery(con, paste0('update ',db_table_name, ' set modelformula=:modelformula, nclass=:nclass, resid.df=:resid.df, content=:content, llik=:llik, aic=:aic, bic=:bic, Gsq=:Gsq, Chisq=:Chisq, Nobs=:Nobs, nrep=:nrep, maxiter=:maxiter WHERE modelformula=:cond_modelformula AND nclass=:cond_nclass'))
                  #updatesql <- DBI::dbSendQuery(con, paste0('UPDATE `',db_table_name, '` SET `modelformula`=$modelformula, `nclass`=$nclass, `residdf`=$residdf, `content`=$content, `llik`=$llik, `aic`=$aic, `bic`=$bic, `Gsq`=$Gsq, `Chisq`=$Chisq, `Nobs`=$Nobs, `nrep`=$nrep, `maxiter`=$maxiter WHERE `modelformula`=$cond_modelformula AND `nclass`=$cond_nclass AND `.imp`=$.imp'))
                  #updatesql <- DBI::dbSendQuery(con, paste0('update ',db_table_name, ' set "modelformula"=?, "nclass"=?, "resid.df"=?, "content"=?, "llik"=?, "aic"=?, "bic"=?, "Gsq"=?, "Chisq"=?, "Nobs"=?, "nrep"=?, "maxiter"=? WHERE "modelformula"=? AND "nclass"=?'))
                  #DBI::dbBind(updatesql, to_be_updated_rows_list_of_degree_of_freedom_in_db_for_bindquery)  # send the updated data
                  updatesql <- DBI::dbSendQuery(con, paste0('UPDATE `',db_table_name, '` SET `modelformula`=?, `nclass`=?, `residdf`=?, `content`=?, `llik`=?, `aic`=?, `bic`=?, `Gsq`=?, `Chisq`=?, `Nobs`=?, `nrep`=?, `maxiter`=? WHERE `.imp`=? AND `modelformula`=? AND `nclass`=?'))
                  
                  to_be_updated_rows_list_of_degree_of_freedom_in_db_for_bindquery %>% dplyr::mutate("cond_modelformula"=modelformula, "cond_nclass"=nclass) %>% as.list() %>% unname() %>%
                    DBI::dbBind(updatesql, .)
                  DBI::dbClearResult(updatesql)  # release the prepared statement
                  list_of_degree_of_freedom_of_a_survey %<>% dplyr::anti_join(to_be_updated_rows_list_of_degree_of_freedom_in_db_for_bindquery, by=c("modelformula", "nclass", ".imp") ) %>%
                    dplyr::bind_rows(to_be_updated_rows_list_of_degree_of_freedom_in_db_for_bindquery)
                }
                #sqlwriteresult<-switch(as.character(nrow(list_of_degree_of_freedom[[needsurvey]])),
                #  "1"=DBI::dbWriteTable(con, db_table_name, to_be_inserted_rows_list_of_degree_of_freedom, append=TRUE),
                #  "2"=DBI::dbWriteTable(con, db_table_name, to_be_inserted_rows_list_of_degree_of_freedom, append=TRUE)
                #)
                if (nrow(to_be_inserted_rows_list_of_degree_of_freedom)>0) {
                  custom_df_replaceinto_db(dbconnect_info, db_table_name, to_be_inserted_rows_list_of_degree_of_freedom, columns=c('modelformula','nclass','residdf','content','llik','aic','bic','Gsq','Chisq','Nobs','nrep','maxiter','.imp'))
                  list_of_degree_of_freedom_of_a_survey %<>% dplyr::anti_join(to_be_inserted_rows_list_of_degree_of_freedom, by=c("modelformula", "nclass", ".imp") ) %>%
                    dplyr::bind_rows(to_be_inserted_rows_list_of_degree_of_freedom)
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
  }
}