# 第Ｏ部份：環境設定 --------------------------------
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
source(file = "shared_functions.R")
survey_data_title<-c("2004citizen","2010env","2010overall","2016citizen") %>% sort()

gc(verbose=TRUE)



# 第六部份：LCA latent variables 潛在類別模式資料清理  ================================= 
load(paste0(filespath,"data",slash,"miced_survey_7_Ubuntu18.04.2LTSdf_with_mirt.RData"))

library(poLCA)
library(parallel)

lcaneed_independence_attitude<-list(
  "2004citizen"=c("v95","v96","v97"),#公投選項共變
  "2010env"=c(),#沒有統獨
  "2010overall"=c("v90","v91","v92"),
  "2016citizen"=c("h10r")#2016只有一題統獨傾向"
)
lcaneed_party_constituency<-list(
  "2004citizen"=c("v88","v89","v90","v98r","v99"),#可用 v88 v89 v90 v99 (多類別)"v98r"
  "2010env"=c("v103r"),#2010env只有一題政黨傾向
  "2010overall"=c("v84","v86","v87a1r","v85v86v87sumup","v93v94sumup","v85v88sumup"), #可用 v84 v86 v87a1r v93 v94(多類):v88,"v85v86v87sumup","v93v94sumup","v85v88sumup"
  "2016citizen"=c("h5","h6r_recode_party_for_forgotten","h7","h8r","h9r")
)
#lapply(survey_data_test[[1]][,c(lcaneed_party_constituency[[1]],"v98r")], is.na) %>%
#  lapply(sum)
lcaneed_ethnicity<-list(
  "2004citizen"=c("myown_selfid"),#"myown_dad_ethgroup","myown_mom_ethgroup",
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
  "2004citizen"=c("v87_1","v87_2","v87_3","v87_4","v87_5","v91"),#共變(公投投票選擇及參與政治活動例如凱道選擇),填補太多:"v91a","v91b","v92_1","v92_2","v92_3","v92_4"
  "2010env"=c(),
  "2010overall"=c(),
  "2016citizen"=c()
)
# 第六-1部份：LCA latent variables 潛在類別模式統獨傾向 =====================
#load(paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_eth_iden_othercovWindows8x64build9200.RData"))
#LCAmodel_with_indp

custom_generate_LCA_model<-function(X, n_latentclasses=c(3:5), nrep=30, maxiter=1000, firstlcaneed, secondlcaneed=c(), ..., exportlib=c("base"), exportvar=c(), outfile="") {#,thirdlcaneed=c(),fourthlcaneed=c(),fifthlcaneed=c(), exportlib=c("base"), exportvar=c(), outfile=""
  #X此時就是一個問卷的dataset
  #lcaneed_independence_attitude
  #lcaneed_party_constituency
  #lcaneed_ethnicity
  #lcaneed_identity
  #lcaneed_other_cov
  #message("<===== at custom_generate_LCA_model exportlib is ", exportlib, " and exportvar is ", exportvar, " and outfile is ", outfile, "=====>")
  otherlcaneed<-unlist(list(...)) #testing: otherlcaneed<-NULL
  needsurveyi<-X$SURVEY[1]
  cov_parameter_in_formula<-dplyr::union_all(
    magrittr::extract2(secondlcaneed,needsurveyi),
    magrittr::extract2(otherlcaneed,needsurveyi)
  )
  if (needsurveyi %in% names(n_latentclasses)) {
    #n_latentclasses <- magrittr::extract2(n_latentclasses,needsurveyi)
    n_latentclasses <- getElement(n_latentclasses,needsurveyi)
  }
  if (identical(cov_parameter_in_formula,logical())) {
    cov_parameter_in_formula<-"1"
  }
  modelformula<-paste0(
    "cbind(",
    paste(magrittr::extract2(firstlcaneed,needsurveyi),collapse=","),
    ") ~ ",
    paste0(cov_parameter_in_formula,collapse="+"),
    collapse=""
  )
  poLCAresult<-switch(
    as.character(length(magrittr::extract2(firstlcaneed,needsurveyi))),
    "0"=NULL,
    "1"=X[,magrittr::extract2(firstlcaneed,needsurveyi)],
    {
      custom_parallel_lapply(X=n_latentclasses,FUN=function(poXi,s_survey_data,modelformula,nrep,maxiter,...)
      {
        library(poLCA) #for fixing stange situation that Windows does not fork well
        #### lapply(2:7,function(poXi,s_survey_data) {
        lcamodelbuildtresult <-if (gtools::invalid(poXi) || (is.integer(poXi) && poXi<=0)) {
          list("model"=NULL)
        } else {
          poLCA(
            data = s_survey_data,
            formula = as.formula(modelformula),
            nclass = poXi,
            nrep = nrep,
            maxiter = maxiter
            #graphs = TRUE,
          )
        }
        lcamodelbuildtresult$nclass <- poXi
        lcamodelbuildtresult$modelformula <- modelformula
        return(lcamodelbuildtresult)
        #poXi
      },s_survey_data = X,
      modelformula = modelformula,
      nrep = nrep,
      maxiter = maxiter,
      exportvar = exportvar,
      exportlib = exportlib,
      outfile = outfile
      )
    }
  ) #end of switch
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
          "residf"=model$resid.df,
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
          wheredfbiggerthanzero<-which(sapply(X,getElement,"resid.df")>0)
          class_assigned[wheredfbiggerthanzero]
        },
        "poLCA"=if(X$resid.df>0) X$nclass else NULL,
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

#load(paste0(dataset_file_directory,"rdata",slash,"LCAmodel_with_indp_covpartyUbuntu18.04.1LTS_do_not_delete.RData"))
load(paste0(dataset_file_directory,"rdata",slash,"LCAmodel_2004citizen_final.RData"))
load(paste0(dataset_file_directory,"rdata",slash,"LCAmodel_2010overall_final.RData"))

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

survey_data_test[[1]]$myown_indp_atti <- LCAmodel_2004citizen$predclass %>%
  dplyr::recode_factor(`1`="[1] 統一", `2`="[2] 中立", `3`="[3] 獨立", .ordered = TRUE)
survey_data_test[[3]]$myown_indp_atti <- LCAmodel_2010overall$predclass %>%
  dplyr::recode_factor(`3`="[1] 統一", `2`="[2] 中立", `1`="[3] 獨立", .ordered = TRUE)
survey_data_test[[4]]$myown_indp_atti<-survey_data_test[[4]]$h10r


#save(survey_data_test,file=paste0(filespath,"data",slash,"survey_data_test.RData"))

