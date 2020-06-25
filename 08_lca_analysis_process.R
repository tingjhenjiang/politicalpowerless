# 第Ｏ部份：環境設定 --------------------------------
if (!("benchmarkme" %in% rownames(installed.packages()))) try(install.packages("benchmarkme"))
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-try(paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model))
t_sessioninfo_running_with_cpu_locale<-try(gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2])))
source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/shared_functions.R"), encoding="UTF-8")

gc(verbose=TRUE)


# 第八部份：LCA latent variables 潛在類別模式資料清理  ================================= 
if (!file.exists(paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt.RData"))) {
  dir.create(dataset_in_scriptsfile_directory,recursive=TRUE)
  download.file(
    "http://homepage.ntu.edu.tw/~r03a21033/voterecord/miced_survey_9_with_mirt.RData",
    paste0(dataset_in_scriptsfile_directory,"miced_survey_9_mirt.RData"))
}
load(paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt.RData"), verbose=TRUE)
load(file=paste0(save_dataset_in_scriptsfile_directory,"miced_survey_2surveysonly_mirt.RData"))

imps <- imputation_sample_i_s
imps <- 1:15
detectedcores <- ifelse(check_if_windows(),1,parallel::detectCores())
load_lib_or_install(c("magrittr","parallel","rvest","dplyr","RMariaDB","poLCA","parallel","LCAvarsel","mitools","gtools","future","future.apply"))


# 環境設定測試連線 --------------------------------
message(myremoteip)
dbtype <- RMariaDB::MariaDB() #RSQLite::SQLite()
dbhost <- mysqldbhost
dbname <- "thesis"
dbusername <- "j"
dbpassword <- ifelse(exists("dbpassword"),dbpassword,askpass::askpass(prompt = "Please enter your password: "))#rstudioapi::askForPassword("input password")
dbport <-  3306
dbhost<-"tjhome.crabdance.com"
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


# refresh multicore from here --------------------------------
library(future)
reset_multi_p()
library(future.apply)


# 環境設定調整起始位置
#for win7 eng
#t_sessioninfo_running_with_cpu_locale<-"Windows7x64build7601ServicePack1Intel(R)Xeon(R)CPUE5-2650v3@2.30GHzEnglish"
general_start_iters_name <- c(
  rep("Ubuntu18.04.2LTSIntel(R)Core(TM)i5-4210UCPU@1.70GHzzh_TW.UTF-8",7),
  t_sessioninfo_running_with_cpu_locale,
  rep("Ubuntu18.04.2LTSIntel(R)Core(TM)i5-4210UCPU@1.70GHzzh_TW.UTF-8",4)
)
idx_process_ratio<-0 #2.5 #setwd('/mnt/e/Software/scripts/R/vote_record')
# 迴圈開始處 --------------------------------
detectedcores<-detectedcores #detectedcores #detectedcores #
source("08_lca_analysis_process_commonpart.R")
stop()





# 第六-3部份：潛在類別分析：將分析結果整併入dataset Apply poLCA results --------------------------------

needpoLCAsurveys<-c("2004citizen","2010overall")
needpoLCAsurveys_with_imp<-lapply(needpoLCAsurveys, function(survey) {
  paste0(rep.int(survey,times=length(imps)),"_imp",imps)
}) %>% unlist()
needpoLCAsurveys_arguments_df<-data.frame("survey"=needpoLCAsurveys) %>%
  cbind(., imp = rep(imps, each = nrow(.))) %>%
  dplyr::mutate(store_key=paste0(survey,"_",imp)) %>%
  dplyr::mutate_at(c("survey","store_key"),as.character) %>%
  dplyr::mutate_at("imp", as.integer) %>%
  dplyr::filter(survey %in% c("2010overall")) %>%
  dplyr::arrange(survey, imp)

poLCA_infodf_notshrink<-lapply(needpoLCAsurveys_with_imp, function (survey_with_imp) {
  survey_with_imp_list<-unlist(strsplit(survey_with_imp,split="_imp"))
  survey<-survey_with_imp_list[1]
  imp<-survey_with_imp_list[2]
  db_table_name<-paste0("list_of_degree_of_freedom","_",survey)
  con <- do.call(DBI::dbConnect, dbconnect_info)
  rs <- DBI::dbSendQuery(con, paste0("SELECT * FROM ",db_table_name," WHERE `residdf`>0 AND `nrep`>1 AND `.imp`=",imp))
  already_in_sqltable_polca_records<-DBI::dbFetch(rs) %>%
    dplyr::arrange(bic,aic) %>%
    .[1:5,] %>%
    dplyr::mutate(survey=!!survey, surveyimp=!!survey_with_imp)
  DBI::dbClearResult(rs)
  DBI::dbDisconnect(con)
  return(already_in_sqltable_polca_records)
}) %>% magrittr::set_names(needpoLCAsurveys_with_imp)
dplyr::bind_rows(poLCA_infodf_notshrink) %>% write.csv(file="TMP.csv")
poLCA_infodf<-dplyr::bind_rows(poLCA_infodf_notshrink) %>%
  dplyr::group_by(surveyimp) %>%
  dplyr::arrange(bic) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(residdf), survey %in% !!names(survey_data_imputed))


#load(file=paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt_lca_clustering.RData"), verbose=TRUE)

poLCA_survey_results<-needpoLCAsurveys_arguments_df$store_key %>% #poLCA_infodf$surveyimp
  magrittr::set_names(custom_parallel_lapply(., function (survey_with_imp, ...) {
    needrow<-dplyr::filter(needpoLCAsurveys_arguments_df, store_key==!!survey_with_imp)
    #survey_with_imp_list<-unlist(strsplit(survey_with_imp,split="_imp"))
    survey<-needrow$survey #survey_with_imp_list[1]
    imp<-needrow$imp #survey_with_imp_list[2]
    singleargumentdf <- poLCA_infodf[1,] #dplyr::filter(poLCA_infodf, surveyimp==!!survey_with_imp) #magrittr::extract2(poLCA_infodf,survey_with_imp)
    dplyr::filter(survey_data_imputed[[survey]], .imp==!!imp) %>%
      poLCA(formula=as.formula(singleargumentdf$modelformula), data=., nclass=singleargumentdf$nclass, nrep=35) %>%
      return()
  }, method=parallel_method, poLCA_infodf=poLCA_infodf, needpoLCAsurveys_arguments_df=needpoLCAsurveys_arguments_df, survey_data_impute=survey_data_imputed), .) 
#poLCA_survey_result_tables<-lapply(names(poLCA_infodf), function(survey) {
#  extract2(lcaneed_independence_attitude,survey)[1] %>%
#    paste0("~1") %>%
#    rlist::list.append(condition=list(),lc=extract2(poLCA_survey_results,survey) ) %>%
#    do.call(poLCA.table, .) %>%
#    return()
#})
openxlsx::read.xlsx(path_to_survey_imputation_and_measurement_file,sheet = 1) %>%
  dplyr::filter(SURVEY %in% needpoLCAsurveys, ID %in% !!c(unlist(lcaneed_independence_attitude[survey_data_title]), "v90") ) %>%
  dplyr::distinct(SURVEY, ID, QUESTION, ANSWER) %>%
  View()
openxlsx::read.xlsx(path_to_survey_imputation_and_measurement_file,sheet = 1) %>%
  dplyr::distinct(SURVEY,ID,QUESTION,ANSWER) %>%
  View()

#v90 就兩岸關係而言,請問您覺得台灣獨立,統一,維持現狀何者比較好?
#v91 請問您同不同意若台灣宣佈獨立,仍可和中共維持和平關係,則台灣應成為一個新國家?
#v92 請問您同不同意若大陸和台灣在經濟,社會,政治各方面的條件相當,則兩岸應該統一?
recode_indp_list<-list()
for (survey_with_imp in needpoLCAsurveys_arguments_df$store_key) { #needpoLCAsurveys_with_imp
  model<-magrittr::extract2(poLCA_survey_results, survey_with_imp)
  survey_with_imp_list<-unlist(strsplit(survey_with_imp,split="_imp"))
  survey<-survey_with_imp_list[1]
  imp<-survey_with_imp_list[2]
  cat("\014")
  print(model)
  reun_catg<-readline("Group Number of 統派:")
  neutral_catg<-readline("Group Number of 中立:")
  indp_catg<-readline("Group Number of 獨派:")
  cat("\014")
  recode_indp_list[[survey_with_imp]]<-list()
  recode_indp_list[[survey_with_imp]][[reun_catg]]<-"[1] 統一"
  recode_indp_list[[survey_with_imp]][[neutral_catg]]<-"[2] 中立"
  recode_indp_list[[survey_with_imp]][[indp_catg]]<-"[3] 獨立"
  
}

if ({using_poLCA_reorder<-FALSE;using_poLCA_reorder}) {
  #poLCA的reorder很不好用，每次執行都會得到不一樣的結果，即便參數都固定
  #2004citizen 1=統派 2=騎牆(v95r 0.0882 0.3387 0.5731 0.000 0.000) 3=獨派
  #2010overall 1=騎牆 2=統派 3=獨派
  #target 1統一;2中立;3獨立	
  polcareorderlist<-list(
    "2004citizen"=c(1,2,3), #become 1=騎牆 2獨派 3=統派
    "2010overall"=c(2,1,3) #become 1=騎牆 2統派 3=獨派
  )
  poLCA_survey_results_new<-mclapply(needpoLCAsurveys, function(survey) {
    singleargumentdf <- extract2(poLCA_infodf,survey)
    probs.start <- poLCA_survey_results[[survey]]$probs.start
    new.probs.start <- poLCA.reorder(probs.start, extract2(polcareorderlist,survey) )
    dplyr::filter(survey_data_imputed[[survey]], .imp==!!singleargumentdf$.imp) %>%
      poLCA(formula=as.formula(singleargumentdf$modelformula), data=., nclass=singleargumentdf$nclass, nrep=35, probs.start = new.probs.start) %>%
      return()
  }, mc.cores = detectedcores)
  poLCA_survey_results_new[[1]]
  poLCA_survey_results_new[[2]]
}

myown_indp_atti_array_order<-sort(unlist(recode_indp_list[[1]]))
for (survey_with_imp in needpoLCAsurveys_arguments_df$store_key) { #needpoLCAsurveys_with_imp
  #survey_with_imp_list<-unlist(strsplit(survey_with_imp,split="_imp"))
  #survey<-survey_with_imp_list[1]
  #imp<-survey_with_imp_list[2]
  needrow<-dplyr::filter(needpoLCAsurveys_arguments_df, store_key==!!survey_with_imp)
  survey<-needrow$survey
  imp<-needrow$imp
  tp_check_df_imppos<-which(survey_data_imputed[[survey]]$.imp==imp)
  survey_data_imputed[[survey]]$myown_indp_atti[tp_check_df_imppos]<-dplyr::recode(poLCA_survey_results[[survey_with_imp]]$predclass, !!!recode_indp_list[[survey_with_imp]]) #, .ordered=TRUE
  #dplyr::recode_factor
}

for (survey in needpoLCAsurveys) {
  survey_data_imputed[[survey]]$myown_indp_atti %<>% as.ordered() %>%
    forcats::fct_relevel(myown_indp_atti_array_order)
}
survey_data_imputed[["2016citizen"]]$myown_indp_atti<-survey_data_imputed[["2016citizen"]]$h10r

#custom_generate_LCA_model<-function(X, n_latentclasses=3, nrep=30, maxiter=1000, modelformula=NA, firstlcaneed=c(), secondlcaneed=c(), ..., exportlib=c("base"), exportvar=c(), outfile="")

# old method for applyinh poLCA results --------------------------------
if ({usingRSQLite<-FALSE;usingRSQLite}) {
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
}

#levels(t_survey_data_test[[1]]$myown_atti_ind)[levels(t_survey_data_test[[1]]$myown_atti_ind)=="1"] <- "統一"

# 第六-2部份：LCA latent variables 潛在類別模式政黨傾向 ====================
t_survey_data_test<-survey_data_test
if ({calculatingpartyconstituency<-FALSE;calculatingpartyconstituency}) {
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
}

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



#save(survey_data_imputed,file=paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt_lca.RData"))
save(survey_data_imputed,file=paste0(save_dataset_in_scriptsfile_directory,"miced_survey_2surveysonly_mirt_lca.RData"))


