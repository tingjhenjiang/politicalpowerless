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
load(file=paste0(save_dataset_in_scriptsfile_directory,"miced_survey_2surveysonly_mirt.RData"), verbose=TRUE)

imps <- imputation_sample_i_s
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



# condense survey questions --------------------------------

library(poLCA)
needimps<-custom_ret_appro_kamila_clustering_parameters() %>%
  dplyr::select(-newimp) %>%
  dplyr::mutate_at("imp",as.integer)
needimps_with_construct_nclass<-needimps %>%
  cbind(., construct = rep(c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12"), each = nrow(.))) %>%
  cbind(., nclass = rep(2:7, each = nrow(.))) %>%
  mutate_cond(survey=="2016citizen" & construct=="a1", nclass=5) %>% #5
  mutate_cond(survey=="2016citizen" & construct=="a2", nclass=3) %>%
  mutate_cond(survey=="2016citizen" & construct=="a3", nclass=6) %>%
  mutate_cond(survey=="2016citizen" & construct=="a4", nclass=6) %>%
  mutate_cond(survey=="2016citizen" & construct=="a5", nclass=4) %>%
  mutate_cond(survey=="2016citizen" & construct=="a6", nclass=4) %>%
  mutate_cond(survey=="2016citizen" & construct=="a7", nclass=2) %>%
  mutate_cond(survey=="2016citizen" & construct=="a8", nclass=4) %>%
  mutate_cond(survey=="2016citizen" & construct=="a9", nclass=4) %>%
  mutate_cond(survey=="2016citizen" & construct=="a10", nclass=4) %>%
  mutate_cond(survey=="2016citizen" & construct=="a11", nclass=3) %>%
  dplyr::filter(!(survey=="2010overall" & construct %in% c("a1","a7","a8","a9","a10","a11") )) %>%
  dplyr::distinct_all() %>%
  dplyr::mutate_all(as.character) %>%
  dplyr::mutate_at(c("nclass","imp",".imp"),as.integer)

formula_reduction_args<-dplyr::bind_rows(
  data.frame("survey"="2016citizen","construct"=c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12"),
             "modelformula"=c(
               "cbind(d5e,d5b,d7j,d6f,d7k,d7b,d6g,d7g,d7e,d7a,d7h,d7i,d7c,d7f,d7d)~1",
               "cbind(d2a,d2b,d3b,d3a)~1",
               "cbind(d13a,d13b,d14b,d11b,d14a)~1",
               "cbind(c12,f3,f5,f4)~1",
               "cbind(c2,c3,c1a,c1b,c1c,c1e,c1d)~1",
               "cbind(d8a,d8b)~1",
               "cbind(d6g,d17c,d6f,d5e,d5f)~1",
               "cbind(d17a,c10,d7g,d7j)~1",
               "cbind(d14b,d14a,d14c,d11b,d13a)~1",
               "cbind(d6c,d6b,d6a,d6d,d6g,d6h,d6f,d6e)~1",
               "cbind(d5a,d5b,d5d)~1"#,
               #"cbind( )~1"
             )),
  data.frame("survey"="2010overall","construct"=c("a2","a3","a4","a5","a6"),
             "modelformula"=c(
               "cbind(v78i,v68g,v78b,v39c,v78c,v78e,v78h,v78d,v78g,v78f)~1",
               "cbind(v78i,v78a,v78e,v78b)~1",
               "cbind(v39e,v39d)~1",
               "cbind(v40,v27b)~1",
               "cbind(v78d,v78c)~1"
             ))
) %>%
  cbind(., imp = rep(imps, each = nrow(.))) %>%
  cbind(., nclass = rep(2:7, each = nrow(.))) %>%
  cbind(., nrep = rep(1, each = nrow(.))) %>%
  cbind(., maxiter = rep(100, each = nrow(.))) %>%
  dplyr::mutate(storekey=paste0(survey,"_imp",imp,"_",construct,"_nc_",nclass)) %>%
  dplyr::mutate_all(as.character) %>%
  dplyr::mutate_at(c("nclass","nrep","maxiter","imp"),as.integer)

formula_reduction_args %<>% dplyr::semi_join(needimps_with_construct_nclass, by=c("survey","construct","imp","nclass")) %>%
  dplyr::arrange(survey, construct, imp)

load(file=paste0(save_dataset_in_scriptsfile_directory,"/analyse_res/polca_models_2016policy.RData"), verbose=TRUE)
already_processed_polca_models_2016policy<-lapply(names(polca_models_2016policy), function(fikey, polca_models, formula_reduction_args) {
  X<-magrittr::extract2(polca_models, fikey)
  data.frame(nclass=X$nclass,resid.df=X$resid.df,aic=X$aic,bic=X$bic,nrep=X$nrep,storekey=fikey) %>%
    return()
}, polca_models=polca_models_2016policy) %>%
  plyr::rbind.fill() %>%
  dplyr::filter(nrep>1)

for (i in 1:2) {
  if (i==2) {
    need_formula_reduction_args<-dplyr::filter(polca_models_inf,resid.df>0) %>%
      dplyr::semi_join(formula_reduction_args, .) %>%
      dplyr::mutate(nrep=35, maxiter=1000)
  } else {
    need_formula_reduction_args<-formula_reduction_args %>%
      dplyr::filter(!(storekey %in% !!already_processed_polca_models_2016policy$storekey ))
  }
  polca_models_2016policy<-custom_apply_thr_argdf(need_formula_reduction_args, "storekey", function(fikey, loopargdf, datadf, ...) {
    needrow<-dplyr::filter(loopargdf, storekey==!!fikey)
    polcaarg<-list(
      X=dplyr::filter(datadf[[needrow$survey]], .imp==!!needrow$imp) %>% dplyr::select(-tidyselect::ends_with("NA")) ,
      n_latentclasses=needrow$nclass,
      nrep=needrow$nrep,
      maxiter=needrow$maxiter,
      modelformula=needrow$modelformula
    )
    retmodel<-do.call(custom_generate_LCA_model, args=polcaarg)
    return(retmodel)
    #custom_generate_LCA_model(X, n_latentclasses=3, nrep=30, maxiter=1000, modelformula=NA)
  }, datadf=survey_data_imputed)
  
  polca_models_inf<-lapply(names(polca_models_2016policy), function(fikey, polca_models, formula_reduction_args) {
    X<-magrittr::extract2(polca_models, fikey)
    data.frame(nclass=X$nclass,resid.df=X$resid.df,aic=X$aic,bic=X$bic,nrep=X$nrep,storekey=fikey) %>%
      dplyr::left_join(dplyr::select(formula_reduction_args, -nrep), by=c("storekey", "nclass"))
  }, polca_models=polca_models_2016policy, formula_reduction_args=formula_reduction_args) %>%
    rbind.fill()
}
backup_polca_models_2016policy<-polca_models_2016policy
load(file=paste0(save_dataset_in_scriptsfile_directory,"/analyse_res/polca_models_2016policy.RData"), verbose=TRUE)
polca_models_2016policy<-rlist::list.merge(polca_models_2016policy,backup_polca_models_2016policy)
#save(polca_models_2016policy,file=paste0(save_dataset_in_scriptsfile_directory,"/analyse_res/polca_models_2016policy.RData"))

shrink_polca_models_inf<-dplyr::arrange(polca_models_inf, bic, aic) %>%
  dplyr::semi_join(needimps_with_construct_nclass) %>%
  dplyr::group_by(survey, construct, imp) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    dplyr::bind_rows(
      list(
        "a1"="政府介入人民經濟、生存、平權職責",
        "a2"="跨國人民與資金流動",
        "a3"="治安維護與公民權",
        "a4"="言論、集會自由",
        "a5"="政府介入經濟與公用事業",
        "a6"="政府介入社福衛環",
        "a7"="與中國大陸資本往來看法",
        "a8"="對就業與生存弱勢保障",
        "a9"="政府角色、責任與義務",#（a9與a3需要在其他題項區分）
        "a10"="政府支出期望",
        "a11"="政府帶領經濟發展",
        "a12"="稅務與所得重分配") %>%
        unlist() %>%
        data.frame(survey="2016citizen",construct=names(.), constructname=.),
      list(
        "a2"="言論自由與政治競爭看法",
        "a3"="經濟平權觀",
        "a4"="家父長",
        "a5"="工作保障與社福觀",
        "a6"="和諧社會價值觀") %>%
        unlist() %>%
        data.frame(survey="2010overall",construct=names(.), constructname=.)
    )
  ) %>%
  dplyr::arrange(survey, construct, imp)

#save(shrink_polca_models_inf, recode_constructclass_list, file=paste0(dataset_in_scriptsfile_directory,"shrink_polca_models_inf.RData"))
load(file=paste0(dataset_in_scriptsfile_directory,"shrink_polca_models_inf.RData"), verbose=TRUE)
recode_constructclass_list<-list()
for (rowi in 1:nrow(shrink_polca_models_inf)) { #needpoLCAsurveys_with_imp
  needrow<-shrink_polca_models_inf[rowi, ]
  if ( !((needrow$survey=="2016citizen" & needrow$imp==1) |  (needrow$survey=="2010overall" & needrow$imp==2)  )) { #
    next
  }
  #if ( needrow$construct != "a12") {
  #  next
  #}
  repeat {
    survey_with_imp<-paste0(needrow$survey,needrow$imp)
    prefixinfstr<-paste("now in imp", needrow$imp, "c:", needrow$construct, needrow$constructname, "number of class is",needrow$nclass)
    model<-needrow$storekey %>%
      magrittr::extract2(polca_models_2016policy, .)
    cat("\014")
    print(model$probs)
    tmp_recode_list<-list()
    for (groupn in 1:needrow$nclass) {
      inputofclass<-readline(paste(prefixinfstr, "groupN of", groupn, "is(AG to PRO):") ) %>%
        paste0(needrow$constructname,"_",.,"of",needrow$nclass)
      tmp_recode_list<-c(tmp_recode_list, magrittr::set_names(c(inputofclass),groupn) )
    }
    if (is.null(recode_constructclass_list[[survey_with_imp]])) {
      recode_constructclass_list[[survey_with_imp]]<-list()
    }
    if (is.null(recode_constructclass_list[[survey_with_imp]][[needrow$construct]])) {
      recode_constructclass_list[[survey_with_imp]][[needrow$construct]]<-list()
    }
    recode_constructclass_list[[survey_with_imp]][[needrow$construct]]<-tmp_recode_list
    if (readline("Next?")=="Y") {
      save(shrink_polca_models_inf, recode_constructclass_list, file=paste0(dataset_in_scriptsfile_directory,"shrink_polca_models_inf.RData"))
      break
    }
  }
}

need_shrink_polca_models_inf<-dplyr::filter(shrink_polca_models_inf, 
                                            ((survey=="2016citizen" & imp==1) |  (survey=="2010overall" & imp==2)  )
                                            )
survey_data_with_condensed_opinion<-lapply(survey_data_imputed, function(X) {
  dplyr::select(X, id, .id, .imp) %>% return()
})
for (rowi in 1:nrow(need_shrink_polca_models_inf)) {
  needrow<-need_shrink_polca_models_inf[rowi, ]
  survey_with_imp<-paste0(needrow$survey,needrow$imp)
  message(paste(survey_with_imp,needrow$construct))
  constructlist<-magrittr::extract2(recode_constructclass_list, survey_with_imp)
  constructlist<-magrittr::extract2(constructlist, needrow$construct)
  print(constructlist)
  needmodel<-magrittr::extract2(polca_models_2016policy, needrow$storekey)
  #readline("Next")
  targetupdaterows<-which(survey_data_with_condensed_opinion[[needrow$survey]]$.imp==needrow$imp)
  survey_data_with_condensed_opinion[[needrow$survey]][targetupdaterows, paste0("construct_",needrow$survey,"_",needrow$construct)] <-
    dplyr::recode_factor(needmodel$predclass, !!!constructlist)
  #survey_data_imputed[[survey]]$myown_indp_atti[tp_check_df_imppos]<-dplyr::recode(poLCA_survey_results[[survey_with_imp]]$predclass, !!!recode_indp_list[[survey_with_imp]])
}
save(survey_data_with_condensed_opinion, file=paste0(dataset_in_scriptsfile_directory,"survey_data_with_condensed_opinion.RData"))

# 第六-3部份：潛在類別分析：將分析結果整併入dataset Apply poLCA results --------------------------------

needpoLCAsurveys<-c("2004citizen","2010overall")
needpoLCAsurveys_arguments_df<-data.frame("survey"=needpoLCAsurveys) %>%
  cbind(., imp = rep(imps, each = nrow(.))) %>%
  dplyr::mutate(store_key=paste0(survey,"_",imp)) %>%
  dplyr::mutate_at(c("survey","store_key"),as.character) %>%
  dplyr::mutate_at("imp", as.integer) %>%
  dplyr::filter(survey %in% c("2010overall")) %>%
  dplyr::arrange(survey, imp)

poLCA_infodf_notshrink<-needpoLCAsurveys_arguments_df$store_key %>%
  magrittr::set_names(lapply(., function (fikey, ...) {
    needrow<-dplyr::filter(needpoLCAsurveys_arguments_df, store_key==!!fikey)
    survey<-needrow$survey
    imp<-needrow$imp
    db_table_name<-paste0("list_of_degree_of_freedom","_",survey)
    con <- do.call(DBI::dbConnect, dbconnect_info)
    rs <- DBI::dbSendQuery(con, paste0("SELECT * FROM ",db_table_name," WHERE `residdf`>0 AND `nrep`>1 AND `.imp`=",imp))
    already_in_sqltable_polca_records<-DBI::dbFetch(rs) %>%
      dplyr::arrange(bic,aic) %>%
      .[1:5,] %>%
      dplyr::mutate(survey=!!survey, store_key=!!fikey)
    DBI::dbClearResult(rs)
    DBI::dbDisconnect(con)
    return(already_in_sqltable_polca_records)
  }, needpoLCAsurveys_arguments_df=needpoLCAsurveys_arguments_df), .)
  
dplyr::bind_rows(poLCA_infodf_notshrink) %>% write.csv(file="TMP.csv")
poLCA_infodf<-dplyr::bind_rows(poLCA_infodf_notshrink) %>%
  dplyr::group_by(store_key) %>%
  dplyr::arrange(bic) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(residdf), survey %in% !!names(survey_data_imputed)) %>%
  dplyr::distinct(modelformula, nclass)



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
if (FALSE) {
  openxlsx::read.xlsx(path_to_survey_imputation_and_measurement_file,sheet = 1) %>%
    dplyr::filter(SURVEY %in% needpoLCAsurveys, ID %in% !!c(unlist(lcaneed_independence_attitude[survey_data_title]), "v90") ) %>%
    dplyr::distinct(SURVEY, ID, QUESTION, ANSWER) %>%
    View()
  openxlsx::read.xlsx(path_to_survey_imputation_and_measurement_file,sheet = 1) %>%
    dplyr::distinct(SURVEY,ID,QUESTION,ANSWER) %>%
    View()
}


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

for (survey in unique(needpoLCAsurveys_arguments_df$survey)) {
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

# pick common factor ====

vec1<-stri_split("v78d	v78c	v39c	v68g", regex="\\t") %>% unlist()
vec2<-stri_split("v78h	v78i	v78a	v78e	v78b", regex="\\t") %>% unlist()
vec3<-stri_split("v78h	v78i	v78a	v78e	v78b", regex="\\t") %>% unlist()
vec4<-stri_split("v78b	v78e	v78a	v78i	v78h", regex="\\t") %>% unlist()
vec5<-stri_split("v78g	v78i	v78a	v78e	v78b", regex="\\t") %>% unlist()
vec6<-stri_split("v78h	v78i	v78a	v78e	v78b", regex="\\t") %>% unlist()
Reduce(base::intersect, list(vec1,vec2,vec3,vec4,vec5,vec6)) %>% paste0(sep=",") %>% message()

# test：CDM::slca ====================
need_formula_reduction_args <- needimps %>%
  dplyr::mutate(storekey=paste0(survey,"_imp",imp))
loopargdf<-need_formula_reduction_args
polca_models_2016policy<-custom_apply_thr_argdf(need_formula_reduction_args, "storekey", function(fikey, loopargdf, datadf, ...) {
  needrow<-dplyr::filter(loopargdf, storekey==!!fikey)
  tmpdata<-dplyr::filter(datadf[[needrow$survey]], .imp==!!needrow$imp) %>% dplyr::select(-tidyselect::ends_with("NA"))
  modelinginputdata<-tmpdata %>%
    dplyr::select( tidyselect::any_of(c("v91","v90r","v92",
                                        "v78e","v78h","v78d","v78g","v78f"
                                      ))) %>%
    dplyr::mutate_all(unclass) %>%
    as.matrix()
  I<-ncol(modelinginputdata)
  #Design matrix for x_{ijh} with q_{ihjv} entries. 
  #must be an array with four dimensions referring to
  #items (i), categories (h), latent classes (j) and λ parameters (v).
  #needrow$nclass
  nclass<-3
  ncatg<-2
  #nclass<-2
  #ncatg<-2
  desmatr<-array(0, dim=c(I,ncatg,nclass,ncatg*I) ) #I+nclass
  dimnames(desmatr)[[1]] <- colnames(modelinginputdata)
  dimnames(desmatr)[[2]] <- paste0("Cat", 1:ncatg )
  dimnames(desmatr)[[3]] <- paste0("Class", 1:nclass )
  dimnames(desmatr)[[4]] <-paste0( colnames(modelinginputdata)) %>%
    sapply(paste0, paste0("Cat",1:ncatg) ) %>% c() %>%
    sapply(paste0, paste0("Class",1:nclass) ) %>% c()
  # items, categories, classes, parameters
  for (ii in 1:I){
    # define theta design and item discriminations
    # for (hh in 1:(maxK-1) ){
    #   Xdes[ii, hh + 1,, NXlam ] <- hh * theta.k
    # }
    # item intercepts
    # for (ii in 1:I){
    #   for (hh in 1:(maxK-1) ){
    #     # ii <- 1  # Item    # hh <- 1  # category
    #     Xdes[ii,hh+1,, ( ii - 1)*(maxK-1) + hh] <- 1
    #   }
    # }
    for (hh in 1:(ncatg-1) ){
      for (nclassi in 1:nclass) {
        desmatr[ ii, hh+1, nclassi, ii+(ncatg-1)*I ] <- 1    # probabilities class 1
      }
    }
  }
  lcaarg<-list(
    data=modelinginputdata,
    group=NULL,
    weights=tmpdata$myown_wr,
    Xdes=desmatr
  )
  retmodel<-try( do.call(CDM::slca, args=lcaarg) )
  return(retmodel)
  #custom_generate_LCA_model(X, n_latentclasses=3, nrep=30, maxiter=1000, modelformula=NA)
}, datadf=survey_data_imputed, mc.cores=1)


#save(survey_data_imputed,file=paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt_lca.RData"))
save(survey_data_imputed,file=paste0(save_dataset_in_scriptsfile_directory,"miced_survey_2surveysonly_mirt_lca.RData"))