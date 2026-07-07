source(file = "08_preprocessing_lca_analysis_process_commonpart.R")
# 第八部份：LCA latent variables 潛在類別模式資料清理與套用 -------------------------------------------
lca_analysis_process_class <- R6::R6Class("lca_analysis_process", inherit=lca_analysis_commonpart_class, public = list(
  miced_survey_9_with_mirt_filepath = NULL,
  polca_models_from_idealpoint_filepath = NULL,
  shrink_polca_models_inf_filepath = NULL,
  survey_data_with_condensed_opinion_filepath = NULL,
  initialize = function(dataset_in_scriptsfile_directory="/mnt", filespath="/mnt", dataset_file_directory="/mnt", debug_func_mode=TRUE) {
    super$initialize(dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory, filespath=filespath, dataset_file_directory=dataset_file_directory, debug_func_mode=debug_func_mode)
    self$miced_survey_9_with_mirt_filepath <- file.path(dataset_in_scriptsfile_directory, "miced_survey_9_with_mirt.RData")
    self$polca_models_from_idealpoint_filepath <- file.path(save_dataset_in_scriptsfile_directory, "analyse_res", "polca_models_from_idealpoint.RData")
    self$shrink_polca_models_inf_filepath <- file.path(dataset_in_scriptsfile_directory, "shrink_polca_models_inf.RData")
    self$survey_data_with_condensed_opinion_filepath <- file.path(dataset_in_scriptsfile_directory, "survey_data_with_condensed_opinion.RData")
  },
  # 第八-0部份：LCA latent variables 潛在類別模式資料下載
  ensure_mirt_data_downloaded = function() {
    if (!file.exists(self$miced_survey_9_with_mirt_filepath)) {
      dir.create(dirname(self$miced_survey_9_with_mirt_filepath),recursive=TRUE)
      download.file(
        "http://homepage.ntu.edu.tw/~r03a21033/voterecord/miced_survey_9_with_mirt.RData",
        file.path(dirname(self$miced_survey_9_with_mirt_filepath), "miced_survey_9_mirt.RData"))
    }
  },
  # condense survey questions --------------------------------
  get_needimps = function() {
    custom_ret_appro_kamila_clustering_parameters() %>%
      dplyr::select(-newimp) %>%
      dplyr::mutate_at("imp",as.integer)
  },
  get_needimps_with_construct_nclass = function(needimps=self$get_needimps()) {
    needimps_with_construct_nclass<-needimps %>%
      cbind(., construct = rep(c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12"), each = nrow(.))) %>%
      cbind(., nclass = rep(2:7, each = nrow(.))) %>%
      dplyr::filter(!(survey=="2010overall" & construct %in% c("a1","a7","a8","a9","a10","a11") )) %>%
      dplyr::distinct_all() %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate_at(c("nclass","imp",".imp"),as.integer)
    needimps_with_construct_nclass %<>% mutate_cond(survey=="2016citizen" & construct=="a1", nclass=5) %>% #5
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
      dplyr::mutate_all(as.character)
    needimps_with_construct_nclass
  },
  get_formula_reduction_args = function(needimps_with_construct_nclass=self$get_needimps_with_construct_nclass()) {
    dplyr::bind_rows(
      data.frame("survey"="2016citizen","construct"=c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11"),
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
      dplyr::left_join(needimps_with_construct_nclass) %>%
      cbind(., nrep = rep(1, each = nrow(.))) %>%
      cbind(., maxiter = rep(100, each = nrow(.))) %>%
      dplyr::mutate(storekey=paste0(survey,"_imp",imp,"_",construct,"_nc_",nclass)) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate_at(c("nclass","nrep","maxiter","imp"),as.integer) %>%
      dplyr::arrange(survey, construct, imp)
    #formula_reduction_args %<>% dplyr::semi_join(needimps_with_construct_nclass, by=c("survey","construct","imp","nclass")) %>%
    #  dplyr::arrange(survey, construct, imp)
  },
  get_polca_models_from_idealpoint = function() {
    load_env <- new.env()
    load(file=self$polca_models_from_idealpoint_filepath, envir=load_env, verbose=TRUE)
    load_env$polca_models
  },
  get_already_processed_polca_models = function(polca_models) {
    lapply(names(polca_models), function(fikey, polca_models, formula_reduction_args) {
      X<-magrittr::extract2(polca_models, fikey)
      data.frame(nclass=X$nclass,resid.df=X$resid.df,aic=X$aic,bic=X$bic,nrep=X$nrep,storekey=fikey) %>%
        return()
    }, polca_models=polca_models) %>%
      plyr::rbind.fill() %>%
      dplyr::filter(nrep>1)
  },
  return_polca_models_inf = function(polca_models,formula_reduction_args) {
    lapply(names(polca_models), function(fikey, polca_models, formula_reduction_args) {
      X<-magrittr::extract2(polca_models, fikey)
      data.frame(nclass=X$nclass,resid.df=X$resid.df,aic=X$aic,bic=X$bic,nrep=X$nrep,maxiter=X$maxiter,storekey=fikey) %>%
        dplyr::left_join(dplyr::select(formula_reduction_args, -tidyselect::any_of(c("nrep","maxiter"))), by=c("storekey", "nclass"))
    }, polca_models=polca_models, formula_reduction_args=formula_reduction_args) %>%
      rbind.fill() %>%
      return()
  },
  # 對各construct的縮減題組跑poLCA（第一輪快速掃描、第二輪對resid.df>0者完整估計），並與既有結果合併
  run_condense_polca_models = function(survey_data_imputed, formula_reduction_args=self$get_formula_reduction_args()) {
    custom_generate_LCA_model<-self$custom_generate_LCA_model
    already_processed_polca_models<-self$get_already_processed_polca_models(self$get_polca_models_from_idealpoint())
    polca_models<-NULL
    polca_models_inf<-NULL
    for (i in 1:2) {
      if (i==2) {
        need_formula_reduction_args<-dplyr::filter(polca_models_inf,resid.df>0) %>%
          dplyr::semi_join(formula_reduction_args, .) %>%
          dplyr::mutate(nrep=40, maxiter=1200)
      } else {
        need_formula_reduction_args<-formula_reduction_args %>%
          dplyr::filter(!(storekey %in% !!already_processed_polca_models$storekey ))
      }
      polca_models<-custom_apply_thr_argdf(need_formula_reduction_args, "storekey", function(fikey, loopargdf, datadf, ...) {
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
      }, datadf=survey_data_imputed)
      polca_models_inf<-self$return_polca_models_inf(polca_models,formula_reduction_args)
    }
    backup_polca_models<-polca_models
    polca_models<-rlist::list.merge(self$get_polca_models_from_idealpoint(),backup_polca_models)
    #save(polca_models,file=self$polca_models_from_idealpoint_filepath)
    polca_models_inf<-self$return_polca_models_inf(polca_models,formula_reduction_args)
    list("polca_models"=polca_models, "polca_models_inf"=polca_models_inf)
  },
  get_shrink_polca_models_inf = function(polca_models_inf, needimps_with_construct_nclass=self$get_needimps_with_construct_nclass()) {
    dplyr::arrange(polca_models_inf, bic, aic) %>%
      dplyr::mutate_at(c(".imp"), as.integer) %>%
      dplyr::semi_join(needimps_with_construct_nclass %>%
                         dplyr::mutate_at(c("nclass","imp",".imp"), as.integer)) %>%
      dplyr::group_by(survey, construct, imp) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(
        dplyr::bind_rows(
          list(
            "a1"="政府介入人民經濟、生存、平權職責",
            "a2"="言論、集會自由",
            "a3"="治安維護與公民權",
            "a4"="跨國人民與資金流動",
            "a5"="政府介入經濟與公用事業",
            "a6"="政府介入社福衛環",
            "a7"="對就業與生存弱勢保障",
            "a8"="自由放任或稅務、所得重分配與平權義務",
            "a9"="政府角色、責任與義務",#（a9與a3需要在其他題項區分） #與中國大陸資本往來看法
            "a10"="政府支出期望",
            "a11"="政府帶領經濟發展",
            "a12"="") %>%
            unlist() %>%
            data.frame(survey="2016citizen",construct=names(.), constructname=.),
          list(
            "a2"="言論自由與政治競爭看法",
            "a3"="家父長",
            "a4"="經濟平權觀",
            "a5"="工作保障與社福觀",
            "a6"="和諧社會價值觀") %>%
            unlist() %>%
            data.frame(survey="2010overall",construct=names(.), constructname=.)
        )
      ) %>%
      dplyr::arrange(survey, construct, imp)
  },
  get_saved_shrink_polca_models_inf = function() {
    #save(shrink_polca_models_inf, recode_constructclass_list, file=self$shrink_polca_models_inf_filepath)
    load_env <- new.env()
    load(file=self$shrink_polca_models_inf_filepath, envir=load_env, verbose=TRUE)
    list("shrink_polca_models_inf"=load_env$shrink_polca_models_inf, "recode_constructclass_list"=load_env$recode_constructclass_list)
  },
  # 互動式將各LCA class人工命名（AG to PRO）
  interactive_recode_constructclass = function(shrink_polca_models_inf, polca_models, recode_constructclass_list=list()) {
    #70 a5 imp19
    #68
    #60
    for (rowi in 1:nrow(shrink_polca_models_inf)) { #needpoLCAsurveys_with_imp
      needrow<-shrink_polca_models_inf[rowi, ]
      #if ( !((needrow$survey=="2016citizen" & needrow$imp==1) |  (needrow$survey=="2010overall" & needrow$imp==2)  )) { #
      #  next
      #}
      repeat {
        survey_with_imp<-paste0(needrow$survey,needrow$imp)
        if (survey_with_imp %in% names(recode_constructclass_list)) {
          if (needrow$construct %in% names(recode_constructclass_list[[survey_with_imp]])) {
            break
          }
        }
        prefixinfstr<-paste("now in imp", needrow$imp, "c:", needrow$construct, needrow$constructname, "number of class is",needrow$nclass)
        model<-needrow$storekey %>%
          magrittr::extract2(polca_models, .)
        cat("\014")
        lcaresdims<-lapply(1:length(model$probs), function(matrixi,polocaprobmatrix) {
          data.frame(matrix_i=matrixi, name=names(model$probs)[matrixi], dimx=dim(polocaprobmatrix[[matrixi]])[1], dimy=dim(polocaprobmatrix[[matrixi]])[2]) %>%
            return()
        }, polocaprobmatrix=model$probs) %>%
          plyr::rbind.fill()
        lcagrouped_dims<-dplyr::group_by(lcaresdims,dimx,dimy) %>% dplyr::summarise(groupdimname=paste(name, collapse=" "))
        lcagrouped_sums<-list()
        for (lcagrouped_dims_i in 1:nrow(lcagrouped_dims)) {
          needmatrix_names<-lcagrouped_dims[lcagrouped_dims_i,"groupdimname"] %>% stri_split(regex=" ") %>% unlist() %>% base::setdiff("c2")# %>% base::setdiff("c3")
          length_needmatrix_names<-length(needmatrix_names)
          lcagrouped_sums[[lcagrouped_dims_i]]<-magrittr::extract(model$probs,needmatrix_names) %>%
            Reduce(magrittr::add, .) %>%
            magrittr::divide_by(length_needmatrix_names)
        }
        print(model$probs)
        print("below are res of sum")
        print(lcagrouped_sums)
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
          #save(shrink_polca_models_inf, recode_constructclass_list, file=self$shrink_polca_models_inf_filepath)
          break
        } else {
          recode_constructclass_list[[survey_with_imp]][[needrow$construct]]<-NULL
        }
      }
    }
    recode_constructclass_list
  },
  # output probability table ---------
  output_probability_table = function(shrink_polca_models_inf, polca_models, recode_constructclass_list, outputcsv="TMP.csv") {
    needrows<-dplyr::filter(shrink_polca_models_inf, customgrepl(storekey, pattern="(2010overall_imp2_|2016citizen_imp1_)" ) )
    neednewlst<-list()
    for (rowi in 1:nrow(needrows)) {
      needrow<-needrows[rowi, ]
      survey_with_imp<-paste0(needrow$survey,needrow$imp)
      prefixinfstr<-paste("now in imp", needrow$imp, "c:", needrow$construct, needrow$constructname, "number of class is",needrow$nclass)
      model<-needrow$storekey %>%
        magrittr::extract2(polca_models, .)
      lst<-model$probs
      lst<-lst[order(names(lst))]
      classdisplayname<-recode_constructclass_list[[survey_with_imp]][[needrow$construct]] %>% unlist()
      newlst<-lapply(names(lst), function(X,lst,cdname,needrow) {
        as.data.frame(lst[[X]]) %>%
          cbind("cdname"=cdname,"construct"=X,"constructname"=as.character(needrow$constructname),.)
      },lst=lst,cdname=classdisplayname,needrow=needrow) %>%
        dplyr::bind_rows()
      neednewlst[[rowi]]<-newlst
    }
    dplyr::bind_rows(neednewlst) %>% write.csv(outputcsv)
  },
  # 依LCA預測類別為每位受訪者加上condensed opinion欄位並存檔
  build_survey_data_with_condensed_opinion = function(survey_data_imputed, shrink_polca_models_inf, polca_models, recode_constructclass_list, needimps=self$get_needimps(), save=TRUE) {
    need_shrink_polca_models_inf<-shrink_polca_models_inf
    #need_shrink_polca_models_inf %<>% dplyr::semi_join(t)
    #dplyr::filter(shrink_polca_models_inf,  ((survey=="2016citizen" & imp==1) |  (survey=="2010overall" & imp==2)  ))
    survey_data_with_condensed_opinion<-lapply(survey_data_imputed, function(X) {
      dplyr::select(X, SURVEY, id, .id, .imp) %>% return()
    }) %>% lapply(dplyr::semi_join, dplyr::rename(needimps, SURVEY=survey))
    for (rowi in 1:nrow(need_shrink_polca_models_inf)) { #37 38(NA) 39(NA) 40(NA) 41(NA) 42(NA)
      needrow<-need_shrink_polca_models_inf[rowi, ]
      survey_with_imp<-paste0(needrow$survey,needrow$imp)
      message(paste(survey_with_imp,needrow$construct))
      constructlist<-magrittr::extract2(recode_constructclass_list, survey_with_imp)
      constructlist<-magrittr::extract2(constructlist, needrow$construct) %>%
        lapply(customgsub, pattern=needrow$constructname,replacement="") %>%
        lapply(customgsub, pattern=needrow$constructname,replacement="") %>%
        lapply(customgsub, pattern="_",replacement="") %>%
        lapply(customgsub, pattern="of\\d",replacement="")
      print(constructlist)
      needmodel<-magrittr::extract2(polca_models, needrow$storekey)
      targetupdaterows<-which(survey_data_with_condensed_opinion[[needrow$survey]]$.imp==needrow$imp)
      survey_data_with_condensed_opinion[[needrow$survey]][targetupdaterows, paste0("construct_",needrow$survey,"_",needrow$construct)] <- dplyr::recode(needmodel$predclass, !!!constructlist) #dplyr::recode_factor(needmodel$predclass, !!!constructlist)
    }
    survey_data_with_condensed_opinion<-lapply(survey_data_with_condensed_opinion, function(X) {
      dplyr::mutate_at(X, dplyr::vars(dplyr::contains("construct")), as.factor)
    })
    if (save==TRUE) {
      save(survey_data_with_condensed_opinion, file=self$survey_data_with_condensed_opinion_filepath)
    }
    survey_data_with_condensed_opinion
  },
  check_condensed_opinion_na = function(survey_data_with_condensed_opinion) { #testing if NA exists
    grep(pattern="construct_2010overall", x=names(survey_data_with_condensed_opinion$`2010overall`), value=TRUE) %>%
      {dplyr::select(survey_data_with_condensed_opinion$`2010overall`, tidyselect::any_of(.))} %>%
      lapply(unique)
    grep(pattern="construct_2016citizen", x=names(survey_data_with_condensed_opinion$`2016citizen`), value=TRUE) %>%
      {dplyr::select(survey_data_with_condensed_opinion$`2016citizen`, tidyselect::any_of(.))} %>%
      lapply(unique)
    #c("construct_2016citizen_a3", "construct_2016citizen_a5", "construct_2016citizen_a10", "construct_2016citizen_a11")
    t<-survey_data_with_condensed_opinion$`2016citizen`[!complete.cases(survey_data_with_condensed_opinion$`2016citizen`),]
    t<-reshape2::melt(t, id.vars=c("SURVEY", "id", ".id", ".imp")) %>%
      dplyr::filter(is.na(value)) %>%
      dplyr::mutate_at("variable", ~customgsub(variable,pattern="construct_2016citizen_",replacement="")) %>%
      dplyr::rename(construct=variable, survey=SURVEY) %>%
      dplyr::distinct(survey,.imp,construct)# %>% dplyr::left_join(need_shrink_polca_models_inf)
    t
  },
  # 第六-3部份：潛在類別分析：將分析結果整併入dataset Apply poLCA results --------------------------------
  get_needpoLCAsurveys_arguments_df = function(needpoLCAsurveys=c("2004citizen","2010overall"), imps=self$imps) {
    data.frame("survey"=needpoLCAsurveys) %>%
      cbind(., imp = rep(imps, each = nrow(.))) %>%
      dplyr::mutate(store_key=paste0(survey,"_",imp)) %>%
      dplyr::mutate_at(c("survey","store_key"),as.character) %>%
      dplyr::mutate_at("imp", as.integer) %>%
      dplyr::filter(survey %in% c("2010overall")) %>%
      dplyr::arrange(survey, imp)
  },
  get_poLCA_infodf_notshrink = function(needpoLCAsurveys_arguments_df=self$get_needpoLCAsurveys_arguments_df()) {
    stopifnot(!is.null(self$dbconnect_info))
    dbconnect_info<-self$dbconnect_info
    needpoLCAsurveys_arguments_df$store_key %>%
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
  },
  get_poLCA_infodf = function(poLCA_infodf_notshrink, survey_data_imputed) {
    dplyr::bind_rows(poLCA_infodf_notshrink) %>%
      dplyr::group_by(store_key) %>%
      dplyr::arrange(bic) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(residdf), survey %in% !!names(survey_data_imputed)) %>%
      dplyr::distinct(modelformula, nclass)
  },
  run_poLCA_survey_results = function(survey_data_imputed, poLCA_infodf, needpoLCAsurveys_arguments_df=self$get_needpoLCAsurveys_arguments_df()) {
    needpoLCAsurveys_arguments_df$store_key %>% #poLCA_infodf$surveyimp
      magrittr::set_names(custom_parallel_lapply(., function (survey_with_imp, ...) {
        needrow<-dplyr::filter(needpoLCAsurveys_arguments_df, store_key==!!survey_with_imp)
        survey<-needrow$survey
        imp<-needrow$imp
        singleargumentdf <- poLCA_infodf[1,]
        dplyr::filter(survey_data_imputed[[survey]], .imp==!!imp) %>%
          poLCA(formula=as.formula(singleargumentdf$modelformula), data=., nclass=singleargumentdf$nclass, nrep=35) %>%
          return()
      }, method=parallel_method, poLCA_infodf=poLCA_infodf, needpoLCAsurveys_arguments_df=needpoLCAsurveys_arguments_df, survey_data_impute=survey_data_imputed), .)
  },
  #v90 就兩岸關係而言,請問您覺得台灣獨立,統一,維持現狀何者比較好?
  #v91 請問您同不同意若台灣宣佈獨立,仍可和中共維持和平關係,則台灣應成為一個新國家?
  #v92 請問您同不同意若大陸和台灣在經濟,社會,政治各方面的條件相當,則兩岸應該統一?
  interactive_recode_indp = function(poLCA_survey_results, needpoLCAsurveys_arguments_df=self$get_needpoLCAsurveys_arguments_df()) {
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
    recode_indp_list
  },
  apply_indp_atti = function(survey_data_imputed, poLCA_survey_results, recode_indp_list, needpoLCAsurveys_arguments_df=self$get_needpoLCAsurveys_arguments_df()) {
    myown_indp_atti_array_order<-sort(unlist(recode_indp_list[[1]]))
    for (survey_with_imp in needpoLCAsurveys_arguments_df$store_key) { #needpoLCAsurveys_with_imp
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
    survey_data_imputed
  },
  # pick common factor ====
  pick_common_factor = function() {
    vec1<-stri_split("v78d	v78c	v39c	v68g", regex="\\t") %>% unlist()
    vec2<-stri_split("v78h	v78i	v78a	v78e	v78b", regex="\\t") %>% unlist()
    vec3<-stri_split("v78h	v78i	v78a	v78e	v78b", regex="\\t") %>% unlist()
    vec4<-stri_split("v78b	v78e	v78a	v78i	v78h", regex="\\t") %>% unlist()
    vec5<-stri_split("v78g	v78i	v78a	v78e	v78b", regex="\\t") %>% unlist()
    vec6<-stri_split("v78h	v78i	v78a	v78e	v78b", regex="\\t") %>% unlist()
    Reduce(base::intersect, list(vec1,vec2,vec3,vec4,vec5,vec6)) %>% paste0(sep=",") %>% message()
  },
  # test：CDM::slca 有支援weights的LCA替代方案測試 ====================
  test_cdm_slca = function(survey_data_imputed, needimps=self$get_needimps()) {
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
      nclass<-3
      ncatg<-2
      desmatr<-array(0, dim=c(I,ncatg,nclass,ncatg*I) ) #I+nclass
      dimnames(desmatr)[[1]] <- colnames(modelinginputdata)
      dimnames(desmatr)[[2]] <- paste0("Cat", 1:ncatg )
      dimnames(desmatr)[[3]] <- paste0("Class", 1:nclass )
      dimnames(desmatr)[[4]] <-paste0( colnames(modelinginputdata)) %>%
        sapply(paste0, paste0("Cat",1:ncatg) ) %>% c() %>%
        sapply(paste0, paste0("Class",1:nclass) ) %>% c()
      # items, categories, classes, parameters
      for (ii in 1:I){
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
    }, datadf=survey_data_imputed, mc.cores=1)
    polca_models_2016policy
  }
))

# old method for applyinh poLCA results（僅保留參考用，不會執行） --------------------------------
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
  }
}

#levels(t_survey_data_test[[1]]$myown_atti_ind)[levels(t_survey_data_test[[1]]$myown_atti_ind)=="1"] <- "統一"

# 第六-2部份：LCA latent variables 潛在類別模式政黨傾向（僅保留參考用，不會執行） ====================
if ({calculatingpartyconstituency<-FALSE;calculatingpartyconstituency}) {
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

if (FALSE) { #檢視統獨題項題目與選項（僅保留參考用）
  openxlsx::read.xlsx(path_to_survey_imputation_and_measurement_file,sheet = 1) %>%
    dplyr::filter(SURVEY %in% needpoLCAsurveys, ID %in% !!c(unlist(lcaneed_independence_attitude[survey_data_title]), "v90") ) %>%
    dplyr::distinct(SURVEY, ID, QUESTION, ANSWER) %>%
    View()
  openxlsx::read.xlsx(path_to_survey_imputation_and_measurement_file,sheet = 1) %>%
    dplyr::distinct(SURVEY,ID,QUESTION,ANSWER) %>%
    View()
}
