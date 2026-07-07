source(file = "08_preprocessing_lca_analysis_process.R")
# 第九部份：clustering =================================
#clustering
#clustrd
#https://cran.r-project.org/web/packages/clustrd/clustrd.pdf
#http://www.amarkos.gr/clustrd/
#https://www.jamleecute.com/hierarchical-clustering-%E9%9A%8E%E5%B1%A4%E5%BC%8F%E5%88%86%E7%BE%A4/
#weight https://sdaza.com/blog/2012/raking/
clustering_process_class <- R6::R6Class("clustering_process", inherit=lca_analysis_process_class, public = list(
  clustering_var = NULL,
  nclusrange = NULL,
  ndimrange = NULL,
  reskamilaclusterfile = NULL,
  reskamilaclusterbackupfile = NULL,
  reskamilaclusterweightedfile = NULL,
  kamila_clustering_parameters_filepath = NULL,
  resvarselclusterfile = NULL,
  clustrd_results_after_assesment_filepath = NULL,
  initialize = function(dataset_in_scriptsfile_directory="/mnt", filespath="/mnt", dataset_file_directory="/mnt", debug_func_mode=TRUE) {
    super$initialize(dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory, filespath=filespath, dataset_file_directory=dataset_file_directory, debug_func_mode=debug_func_mode)
    #"myown_dad_ethgroup","myown_mom_ethgroup","myown_working_status","myown_occp","myown_ses","myown_income","myown_family_income",
    self$clustering_var <- list(
      "2004citizen"=c("myown_sex","myown_age","myown_selfid","myown_marriage","myown_areakind","myown_factoredses","myown_religion"),
      "2010env"=c("myown_sex","myown_age","myown_selfid","myown_marriage","myown_areakind","myown_factoredses","myown_religion"),
      "2010overall"=c("myown_sex","myown_age","myown_selfid","myown_marriage","myown_areakind","myown_factoredses","myown_religion"),
      "2016citizen"=c("myown_sex","myown_age","myown_selfid","myown_marriage","myown_areakind","myown_factoredses","myown_religion")
    )
    self$nclusrange<-2:12
    self$ndimrange<-1:6
    #reskamilaclusterfile <- paste0(save_dataset_in_scriptsfile_directory, "kamilacluster.Rdata")
    self$reskamilaclusterfile <- file.path(save_dataset_in_scriptsfile_directory, "kamilacluster_inflated.Rdata")
    self$reskamilaclusterbackupfile <- file.path(save_dataset_in_scriptsfile_directory, "kamilacluster_inflated_backup.Rdata")
    self$reskamilaclusterweightedfile <- file.path(save_dataset_in_scriptsfile_directory, "kamilacluster_weighted.Rdata")
    self$kamila_clustering_parameters_filepath <- file.path(dataset_in_scriptsfile_directory, "kamila_clustering_parameters.Rdata")
    self$resvarselclusterfile <- file.path(dataset_in_scriptsfile_directory, "varselcluster.Rdata")
    self$clustrd_results_after_assesment_filepath <- file.path(dataset_in_scriptsfile_directory, "clustrd_results_after_assesment.RData")
  },
  change_survey_data_religion = function(survey_data_imputed) {
    survey_data_imputed$`2016citizen` %<>% mutate_cond(myown_religion=="[5] 回教(伊斯蘭教)", myown_religion="[9] 其他,請說明") %>%
      dplyr::mutate_at("myown_religion", droplevels)
    survey_data_imputed$`2010overall` %<>% dplyr::mutate_at("myown_religion", droplevels)
    return(survey_data_imputed)
  },
  # 讀入08階段輸出（mirt_lca）並整理宗教變數
  get_survey_data_imputed_for_clustering = function() {
    survey_data_imputed <- self$get_survey_data_imputed_stage(stage="mirt_lca")
    survey_data_imputed %<>% self$change_survey_data_religion()
    survey_data_imputed
  },
  get_clustrd_arguments_df = function(imps=self$imps) {
    data.frame("survey"=survey_data_title) %>%
      cbind(., imp = rep(imps, each = nrow(.))) %>%
      cbind(., nclus = rep(self$nclusrange, each = nrow(.))) %>%
      cbind(., ndim = rep(self$ndimrange, each = nrow(.))) %>%
      cbind(., alpha = rep(c(0,0.25,0.5,0.75,1), each = nrow(.))) %>%
      cbind(., method = rep(c("mixedRKM"), each = nrow(.))) %>% #,"mixedFKM"
      cbind(., rotation = rep(c("none","varimax","promax"), each = nrow(.))) %>%
      cbind(., criterion = rep(c("asw"), each = nrow(.))) %>%
      cbind(., dst = rep(c("low"), each = nrow(.))) %>% #,"full"
      filter(nclus>ndim)
  },
  # * model-based clustering by VarSelLCM ----------------
  get_varsellcm_arguments_df = function(imps=self$imps) {
    data.frame("survey"=survey_data_title) %>%
      cbind(., imp = rep(imps, each = nrow(.))) %>%
      cbind(., varsel = rep(c("wtho","wth"), each = nrow(.))) %>%
      dplyr::mutate(keyprefix=paste0(survey,"_imp",imp)) %>%
      dplyr::mutate(store_key=paste0(survey, "_imp", imp, "_", varsel)) #%>%
      #dplyr::filter(survey %in% c("2010overall","2016citizen")) %>%
      #dplyr::filter(!(store_key %in% !!names(varsellcm_results)) )
  },
  run_varsellcm = function(survey_data_imputed, varsellcm_arguments_df=self$get_varsellcm_arguments_df(), n_select_components=2:12) {
    clustering_var<-self$clustering_var
    resvarselclusterfile<-self$resvarselclusterfile
    varsellcm_results<-list()
    varsellcm_results<- varsellcm_arguments_df$store_key %>%
      magrittr::set_names( custom_parallel_lapply(., function (fikey, varsellcm_arguments_df, ...) {
        needrow <- dplyr::filter(varsellcm_arguments_df, store_key==!!fikey)
        survey_key <- needrow$survey
        needimp <- needrow$imp
        needvarsel <- needrow$varsel
        needdf<-dplyr::filter(survey_data_imputed[[survey_key]], .imp==!!needimp) %>%
          dplyr::mutate_at("myown_age",scale) %>%
          dplyr::mutate_at("myown_age",function (X) {X[,1]}) %>%
          dplyr::mutate_at("myown_age",as.numeric)
        surveyweight<-needdf$myown_wr
        inputData<-dplyr::select(needdf, !!clustering_var[[survey_key]])
        resmod <- if (needvarsel=="wtho") {
          # Cluster analysis without variable selection
          VarSelLCM::VarSelCluster(inputData, gvals = n_select_components, nbcores = parallel::detectCores(), vbleSelec = FALSE, crit.varsel = "BIC")
        } else {
          # Cluster analysis with variable selection (with parallelisation)
          VarSelLCM::VarSelCluster(inputData, gvals = n_select_components, nbcores = parallel::detectCores(), crit.varsel = "BIC")
        }
        while (TRUE) {
          loadsavestatus<-try({
            load(file=resvarselclusterfile, verbose=TRUE)
            varsellcm_results[[needrow$store_key]] <- resmod
            save(varsellcm_results, file=resvarselclusterfile)
          })
          if(!is(loadsavestatus, 'try-error')) break
        }
        return(resmod)
      }, varsellcm_arguments_df=varsellcm_arguments_df,
      survey_data_imputed=survey_data_imputed,
      clustering_var=clustering_var,
      resvarselclusterfile=resvarselclusterfile, method=parallel_method), . )
    save(varsellcm_results, file=resvarselclusterfile)
    varsellcm_results
  },
  apply_varsellcm_results = function(survey_data_imputed, varsellcm_arguments_df=self$get_varsellcm_arguments_df()) {
    load_env <- new.env()
    load(file=self$resvarselclusterfile, envir=load_env, verbose=TRUE)
    varsellcm_results <- load_env$varsellcm_results
    for (varsellcm_results_key in unique(sort(varsellcm_arguments_df$keyprefix))) {
      needrow<-dplyr::filter(varsellcm_arguments_df, keyprefix==!!varsellcm_results_key)
      imp <- needrow$imp[1] %>% as.integer()
      res_with <- paste0(varsellcm_results_key, "_wth") %>%
        magrittr::extract2(varsellcm_results, .)
      res_without <- paste0(varsellcm_results_key, "_wtho") %>%
        magrittr::extract2(varsellcm_results, .)
      resmodel<-if ( abs(VarSelLCM::BIC(res_without)) > abs(VarSelLCM::BIC(res_with)) ) res_with else res_without
      #VarSelLCM::BIC(resmodel) %>% print()
      #VarSelLCM::summary(resmodel) %>% print()
      # Estimated probabilities of classification
      #head(VarSelLCM::fitted(resmodel, type="probability")) %>% print()
      table_of_cluster_dist_orig<-table(VarSelLCM::fitted(resmodel))
      for (interpretvar in self$clustering_var[[needrow$survey[1]]]) {
        #if (readline("next var:")=="N") break
        #VarSelLCM::plot(x=resmodel, y=interpretvar)
      }
      factored_cluster<-VarSelLCM::fitted(resmodel) %>%
        as.factor() %>%
        forcats::fct_infreq() %>%
        forcats::fct_recode(., !!!{
          set_names(levels(.), as.list(sort(unique(.))))
        })
      table_of_cluster_dist<-table(factored_cluster)
      message(paste0("distribution of cluster of ",varsellcm_results_key," is:"))
      print(table_of_cluster_dist)
      survey_data_imputed[[needrow$survey[1]]][survey_data_imputed[[needrow$survey[1]]]$.imp==imp,"cluster_varsellcm"] <- as.integer(as.character(factored_cluster))
    }
    survey_data_imputed
  },
  # * model-based clustering by KAMILA（論文定案方法）----------------
  # 註：原始kamila不支援sample weights，此處以inflate_df_from_weight把觀察值依權重複製擴增近似
  # （新版原生加權作法見下方run_kamila_clustering_weighted，使用external/kamila子模組）
  get_kamila_arguments_df = function(imps=self$imps) {
    data.frame("survey"=survey_data_title) %>%
      cbind(., imp = rep(imps, each = nrow(.))) %>%
      dplyr::mutate(store_key=paste0(survey,"_",imp)) %>%
      dplyr::mutate_at(c("survey","store_key"),as.character) %>%
      dplyr::mutate_at("imp", as.integer) %>%
      dplyr::filter(survey %in% c("2010overall","2016citizen")) %>%
      dplyr::arrange(survey, imp)
  },
  filter_processed_kamila = function(kamila_arguments_df) {
    load_env <- new.env()
    load(file=self$reskamilaclusterfile, envir=load_env, verbose=TRUE)
    processed_kamila_key<-sapply(load_env$kamila_results, class) %>%
      .[.=="list"] %>%
      names()
    kamila_arguments_df %>% dplyr::filter(!(store_key %in% !!processed_kamila_key))
  },
  run_kamila_clustering = function(survey_data_imputed, kamila_arguments_df=self$get_kamila_arguments_df(), inflate_rate=30, numClust=3:8, mc.cores=24) {
    clustering_var<-self$clustering_var
    reskamilaclusterfile<-self$reskamilaclusterfile
    kamila_results<-list()
    kamila_results<-dplyr::arrange(kamila_arguments_df, imp) %>% magrittr::use_series("store_key") %>%
      magrittr::set_names( custom_parallel_lapply(., function (fikey, ...) {
        message(paste("now in",fikey))
        needrow<-dplyr::filter(kamila_arguments_df, store_key==!!fikey)
        survey_key <- needrow$survey
        needimp <- needrow$imp
        resmodel <- dplyr::filter(survey_data_imputed[[survey_key]], .imp==!!needimp) %>%
          dplyr::mutate(myown_age=as.numeric(scale(myown_age))) %>%
          dplyr::mutate(myown_factoredses=myown_factoredses_scaled) %>%
          dplyr::select(tidyselect::any_of(c("myown_wr",clustering_var[[survey_key]]))) %>% #".imp",".id","id",
          inflate_df_from_weight(rate=inflate_rate) %>%
          dplyr::select(!!clustering_var[[survey_key]]) %>%
          {kamila::kamila(conVar=.[,c("myown_age","myown_factoredses")],
                          catFactor=.[c("myown_sex","myown_selfid","myown_marriage","myown_areakind","myown_religion")],
                          numClust = numClust, numInit = 10, calcNumClust = "ps", numPredStrCvRun = 10,
                          predStrThresh = 0.5, maxIter=1000, verbose=TRUE)}
        tryn<-1
        while (TRUE) {
          loadsavestatus<-try({
            load(file=reskamilaclusterfile, verbose=TRUE)
            kamila_results[[fikey]]<-resmodel
            save(kamila_results, file=reskamilaclusterfile)
          })
          tryn<-tryn+1
          if(!is(loadsavestatus, 'try-error') | tryn>11) break
        }
        return(resmodel)
        #return(kamila_results[[store_key]]$nClust$bestNClust)
      }, kamila_arguments_df=kamila_arguments_df,
      nclusrange=base::setdiff(self$nclusrange,1),
      survey_data_imputed=survey_data_imputed,
      clustering_var=clustering_var,
      reskamilaclusterfile=reskamilaclusterfile,
      mc.cores=mc.cores,
      method=parallel_method)  , .)
    #save(kamila_results, file=self$reskamilaclusterbackupfile)
    #save(kamila_results, file=self$reskamilaclusterfile)
    kamila_results
  },
  # * weighted KAMILA（external/kamila子模組版，原生支援sample weights）----------------
  # 取代inflate_df_from_weight複製擴增的近似作法；submodule修改內容：
  # 加權群心、加權radial KDE（Kish有效樣本數頻寬）、加權類別機率表、加權目標函數與prediction strength
  install_weighted_kamila = function(force=FALSE) {
    has_obsweights<-"obsWeights" %in% names(formals(kamila::kamila))
    if (!has_obsweights | force) {
      kamila_src<-here::here("external","kamila")
      install.packages(kamila_src, repos=NULL, type="source")
      message("已從external/kamila子模組安裝支援obsWeights之kamila；若原kamila已載入請重啟R session")
    }
    invisible("obsWeights" %in% names(formals(kamila::kamila)))
  },
  run_kamila_clustering_weighted = function(survey_data_imputed, kamila_arguments_df=self$get_kamila_arguments_df(), numClust=3:8, mc.cores=24) {
    if (!self$install_weighted_kamila()) stop("kamila::kamila不支援obsWeights，請重啟R session後再執行")
    clustering_var<-self$clustering_var
    reskamilaclusterweightedfile<-self$reskamilaclusterweightedfile
    kamila_results<-list()
    kamila_results<-dplyr::arrange(kamila_arguments_df, imp) %>% magrittr::use_series("store_key") %>%
      magrittr::set_names( custom_parallel_lapply(., function (fikey, ...) {
        message(paste("now in",fikey))
        needrow<-dplyr::filter(kamila_arguments_df, store_key==!!fikey)
        survey_key <- needrow$survey
        needimp <- needrow$imp
        resmodel <- dplyr::filter(survey_data_imputed[[survey_key]], .imp==!!needimp) %>%
          dplyr::mutate(myown_age=as.numeric(scale(myown_age))) %>%
          dplyr::mutate(myown_factoredses=myown_factoredses_scaled) %>%
          dplyr::select(tidyselect::any_of(c("myown_wr",clustering_var[[survey_key]]))) %>%
          {kamila::kamila(conVar=.[,c("myown_age","myown_factoredses")],
                          catFactor=.[c("myown_sex","myown_selfid","myown_marriage","myown_areakind","myown_religion")],
                          numClust = numClust, numInit = 10, calcNumClust = "ps", numPredStrCvRun = 10,
                          predStrThresh = 0.5, maxIter=1000, verbose=TRUE,
                          obsWeights=.$myown_wr)}
        tryn<-1
        while (TRUE) {
          loadsavestatus<-try({
            load(file=reskamilaclusterweightedfile, verbose=TRUE)
            kamila_results[[fikey]]<-resmodel
            save(kamila_results, file=reskamilaclusterweightedfile)
          })
          tryn<-tryn+1
          if(!is(loadsavestatus, 'try-error') | tryn>11) break
        }
        return(resmodel)
      }, kamila_arguments_df=kamila_arguments_df,
      survey_data_imputed=survey_data_imputed,
      clustering_var=clustering_var,
      reskamilaclusterweightedfile=reskamilaclusterweightedfile,
      mc.cores=mc.cores,
      method=parallel_method)  , .)
    kamila_results
  },
  # 加權版無擴增，finalMemb與原始觀察值一一對應，直接併回
  apply_weighted_kamila_results_to_survey_data = function(survey_data_imputed=NULL) {
    if (is.null(survey_data_imputed)) {
      survey_data_imputed <- self$get_survey_data_imputed_for_clustering()
    }
    load_env <- new.env()
    load(file=self$reskamilaclusterweightedfile, envir=load_env, verbose=TRUE)
    kamila_results <- load_env$kamila_results
    combine_kamila_df_lst<-names(kamila_results) %>%
      magrittr::set_names(custom_parallel_lapply(., function(survey_imp_key, surveyd, ...) {
        message(paste("now in",survey_imp_key))
        survey_imp_key_i<-unlist(strsplit(survey_imp_key,"_"))
        surveykey<-survey_imp_key_i[1]
        imp<-as.integer(survey_imp_key_i[2])
        kamila_model<-kamila_results[[survey_imp_key]]
        needdf<-dplyr::filter(surveyd[[surveykey]], .imp==!!imp)
        if (nrow(needdf)!=length(kamila_model$finalMemb)) stop(paste("列數與finalMemb長度不符：",survey_imp_key))
        # 依原作法以出現頻率重新編號集群（最多者為1）
        cluster_kamila<-as.factor(kamila_model$finalMemb) %>%
          forcats::fct_infreq() %>%
          forcats::fct_recode(., !!!{
            magrittr::set_names(levels(.), as.list(sort(unique(.))))
          }) %>% as.character() %>% as.integer()
        dplyr::select(needdf, -tidyselect::any_of(c("cluster_kamila"))) %>%
          dplyr::mutate(cluster_kamila=!!cluster_kamila) %>%
          return()
      }, kamila_results=kamila_results, surveyd=survey_data_imputed, method=parallel_method),.)
    survey_data_imputed<-lapply(names(survey_data_imputed), grep, x=names(combine_kamila_df_lst), value=TRUE) %>%
      lapply(FUN=function(n,tdflist) {magrittr::extract(tdflist, n)}, tdflist=combine_kamila_df_lst)  %>%
      lapply(dplyr::bind_rows) %>%
      lapply(FUN=function(X) {dplyr::arrange(X, .imp, id)}) %>%
      magrittr::set_names(names(survey_data_imputed))
    survey_data_imputed
  },
  # * applying kamila_results to survey data ----------------------------------------------
  inflated_kamila_input_df_to_original_df = function(one_kamila_model, distinctall=FALSE) {
    t<-dplyr::bind_cols(one_kamila_model$input$conVar, one_kamila_model$input$catFactor) %>%
      data.frame(., "memb"=one_kamila_model$finalMemb)
    if (distinctall==TRUE) {
      t %<>% dplyr::distinct_all()
    }
    return(t)
  },
  kamila_inflated_results_filepath = function(rate=10) {
    file.path(save_dataset_in_scriptsfile_directory, paste0("kamilacluster_inflated_times",rate,".Rdata"))
  },
  apply_kamila_results_to_survey_data = function(survey_data_imputed=NULL, rate=10, kamila_arguments_df=self$get_kamila_arguments_df()) {
    if (is.null(survey_data_imputed)) {
      survey_data_imputed <- self$get_survey_data_imputed_for_clustering()
    }
    inflated_kamila_input_df_to_original_df<-self$inflated_kamila_input_df_to_original_df
    load_env <- new.env()
    load(file=self$kamila_inflated_results_filepath(rate=rate), envir=load_env, verbose=TRUE)
    kamila_results <- load_env$kamila_results
    combine_kamila_df_lst<-names(kamila_results) %>%
      magrittr::set_names(custom_parallel_lapply(., function(survey_imp_key, surveyd, ...) {
        message(paste("now in",survey_imp_key))
        survey_imp_key_i<-unlist(strsplit(survey_imp_key,"_"))
        surveykey<-survey_imp_key_i[1]
        imp<-as.integer(survey_imp_key_i[2])
        kamila_model<-kamila_results[[survey_imp_key]]
        kamilasrcdf<-inflated_kamila_input_df_to_original_df(kamila_model) %>%
          dplyr::select(memb) %>%
          dplyr::bind_cols(dplyr::filter(survey_data_imputed[[surveykey]], .imp==!!imp) %>%
                             dplyr::select(id,.imp,myown_wr) %>%
                             inflate_df_from_weight(rate=rate) ) %>%
          dplyr::rename(cluster_kamila=memb) %>%
          dplyr::distinct_all()
        kamilasrcdf$cluster_kamila %<>% as.factor() %>%
          forcats::fct_infreq() %>%
          forcats::fct_recode(., !!!{
            magrittr::set_names(levels(.), as.list(sort(unique(.))))
          }) %>% as.character() %>% as.integer()
        dplyr::filter(surveyd[[surveykey]], .imp==!!imp) %>%
          dplyr::select(-tidyselect::any_of(c("cluster_kamila"))) %>%
          dplyr::left_join(kamilasrcdf) %>%
          return()
      }, kamila_results=kamila_results, surveyd=survey_data_imputed, method=parallel_method),.)
    survey_data_imputed<-lapply(names(survey_data_imputed), grep, x=names(combine_kamila_df_lst), value=TRUE) %>%
      lapply(FUN=function(n,tdflist) {magrittr::extract(tdflist, n)}, tdflist=combine_kamila_df_lst)  %>%
      lapply(dplyr::bind_rows) %>%
      lapply(FUN=function(X) {dplyr::arrange(X, .imp, id)}) %>%
      magrittr::set_names(names(survey_data_imputed))
    for (survey_imp_key in names(kamila_results)) {
      needrow<-dplyr::filter(kamila_arguments_df, store_key==survey_imp_key)
      message(paste("now in", survey_imp_key))
      unique(kamila_results[[needrow$store_key]]$finalMemb) %>% print()
    }
    survey_data_imputed
  },
  # * check kamila prob result ----------------------------------------------
  build_kamila_clustering_parameters = function(rate=10, kamila_arguments_df=self$get_kamila_arguments_df(), save=TRUE) {
    inflated_kamila_input_df_to_original_df<-self$inflated_kamila_input_df_to_original_df
    load_env <- new.env()
    load(file=self$kamila_inflated_results_filepath(rate=rate), envir=load_env, verbose=TRUE)
    kamila_results <- load_env$kamila_results
    kamila_clustering_parameters<-custom_parallel_lapply(names(kamila_results), function(fikey, ...) {
      needkamilamodel<-kamila_results[[fikey]]
      needargrow<-dplyr::filter(kamila_arguments_df, store_key==!!fikey)
      ratiotable<-table(inflated_kamila_input_df_to_original_df(needkamilamodel)$memb) %>% prop.table() %>% as.data.frame() %>%
        dplyr::rename(clustn=Var1, ratio=Freq) %>%
        dplyr::arrange(dplyr::desc(ratio)) %>%
        data.frame(., populationsize=1:nrow(.)) %>%
        dplyr::mutate_at("clustn", as.integer)
      # syvdes <- survey::svydesign(id=~1, weights=~myown_wr, data=baseinfdo)
      # ratiotable<-survey::svymean(~memb, syvdes) %>%
      #   as.data.frame() %>%
      #   dplyr::arrange(dplyr::desc(mean)) %>%
      #   dplyr::mutate(clustn=row.names(.)) %>%
      #   dplyr::mutate_at("clustn", ~as.numeric(customgsub(clustn,"memb",""))) %>%
      #   cbind(., clustsize = seq(1,nrow(.)))
      catgnames<-colnames(needkamilamodel$input$catFactor) %>%
        magrittr::set_names(names(needkamilamodel$finalProbs))
      catglevels<-lapply(needkamilamodel$input$catFactor,levels)
      continames<-colnames(needkamilamodel$input$conVar)
      probtable<-lapply(names(needkamilamodel$finalProbs), function(X, ...) {
        matchcatgvarname<-catgnames[[X]]
        catgprob<-needkamilamodel$finalProbs[[X]] %>%
          magrittr::set_colnames(catglevels[[matchcatgvarname]])
      },needkamilamodel=needkamilamodel, catgnames=catgnames, catglevels=catglevels, continames=continames) %>%
        do.call(cbind,.) %>%
        cbind(needkamilamodel$finalCenters %>%
                magrittr::set_colnames(continames), .)
      data.frame("totlclusters"=needkamilamodel$nClust$bestNClust,"clustn"=1:needkamilamodel$nClust$bestNClust) %>%
        cbind(needargrow,.) %>%
        cbind(probtable) %>%
        dplyr::left_join(ratiotable, .) %>%
        dplyr::select(survey, imp, store_key, totlclusters, populationsize, dplyr::everything(), -clustn) %>%
        return()
    }, kamila_results=kamila_results, kamila_arguments_df=kamila_arguments_df, method=parallel_method, rate=rate) %>%
      dplyr::bind_rows() %>%
      dplyr::arrange(survey, imp)
    if (save==TRUE) {
      save(kamila_clustering_parameters, file=self$kamila_clustering_parameters_filepath)
      #write.csv(kamila_clustering_parameters, file="TMP.csv")
    }
    kamila_clustering_parameters
  },
  check_cluster_distribution = function(survey_data_imputed, imps=self$imps) {
    for (s in names(survey_data_imputed)) {
      for (imp in imps) {  #"cluster_varsellcm" "cluster_clustrd" "cluster_kamila"
        message(paste0("SURVEY is ",s," and imp is",imp))
        dplyr::filter(survey_data_imputed[[s]], .imp==!!imp) %>%
          magrittr::use_series("cluster_kamila") %>%
          table() %>%
          print()
      }
    }
  }
))

# 以下皆為探索過程的其他分群方法與檢核，僅保留參考用（不會執行） ==========================================

# * clustrd clustering single result --------------------------------
if (FALSE) {
  load_lib_or_install(c("clustrd")) #,"future","future.apply"
  idx_process_ratio<-8
  idx_process_ratio<-9.2
  source("(ignore)09_clustering_clustrd_commonpart_smallrange.R")
  stop() #setwd('/mnt/e/Software/scripts/R/vote_record')
}

## Establishing connections --------------------------------
if (FALSE) {
  db_table_name<-"demographic_clusters"
  message(myremoteip)
  dbtype <- RMariaDB::MariaDB() #RSQLite::SQLite()
  dbname <- "thesis"
  dbhost <- mysqldbhost
  dbusername <- "j"
  dbpassword <- ifelse(exists("dbpassword"),dbpassword,getPass::getPass("Please enter your password: ")) #rstudioapi::askForPassword("input password")
  dbport <- 3306
  dbconnect_info <- list(
    "drv"=dbtype,
    "host"=dbhost,
    "dbname"=dbname,
    "username"=dbusername,
    "password"=dbpassword,
    "port"=dbport
  )
  con <- do.call(DBI::dbConnect, dbconnect_info)
  DBI::dbDisconnect(con)

  tryCatch({
    con <- do.call(DBI::dbConnect, dbconnect_info)
    clustering_result_to_infotable() %>%
      DBI::dbWriteTable(con, db_table_name, .)
    DBI::dbDisconnect(con)}, error=function(e) {
      cat("Failed on dbWriteTable")
      cat(str(e))
    }
  )
}

# * loading clustrd cluster assement and applying --------------------------------
if (FALSE) {
  con <- do.call(DBI::dbConnect, dbconnect_info)
  clustrd_assesment_result_argu_df_basis<-RMariaDB::dbReadTable(con, "demographic_clusters") %>%
    dplyr::filter(dst=="low", criterion=="asw") %>%
    dplyr::inner_join({
      dplyr::group_by(., survey, imp, weight) %>%
        dplyr::summarise(critbest = max(critbest))
    }) %>%
    dplyr::inner_join({
      dplyr::group_by(., survey, imp, weight) %>%
        dplyr::summarise(nclusbest = max(nclusbest))
    }) %>%
    dplyr::inner_join({
      dplyr::group_by(., survey, imp, weight) %>%
        dplyr::summarise(criterion_in_obj = max(criterion_in_obj))
    }) %>%
    dplyr::arrange(survey, imp, weight) %>%
    mutate_cond(is.na(scale), scale=1) %>%
    mutate_cond(is.na(center), center=1) %>%
    mutate_cond(is.na(nstart), nstart=100) %>%
    dplyr::mutate(title2=paste0(survey,"_imp",imp))
  DBI::dbDisconnect(con)

  i<-1
  clustrd_assesment_result_argu_df<-clustrd_assesment_result_argu_df_basis  %>%
    dplyr::inner_join({
      dplyr::group_by(., survey, imp, weight) %>%
        dplyr::slice(!!i)
    }) %>%
    dplyr::filter(weight==0)
  clustrd_results_after_assesment_file<-paste0(dataset_in_scriptsfile_directory, "clustrd_results_after_assesment.RData")
  #re-model if error occurs; do not delete
  #errorkeys<-which(sapply(clustrd_results_after_assesment,class)=="try-error")
  #clustrd_assesment_result_argu_df %<>% .[errorkeys,]
  #clustrd_assesment_result_argu_df %<>% dplyr::filter(title2 %in% !!assign_results)
  clustrd_assesment_result_argu_df$title2 %>%
    magrittr::set_names(custom_parallel_lapply(., function(fikey,...) {
      needrow<-dplyr::filter(clustrd_assesment_result_argu_df, title2==!!fikey)
      needsurvey<-needrow$survey
      needdf<-survey_data_imputed[[needsurvey]] %>%
        .[.$.imp==needrow$imp,magrittr::extract2(clustering_var,needsurvey)]
      clustrd_model<-try({needdf %>%
          clustrd::cluspcamix(nclus=needrow$nclusbest, ndim=needrow$ndimbest, method=needrow$method,
                              center=as.logical(needrow$center), scale=as.logical(needrow$scale), alpha=as.logical(needrow$alpha), rotation=needrow$rotation,
                              nstart=needrow$nstart)
      })
      if(is(clustrd_model, 'try-error')) {
        clustrd_model<-try({needdf %>%
            clustrd::tuneclus(nclusrange=needrow$nclusbest,ndimrange=needrow$ndimbest, method="mixedRKM", dst="low", criterion="asw",
                              center=as.logical(needrow$center), scale=as.logical(needrow$scale), alpha=as.numeric(needrow$alpha), rotation=needrow$rotation,
                              nstart=needrow$nstart)
        } %>% magrittr::extract2("clusobjbest"))
      }
      if(!is(clustrd_model, 'try-error')) {
        while(TRUE){
          loadsavestatus<-try({
            load(file=clustrd_results_after_assesment_file, verbose=TRUE)
            clustrd_results_after_assesment[[fikey]]<-clustrd_model
            save(clustrd_results_after_assesment, file=clustrd_results_after_assesment_file)
          })
          if(!is(loadsavestatus, 'try-error')) break
        }
        return(clustrd_model)
      } else {
        return("ERROR")
      }
    },survey_data_imputed=survey_data_imputed,
    clustrd_assesment_result_argu_df=clustrd_assesment_result_argu_df,
    clustering_var=clustering_var,
    clustrd_results_after_assesment_file=clustrd_results_after_assesment_file,
    method=parallel_method
    ), .)

  load(file=clustrd_results_after_assesment_file, verbose=TRUE)
  assign_results<-c()
  for (fi in 1:nrow(clustrd_assesment_result_argu_df)) {
    needrow<-clustrd_assesment_result_argu_df[fi,]
    clustrdmodel<-clustrd_results_after_assesment[[needrow$title2]]
    message(paste("now in", needrow$title2))
    table(clustrdmodel$cluster) %>% print()
    tempdf_imppos<-which(survey_data_imputed[[needrow$survey]]$.imp==needrow$imp)
    if (!("cluster_clustrd" %in% names(survey_data_imputed[[needrow$survey]]))) {
      survey_data_imputed[[needrow$survey]] %<>% dplyr::mutate(cluster_clustrd=NA)
    }
    assign_result<-try({
      survey_data_imputed[[needrow$survey]]$cluster_clustrd[tempdf_imppos]<-clustrdmodel$cluster
    })
    if(is(assign_result, 'try-error')) {
      assign_results %<>% c(needrow$title2)
    }
  }
  if (length(assign_results)==0) message(assign_results)
}

# * My own Using WeightedCluster Examples --------------------------------
if (FALSE) {
  load_lib_or_install(c("rvest","rlist","parallel","itertools")) #,"future","future.apply"
  load_lib_or_install(c("RMariaDB","getPass"))
  load_lib_or_install(c("WeightedCluster","cluster","ggplot2"))

  k_range<-1:2#30
  hclustermethods<-c("single", "mcquitty", "complete", "ward.D", "ward.D2", "median", "centroid", "average")
  pclustermethods<-c("KMedoids", "PAMonce")
  needindicator<-c("ASWw", "HG", "PBC", "HC")
  cluster_quality_resultsdf<-data.frame()
  combine_hclust_pam_results<-list()
  hclustrange_detects<-list()
  pamrange_detects<-list()
  load(file=(paste0(dataset_in_scriptsfile_directory,"weightedclustering_detect_results.RData")), verbose=TRUE )
  it <- ihasNext(product(needsurveykey = 1:4, needimp = 1:5))
  while(hasNext(it)) {
    iterx <- nextElem(it)
    print(iterx)
    if (iterx$needsurveykey %in% c(1,2)) {next}
    inputData<-survey_data_imputed[[iterx$needsurveykey]]
    survey<-inputData$SURVEY[1]
    surveyweight<-inputData$myown_wr[inputData$.imp==iterx$needimp]
    inputData<-inputData[inputData$.imp==iterx$needimp,clustering_var[[iterx$needsurveykey]]]
    inputData$myown_age %<>% scale() %>% .[,1]
    if ({plotingattr_distribution<-FALSE;plotingattr_distribution}) {
      gplotd <- ggplot(inputData)
      gplotd+geom_density(aes(myown_age),kernel = "gaussian")
      gplotd+geom_density(aes(myown_factoredses),kernel = "gaussian")
    }
    G.dist <- cluster::daisy(x = inputData, metric = "gower")
    gower_mat <- as.matrix(G.dist)
    cluster_arguments_df<-data.frame("survey"=survey,"imp"=iterx$needimp,"hmethod"=hclustermethods) %>%
      cbind(., k = rep(k_range, each = nrow(.))) %>%
      cbind(., pmethod = rep(pclustermethods, each = nrow(.))) %>%
      dplyr::mutate(title=paste0(survey,"_","imp",imp,"_",hmethod,"_",pmethod,"_",k)) %>%
      dplyr::mutate_at(c("hmethod","pmethod"),.funs=as.character)
    hclusterresults<-mclapply(hclustermethods, function(hclustermethod) {
      signlehclustresult<-hclust(G.dist,method=hclustermethod,members=surveyweight)
      return(signlehclustresult)
    },mc.cores = parallel::detectCores()) %>%
      set_names(hclustermethods)
    hcluster_cor_results<-mclapply(names(hclusterresults), function(signlehclustresult_idx) {
      signlehclustresult<-extract2(hclusterresults,signlehclustresult_idx)
      #fviz_dend(hclusterresults[[hclustermethod]], cex = 0.5)
      res.coph <- cophenetic(signlehclustresult)
      cophentic_distance_relation<-cor(G.dist, res.coph)
      return(data.frame(hmethod=as.character(signlehclustresult_idx),cor=as.numeric(cophentic_distance_relation)))
    },mc.cores = parallel::detectCores()) %>% #
      dplyr::bind_rows()
    hcluster_quality_results<-mcmapply(function(hmethod,k,hclusterresults) {
      singlehclusterresult<-extract2(hclusterresults, hmethod)
      clust4 <- cutree(singlehclusterresult, k=k)
      clustqual4 <- wcClusterQuality(G.dist, clust4, weights=surveyweight)
      df1<-data.frame("survey"=survey,"imp"=iterx$needimp,"by"="hierarchical","indicator"=names(clustqual4$stats), "stats"=clustqual4$stats) %>%
        cbind(., hmethod = rep(hmethod, each = nrow(.))) %>%
        cbind(., k = rep(k, each = nrow(.)))
      rownames(df1) <- NULL
      return(df1)
    }, hmethod=cluster_arguments_df$hmethod, k=cluster_arguments_df$k, SIMPLIFY = FALSE, MoreArgs = list(hclusterresults=hclusterresults),mc.cores = parallel::detectCores()) %>%
      set_names(cluster_arguments_df$title) %>%
      dplyr::bind_rows() %>%
      dplyr::left_join(hcluster_cor_results)
    new_combine_hclust_pam_results<-mcmapply(
      function(hmethod, pmethod, k, hclusterresults) {
        message("hmethod is ", hmethod, " and pmethod is ", pmethod, " and k is ", k)
        singlehclusterresult<-extract2(hclusterresults, hmethod)
        singlepamresult<-wcKMedoids(G.dist, k=k, weights=surveyweight, initialclust=singlehclusterresult, method=pmethod)
        signleclustresult<-singlepamresult
        return(signleclustresult)
      }, hmethod=cluster_arguments_df$hmethod, pmethod=cluster_arguments_df$pmethod, k=cluster_arguments_df$k, SIMPLIFY = FALSE, MoreArgs = list(hclusterresults=hclusterresults), mc.cores = parallel::detectCores()) %>%
      set_names(cluster_arguments_df$title)
    combine_hclust_pam_results<-c(combine_hclust_pam_results, new_combine_hclust_pam_results)
    combine_hclust_pam_results_stats<-mclapply(names(new_combine_hclust_pam_results), function(idx) {
      single_combine_hclust_pam_result<-extract2(combine_hclust_pam_results,idx)
      title<-strsplit(idx,"_") %>% unlist()
      survey<-title[1]
      imp<-as.integer(stri_replace(str=title[2],replacement="",fixed="imp"))
      hmethod<-title[3]
      pmethod<-title[4]
      kv<-as.integer(title[5])
      data.frame(
        "survey"=survey,
        "imp"=imp,
        "by"="Partition",
        "indicator"=names(single_combine_hclust_pam_result$stats),
        "stats"=unname(single_combine_hclust_pam_result$stats),
        "hmethod"=hmethod,
        "pmethod"=pmethod,
        "k"=kv
      ) %>%
        return()
    },mc.cores = parallel::detectCores()) %>%
      dplyr::bind_rows()

    cluster_quality_resultsdf<-dplyr::bind_rows(cluster_quality_resultsdf,{
      dplyr::bind_rows(hcluster_quality_results,combine_hclust_pam_results_stats) %>%
        dplyr::filter(indicator %in% needindicator) %>%
        dplyr::arrange(by, indicator, stats)}
    )
    if ({using_default_range_detects<-FALSE;using_default_range_detects}) {
      hclustrange_detects<-c(hclustrange_detects,mclapply(hclusterresults, function(single_hclusterresult) {
        as.clustrange(single_hclusterresult, diss=G.dist, weights=surveyweight, ncluster=max(k_range))
      },mc.cores = parallel::detectCores()) %>%
        set_names(paste0(survey, "_", "imp", iterx$needimp, "_", names(.) ) ) )
      pamrange_detects <- c(pamrange_detects,
                            list(wcKMedRange(G.dist, kvals=k_range, weights=surveyweight)) %>% set_names(paste0(survey, "_imp", iterx$needimp))
      )
    }
    if ({printingandsummary_detects<-FALSE;printingandsummary_detects}) {
      for (idx in names(hclustrange_detects)) {
        message(idx)
        print(summary(hclustrange_detects[[idx]], max.rank=3))
        plot(hclustrange_detects[[idx]], stat=needindicator, norm="zscore" , main=idx)
      }
      summary(pamrange_detects, max.rank=3)
      plot(pamrange_detects, stat=needindicator, norm="zscore" , main="wcKMedRange")
    }
    tryCatch(save(
      combine_hclust_pam_results, cluster_quality_resultsdf, file=paste0(dataset_in_scriptsfile_directory,"weightedclustering_detect_results.RData")
    ), error=function(e) {
      message(e)
    })
  }
}

# http://fenyolab.org/presentations/Machine_Learning_2018/slides/2.%20Cluster%20Analysis.pdf

# * loading processed hierarchical Kmed cluster quality examination ------------------------
if (FALSE) {
  load(file=(paste0(dataset_in_scriptsfile_directory,"weightedclustering_detect_results.RData")), verbose=TRUE )
  cluster_quality_resultsdf %<>% dplyr::mutate(title=paste0(survey, "_", "imp", imp, "_", hmethod, "_", pmethod, "_", k))
  View(dplyr::distinct(dplyr::arrange(cluster_quality_resultsdf, survey, imp, by, indicator, desc(stats), hmethod),survey, imp, by, indicator, stats, hmethod, k, cor, pmethod))
  #（歷次檢核結果數據紀錄已省略，見git歷史版本）
}

# * DBSCAN ----------------
if (FALSE) {
  load_lib_or_install(c("fpc","dbscan","factoextra"))
  #For more than 2 dimensions: minPts=2*dim (Sander et al., 1998)
  DBSCAN_arguments_df<-data.frame("survey"=survey_data_title) %>%
    cbind(., imp = rep(imps, each = nrow(.))) %>%
    cbind(., minpts = rep(12, each = nrow(.))) %>%
    cbind(., minclusters = rep(2, each = nrow(.))) %>%
    dplyr::mutate(dbscan_key=paste0(survey,"_imp",imp,"_minpts",minpts,"_minclusters",minclusters))
  DBSCAN_results<-list()
  DBSCANclusterfile <- paste0(dataset_in_scriptsfile_directory, "DBSCANcluster.Rdata")
  #OPTICS
  for (fi in 1:nrow(DBSCAN_arguments_df)) { #22
    survey_key <- DBSCAN_arguments_df$survey[fi]
    needimp <- DBSCAN_arguments_df$imp[fi]
    need_minclusters<-DBSCAN_arguments_df$minclusters[fi]
    store_key <- DBSCAN_arguments_df$dbscan_key[fi]
    needdf<-dplyr::filter(survey_data_imputed[[survey_key]], .imp==!!needimp) %>%
      dplyr::mutate_at("myown_age",scale) %>%
      dplyr::mutate_at("myown_age",function (X) {X[,1]})
    targetminpts <- DBSCAN_arguments_df$minpts[fi] %>%
      as.integer() #readline(paste("assigning k value for detecting survey",survey_key, "imp", needimp, ":"))
    surveyweight<-needdf$myown_wr
    inputData<-dplyr::select(needdf, !!clustering_var[[survey_key]])
    G.dist <- cluster::daisy(x = inputData, metric = "gower")
    gower_mat <- as.matrix(G.dist)
    #finding epsilon distance(eps)
    dbscan::kNNdistplot(G.dist, k = targetminpts-1)
    title(main = paste("survey",survey_key, "imp", needimp, "minpts", targetminpts))
    iteri <- 1
    repeat { #透過檢核有無dbscan分析出來的集群出現自動往上找eps參數
      assigned_dbscan_optimal_eps <- 2+.0025*iteri
      message(paste("now in iter", iteri,"of",store_key,"and try eps at",assigned_dbscan_optimal_eps))
      abline(h = as.numeric(assigned_dbscan_optimal_eps), lty = 2)
      #generating clusters
      iteri <- iteri+1
      #dbcluster_obj<-dbscan::dbscan(gower_mat, eps=assigned_dbscan_optimal_eps, minPts = targetminpts, weights=surveyweight)
      optic_obj<-dbscan::optics(gower_mat, minPts = targetminpts)
      dbcluster_obj<-dbscan::extractDBSCAN(optic_obj, eps_cl=assigned_dbscan_optimal_eps)
      if (length(unique(dbcluster_obj$cluster))<need_minclusters) next
      silhouetteresult<-cluster::silhouette(dbcluster_obj$cluster, dmatrix=gower_mat)
      sil_avg_width<-tryCatch(
        summary(silhouetteresult)$avg.width,
        error = function(e) {return("ERROR")}
      )
      if (sil_avg_width!="ERROR") {
        DBSCAN_arguments_df[fi, "ncluster"]<-length(unique(dbcluster_obj$cluster))
        DBSCAN_arguments_df[fi, "silhouetteresult"]<-sil_avg_width
        DBSCAN_arguments_df[fi, "eps"]<-assigned_dbscan_optimal_eps
        break
      }
    }
    DBSCAN_results[[store_key]]<-dbcluster_obj
  }
  save(DBSCAN_results, DBSCAN_arguments_df, file=DBSCANclusterfile)
  load(file=DBSCANclusterfile, verbose=TRUE)
  for (fi in 1:nrow(DBSCAN_arguments_df)) {
    survey_key <- DBSCAN_arguments_df$survey[fi]
    needimp <- DBSCAN_arguments_df$imp[fi]
    store_key <- DBSCAN_arguments_df$dbscan_key[fi]
    if (grepl("minclusters3", store_key)==FALSE) next
    dbcluster_obj <- DBSCAN_results[[store_key]]
    for (cluster_i in unique(dbcluster_obj$cluster)) {
      dplyr::filter(survey_data_imputed[[survey_key]], .imp==!!needimp) %>%
        .[dbcluster_obj$cluster==cluster_i,clustering_var[[survey_key]]] %>% View()
      readline(paste("now in",store_key,"and cluster is",cluster_i,"continue? "))
    }
  }
}

# * model-based clustering by mixtools and pdfCluster----------------
#https://tinyheero.github.io/2015/10/13/mixture-model.html
if (FALSE) {
  mixtools_args<-data.frame("survey"=survey_data_title) %>%
    cbind(., imp = rep(imps, each = nrow(.))) %>%
    dplyr::mutate(store_key=paste0(survey,"_",imp)) %>%
    dplyr::filter(survey %in% !!c("2010overall","2016citizen")) %>%
    dplyr::mutate_at("survey", as.character) %>%
    dplyr::mutate_at("imp", as.integer) %>%
    dplyr::arrange(survey, imp)
  needrow<-mixtools_args[1,]
  t<-survey_data_imputed[[needrow$survey]] %>%
    dplyr::filter(.imp==!!needrow$imp) %>%
    dplyr::select(!!clustering_var[[needrow$survey]])
}

# * model-based clustering by clustMD ----------------
#load_lib_or_install(c("clustMD"))
if (FALSE) {
  clustrd_results_with_best_argument<-list()
  it <- ihasNext(product(needsurvey = survey_data_title[1], needimp = 1))
  while(hasNext(it)) {
    iterx <- nextElem(it)
    needdf<-dplyr::filter(survey_data_imputed[[iterx$needsurvey]], .imp==!!iterx$needimp)
    surveyweight<-needdf$myown_wr
    needdf<-dplyr::select(needdf, !!clustering_var[[iterx$needsurvey]])
    inputData <- dplyr::bind_cols(dplyr::select_if(needdf, is.numeric),dplyr::select_if(needdf, is.factor))
    #measure skewness
    #If skewness value lies above +1 or below -1, data is highly skewed. If it lies between +0.5 to -0.5, it is moderately skewed. If the value is 0, then the data is symmetric
    numericvars<-lapply(inputData, is.numeric) %>%
      .[.==TRUE] %>%
      names()
    to_skew_num_var<-lapply(dplyr::select_if(inputData, is.numeric), e1071::skewness) %>%
    {is_greater_than(.,0.05) | is_less_than(.,-0.05)} %>%
      .[.==TRUE] %>%
      names()
    inputData %<>% dplyr::mutate_at(to_skew_num_var,custom_shift_sqrt) %>%
      dplyr::mutate_at(numericvars,scale) %>%
      dplyr::mutate_at(numericvars,function (X) {X[,1]})
    #ploting var to check the skewness
    for (numericvar in numericvars) {
      custom_plot(cbind(inputData,surveyweight), numericvar, weightvar="surveyweight") %>% print()
      readline("wait")
    }
    factorvars<-sapply(inputData, class) %>%
      .[.=="factor"] %>%
      names()
    previous_levellables<-lapply(inputData[,factorvars],levels) %>%
      lapply(sort)
    afterward_levellables<-lapply(previous_levellables, function(x) {
      x<-gsub("]\\s{1}.+","",x,perl=TRUE)
      x<-gsub("\\[{1}","",x,perl=TRUE)
      return(x)
    })
    recode_factorvar_levellabels<-lapply(names(previous_levellables), function(factorvar) {
      to_labels<- previous_levellables[[factorvar]] %>%
        set_names(afterward_levellables[[factorvar]])
      return(to_labels)
    }) %>% unlist()
    inputData <- dplyr::mutate_if(inputData, is.factor, forcats::fct_recode, !!!recode_factorvar_levellabels)
  }
}

# Assessing Cluster clustrd Stability --------------------------------
#https://zh-tw.coursera.org/lecture/cluster-analysis/6-9-cluster-stability-65y3a
#https://www.cyut.edu.tw/~rcchen/research/html/ms/s8914617/cyut-8-324.pdf
if (FALSE) {
  clustering_result_compare_table<-future_mapply(function(title,listelement) {
    df1<-data.frame("title"=c(title))
    df2<-stri_split_fixed(title, "_",simplify = TRUE) %>%
      as.data.frame() %>%
      set_colnames(c("survey","alpha","method","rotation","criterion","dst"))
    df3<-listelement[c("nclusbest","ndimbest", "crit", "critbest")] %>%
      as.data.frame()
    df4<-listelement$clusobjbest[c("scale","center","nstart")] %>% #"criterion",
      as.data.frame()
    df5<-data.frame("criterion_in_obj"=as.character(listelement$clusobjbest$criterion))
    cbind(df1,df2,df3,df4,df5)
  },title=names(tmpdetect_best_results), listelement=tmpdetect_best_results,SIMPLIFY = FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(survey, desc(critbest), crit, desc(criterion_in_obj))
  View(clustering_result_compare_table)

  #The ASW index, which ranges from −1 to 1, reflects the compactness of the clusters and indicates whether a cluster structure is well separated or not.
  #The CH index is the ratio of between-cluster variance to within-cluster variance, corrected according to the number of clusters, and takes values between 0 and infinity.
  #In general, the higher the ASW and CH values, the better the cluster separation.

  for (i in 1:length(detect_best_results)) {
    message(names(detect_best_results)[[i]])
    print(detect_best_results[[i]])
  }
}

# Apply Clustrd results according to previously retrieved information db--------------------------------
if (FALSE) {
  con <- do.call(DBI::dbConnect, dbconnect_info)
  already_in_sqltable_clustrd_records<-DBI::dbReadTable(con, db_table_name)
  DBI::dbDisconnect(con)

  nclusbest_threshold<-3
  comparing_appro_clustrd_result<-lapply(names(clustering_var), function(survey) {
    dplyr::filter(already_in_sqltable_clustrd_records, survey==!!survey, criterion=="asw") %>% #, imp==needimp
      dplyr::filter(nclusbest>=!!nclusbest_threshold) %>%
      dplyr::filter(critbest==max(critbest)) %>%
      dplyr::distinct(alpha, method, rotation, dst, nclusbest, ndimbest) %>%
      dplyr::arrange(nclusbest, ndimbest, alpha, method, rotation, dst) %>%
      return()
  }) %>% set_names(names(clustering_var))
  #try finding common clustrd argument
  for (i in 2:3) {
    if (i==2) common_argument_for_clustrd_results<-comparing_appro_clustrd_result[[i]]
    common_argument_for_clustrd_results %<>% dplyr::semi_join(comparing_appro_clustrd_result[[i+1]])
  }
  #if no common clustrd argument, using single best argument
  if (nrow(common_argument_for_clustrd_results)>0) {
    common_argument_for_clustrd_results<-common_argument_for_clustrd_results[1,]
    bestarguments_for_clustrd<-list(
      "2004citizen"=comparing_appro_clustrd_result$`2004citizen`,
      "2010env"=common_argument_for_clustrd_results,
      "2010overall"=common_argument_for_clustrd_results,
      "2016citizen"=common_argument_for_clustrd_results
    )
  } else {
    bestarguments_for_clustrd<-lapply(names(clustering_var), function(survey) {
      extract2(comparing_appro_clustrd_result,survey) %>%
        .[1,]
    }) %>% set_names(names(clustering_var))
  }

  clustrd_using_surveyweight<-TRUE
  clustrd_results_with_best_argument<-parallel::mclapply(names(bestarguments_for_clustrd), function(survey) {
    needdata<-extract2(survey_data_imputed,survey) %>%
      dplyr::filter(.imp==1)
    surveyweight<-needdata$myown_wr
    needdata<-needdata %>%
      extract(clustering_var[[survey]]) %>%
      dplyr::mutate_at("myown_age",scale) %>%
      dplyr::mutate_at("myown_age",function (X) {X[,1]})
    extract2(bestarguments_for_clustrd,survey) %>%
      dplyr::rename(nclus=nclusbest, ndim=ndimbest) %>%
      dplyr::select(-dst) %>%
      as.list() %>%
      rlist::list.append(data=needdata, center=TRUE, scale=TRUE, smartStart=surveyweight) %>%
      {
        if (clustrd_using_surveyweight) {
          rlist::list.append(smartStart=surveyweight)
        } else {
          .
        }
      } %>%
      do.call(clustrd::cluspcamix,.) %>%
      return()
  }) %>%
    set_names(names(clustering_var))
  save(clustrd_results_with_best_argument,file=paste0(dataset_in_scriptsfile_directory,"clustrd_results_with_best_argument_nclus-threshold-",nclusbest_threshold,"_with_weight.RData"))

  load(file=paste0(dataset_in_scriptsfile_directory,"clustrd_results_with_best_argument_nclus-threshold-",nclusbest_threshold,".RData"), verbose=TRUE)
  # for using in plotting
  it <- ihasNext(product(needsurveykey = survey_data_title, needimp = 1))
  while(hasNext(it)) {
    iterx <- nextElem(it)
    clustrd_results_with_best_argument[[iterx$needsurveykey]]$cluster<-clustrd_results_with_best_argument[[iterx$needsurveykey]]$cluster
    needdf<-dplyr::filter(survey_data_imputed[[iterx$needsurvey]], .imp==!!iterx$needimp)
    surveyweight<-needdf$myown_wr
    inputData<-dplyr::select(needdf, !!clustering_var[[iterx$needsurvey]]) %>%
      dplyr::mutate_at("myown_age",scale) %>%
      dplyr::mutate_at("myown_age",function (X) {X[,1]})
    G.dist <- cluster::daisy(x = inputData, metric = "gower")
    gower_mat <- as.matrix(G.dist)
    silhouetteresult<-cluster::silhouette(clustrd_results_with_best_argument[[iterx$needsurveykey]]$cluster, dmatrix=gower_mat)
    message(iterx$needsurveykey)
    print(summary(silhouetteresult))
  }
}

# Factor analysis of mixed data and Graphing cluster result --------------------------------
#load_lib_or_install(c("FactoMineR","factoextra","parallel","magrittr"))
if (FALSE) {
  it <- ihasNext(product(needsurveykey = 1:4, needimp = 1))
  while(hasNext(it)) {
    iterx <- nextElem(it)
    if (readline(paste0("Continue to next imputation where imp=",iterx$needimp," ? (Y/N)"))=="N") {
      next
    }
    df<-survey_data_imputed[[iterx$needsurveykey]] %>%
      .[.$.imp==iterx$needimp,]
    survey<-df$SURVEY[1]
    surveyweight<-df$myown_wr
    df<-df[,extract2(clustering_var,iterx$needsurveykey)]
    df$myown_age %<>% scale() %>% .[,1]
    res.famd <- FAMD(df, graph = FALSE, row.w=surveyweight)
    summary.FAMD(res.famd)
    eig.val <- get_eigenvalue(res.famd)
    head(eig.val) #要選擇特徵值大於一的
    fviz_screeplot(res.famd)
    if ({usingpamresult<-exists("cluster_quality_resultsdf");usingpamresult}) {
      needclusteringmethod<-dplyr::arrange(cluster_quality_resultsdf, survey, imp, by, indicator, desc(stats), hmethod) %>%
        dplyr::filter(!is.na(pmethod)) %>%
        dplyr::distinct(survey, imp, by, indicator, stats, hmethod, k, cor, pmethod, title) %>%
        dplyr::filter(survey==!!survey) %>%
        head() %>%
        extract2("title") %>%
        .[1] %>%
        extract2(combine_hclust_pam_results, .)
    }
    clusterf<-switch(as.character(exists("clustrd_results_with_best_argument")),
                     "TRUE"=clustrd_results_with_best_argument[[iterx$needsurveykey]]$cluster,
                     "FALSE"=needclusteringmethod$clustering) %>%
      as.factor() %>%
      forcats::fct_infreq()
    table(clusterf)
    var <- get_famd_var(res.famd)
    #weired: 2010env dim 4
    for(dim in combn(1:5,2,simplify = FALSE)) {#ucscgb #simpsons
      fviz_famd_ind(res.famd, axes=dim, geom = "point", ggtheme = theme_classic(), legend = "bottom",
                    title = paste0("FAMD ", survey, " imp ", iterx$needimp," dim ", dim),
                    habillage=clusterf,
                    palette = "jco") %>%
        print()
      # Coordinates of variables
      print("Coordinates")
      head(var$coord[,dim]) %>% print()
      # Cos2: quality of representation on the factore map
      print("quality")
      head(var$cos2[,dim]) %>% print()
      # Contributions to the dimensions
      print("Contributions")
      head(var$contrib[,dim]) %>% print()
      if (iterx$needimp==1) {
        if (readline("Continue to show dim contribution graph? (Y/N)")=="Y") {
          # Plot of variables
          fviz_famd_var(res.famd, axes=dim, repel = TRUE)
          # Contribution to the ?th dimension
          print(fviz_contrib(res.famd, "var", axes = dim))
        }
      }
      if (readline("Continue to next dim graph? (Y/N)")=="N") {
        break
      }
    }
  }
}

# VAT (Visual Assessment of Tendency) assessing clustering tendency --------------------------------
if (FALSE) {
  load_lib_or_install(c("itertools","cluster","factoextra","dendextend","clustertend","factoextra","NbClust"))
  it <- ihasNext(product(needsurveykey = 1:4, needimp = 1)) #:5
  while(hasNext(it)) {
    iterx <- nextElem(it)
    inputData<-survey_data_imputed[[iterx$needsurveykey]]
    survey<-inputData$SURVEY[1]
    surveyweight<-inputData$myown_wr[inputData$.imp==iterx$needimp]
    inputData<-inputData[inputData$.imp==iterx$needimp,clustering_var[[iterx$needsurveykey]]]
    inputData$myown_age %<>% scale() %>%
      .[,1]
    #hopkins(inputData, n = nrow(inputData)-1)
    #get_clust_tendency(inputData, n = nrow(inputData)-1)
    G.dist <- cluster::daisy(x = inputData, metric = "gower")
    gower_mat <- as.matrix(G.dist)
    print(
      fviz_dist(G.dist, show_labels = FALSE)+
        labs(title = survey)
    )
    if (readline("Continue to next VAT? (Y/N)")=="N") {
      break
    }
  }
}

#save(survey_data_imputed,file=paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt_lca_clustering",".RData"))
#save(survey_data_imputed,file=paste0(save_dataset_in_scriptsfile_directory,"miced_survey_2surveysonly_mirt_lca_clustering.RData"))
#load(file=paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt_lca_clustering",".RData"), verbose=TRUE)
