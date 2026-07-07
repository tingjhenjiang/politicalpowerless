source(file = "05_preprocessing_survey_adding_area_settingNA.R")
# 第四部份：清理資料：填補遺漏值 -------------------------------------------
survey_imputation_class <- R6::R6Class("survey_imputation", inherit=survey_adding_area_settingNA_class, public = list(
  all_survey_combined_after_settingNA_filepath = NULL,
  miced_survey_2surveysonly_filepath = NULL,
  miced_survey_9_with_mirt_lca_clustering_filepath = NULL,
  imputingcalculatebasiscolumn = NULL,
  imputedvaluecolumn = NULL,
  imputation_target_surveys = NULL,
  initialize = function(dataset_in_scriptsfile_directory="/mnt", filespath="/mnt", dataset_file_directory="/mnt", debug_func_mode=TRUE) {
    super$initialize(dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory, filespath=filespath, dataset_file_directory=dataset_file_directory, debug_func_mode=debug_func_mode)
    self$all_survey_combined_after_settingNA_filepath <- file.path(dataset_in_scriptsfile_directory, "all_survey_combined_after_settingNA.RData")
    self$miced_survey_2surveysonly_filepath <- file.path(save_dataset_in_scriptsfile_directory, "miced_survey_2surveysonly.RData")
    self$miced_survey_9_with_mirt_lca_clustering_filepath <- file.path(dataset_in_scriptsfile_directory, "miced_survey_9_with_mirt_lca_clustering.RData")
    self$imputation_target_surveys <- c("2010overall","2016citizen")
    #job_status暫時先刪掉，因為不同問卷概念與選項不一樣難以合併
    self$imputingcalculatebasiscolumn <- lapply(survey_data_title, function(X,df) {
      dplyr::filter(df,SURVEY==!!X,IMPUTATION %in% c("basis","both")) %>%
        dplyr::select(ID) %>% unlist() %>% union(c("admincity")) %>% unname()
    }, df=self$survey_imputation_and_measurement) %>%
      magrittr::set_names(survey_data_title)
    self$imputedvaluecolumn <- lapply(survey_data_title, function(X,df) {
      dplyr::filter(df,SURVEY==!!X,IMPUTATION %in% c("both")) %>%
        dplyr::select(ID) %>% unlist() %>% unname()
    }, df=self$survey_imputation_and_measurement) %>%
      magrittr::set_names(survey_data_title)
  },
  get_survey_data = function() {
    # 讀入05階段輸出並修正個別題項後回傳survey_data
    load_env <- new.env()
    load(self$all_survey_combined_after_settingNA_filepath, envir=load_env, verbose=TRUE)
    survey_data <- load_env$survey_data
    #dplyr::filter(survey_data[["2016citizen"]], customgrepl(myown_selfid, pattern="[6]"))
    #dplyr::filter(survey_data[["2016citizen"]], id %in% c(116213,220130,234140,806123)) %>% View()
    survey_data[["2016citizen"]] %<>% mutate_cond(customgrepl(myown_selfid, "[6]"), myown_selfid=NA) %>%
      dplyr::mutate_at("myown_selfid", droplevels)
    survey_data %<>% self$custom_edit_survey_data()
    survey_data
  },
  custom_edit_survey_data = function(surveydatadflist) {
    surveydatadflist[["2010overall"]] %<>% mutate_cond(myown_age<22, v83="[2] 沒有(跳答85)") %>%
      mutate_cond(myown_age<22, v85="[2] 沒有(跳答89)") %>%
      dplyr::mutate_at(c("v83","v85"), as.ordered) %>%
      dplyr::mutate_at(c("v83","v85"), ~forcats::fct_relevel(., function(s) {sort(s, decreasing = TRUE)}))
    surveydatadflist[["2016citizen"]] %<>% dplyr::mutate(h4r=h4) %>%
      mutate_cond(myown_age<20, h4r="[2] 沒有(跳答h8)" ) %>%
      mutate_cond(h4r=="[3] 沒有投票權(跳答h6)", h4r="[2] 沒有(跳答h8)" ) %>%
      dplyr::mutate_at("h4r",droplevels) %>%
      dplyr::mutate_at("h4r",as.ordered) %>%
      dplyr::mutate_at(c("h4r"), ~forcats::fct_relevel(., function(s) {sort(s, decreasing = TRUE)}))
    return(surveydatadflist)
  },
  generate_predictor_matrix = function(df, calculationbasisvar=c(), imputedOnlyVars=c()) {
    #calculationbasisvar<-imputingcalculatebasiscolumn_assigned
    #imputedOnlyVars<-imputedvaluecolumn_assigned
    ## Extract all variable names in dataset
    allVars <- names(df)
    ## names of variables with missingness
    missVars <- names(df)[colSums(is.na(df)) > 0]
    predictorMatrix <- matrix(0, ncol = length(allVars), nrow = length(allVars))
    rownames(predictorMatrix) <- allVars
    colnames(predictorMatrix) <- allVars
    calculationbasisvar <- if(length(calculationbasisvar)==0) allVars else calculationbasisvar
    imputerVars <- intersect(allVars,calculationbasisvar)
    imputerMatrix <- predictorMatrix
    imputerMatrix[,imputerVars] <- 1
    imputedOnlyVars <- if(length(imputedOnlyVars)==0) missVars else imputedOnlyVars
    ## Imputers that have missingness must be imputed.
    imputedVars <- intersect(unique(c(imputedOnlyVars, imputerVars)), missVars)
    imputedMatrix <- predictorMatrix
    imputedMatrix[imputedVars,] <- 1
    predictorMatrix <- imputerMatrix * imputedMatrix
    ## Diagonals must be zeros (a variable cannot impute itself)
    diag(predictorMatrix) <- 0
    return(predictorMatrix)
  },
  run_mcar_tests = function(survey_data) {
    #Package ‘MissMech’
    #To test whether the missing data mechanism, in a set of incompletely ob-served data, is one of missing completely at random (MCAR).For detailed description see Jamshidian, M. Jalal, S., and Jansen, C. (2014). ``Miss-Mech: An R Package for Testing Homoscedasticity, Multivariate Normality, and Missing Com-pletely at Random (MCAR),'' Journal of Statistical Software,  56(6), 1-31. URL http://www.jstatsoft.org/v56/i06/.
    #lapply(survey_data, BaylorEdPsych::LittleMCAR) #mlest cannot handle more than 50 variables.
    testresults<-lapply(survey_data, FUN=function(X) {try(MissMech::TestMCARNormality(
      dplyr::select_if(X, dplyr::funs(!is.character(.))) %>%
        dplyr::mutate_if(is.factor, .funs=~as.numeric(as.character(.)))
      ))})
    mice::md.pattern(survey_data[[3]])
    testresults
  },
  run_vim_diagnostics = function(survey_data) {
    #https://statistics.ohlsen-web.de/multiple-imputation-with-mice/
    #https://bookdown.org/mwheymans/bookmi/missing-data-evaluation.html
    survey_data_imputed <- lapply(survey_data,function(X,imputedvaluecolumn,imputingcalculatebasiscolumn) {
      X<-droplevels(X)
      imputingcalculatebasiscolumn_assigned <-magrittr::extract2(imputingcalculatebasiscolumn,X$SURVEY[1]) %>%
        base::intersect(names(X))
      imputedvaluecolumn_assigned <- magrittr::extract2(imputedvaluecolumn,X$SURVEY[1]) %>%
        base::intersect(names(X))
      allcols<-base::union(imputingcalculatebasiscolumn_assigned,imputedvaluecolumn_assigned)
      allcolx<-1:length(allcols)
      splits_of_allcols<-split(allcolx, sort(allcolx%%5))
      for (splits_of_allcol in splits_of_allcols) {
        needcols<-allcols[splits_of_allcol]
        testresult<-BaylorEdPsych::LittleMCAR(X[,needcols])
        MissMech::TestMCARNormality(X[,needcols]) %>% print()
        print(testresult$p.value)
        readline("continue?")
      }
      VIM::aggr(X[,allcols], col=c('white','red'), numbers=TRUE, sortVars=TRUE, cex.axis=.7, gap=3, ylab=c("Percentage of missing data","Missing Data Pattern"))
      mice::md.pattern(X[,allcols])
      return(testresult)
    },imputedvaluecolumn=self$imputedvaluecolumn,imputingcalculatebasiscolumn=self$imputingcalculatebasiscolumn)
    survey_data_imputed
  },
  compute_missing_proportion = function(survey_data) {
    propMissing <- lapply(survey_data, function(X) {
      missing.indicator <- data.frame(is.na(X))
      propMissing <- apply(missing.indicator,2,mean)
      #create dummy missing value indicators
      names(missing.indicator)[propMissing>0] <- paste(names(X)[propMissing>0],"NA",sep="")
      #convert dummy missing indicators from logical to numeric variables
      for (var in 1:ncol(missing.indicator)) {
        missing.indicator[,var] <- as.numeric(missing.indicator[,var])
      }
      #merge covariate names with missing indicator names
      X %<>% cbind(missing.indicator[,propMissing>0])
      #show percentage missing
      print(round(propMissing,3))
    })
    propMissing
  },
  myown_imp_function = function(X,imputedvaluecolumn,imputingcalculatebasiscolumn,imputation_sample_i_s,...) {
    message("now step 1")
    #library(mice)
    X<-droplevels(X)
    missing.indicator <- data.frame(is.na(X))
    propMissing <- apply(missing.indicator,2,mean)
    #create dummy missing value indicators
    names(missing.indicator)[propMissing>0] <- paste(names(X)[propMissing>0],"NA",sep="")
    #convert dummy missing indicators from logical to numeric variables
    for (var in 1:ncol(missing.indicator)) {
      missing.indicator[,var] <- as.numeric(missing.indicator[,var])
    }
    #merge covariate names with missing indicator names
    #X %<>% cbind(missing.indicator[,propMissing>0])
    message("now step 2")
    imputingcalculatebasiscolumn_assigned <-magrittr::extract2(imputingcalculatebasiscolumn,X$SURVEY[1]) %>%
      base::intersect(names(X))
    imputedvaluecolumn_assigned <- magrittr::extract2(imputedvaluecolumn,X$SURVEY[1]) %>%
      base::intersect(names(X))
    foundationvar<-base::union(imputingcalculatebasiscolumn_assigned,imputedvaluecolumn_assigned)
    ini <- mice::mice(X[,foundationvar], maxit = 0)
    sapply(c("----------------", X$SURVEY[1], "----------------"),print)
    message("now step 3")
    message("table ini nmis")
    print(table(ini$nmis))
    outlist4 <- as.character(ini$loggedEvents[, "out"])
    message("ini logged events")
    print(ini$loggedEvents, 2)
    fx2 <- mice::flux(X[,foundationvar])
    message("now step 4")
    outlist2<-row.names(fx2)[fx2$outflux < 0.45]
    outlist <- unique(c(outlist2, outlist4))
    foundationvar %<>% dplyr::setdiff(outlist)
    print(paste0(c("foundationvar are ",foundationvar), collapse=" "))
    unusefulcolumns <- dplyr::setdiff(names(X),foundationvar)
    #predictor_matrix<-self$generate_predictor_matrix(X,imputingcalculatebasiscolumn_assigned,imputedvaluecolumn_assigned)
    predictor_matrix<-mice::quickpred(X[,foundationvar], mincor=0.1, include=c('myown_eduyr','myown_sex','myown_age','myown_selfid'))
    #sol: https://stackoverflow.com/questions/13495041/random-forests-in-r-empty-classes-in-y-and-argument-legth-0
    #sol: https://stackoverflow.com/questions/24239595/error-using-random-forest-mice-package-during-imputation
    message("now step 5")
    mice_parallel_imp_type <- switch(as.character(grepl("Windows", sessionInfo()$running)), "TRUE"="PSOCK", "FALSE"="FORK")
    #also check: micemd::mice.par
    data_to_mice_imp <- X[,foundationvar]
    while (TRUE) {
      complete_imputed_survey_data <- try({
        miceMod <- micemd::mice.par( #mice::mice #mice::parlmice
          data_to_mice_imp,
          predictorMatrix = predictor_matrix,
          visitSequence="monotone",
          m=imputation_sample_i_s,#1,#
          method="rf",
          maxit=7,
          nnodes=parallel::detectCores()#,
          #n.core=parallel::detectCores()#,
          #cl.type=mice_parallel_imp_type
        )  # perform mice imputation, based on random forests.
        #linear imputation might have error message: system is computationally singular: reciprocal condition number
        #https://stats.stackexchange.com/questions/214267/why-do-i-get-an-error-when-trying-to-impute-missing-data-using-pmm-in-mice-packa
        message("now step 6")
        imputed_survey_data <- mice::complete(miceMod, action="long")  # generate the completed data.
        message("now step 7")
        imputed_survey_data$id <- X$id
        dplyr::left_join(imputed_survey_data, X[,unusefulcolumns], by=c("id"))
      })
      if(!is(complete_imputed_survey_data, 'try-error')) break
    }
    return(complete_imputed_survey_data)
  },
  # 1st imputation
  get_survey_data_imputed = function(loadExisted=TRUE, save=FALSE, survey_data=NA) {
    if (loadExisted==TRUE) {
      load_env <- new.env()
      load(file=self$miced_survey_2surveysonly_filepath, envir=load_env, verbose=TRUE)
      survey_data_imputed <- load_env$survey_data_imputed
    } else {
      survey_data_imputed <- self$parse_survey_data_imputed(survey_data=survey_data)
    }
    if (save==TRUE) {
      save(survey_data_imputed, file=self$miced_survey_2surveysonly_filepath)
    }
    survey_data_imputed
  },
  parse_survey_data_imputed = function(survey_data=NA) {
    if (!inherits(survey_data, what=c("list"))) {
      survey_data <- self$get_survey_data()
    }
    survey_data_imputed <- lapply( #custom_parallel_lapply
      X=survey_data[self$imputation_target_surveys],
      FUN=self$myown_imp_function,
      imputedvaluecolumn=self$imputedvaluecolumn,
      imputingcalculatebasiscolumn=self$imputingcalculatebasiscolumn,
      imputation_sample_i_s=length(imputation_sample_i_s), #24
      exportvar=c("imputedvaluecolumn","parlMICE","imputingcalculatebasiscolumn"),
      exportlib=c("dplyr","base","magrittr","parallel","mice","micemd","randomForest"), #,"MissMech","fastDummies"
      outfile=file.path(dataset_file_directory, "rdata", paste0("parallel_handling_process-", t_sessioninfo_running_with_cpu, ".txt")),
      mc.set.seed = TRUE,
      mc.cores=parallel::detectCores()
    )
    survey_data_imputed
  },
  # further imputation：對第一次填補結果的每個imputed dataset再各補一次
  get_further_imputed_survey_data = function(loadExisted=FALSE, save=FALSE) {
    if (loadExisted==TRUE) {
      load_env <- new.env()
      load(file=self$miced_survey_2surveysonly_filepath, envir=load_env, verbose=TRUE)
      return(load_env$survey_data_imputed)
    }
    survey_data_imputed <- self$parse_further_imputed_survey_data()
    if (save==TRUE) {
      save(survey_data_imputed, file=self$miced_survey_2surveysonly_filepath)
    }
    survey_data_imputed
  },
  parse_further_imputed_survey_data = function() {
    load_env <- new.env()
    load(file=self$miced_survey_9_with_mirt_lca_clustering_filepath, envir=load_env, verbose=TRUE)
    load(file=self$miced_survey_2surveysonly_filepath, envir=load_env, verbose=TRUE)
    survey_data_imputed <- load_env$survey_data_imputed
    furtherimp_argument_df<-data.frame("survey"=self$imputation_target_surveys) %>%
      cbind(., imp = rep(imputation_sample_i_s, each = nrow(.))) %>%
      dplyr::mutate(store_key=paste0(survey,"_imp",imp))
    furtherimp_data<-magrittr::extract(survey_data_imputed,self$imputation_target_surveys) %>%
      self$custom_edit_survey_data()
    furtherimp_imputedvaluecolumn<-self$imputedvaluecolumn
    furtherimp_imputingcalculatebasiscolumn<-self$imputingcalculatebasiscolumn
    furthurimplist<-dplyr::filter(furtherimp_argument_df, survey %in% self$imputation_target_surveys) %>%
      .$store_key %>%
      {magrittr::set_names(custom_parallel_lapply(., function(storekey, ...) {
        message(paste("now in",storekey))
        needrow<-dplyr::filter(furtherimp_argument_df, store_key==!!storekey)
        surveytitle<-needrow$survey
        needimp <-needrow$imp
        impsrcdf<-magrittr::extract2(furtherimp_data, surveytitle) %>%
          dplyr::filter(.imp==!!needimp) %>%
          dplyr::select(-.id, -.imp)
        myown_imp_function(impsrcdf, imputedvaluecolumn=furtherimp_imputedvaluecolumn, imputingcalculatebasiscolumn=furtherimp_imputingcalculatebasiscolumn, imputation_sample_i_s=1) %>%
          dplyr::mutate(.imp=!!needimp)
      },myown_imp_function=self$myown_imp_function,
      furtherimp_imputedvaluecolumn=furtherimp_imputedvaluecolumn,
      furtherimp_imputingcalculatebasiscolumn=furtherimp_imputingcalculatebasiscolumn,
      imputation_sample_i_s=1,
      furtherimp_data=furtherimp_data,
      furtherimp_argument_df=furtherimp_argument_df,
      method=parallel_method, mc.cores=12), .)}
    for (pattern in self$imputation_target_surveys) {
      survey_data_imputed[[pattern]]<-grep(pattern=pattern, x=names(furthurimplist), value=TRUE) %>%
        magrittr::extract(furthurimplist, .) %>%
        dplyr::bind_rows()
    }
    survey_data_imputed
  },
  # 讀取已經填補完成的dataset並檢查
  check_imputed_survey_data = function(survey_data_imputed) {
    #checking imputed df
    lapply(survey_data_imputed,function(X,need_particip_var,need_ses_var_assigned,imputedvaluecolumn) {
      survey<-X$SURVEY[1]
      checkdf<-magrittr::extract(X,dplyr::intersect(names(X),unique(c(
        getElement(need_particip_var,survey),
        need_ses_var_assigned,
        getElement(imputedvaluecolumn,survey)
      )))) %>%
        {colSums(is.na(.))}
      return(checkdf)
    },need_particip_var=need_particip_var,need_ses_var_assigned=need_ses_var_assigned,imputedvaluecolumn=self$imputedvaluecolumn)
  }
))

#此部分屬於舊code，僅保留參考用（原oldimputationmethod區塊，未重構為方法、不會被呼叫）
if ({oldimputationmethod<-FALSE; oldimputationmethod}) {
  #1) numeric data
  which(as.vector(sapply(survey_data$`2010env.sav`,is.numeric)))
  #2) factor data with 2 levels
  which(as.vector(sapply(survey_data$`2010env.sav`,function (X) {
    if (!is.factor(X) | !is.ordered(X)) {
      return(FALSE)
    }
    if (length(levels(X))!=2) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  })))
  #3) factor data with > 2 unordered levels
  which(as.vector(sapply(survey_data$`2010env.sav`,function (X) {
    if (!is.factor(X)) {
      return(FALSE)
    }
    if (length(levels(X))>2) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })))
  #4) factor data with > 2 ordered levels
  which(as.vector(sapply(survey_data$`2010env.sav`,function(X) {
    if (!is.ordered(X)) {
      return(FALSE)
    }
    if (length(levels(X))>2) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })))

  #Himsc填補遺漏值 filling in missing value
  library(Hmisc)

  survey_data_imputed <- custom_parallel_lapply(
    data=t_survey_data_imputed,
    f=function(X,imputedvaluecolumn,imputingcalculatebasiscolumn) {
      X <- droplevels(X)
      imputingcalculatebasiscolumn_assigned <- extract2(imputingcalculatebasiscolumn,X$SURVEY[1]) %>%
        intersect(names(X))
      imputedvaluecolumn_assigned <- extract2(imputedvaluecolumn,X$SURVEY[1]) %>%
        intersect(names(X))
      allimpcolumns<-union(imputedvaluecolumn_assigned,imputingcalculatebasiscolumn_assigned) %>%
        setdiff(c("myown_marriage","v89","v90","v98b","v128_2b","v128_2c","v132b","v125br","v138dr","v43","myown_working_status","v122ar","v122b1r",
                  "v41","myown_vote","v87_3","v88","v91","v17","v16","v87_4","myown_dad_ethgroup","v87_5")) #too few
      cal_formula<-as.formula(paste("~", paste(allimpcolumns, collapse="+")))
      print(c("SURVEY IS ",X$SURVEY[1]," AND FORMULA IS ",cal_formula))
      impute_arg <- aregImpute(cal_formula, data = X[,allimpcolumns], n.impute = 5)
      return(impute_arg)
    },
    imputedvaluecolumn=imputedvaluecolumn,
    imputingcalculatebasiscolumn=imputingcalculatebasiscolumn,
    exportvar=c("survey_data","imputedvaluecolumn","imputingcalculatebasiscolumn"),
    exportlib=c("base",lib,"Hmisc"),
    outfile=paste0(dataset_file_directory,"rdata",slash,"parallel_handling_process-",t_sessioninfo_running_with_cpu,".txt"),
    mc.set.seed = TRUE,
    mc.cores=parallel::detectCores()
  )

  #檢查亂報投票意向

  survey_data_imputed <- lapply(survey_data,function(X,need_ses_var_assigned) {
    need_ses_var_assigned %<>% extract2(X$SURVEY[1]) %>%
      intersect(names(X))
    X %<>% mutate_at(missingvaluecolumn_assigned,funs(replace(.,. %in% c(93:99,996:999,9996:9999),NA ) ) )
    efa.results<-factanal(x=as.matrix(X[,need_ses_var_assigned]) ,factors=4, rotation="promax")
  },need_ses_var)

  zip_to_party<-distinct(elections_df,term,zip,party,wonelection) %>%
    mutate_at("zip", as.character) %>%
    mutate_at("party", as.character) %>%
    unique()
  #正確的選區與參選人
  lieing_check<-read.xlsx(paste(dataset_file_directory,"merger_survey_dataset",slash,"recode_record.xlsx",sep=""), sheet = 4) %>% #, endRow = 1896
    distinct(lieing_check,h5,h6r,h7,h8,h9,id,zip,code) %>% #,h5,h6r,h7,h8,h9 #,v84,v85,v86,v88,v93,v94
    mutate("term"=9) %>%
    rename(party=code) %>%
    mutate_at(c("zip","party"), as.character) #要檢驗的所有投票意向
  zip_to_party_with_jump_answer<-distinct(lieing_check,term,party,zip) %>%
    filter(customgrepl(party,"廢票|沒有投票權")) %>%
    cbind("wonelection"=NA) %>%
    rbind(zip_to_party)

  lieing_check<-mutate(lieing_check,bluepoints=0,greenpoints=0) %>%
    #mutate(bluepoints=ifelse(v84 %in% c(1),bluepoints+1,bluepoints)) %>% #
    mutate(bluepoints=ifelse(h5 %in% c(1,3),bluepoints+1,bluepoints)) %>%
    #mutate(bluepoints=ifelse(v86 %in% c(1,3),bluepoints+1,bluepoints)) %>% #
    mutate(bluepoints=ifelse(h6r %in% c(1,3),bluepoints+1,bluepoints)) %>%
    #mutate(bluepoints=ifelse(v88 %in% c(1,3),bluepoints+1,bluepoints)) %>% #
    mutate(bluepoints=ifelse(h7 %in% c(1,3,4),bluepoints+1,bluepoints)) %>%
    #mutate(bluepoints=ifelse(v93 %in% c(1,3,5),bluepoints+1,bluepoints)) %>% #
    mutate(bluepoints=ifelse(h8 %in% c(1,3,4,9),bluepoints+1,bluepoints)) %>%
    #mutate(bluepoints=ifelse(v94 %in% c(1,3,5),bluepoints+1,bluepoints)) %>% #
    mutate(bluepoints=ifelse(h9 %in% c(1,3,4,9),bluepoints+1,bluepoints)) %>%
    #mutate(greenpoints=ifelse(v84 %in% c(2),greenpoints+1,greenpoints)) %>% #
    mutate(greenpoints=ifelse(h5 %in% c(2),greenpoints+1,greenpoints)) %>%
    #mutate(greenpoints=ifelse(v86 %in% c(2,4),greenpoints+1,greenpoints)) %>% #
    mutate(greenpoints=ifelse(h6r %in% c(2,6,9,19),greenpoints+1,greenpoints)) %>%
    #mutate(greenpoints=ifelse(v88 %in% c(2,4),greenpoints+1,greenpoints)) %>% #
    mutate(greenpoints=ifelse(h7 %in% c(2,6,9,19),greenpoints+1,greenpoints)) %>%
    #mutate(greenpoints=ifelse(v93 %in% c(2,4),greenpoints+1,greenpoints)) %>% #
    mutate(greenpoints=ifelse(h8 %in% c(2,5,7,10),greenpoints+1,greenpoints)) %>%
    #mutate(greenpoints=ifelse(v94 %in% c(2,4),greenpoints+1,greenpoints))#
    mutate(greenpoints=ifelse(h9 %in% c(2,5,7,10),greenpoints+1,greenpoints))

  clear_observed_value_green<-filter(lieing_check, greenpoints>=3, greenpoints>bluepoints, term==9) %>%
    anti_join(zip_to_party_with_jump_answer) %>%
    select(-party) %>%
    left_join(zip_to_party) %>%
    filter(customgrepl(party,"民主進步黨|台灣團結聯盟|時代力量|綠黨社會民主黨聯盟|綠黨"))
  clear_observed_value_blue<-filter(lieing_check, bluepoints>=3, bluepoints>greenpoints, term==9) %>%
    anti_join(zip_to_party_with_jump_answer) %>%
    select(-party) %>%
    left_join(zip_to_party) %>%
    filter(customgrepl(party,"中國國民黨|新黨|親民黨"))

  #c("跳答","忘記了、不知道","拒答")
  lieing_check_with_value<-filter(lieing_check,!(h6r %in% c(96,97,98,99)) ) %>%
    #v86 %in% c(7,9,96,97,98,99)
    anti_join(zip_to_party_with_jump_answer) %>%
    anti_join(clear_observed_value_green,by=c("id")) %>%
    anti_join(clear_observed_value_blue,by=c("id")) %>%
    mutate("party"=NA)
  lieing_check_missing<-filter(lieing_check, h6r %in% c(96,97,98,99)) %>%
    #v86 %in% c(7,9,96,97,98,99) & (v85!=99)
    mutate("party"=NA) %>%
    anti_join(clear_observed_value_green,by=c("id")) %>%
    anti_join(clear_observed_value_blue,by=c("id"))

  exclude_result_blue<-cbind("bluepoints"=rep(3:5,each=4),party=c("民主進步黨","時代力量","台灣團結聯盟","綠黨社會民主黨聯盟")) %>%
    as.data.frame() %>%
    mutate_at(c("party","bluepoints"), as.character) %>%
    mutate_at("bluepoints", as.numeric)
  exclude_result_green<-cbind("greenpoints"=rep(3:5,each=3),party=c("中國國民黨","親民黨","新黨")) %>%
    as.data.frame() %>%
    mutate_at(c("party","greenpoints"), as.character) %>%
    mutate_at("greenpoints", as.numeric)

  correct_check_result<-filter(lieing_check,!(party %in% c("跳答","忘記了、不知道","拒答","忘記了,不知道","跳答或不適用","選人不選黨") ) ) %>% #(v85==99) |
    semi_join(zip_to_party_with_jump_answer) %>%
    bind_rows(clear_observed_value_green,clear_observed_value_blue)
  binded_check_result<-bind_rows(correct_check_result,lieing_check_with_value,lieing_check_missing) %>%
    mutate_at(c("id","zip","h5","h6r","h7","h8","h9","party"), as.factor) %>%
    #,"v84","v86","v88","v93","v94"
    arrange(id,wonelection) %>%
    group_by(id) %>%
    filter(!(duplicated(id)))

  duplicated(X2016_citizen_with_restricted$id)
  length(X2016_citizen_with_restricted$id[duplicated(X2016_citizen_with_restricted$id)])
  #標準化z-normalization或min-max scale
}
