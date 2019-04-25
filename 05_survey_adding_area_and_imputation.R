# 第Ｏ部份：環境設定 --------------------------------
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
source(file = "shared_functions.R")
gc(verbose=TRUE)

survey_imputation_and_measurement<-openxlsx::read.xlsx(path_to_survey_imputation_and_measurement_file,sheet = 1)






# 第三部份：把問卷檔加上行政區、選區屬性  -------------------------------------------

library(haven)
library(labelled)
load(paste0(dataset_file_directory,"rdata",slash,"duplicatedarea.RData"))

##以下部分因為已有既存資料檔，讀取後略過不執行#
#找出所有行政區對選區資料，並且找出同一鄉鎮市區有不同選區的部分
#admin_dist_to_elect_dist <- distinct(elections_df, term, admincity, electionarea, admindistrict, adminvillage) %>%
#  filter(!is.na(admincity))# %>%
##  left_join(all_admin_dist_with_zip)
#duplicated_area <- distinct(admin_dist_to_elect_dist,term,electionarea,admincity,admindistrict) %>% #,zip,zip3rocyear
#  extract(duplicated(.[, c("term", "admincity", "admindistrict")]),)
#把某些共用同一個郵遞區號的行政區合併
#unique_dist_for_elect_dist <- anti_join(admin_dist_to_elect_dist, duplicated_area[, c("term", "admincity", "admindistrict")]) %>%
#  group_by(term, electionarea, admincity) %>% #, zip, zip3rocyear
#  summarise(admindistrict = paste0(admindistrict, collapse = "、"))
#以下註解部分為找出多選區的樣本
#duplicated_area[duplicated_area$term == 6, c("zip")] %>%
#  intersect(survey_data[[4]]$zip) %>%
#  unique() %>% 
#  sort()
#save(admin_dist_to_elect_dist,duplicated_area,unique_dist_for_elect_dist,file=paste0(dataset_file_directory,"rdata",slash,"duplicatedarea.RData"))
#以上部分因為已有既存資料檔，讀取後略過不執行#

#重要！2010環境的資料因為補選選區有改變，所以在一些鄉鎮市區村里會重複出現多筆紀錄，要先處理一下join的選舉資料
#duplicated_area_just_one_electionarea <- group_by(duplicated_area, term, admincity, admindistrict, zip, zip3rocyear) %>%
#  summarise(electionarea = paste0(electionarea, collapse = "、"))
minus_electionarea <- as.data.frame(list(
  "term" = 7, 
  "electionarea" = "桃園縣第06選區", 
  "admincity" = "桃園縣", 
  "admindistrict" = "中壢市", 
  zip = 320, 
  zip3rocyear = 99))
survey_data<-paste0(survey_data_title,".sav") %>%
  sapply(function (X,...) paste0(...,X), dataset_file_directory, "merger_survey_dataset",slash) %>%
  lapply(haven::read_sav) %>%
  lapply(mutate_cond,SURVEY=="2010overall" & sm==91, sm=7) %>% #處理訪問時間不一致
  lapply(mutate_cond,SURVEY=="2010overall" & sd==91, sd=15) %>% #處理訪問時間不一致
  lapply(dplyr::mutate,stdsurveydate=as.Date(paste(year,sm,sd),"%Y %m %d")) %>%
  lapply(function(X,survey_imp_measure) {
    labelledcolumns<-purrr::map_lgl(X,haven::is.labelled) %>% which(isTRUE(.)) %>% names()
    spsssavsurvey<-X$SURVEY[1]
    message(spsssavsurvey)
    need_survey_measure_scale<-dplyr::filter(survey_imp_measure,SURVEY==spsssavsurvey,MEASUREMENT=="scale",ID %in% names(X)) %>%
      dplyr::select(ID) %>% unlist() %>% as.character() %>% intersect(labelledcolumns)
    need_survey_measure_ordinal<-filter(survey_imp_measure,SURVEY==spsssavsurvey,MEASUREMENT=="ordinal",ID %in% names(X)) %>%
      dplyr::select(ID) %>% unlist() %>% as.character() %>% intersect(labelledcolumns)
    need_survey_measure_categorical<-setdiff(names(X),need_survey_measure_scale) %>%
      setdiff(need_survey_measure_ordinal) %>% intersect(labelledcolumns)
    X %<>% dplyr::mutate_at(need_survey_measure_scale, as.numeric) %>%
      dplyr::mutate_at(need_survey_measure_ordinal,haven::as_factor,levels='both',ordered=TRUE) %>%
      dplyr::mutate_at(need_survey_measure_categorical,haven::as_factor,levels='both',only_labelled = TRUE)
    return(X)
  },survey_imp_measure=survey_imputation_and_measurement)

#設定遺漏值
missing_value_labels<-lapply(survey_data,function(X) {
  missingvaluepattern<-paste0("\\[",c(92:99,992:999,9992:9999),"\\]",collapse="|")
  labelsofdf<-sapply(X,levels) %>% unlist() %>% unique() %>%
    customgrep(pattern=missingvaluepattern,value=TRUE) %>%
    {extract(.,which(!customgrepl(.,pattern="(不固定|人或以上|到處跑|業|機構|學術|國外|從不聽|新雲林|竹塹|台北勞工|新農|草嶺|濁水溪|蘭潭|飛揚|11個或更多)",perl=TRUE)))}
  #(拒答|遺漏值|忘記|不適用|不知道|跳答|無法選擇|忘記了|拒答）|不知道）|缺漏|不記得)
  return(labelsofdf)
})
survey_data <- mapply(function(X,Y) {
  newdf <- lapply(X, function(dfcolumnvectors,replaced_keys) {
    if (is.factor(dfcolumnvectors)) {
      replace_key_value_pairs<-rep(NA,times=length(replaced_keys))
      names(replace_key_value_pairs)<-replaced_keys
      dfcolumnvectors<-plyr::revalue(dfcolumnvectors, replace_key_value_pairs)
    }
    return(dfcolumnvectors)
  },replaced_keys=Y) %>%
    as.data.frame() %>%
    mutate_if(is.factor,droplevels)
  newdf
  #to_replace_column<-setdiff(colnames(X),c("myown_age","myown_occp","myown_ses"))
  #  dplyr::mutate_at(X,to_replace_column,dplyr::funs(replace(.,. %in% c(93:99,996:999,9996:9999,99996:99999),NA ) )  )
},X=survey_data,Y=missing_value_labels) %>%
  {.[order(names(.))]} %>%
  lapply(dplyr::left_join,{
    read.xlsx(paste0(dataset_file_directory, "basic_social_survey_restricted_data.xlsx"), sheet = 1)
  }) %>%
  lapply(dplyr::mutate_at,c("myown_job","admincity","admindistrict"),.funs=as.factor)#%>%
#lapply(function (X) { #較早的串連方式，區分會期
#  othervar<-setdiff(names(X),c("term1","term2"))
#  reshape2::melt(X,id.vars = othervar, variable.name = "variable_on_term", value.name = "term") %>%
#    dplyr::filter(!is.na(term))
#})  %>%
#save(survey_data,file=paste0(dataset_in_scriptsfile_directory, "all_survey_combined.RData"))
#save(survey_data,file=paste0(dataset_in_scriptsfile_directory, "all_survey_combined_NAuntransformed.RData"))


if ({labeladjusmentagain <- FALSE; labeladjusmentagain}) {
  survey_data_labels <- lapply(survey_data,function(X) {
    sapply(X,FUN=attr,which="levels") %>%
      #sapply(X,FUN=function(X) {
      #  as.numeric(levels(X))[X]
      #  }) %>%
      return()
  })
  save(survey_data_labels,file=paste0(dataset_file_directory,"rdata",slash,"survey_data_labels.RData"))
}
#survey_data_labels已經預處理過，直接load即可
load(paste0(dataset_file_directory,"rdata",slash,"survey_data_labels.RData"))


if ({writingfeather<-FALSE;writingfeather}) {
  forwritingfeather<-mapply(function(X,Y,A,B) {
    #testing purpose
    #df<-as.data.frame(survey_data[[1]])
    #dfcoltypes<-sapply(Y,class)
    #to_dummy_cols<-names(which(dfcoltypes=="factor"))
    #to_dummy_cols_global <<- to_dummy_cols
    #print(to_dummy_cols)
    #for (to_dummy_col in to_dummy_cols) {
    #  print(to_dummy_col)
    #df<-dummies::dummy.data.frame(data=df,names=to_dummy_cols,sep="_")
    #}
    #View(df[68,])
    #grep("v28",names(df))
    #df<-dummies::dummy.data.frame(data=Y,names=to_dummy_cols,sep="_")#%>%
    #dplyr::filter(variable_on_term=="term1") #2004的問卷橫跨立法院多會期，為了節省運算資源所以只保留一期
    path<-paste0(ntuspace_file_directory,"shared",slash,"dataset",slash,"rdata",slash,"all_survey_combined",X,".feather")
    #tomutatecol<-setdiff(names(Y),"myown_age")
    #df %<>% mutate_at(tomutatecol,dplyr::funs(replace(.,. %in% c(93:99,996:999,9996:9999),NA ) ))
    message("-----writing ",path,"--------------------")
    Y<-droplevels(Y)
    needvars<-c("id", union(A,B)  ) %>%
      intersect(names(Y))
    needcols <<- needvars
    feather::write_feather(Y[,needvars], path=path)
  },X=1:4,Y=survey_data,A=imputingcalculatebasiscolumn,B=imputedvaluecolumn)
  #A=imputingcalculatebasiscolumn,B=imputedvaluecolumn
  #A=1:4,B=1:4
  #feather::write_feather(survey_data, path=paste0(dataset_file_directory,"rdata",slash,"all_survey_combined.feather"))
}



#shaped: 299 295 571
#先依據是否有多數選區存在於單一鄉鎮市區拆開，先串有同一鄉鎮市區內有多選區的，再串同一鄉鎮市區內只有一選區的，然後分別join之後再合併
#mapply(function(X,Y) {
##X=survey_data[[1]]; Y=survey_restricted_data[[1]]; Z<-survey_data_labels[[1]] #for testing purpose
#stopifnot(X$SURVEY[1]==Y$SURVEY[1])
#Y %<>% mutate_at(c("village","zip","admincity","admindistrict","adminvillage"), as.factor)
#in_complicated_district<-filter(X, id %in% Y$id) %>%
#  #mutate_at(c("zip","id"), as.character) %>%
#  left_join(Y,by=c("SURVEY","id")) %>% #不用zip join 因為會有label, factor的問題
#  #mutate_at("term", as.character) %>%
#  left_join(admin_dist_to_elect_dist,by=c("admincity","admindistrict","adminvillage")) %>% #by=c("term","admincity","admindistrict","adminvillage"
#  rename(restricted_zip=zip.y) %>%
#  rename(zip=zip.x)
##findduplicatedrowsindf(in_complicated_district,c("id")) %>% View()
#in_simple_district <- filter(X, !(id %in% Y$id)) %>%
#  mutate_at(c("zip"), as.integer) %>%
#  left_join(unique_dist_for_elect_dist)#串連選區和行政區資料
##mutate_at("term", as.character) %>%
#bind_rows(in_simple_district, in_complicated_district) %>%
#  arrange(id) %>%
#  mutate_at(c("zip","id","myown_sex","myown_dad_ethgroup","myown_mom_ethgroup","myown_selfid","myown_int_pol_efficacy","myown_ext_pol_efficacy"), as.factor) %>%
#  mutate_at(setdiff(names(.),c("myown_age")),dplyr::funs(replace(.,. %in% c(93:99,996:999,9996:9999),NA ) ) ) %>%
#  mutate_if(is.factor,funs(droplevels))
#},X=survey_data,Y=survey_restricted_data)

load(paste0(dataset_in_scriptsfile_directory, "all_survey_combined.RData"))
#load(paste0(dataset_in_scriptsfile_directory, "all_survey_combined_NAuntransformed.RData"))


# 第四部份：清理資料：填補遺漏值 -------------------------------------------

#assinging missing value
library(mice)
library(VIM)
library(parallel)
#job_status暫時先刪掉，因為不同問卷概念與選項不一樣難以合併
imputingcalculatebasiscolumn<-lapply(survey_data_title,function(X,df) {
  filter(df,SURVEY==X,IMPUTATION %in% c("basis","both")) %>%
    select(ID) %>% unlist() %>% union(c("admincity")) %>% unname()
},df=survey_imputation_and_measurement) %>%
  setNames(survey_data_title)
imputedvaluecolumn<-lapply(survey_data_title,function(X,df) {
  filter(df,SURVEY==X,IMPUTATION %in% c("both")) %>%
    select(ID) %>% unlist() %>% unname()
},df=survey_imputation_and_measurement) %>%
  setNames(survey_data_title)

generate_predictor_matrix<-function(df,calculationbasisvar=c(),imputedOnlyVars=c()) {
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
}


if ({VIMtestplot<-FALSE;VIMtestplot}) {
  survey_data_test<-survey_data
  #survey_data_test <- lapply(survey_data,function(X) {
  #  dplyr::mutate_at(X,setdiff(colnames(X),c("myown_age","myown_occp","myown_ses")),dplyr::funs(replace(.,. %in% c(93:99,996:999,9996:9999),NA ) )  )
  #  #設定遺漏值
  #})
  survey_data_test <- lapply(survey_data_test,function(X,imputedvaluecolumn,imputingcalculatebasiscolumn) {
    X<-droplevels(X)
    imputingcalculatebasiscolumn_assigned <- extract2(imputingcalculatebasiscolumn,X$SURVEY[1]) %>%
      intersect(names(X))
    imputedvaluecolumn_assigned <- extract2(imputedvaluecolumn,X$SURVEY[1]) %>%
      intersect(names(X))
    needcols<-union(imputingcalculatebasiscolumn_assigned,imputedvaluecolumn_assigned)[1:50]
    #testresult<-fastDummies::dummy_cols(X[,needcols]) %>% dplyr::select_if(is.numeric) %>% 
    #  MissMech::TestMCARNormality()
    testresult<-BaylorEdPsych::LittleMCAR(X[,needcols])
    #testresult<-X[,needcols]
    return(testresult)
  },imputedvaluecolumn=imputedvaluecolumn,imputingcalculatebasiscolumn=imputingcalculatebasiscolumn)
}

survey_data_test <- na_count <- missingvaluepattern <- imputed_survey_data <- list()
#Package ‘MissMech’
#To test whether the missing data mechanism, in a set of incompletely ob-served data, is one of missing completely at random (MCAR).For detailed description see Jamshidian, M. Jalal, S., and Jansen, C. (2014). ``Miss-Mech: An R Package for Testing Homoscedasticity, Multivariate Normality, and Missing Com-pletely at Random (MCAR),'' Journal of Statistical Software,  56(6), 1-31. URL http://www.jstatsoft.org/v56/i06/.

survey_data_test <- custom_parallel_lapply(
  X=survey_data,
  FUN=function(X,imputedvaluecolumn,imputingcalculatebasiscolumn,...) {
    #missingvaluecolumn_assigned<-missingvaluecolumn
    #imputingcalculatebasiscolumn_assigned<-imputingcalculatebasiscolumn
    #X<-survey_data[[i]] %>%
    # droplevels()
    X<-droplevels(X)
    imputingcalculatebasiscolumn_assigned <- extract2(imputingcalculatebasiscolumn,X$SURVEY[1]) %>%
      intersect(names(X))
    imputedvaluecolumn_assigned <- extract2(imputedvaluecolumn,X$SURVEY[1]) %>%
      intersect(names(X))
    foundationvar<-union(imputingcalculatebasiscolumn_assigned,imputedvaluecolumn_assigned)
    ini <- mice(X[,foundationvar], maxit = 0)
    sapply(c("----------------", X$SURVEY[1], "----------------"),print)
    print(table(ini$nmis))
    outlist4 <- as.character(ini$loggedEvents[, "out"])
    print(ini$loggedEvents, 2)
    fx2 <- flux(X[,foundationvar])
    outlist2<-row.names(fx2)[fx2$outflux < 0.45]
    outlist <- unique(c(outlist2, outlist4))
    foundationvar %<>% setdiff(outlist)
    print(paste0(c("foundationvar are ",foundationvar), collapse=" "))
    unusefulcolumns <- setdiff(names(X),foundationvar)
    #proceeding_na_var<-union(imputingcalculatebasiscolumn_assigned,imputedvaluecolumn_assigned) %>%
    #  setdiff(c("myown_age"))
    #predictor_matrix<-generate_predictor_matrix(X,imputingcalculatebasiscolumn_assigned,imputedvaluecolumn)
    predictor_matrix<-mice::quickpred(X[,foundationvar], mincor=0.2)
    #return(predictor_matrix)
    #X %<>% dplyr::mutate_at(proceeding_na_var,dplyr::funs(replace(.,. %in% c(93:99,996:999,9996:9999),NA ) ) ) %>%
    #  mutate_if(is.factor,funs(factor))
    #sol: https://stackoverflow.com/questions/13495041/random-forests-in-r-empty-classes-in-y-and-argument-legth-0
    #sol: https://stackoverflow.com/questions/24239595/error-using-random-forest-mice-package-during-imputation
    #The frequency distribution of the missing cases per variable can be obtained as:
    #survey_data_test[[i]] <- mice::mice(X, maxit = 0)
    #table(survey_data_test[[i]]$nmis)
    #colSums(is.na(X))
    #na_count[[i]] <- sapply(X, function(y) sum(length(which(is.na(y)))))
    #analysisdfonmissingvalue<-X[,imputedvaluecolumn_assigned]
    #missingvaluepattern[[i]]<-mice::md.pattern(analysisdfonmissingvalue,plot=FALSE)
    #visdat::vis_miss(analysisdfonmissingvalue)
    miceMod <- mice::mice(
      X[,foundationvar],
      predictorMatrix = predictor_matrix,
      m=5,
      method="rf"
    )  # perform mice imputation, based on random forests.
    #linear imputation might have error message: system is computationally singular: reciprocal condition number
    #https://stats.stackexchange.com/questions/214267/why-do-i-get-an-error-when-trying-to-impute-missing-data-using-pmm-in-mice-packa
    #print(imputingcalculatebasiscolumn_assigned)
    imputed_survey_data<- mice::complete(miceMod)  # generate the completed data.
    complete_imputed_survey_data<-bind_cols(X[,unusefulcolumns],imputed_survey_data)
    complete_imputed_survey_data<-complete_imputed_survey_data[,names(X)]
    complete_imputed_survey_data
    return(complete_imputed_survey_data)
  },
  imputedvaluecolumn=imputedvaluecolumn,
  imputingcalculatebasiscolumn=imputingcalculatebasiscolumn,
  exportvar=c("survey_data","imputedvaluecolumn","imputingcalculatebasiscolumn"),
  exportlib=c("base",lib,"mice","randomForest"), #,"MissMech","fastDummies"
  outfile=paste0(dataset_file_directory,"rdata",slash,"parallel_handling_process-",t_sessioninfo_running_with_cpu,".txt"),
  mc.set.seed = TRUE,
  mc.cores=parallel::detectCores()
)
#survey_data_test[[3]]<-t_survey_data_test[[1]]
#kNN imputation
#survey_data_test[[i]]<-VIM::kNN(X,variable=imputedvaluecolumn_assigned,k=5,dist_var=imputingcalculatebasiscolumn_assigned)
#}
#conditional random field
save(survey_data_test,file=paste0(dataset_file_directory,"rdata",slash,"miced_survey_7_",t_sessioninfo_running,"df.RData"))
load(file=paste0(dataset_file_directory,"rdata",slash,"miced_survey_7_",t_sessioninfo_running,"df.RData"))

#write.xlsx(as.data.frame(furtherusefulpredictor),file="furtherusefulpredictor.xlsx")
lapply(survey_data_test,View)
View(survey_data$`2010env.sav`[1:20,union(imputedvaluecolumn$`2010env`,imputingcalculatebasiscolumn$`2010env`)])

#checking imputed df
lapply(survey_data_test,function(X,need_particip_var,need_ses_var_assigned,imputedvaluecolumn) {
  survey<-X$SURVEY[1]
  checkdf<-extract(X,dplyr::intersect(names(X),unique(c(
    getElement(need_particip_var,survey),
    need_ses_var_assigned,
    getElement(imputedvaluecolumn,survey)
  )))) %>%
    {colSums(is.na(.))}
  return(checkdf)
},need_particip_var=need_particip_var,need_ses_var_assigned=need_ses_var_assigned,imputedvaluecolumn=imputedvaluecolumn)

if ({oldimputationmethod<-FALSE; oldimputationmethod}) { #此部分屬於舊code，僅保留參考用
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
  
  survey_data_test <- custom_parallel_lapply(
    data=t_survey_data_test,
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
  
  survey_data_test <- lapply(survey_data,function(X,need_ses_var_assigned) {
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
  
  
  #lieing_check,h6r %in% c(1,3), h7 %in% c(1,3,4), h8 %in% c(1,3,4,9)
  
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
    bind_rows(clear_observed_value_green,clear_observed_value_blue)# %>%
  #mutate("party"=h6r)# %>%
  #mutate_at("party", as.factor)# %>%
  #bind_rows(lieing_check_result) %>%
  #bind_rows(lieing_check_missing)
  binded_check_result<-bind_rows(correct_check_result,lieing_check_with_value,lieing_check_missing) %>%
    #anti_join(exclude_result_blue) %>%
    #anti_join(exclude_result_green) %>% %>%
    #mutate_cond(paste0(bluepoints, party) %in% do.call(paste0, exclude_result_blue),party=NA) %>%
    #mutate_cond(paste0(greenpoints, party) %in% do.call(paste0, exclude_result_green),party=NA) %>%
    mutate_at(c("id","zip","h5","h6r","h7","h8","h9","party"), as.factor) %>%
    #,"v84","v86","v88","v93","v94"
    arrange(id,wonelection) %>%
    group_by(id) %>%
    filter(!(duplicated(id)))
  
  
  #group_by(binded_check_result,id) %>%
  #  filter(n()>1) %>%
  #  View()
  #binded_check_result<-binded_check_result[order(testorder),]
  #binded_check_result<-group_by(binded_check_result,id) %>%
  #  filter(!(duplicated(id)))
  #select(X2016_citizen_with_restricted,id) %>%
  #  group_by(id) %>% 
  #  filter(n()>1) %>%
  #  View()
  #select(X2016_citizen_with_restricted,id) %>%
  #group_by(id) %>% 
  #filter(duplicated(id)) %>%
  #View()
  duplicated(X2016_citizen_with_restricted$id)
  length(X2016_citizen_with_restricted$id[duplicated(X2016_citizen_with_restricted$id)])
  
  #identical(filter(zip_to_party_with_jump_answer,zip==103)[3,2],filter(lieing_check_with_value,zip==103)[1,8])
  #標準化z-normalization或min-max scale
}