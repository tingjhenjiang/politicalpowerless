# 第Ｏ部份：環境設定 --------------------------------
if (!("benchmarkme" %in% rownames(installed.packages()))) try(install.packages("benchmarkme"))
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-try(paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model))
t_sessioninfo_running_with_cpu_locale<-try(gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2])))
source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/shared_functions.R"), encoding="UTF-8")
gc(verbose=TRUE)

survey_imputation_and_measurement<-openxlsx::read.xlsx(path_to_survey_imputation_and_measurement_file,sheet = 1)






# 第三部份：把問卷檔加上行政區、選區屬性  -------------------------------------------

# library(haven)
# library(labelled)

#以下不處理的時候會直接跳到讀取rdata
if ({processing_duplicated_area<-FALSE; processing_duplicated_area}) {
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
} else {
  load(paste0(dataset_file_directory,"rdata",slash,"duplicatedarea.RData"), verbose=TRUE)
}

#重要！2010環境的資料因為補選選區有改變，所以在一些鄉鎮市區村里會重複出現多筆紀錄，要先處理一下join的選舉資料
#duplicated_area_just_one_electionarea <- dplyr::group_by(duplicated_area, term, admincity, admindistrict, zip, zip3rocyear) %>%
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
      dplyr::mutate_at(need_survey_measure_categorical,haven::as_factor,levels='both')#,only_labelled = TRUE
    return(X)
  },survey_imp_measure=survey_imputation_and_measurement) %>%
  magrittr::set_names(survey_data_title)

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

save(survey_data,file=paste0(dataset_in_scriptsfile_directory, "all_survey_combined_after_settingNA.RData"))


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
#load(paste0(dataset_file_directory,"rdata",slash,"survey_data_labels.RData"))


if (exists("writingfeather")) {
  if (writingfeather==TRUE) {
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
    write(x=jsonlite::toJSON(imputingcalculatebasiscolumn), file=paste0(ntuspace_file_directory,"shared",slash,"dataset",slash,"rdata",slash,"imputingcalculatebasiscolumn.json"))
    write(x=jsonlite::toJSON(imputedvaluecolumn), file=paste0(ntuspace_file_directory,"shared",slash,"dataset",slash,"rdata",slash,"imputedvaluecolumn.json"))
    #A=imputingcalculatebasiscolumn,B=imputedvaluecolumn
    #A=1:4,B=1:4
    #feather::write_feather(survey_data, path=paste0(dataset_file_directory,"rdata",slash,"all_survey_combined.feather"))
  }
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

