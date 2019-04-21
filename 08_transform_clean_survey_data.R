# 第Ｏ部份：環境設定 --------------------------------
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
source(file = "shared_functions.R")
#選舉資料
term_to_survey <- data.frame("term"=c(5,6,7,7,8,8,9), "SURVEY"=c("2004citizen","2004citizen","2010env","2010overall","2010env","2010overall","2016citizen"))
gc(verbose=TRUE)

survey_imputation_and_measurement<-openxlsx::read.xlsx(path_to_survey_imputation_and_measurement_file,sheet = 1)
survey_codebook<-openxlsx::read.xlsx(paste0(dataset_file_directory,"all_survey_questions_englished.xlsx"),sheet = 4)






# 第七部份：把問卷資料變形以便串連及行政區、選舉資料 ---------------------------------
load(paste0(filespath,"data",slash,"survey_data_test.RData"))
#library(reshape2)

#survey_oldq_id<-list(
#  "2004citizen"=c("v25","v26","v27","v41","v42","v43","v44","v45","v46","v60","v61","v62","v65","v74","v91a","v91b","v92_1","v92_2","v92_3","v92_4","v92_5","v93a","v93b","v95","v96","v97","v105a","v105b","v105c","v106a","v106b","v106c","v107a","v107b","v107c","v114","v118a","v118b","v118c","v118d"),
#  "2010env"=c("v39a", "v39b", "v39c", "v40", "v78a", "v78b", "v78c", "v78d", "v78e", "v78f", "v78g", "v78h", "v78i", "v90", "v91", "v92"),
#  "2010overall"=c("kv21c_0", "kv31_0", "kv67_0", "v14a", "v14b", "v15a", "v15b", "v16a", "v16b", "v19", "v20a", "v20b", "v21c", "v22a", "v22b", "v22c", "v23a", "v23b", "v23c", "v24a", "v24b", "v24c", "v25a", "v25b", "v25c", "v26a", "v26b", "v26c", "v26d", "v26e", "v26f", "v26g", "v27a", "v27b", "v27c", "v27d", "v27e", "v27f", "v27g", "v28a", "v28b", "v29", "v30a", "v30b", "v31", "v32a", "v32b", "v32c", "v36a", "v36b", "v37a", "v37b", "v37c", "v37d", "v37e", "v37f", "v37g", "v37h", "v37i", "v38a1", "v38a2", "v38b1", "v38b2", "v38c1", "v38c2", "v38d1", "v38d2", "v38e1", "v38e2", "v39a", "v39b", "v39c", "v40", "v57", "v58", "v59", "v63", "v66c", "v66f", "v67", "v68", "v69", "v70b", "v70c", "v70d", "v70e", "v70f"),
#  "2016citizen"=c("c1a",	"c1b",	"c1c",	"c1d",	"c1e",	"c2",	"c3",	"c4",	"c5",	"c6",	"c10",	"c11",	"c12",	"c13",	"c14",	"d1",	"d2a",	"d2b",	"d3a",	"d3b",	"d4",	"d5a",	"d5b",	"d5c",	"d5d",	"d5e",	"d5f",	"d6a",	"d6b",	"d6c",	"d6d",	"d6e",	"d6f",	"d6g",	"d6h",	"d7a",	"d7b",	"d7c",	"d7d",	"d7e",	"d7f",	"d7g",	"d7h",	"d7i",	"d7j",	"d7k",	"d8a",	"d8b",	"d8c",	"d11a",	"d11b",	"d12",	"d13a",	"d13b",	"d14a",	"d14b",	"d14c",	"d17a",	"d17b",	"d17c",	"e2a",	"e2b",	"e2c",	"e2d",	"e2e",	"e2f",	"e2g",	"e2h",	"e2i",	"f3",	"f4",	"f5",	"f8",	"f9",	"h10")
#)
survey_q_id<-sapply(survey_data_title,function(X,df,oldvec=c()) {
  topickeyword<-c("議題","議題（或民主價值與公民意識牽涉群體）","民主價值與公民意識")
  if(identical(oldvec,c())) {
    oldvec[[X]]=c()
  }
  needq<-dplyr::filter(df,SURVEY==X,CATEGORY %in% topickeyword) %>%
    dplyr::select(ID) %>%
    unlist() %>%
    as.character() %>%
    union(oldvec[[X]])
  return(needq)
},df=survey_imputation_and_measurement)

#有些資料在轉換過程中內容會變成label而非coding的資料，要把他變回來

if({covert_label_according_to_xls_codebook<-FALSE;covert_label_according_to_xls_codebook}) {
  mistakinglevelvars<-list(
    "2004citizen"=c('myown_indp_atti','v61','v62','v74','v91a','v91b','v93a','v93b','v95'),
    "2010env"=c('v14a','v14b','v16a','v16b','v20a','v20b','v28a','v28b','v31','v40','v57','v58','v59'),
    "2010overall"=c('myown_indp_atti','v61','v62','v80','v89'),
    "2016citizen"=c('myown_indp_atti','c1a','c1b','c1c','c1d','c1e','c2','c3','c6','c8','c8r','c9','c9r','c11','c14','d1','d4','d8a','d8b','d8c','d9a','d9b','d10','h10','h10r')
  )
  #以下部分是從先前問卷舊標籤factor方式而來，如果要使用還需要重新修改
  prepare_for_label_adj_df<-prepare_for_label_adj_df
  mistakinglevelvars<-mistakinglevelvars
  dfcodebook<-survey_codebook
  prepare_for_label_adj_df <- lapply(mistakinglevelvars,function(mistakinglevelvar,...) {
    dfcodebook<-dfcodebook[
      (dfcodebook$SURVEY==survey_data_title) & (dfcodebook$ID == mistakinglevelvar),
      c('SURVEY','ID','VALUE','LABEL')]#ID %in% Y
    dedf_keyvalues<-as.list(getElement(dfcodebook,'VALUE'))
    names(dedf_keyvalues)<-getElement(dfcodebook,'LABEL')
    #result<-filter(dfcodebook,SURVEY==X,ID %in% Y)
    dedf_keyvalues
  },MoreArgs=list(survey_data_title=survey_data_title,dfcodebook=dfcodebook),
  SIMPLIFY=FALSE)
  names(prepare_for_label_adj_df)<-mistakinglevelvars
  for (recodevar in mistakinglevelvars) {
    tplistforrecode <- getElement(prepare_for_label_adj_df,recodevar)
    X[[recodevar]] <- dplyr::recode(getElement(X,recodevar),!!!tplistforrecode)
    #message("recodevar is ", recodevar," and length is ",length(X$recodevar)," and list is ",tplistforrecode," and names of list is",names(tplistforrecode))
  }
  #X
}

#survey_data_melted
complete_survey_dataset <- mapply(function(X,Y) {
  survey_data_title<-X$SURVEY[1]
  other_var_as_id<-setdiff(names(X),Y)
  X<-mutate_at(X,Y,as.character)
  reshape2::melt(X, id.vars = other_var_as_id, variable.name = "SURVEYQUESTIONID", value.name = "SURVEYANSWERVALUE") %>%
    dplyr::mutate_at("SURVEYANSWERVALUE", as.character)
},X=survey_data_test,Y=survey_q_id) %>%
  {#節省欄位合併
    common_var<-Reduce(intersect, lapply(., names )) %>%
      setdiff(c("sd"))
    lapply(., select_and_fill_nonexistcol, common_var)
  } %>%
  plyr::rbind.fill() %>%
  dplyr::rename(ansv_and_label=SURVEYANSWERVALUE) %>%
  dplyr::mutate("value_on_q_variable"=paste0(SURVEY,"@",SURVEYQUESTIONID)) %>%
  dplyr::select(-zip,-wave,-qtype,-myown_industry,-myown_job,-villagefullname,-myown_family_income_ingroup, -SURVEYQUESTIONID) %>%
  dplyr::select(-term1,-term2,-year,-year_m,-sm,-myown_int_pol_efficacy,-myown_ext_pol_efficacy,-myown_constituency_party_vote)#,-sd
#View(filter(complete_survey_dataset[[1]],SURVEYQUESTIONID=='myown_indp_atti'))
#dplyr::recode(survey_data_test[[1]]$v61,!!!getElement(getElement(prepare_for_label_adj_df,"2004citizen"),"v61"))
#vhead(mergedf_votes_bills_surveyanswer)
#vhead(complete_survey_dataset)
#以下是要把四份問卷合一，但這應該要放棄



#survey_data_melted 沒有節省欄位直接合併
#complete_survey_dataset<-Reduce(plyr::rbind.fill,complete_survey_dataset) %>%
#  extract(common_var)
#vhead(complete_survey_dataset)
#survey_data_melted_names<-lapply(survey_data_melted,names)


#factor to numeric method
#survey_data_melted<-lapply(survey_data_melted,function(X) {
#  X<-mutate_at(c(),as.numeric(levels(f))[f]

#%>%
#reshape2::melt(id.vars = setdiff(colnames(.),c("term1","term2")), variable.name = "variable_on_term", value.name = "term")
#vhead(complete_survey_dataset %>% filter(SURVEYQUESTIONID=='myown_indp_atti'))
#withoutlabelansv <- unique(complete_survey_dataset$ansv_and_label)[c(31,104:116,135:144,167:173,180:188,209:222,285:287,293:299)]
#filter(complete_survey_dataset, ansv_and_label %in% withoutlabelansv) %>%
#  distinct(SURVEY,SURVEYQUESTIONID,ansv_and_label) %>%
#  View()
#c(NA,"","以上皆非等待發明  不知道何種替代能源","用垃圾科技轉換能源",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)

#save(complete_survey_dataset,file=paste0(filespath, "data", slash, "complete_survey_dataset.RData"))

##針對調查問卷資料處理變形，以便合併

#"c1a","c1b","c1c","c1d","c1e","c2","c3","c4","c5","c6","c10","c11","c12","c13","c14","d1","d2a","d2b","d3a","d3b","d4","d5a","d5b","d5c","d5d","d5e","d5f","d6a","d6b","d6c","d6d","d6e","d6f","d6g","d6h","d7a","d7b","d7c","d7d","d7e","d7f","d7g","d7h","d7i","d7j","d7k","d8a","d8b","d8c","d11a","d11b","d12","d13a","d13b","d14a","d14b","d14c","d17a","d17b","d17c","e2a","e2b","e2c","e2d","e2e","e2f","e2g","e2h","e2i","f3","f4","f5","f8","f9","h10","kh10"
