# 第Ｏ部份：環境設定 --------------------------------
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
source(file = "shared_functions.R")
#選舉資料
term_to_survey <- data.frame("term"=c(5,6,7,7,8,8,9), "SURVEY"=c("2004citizen","2004citizen","2010env","2010overall","2010env","2010overall","2016citizen"))
gc(verbose=TRUE)

survey_imputation_and_measurement<-openxlsx::read.xlsx(path_to_survey_imputation_and_measurement_file,sheet = 1)
survey_codebook<-openxlsx::read.xlsx(paste0(dataset_file_directory,"all_survey_questions_englished.xlsx"),sheet = 4)


# 測試survey套件 ---------------------------------

#check srvyr
#checklavaan.survey package
stratified_design_2016citizen <- survey::svydesign(data=survey_data_test[[4]], id=~admindistrict+adminvillage, weights= ~myown_wr, strata=~r_stratum2014, nest=TRUE, fpc=NULL)
stratified_design_2016citizen <- filter(complete_survey_dataset,SURVEY=="2016citizen") %$%
  survey::svydesign(id=~admindistrict+adminvillage, weights= ~myown_wr, strata=~r_stratum2014, nest=TRUE, fpc=NULL)
summary(stratified_design_2016citizen)

# 測試propensity score ---------------------------------