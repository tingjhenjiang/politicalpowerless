# 第Ｏ部份：環境設定 --------------------------------
if (!("benchmarkme" %in% rownames(installed.packages()))) try(install.packages("benchmarkme"))
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-try(paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model))
t_sessioninfo_running_with_cpu_locale<-try(gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2])))
source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/shared_functions.R"), encoding="UTF-8")
#選舉資料
term_to_survey <- data.frame("term"=c(5,6,7,7,8,8,9), "SURVEY"=c("2004citizen","2004citizen","2010env","2010overall","2010env","2010overall","2016citizen"))
gc(verbose=TRUE)

survey_imputation_and_measurement<-custom_ret_survey_imputation_and_measurement(paths_to_survey_imputation_and_measurement_file)
survey_codebook<-openxlsx::read.xlsx(paste0(dataset_file_directory,"all_survey_questions_englished.xlsx"),sheet = 4)


# 測試survey套件 ---------------------------------

#check srvyr
#checklavaan.survey package
stratified_design_2016citizen <- survey::svydesign(data=survey_data_test[[4]], id=~admindistrict+adminvillage, weights= ~myown_wr, strata=~r_stratum2014, nest=TRUE, fpc=NULL)
stratified_design_2016citizen <- filter(complete_survey_dataset,SURVEY=="2016citizen") %$%
  survey::svydesign(id=~admindistrict+adminvillage, weights= ~myown_wr, strata=~r_stratum2014, nest=TRUE, fpc=NULL)
summary(stratified_design_2016citizen)

# 測試propensity score ---------------------------------