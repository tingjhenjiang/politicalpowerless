# 第Ｏ部份：環境設定 --------------------------------
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
source(file = "shared_functions.R")

gc(verbose=TRUE)

t_sessioninfo_running_with_cpu_locale<-sessionInfo()$locale %>% stringi::stri_split(regex=";") %>% unlist() %>% getElement(1) %>% stringi::stri_split(regex="=") %>% unlist() %>% getElement(2) %>%
  paste0(t_sessioninfo_running_with_cpu, .) %>% gsub(pattern=" ",replacement = "", x=.)
# 第O部份：clustering  ================================= 
load(paste0(dataset_in_scriptsfile_directory,"miced_survey_9_Ubuntu18.04.3LTSdf_with_mirt.RData"), verbose=TRUE)


#clustering
#clustrd
#https://cran.r-project.org/web/packages/clustrd/clustrd.pdf
#http://www.amarkos.gr/clustrd/
#https://www.jamleecute.com/hierarchical-clustering-%E9%9A%8E%E5%B1%A4%E5%BC%8F%E5%88%86%E7%BE%A4/