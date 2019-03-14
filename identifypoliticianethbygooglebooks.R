t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
filespath<-switch(
  paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model),
  "Windows8x64build9200Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="E:\\Software\\scripts\\R\\",
  "Windows10x64build17763Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="E:\\Software\\scripts\\R\\",
  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="/mnt/e/Software/scripts/R/",
  "Ubuntu18.04.2LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="/mnt/e/Software/scripts/R/",
  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"="/home/j/rscripts/",
  "Ubuntu18.04.2LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"="/home/j/rscripts/",
  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\",
  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2660 v4 @ 2.00GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\"
)
source(file = paste(filespath, "shared_functions.R", sep = ""))

googlebooksearchapiurlprefix<-'https://www.googleapis.com/books/v1/volumes?q=intitle:%E5%8F%B0%E7%81%A3%E5%AE%A2%E5%AE%B6%E6%94%BF%E6%B2%BB%E9%A2%A8%E9%9B%B2%E9%8C%84+inauthor:%E4%BD%95%E4%BE%86%E7%BE%8E'
googlebooksearchapiurlprefix_hakka<-'https://www.googleapis.com/books/v1/volumes?q=intitle:台灣客家政治風雲錄+inauthor:何來美+'
googlebooksearchapiurlprefix_foreignstates<-'https://www.googleapis.com/books/v1/volumes?q=intitle:外省新頭殼+inauthor:管仁健+'
all_legis_list<-paste0(dataset_file_directory,"legislator_additional_attributes.xlsx") %>%
  openxlsx::read.xlsx(sheet=1) %>%
  filter(!(legislator_ethnicity %in% c('原住民'))) %>%
  getElement('name') %>%
  trimws()
library(parallel)

testlength<-seq(from=1,to=length(all_legis_list))
fetchgooglebookall_legis_listdata_foreignstates <- paste(googlebooksearchapiurlprefix_foreignstates,all_legis_list,sep="")[testlength] %>%
  custom_parallel_lapply(FUN=function (X,...) {return(custom_read_file(X))}) %>%
  setNames(all_legis_list[testlength])
save(fetchgooglebookall_legis_listdata_foreignstates,file=paste0(filespath, "vote_record", slash, "fetchgooglebookall_legis_listdata.RData"))
load(file=paste0(filespath, "vote_record", slash, "fetchgooglebookall_legis_listdata_foreignstates.RData"))
matchresult<- custom_parallel_lapply(fetchgooglebookall_legis_listdata,FUN=function (X,...) {return(jsonlite::fromJSON(X))}) %>%
  custom_parallel_lapply(FUN=function (X,...) {
    switch(as.character('items' %in% names(X)),"TRUE"=X$items$searchInfo,"FALSE"=NA) %>% return()}
  )
View(matchresult)
fetchgooglebookdata[[2]]$items$searchInfo %>% names()

library("RSelenium")
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "firefox")
remDr$open()
remDr$navigate("https://www.google.com.tw/")
