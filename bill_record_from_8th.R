source(file="//tsclient//R//shared_functions.R")
source(file="E:\\Software\\scripts\\R\\shared_functions.R")
bill_reso_url<-c(
  "http://data.ly.gov.tw:80/odw/openDatasetJson.action?id=24&selectTerm=all&page=1",
  "http://data.ly.gov.tw:80/odw/openDatasetJson.action?id=24&selectTerm=all&page=2",
  "http://data.ly.gov.tw:80/odw/openDatasetJson.action?id=24&selectTerm=all&page=3",
  "http://data.ly.gov.tw:80/odw/openDatasetJson.action?id=24&selectTerm=all&page=4",
  "http://data.ly.gov.tw:80/odw/openDatasetJson.action?id=24&selectTerm=all&page=5"
)
bill_url<-c(
  "http://data.ly.gov.tw:80/odw/openDatasetJson.action?id=4&selectTerm=all&page=1",
  "http://data.ly.gov.tw:80/odw/openDatasetJson.action?id=4&selectTerm=all&page=2",
  "http://data.ly.gov.tw:80/odw/openDatasetJson.action?id=4&selectTerm=all&page=3",
  "http://data.ly.gov.tw:80/odw/openDatasetJson.action?id=4&selectTerm=all&page=4",
  "http://data.ly.gov.tw:80/odw/openDatasetJson.action?id=4&selectTerm=all&page=5"
)

bill_reso_tmpjson<-customcurl(bill_reso_url)
bill_tmpjson<-customcurl(bill_url)
tempdf_reso_source <- sapply(bill_reso_tmpjson, fromJSON)
tempdf_bill_source <- sapply(bill_tmpjson, fromJSON)
tempdf_reso_target <- data.frame()
tempdf_bill_target <- data.frame()

i<-1
for (i in (1:length(tempdf_reso_source)) ) {
  tempdf_reso_target <- rbind(tempdf_reso_target,tempdf_reso_source[[i]])
  tempdf_bill_target <- rbind(tempdf_bill_target,tempdf_bill_source[[i]])
}

tempdf<-left_join(tempdf_bill_target, tempdf_reso_target, by = NULL)
