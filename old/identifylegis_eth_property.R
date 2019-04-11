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
  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2660 v4 @ 2.00GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\",
  "Windows8x64build9200Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\Downloads\\"
)
source(file = paste(filespath, "vote_record", slash, "shared_functions.R", sep = ""))

all_legis_list<-paste0(dataset_file_directory,"legislator_additional_attributes.xlsx") %>%
  openxlsx::read.xlsx(sheet=1) %>%
  filter(!(legislator_ethnicity %in% c('原住民'))) %>%
  getElement('name') %>%
  trimws()
testlength<-seq(from=1,to=length(all_legis_list))
testlength<-seq(from=13,to=length(all_legis_list))
testlength<-1:5

#Crawling with common skills
googlebooksearchapiurlprefix <-'https://www.googleapis.com/books/v1/volumes?q=intitle:%E5%8F%B0%E7%81%A3%E5%AE%A2%E5%AE%B6%E6%94%BF%E6%B2%BB%E9%A2%A8%E9%9B%B2%E9%8C%84+inauthor:%E4%BD%95%E4%BE%86%E7%BE%8E'
googlebooksearchapiurlprefix_hakka <-'https://www.googleapis.com/books/v1/volumes?q=intitle:台灣客家政治風雲錄+inauthor:何來美+'
googlebooksearchapiurlprefix_foreignstates <-'https://www.googleapis.com/books/v1/volumes?q=intitle:外省新頭殼+inauthor:管仁健+'
library(parallel)

fetchgooglebookall_legis_listdata_foreignstates <- paste(googlebooksearchapiurlprefix_foreignstates,all_legis_list,sep="")[testlength] %>%
  custom_parallel_lapply(FUN=function (X,...) {return(custom_read_file(X))},exportvar=c("custom_read_file","customgrepl"),exportlib=lib) %>%
  setNames(all_legis_list[testlength])
save(fetchgooglebookall_legis_listdata_foreignstates,file=paste0(dataset_file_directory, slash, "fetchgooglebookall_legis_listdata.RData"))
load(file=paste0(dataset_file_directory, "rdata", slash, "fetchgooglebookall_legis_listdata.RData"))
matchresult<- custom_parallel_lapply(fetchgooglebookall_legis_listdata_foreignstates,FUN=function (X,...) {return(jsonlite::fromJSON(X))},exportvar=c("fetchgooglebookall_legis_listdata_foreignstates"),exportlib=lib) %>%
  custom_parallel_lapply(FUN=function (X,...) {
    switch(as.character('items' %in% names(X)),"TRUE"=X$items$searchInfo,"FALSE"=NA) %>% return()}
  )
View(matchresult)
fetchgooglebookdata[[2]]$items$searchInfo %>% names()


#### Using RSelenium
library(RSelenium)
library(foreach)
library(doParallel)
#cl = makeCluster(parallel::detectCores())
registerDoParallel(cores=parallel::detectCores())
googlebooksearchpageprefix_foreignstates <-'https://books.google.com.tw/books?id=u44pDwAAQBAJ&printsec=frontcover&dq=intitle:%E5%A4%96%E7%9C%81%E6%96%B0%E9%A0%AD%E6%AE%BC+inauthor:%E7%AE%A1%E4%BB%81%E5%81%A5&hl=zh-TW&sa=X&ved=0ahUKEwjgxpqwqoThAhVMHKYKHRs1CA0Q6AEIJzAA#v=onepage&f=false&q='

fprof <- makeFirefoxProfile(list(browser.download.dir = "/mnt/r"))
remDrs <- list(remoteDriver(
    remoteServerAddr = "localhost",
    port = 4444,
    browserName = "firefox",
    extraCapabilities = fprof
  ),remoteDriver(
    remoteServerAddr = "localhost",
    port = 4445,
    browserName = "chrome"
  ))

tempresultresult<-foreach(i=1:2) %dopar% {
  #remDr<-remDrs[[i]]
  remDrs[[i]]$open()
  remDrs[[i]]$navigate(googlebooksearchpageprefix_foreignstates)
  Sys.sleep(1)
  childitemspos<-seq(from=i,to=length(testlength),by=2)
  returnvalue<-lapply(all_legis_list[childitemspos], function(legis_name,urlprefix) {
    navigateurl<-paste(urlprefix,legis_name,sep="")
    remDrs[[i]]$navigate(navigateurl)
    Sys.sleep(1)
    #webElem_searchresulthint <- remDr$findElement(using = 'xpath', "//*[@id='search_bar']")
    webElem_searchresulthint <- remDrs[[i]]$findElement(using = 'id', value = "search_bar")
    if (customgrepl(webElem_searchresulthint$getElementText(),"顯示此書中關於")) {
      furthersearchresult_pages<-remDrs[[i]]$findElements(using = 'class name', "sitb_result") #sitb_result
      unlist(lapply(furthersearchresult_pages,function(X) {X$getElementText()}))
    } else {
      NA
    }
  },urlprefix=googlebooksearchpageprefix_foreignstates)
  remDrs[[i]]$close()
  #remDrs[[i]]$closeServer()
  return(returnvalue)
}
searchresult<-list()
newsultlen<-length(tempresultresult[[1]])+length(tempresultresult[[2]])
for (i in 1:2) {
  childitemspos<-seq(from=i,to=newsultlen,by=2)
  searchresult[childitemspos]<-tempresultresult[[i]]
}
searchresult<-setNames(searchresult,all_legis_list[testlength])

existedlegis<-legislators_ethicity %>%
  stri_split(regex='\\|') %>% unlist() %>% paste0(collapse=" ")
#step by step search
i<-1
for (legisname in all_legis_list[testlength]) {
  #remDrs[[i]]$open()
  if (customgrepl(existedlegis,legisname)) {
    next
  }
  message(legisname)
  googlebooksearchurl_fs<-paste0('https://www.google.com.tw/search?hl=zh-TW&tbo=p&tbm=bks&num=100&q="外省"+"', legisname, '"',sep='')
  googlebooksearchurl_hakka<-paste0('https://www.google.com.tw/search?hl=zh-TW&tbo=p&tbm=bks&num=100&q="客家"+"', legisname, '"',sep='')
  googlebooksearchurl_holo<-paste0('https://www.google.com.tw/search?hl=zh-TW&tbo=p&tbm=bks&num=100&q="閩南"+"', legisname, '"',sep='')
  wikipage<-paste0('https://zh.wikipedia.org/wiki/', legisname, sep='')
  mothersearch<-paste0('https://www.google.com.tw/search?dcr=0&ei=tNSMXP3QKNjmwQOxtrqwDw&q=母親+立法委員+', legisname, sep='')
  fathersearch<-paste0('https://www.google.com.tw/search?dcr=0&ei=tNSMXP3QKNjmwQOxtrqwDw&q=父親+立法委員+', legisname, sep='')
  for (navigateurl in c(googlebooksearchurl_fs,googlebooksearchurl_hakka,googlebooksearchurl_holo,wikipage,mothersearch,fathersearch)) {
    remDrs[[i]]$navigate(navigateurl)
    readline(prompt="Press [enter] to continue")
  }
  #webElem <- remDrs[[i]]$findElement("id", "resultStats")
  #remDrs[[i]]$mouseMoveToLocation(webElement = webElem) # move to the required element
  #remDrs[[i]]$click(0) # right mouse button click
  #webElem$sendKeysToElement(list(key = "control", "t"))
  readline(prompt="Press [enter] to continue")
  #remDrs[[i]]$close()
}



remDrs[[1]]$close()
remDrs[[1]]$closeServer()
save(searchresultinguan,file=paste0(dataset_file_directory, slash, "searchresultinguan.RData"))

#公務人員財產申報

library(curlconverter)
library(jsonlite)
library(httr)
LPForm='https://priq-out.cy.gov.tw/GipExtendWeb/wSite/SpecialPublication/baseList.jsp'
LPForm='https://priq-out.cy.gov.tw/GipExtendWeb/wSite/SpecialPublication/SpecificLP.jsp?ctNode='
#=name&="%"E9"%"A6"%"AC"%"E8"%"8B"%"B1"%"E4"%"B9"%"9D"
myHttpheader<- c(
  "Host"="xxx",
  "User-Agent"="xxx",
  "Accept"="xxx",
  "Accept-Language"="xxx",
  "Accept-Encoding"="xxx",
  "Referer"="xxx",
  "Cookie"="xxx",
  "Connection"="xxx"
)
# current verison
packageVersion("curlconverter")
httpbinrhcurl <- "curl 'https://httpbin.org/headers' -H 'pragma: no-cache' -H 'accept-encoding: gzip, deflate, sdch' -H 'accept-language: en-US,en;q=0.8' -H 'upgrade-insecure-requests: 1' -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/49.0.2623.39 Safari/537.36' -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8' -H 'cache-control: no-cache' -H 'referer: https://httpbin.org/' --compressed"
httpbinrhcurl <- paste0(
  '"https://priq-out.cy.gov.tw/GipExtendWeb/wSite/SpecialPublication/SpecificLP.jsp?ctNode=" -H "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:65.0) Gecko/20100101 Firefox/65.0" -H "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8" -H "Accept-Language: zh-TW,en-US;q=0.7,en;q=0.3" --compressed -H "Referer: https://priq-out.cy.gov.tw/GipExtendWeb/wSite/SpecialPublication/SpecificLP.jsp?ctNode=" -H "Content-Type: application/x-www-form-urlencoded" -H "DNT: 1" -H "Connection: keep-alive" -H "" -H "Upgrade-Insecure-Requests: 1" --data "queryCol=name&queryStr=',
  stri_replace_all(curl::curl_escape(legisname),replacement='"%"',regex='%'),
  #'"%"E6"%"B1"%"9F',
  '"',sep="",collapse=""
  )
straight <- curlconverter::straighten(httpbinrhcurl)
res <- curlconverter::make_req(straight,add_clip=FALSE)
webpagedata<-httr::content(res[[1]]())
xpath<-"//body"
xml_find_all(webpagedata, xpath) %>%
  xml_text()
jsonlite::toJSON(res, pretty=TRUE)
#httr::GET
httr::POST
