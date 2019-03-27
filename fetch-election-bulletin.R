#抓取選舉公報
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
source(file = paste(filespath, "shared_functions.R", sep = ""))
#選舉資料
terms<-c(5,6,7,8,9)

gc(verbose=TRUE)
#選舉公報
election_bulletin_starting_url<-setNames(c(
  "http://ebulletin.cec.gov.tw/%E9%81%B8%E8%88%89/%E7%AB%8B%E6%B3%95%E5%A7%94%E5%93%A1/090%E5%B9%B4%E7%AC%AC5%E5%B1%86/index2.php",
  "http://ebulletin.cec.gov.tw/%E9%81%B8%E8%88%89/%E7%AB%8B%E6%B3%95%E5%A7%94%E5%93%A1/093%E5%B9%B4%E7%AC%AC6%E5%B1%86/index2.php",
  "http://ebulletin.cec.gov.tw/%E9%81%B8%E8%88%89/%E7%AB%8B%E6%B3%95%E5%A7%94%E5%93%A1/097%E5%B9%B4%E7%AC%AC7%E5%B1%86/index2.php",
  "http://ebulletin.cec.gov.tw/%E9%81%B8%E8%88%89/%E7%AB%8B%E6%B3%95%E5%A7%94%E5%93%A1/101%E5%B9%B4%E7%AC%AC8%E5%B1%86/index2.php",
  "http://ebulletin.cec.gov.tw/%E9%81%B8%E8%88%89/%E7%AB%8B%E6%B3%95%E5%A7%94%E5%93%A1/105%E5%B9%B4%E7%AC%AC9%E5%B1%86/"
),c("5","6","7","8","9"))
fetch_until_reach_pdf<-function(url) {
  #url<-urldf$link
  content <- sapply(url,custom_read_file)
  path <- unlist(stringi::stri_split(url,regex="/")) %>%
    extract(.,1:length(.)-1) %>%
    paste0(.,"/",collapse="",sep="")
  doc <- read_html(content,encoding="UTF-8")
  xpath<-"//a"
  a_list<-xml_find_all(doc, xpath)
  a_list_text<-xml_text(a_list)
  a_list_link<-xml_attr(a_list,'href')
  if (length(a_list_link)>=1) {
    further_list_link<-sapply(a_list_link,URLencode) %>%
      sapply(url_absolute,base=path) %>%
      paste0("/",sep="") %>%
      stringi::stri_replace(replacement=".pdf",regex=".pdf/") %>%
      setNames(a_list_text)
  } else {
  }
  pdflinks<-further_list_link[customgrepl(further_list_link,'.pdf')]
  notpdflinks<-further_list_link[!customgrepl(further_list_link,'.pdf')]
  if (length(notpdflinks)==0) {#非完全無連結頁面
    message('decision 1 @ ')
    #print(URLdecode(url))
    return(pdflinks)
    message("-----------------")
  } else {
    message('decision 2 @ ')
    #print(URLdecode(url))
    returnv<-unlist(c(pdflinks,sapply(notpdflinks,fetch_until_reach_pdf, USE.NAMES=FALSE)))
    return(returnv)
    message("-----------------")
  }
}
bulletin_links<-sapply(election_bulletin_starting_url,fetch_until_reach_pdf) %>%
  unlist()
new_bulletin_links<-data.frame(
  'electionarea'=names(bulletin_links) %>% sapply(function(X) {
    last(unlist(strsplit(X,split="\\.")))
  }),
  'link'=bulletin_links,
  'term'=sapply(names(bulletin_links),stringi::stri_sub,from=1,length=1),
  stringsAsFactors = FALSE)
