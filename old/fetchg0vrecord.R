source(file="E:\\Software\\scripts\\R\\shared_functions.R")
overallvotedf<-data.frame(
  "votecontent"=character(),
  "votedecision"=character(),
  "legislator_name"=character(),
  "legislator_party"=character(),
  "legislator_id_g0v"=character(),
  "sameposinsameparty"=integer(),
  "sameposacrossparty"=integer(),
  "billuid"=character(),
  "billterm"=integer(),
  "billperiod"=integer(),
  "billresult"=character(),
  "billconflict"=logical(),
  "concernfield"=character(),
  "positionmeter"=integer()
)
GetVoteContent<-function(vote_seq=9, text) {
  # 傳入的text為"附後"該行以上的所有該次會議內容
  #lines = [line.strip() for line in text.split('\n') if line]
  lines<-(str_split(text, "\n"))[[1]]
  linelength<-length(lines)
  #message(linelength)
  #grep("附後\\S[\\d]+\\S",lines[linelength-1],perl=TRUE)
  #if re.search(u'附後\S[\d]+\S', lines[-2]) or re.search(u'(其他事項|討論事項)[：:]?$', lines[-2]):
  #  return lines[-1]
  return(lines[linelength])
  #votediscussresult_simp
  #tmprawlyvotetxt[votediscussresult_simp]
}

json_legislator_url<- 1:179 %>%
  sapply(custompaste0,c("https://vote.ly.g0v.tw/api/legislator/?format=json&page="),reverse=TRUE)
#json_legislator_content<-customcurl(json_legislator_url[1:2],async=1)
json_vote_url<- 1:130 %>%
  sapply(custompaste0,c("https://vote.ly.g0v.tw/api/vote/?format=json&page="),reverse=TRUE)
num_of_votejson_page=179
#json_vote_text<-sapply(json_vote_url,read_file)
for (num_of_votejson_page in 1:130) {
  message("page is ",num_of_votejson_page,"-----\n")
  json_vote_text<-read_file(json_vote_url[num_of_votejson_page])
  rawdf_vote <- fromJSON(json_vote_text)
  Sys.sleep(1)
  
  #個別議案內容
  rawvote_bills<-rawdf_vote[['results']]
  num_of_bills<-1
  for (num_of_bills in 1:length(rawvote_bills[,"results"])) {
    rawvote_content<-rawvote_bills[num_of_bills,'content']
    rawvote_uid<-rawvote_bills[num_of_bills,'uid']
    rawvote_term<-substr(rawvote_uid,1,2)
    rawvote_period<-substr(rawvote_uid,4,5)
    rawvote_conflict<-rawvote_bills[num_of_bills,'conflict']
    rawvote_passed<-rawvote_bills[num_of_bills,'result']
    #個別議案結果，下分各項細目
    rawvote_result<-rawvote_bills[num_of_bills,'results']
    i<-1
    for (i in 1:length(rawvote_content)) {
      decision_all_types<-rawvote_result[[i]][,"decision"]
      tmp_decisionlist<-rawvote_result[[i]][,"party_list"]
      typeofdecision<-1
      for (typeofdecision in 1:length(decision_all_types)) {
        eachdecisiondf<-rawvote_result[[i]][,"party_list"][[typeofdecision]]
        parties<-eachdecisiondf[,"party"]
        num_of_parties<-1
        legislatorslist<-eachdecisiondf[,"legislators"]
        for (num_of_parties in 1:length(legislatorslist)) {
          testdf<-cbind(
            "votecontent"=rawvote_content[i],
            "votedecision"=decision_all_types[typeofdecision],
            "legislator_name"=eachdecisiondf[,"legislators"][[num_of_parties]][,"name"],
            "legislator_party"=parties[num_of_parties],
            "legislator_id_g0v"=eachdecisiondf[,"legislators"][[num_of_parties]][,"legislator_id"],
            "sameposinsameparty"=eachdecisiondf[,"count"][[num_of_parties]],
            "sameposacrossparty"=rawvote_result[[i]][,"sum"][typeofdecision],
            "billuid"=rawvote_uid,
            "billterm"=as.integer(rawvote_term),
            "billperiod"=as.integer(rawvote_period),
            "billresult"=rawvote_passed,
            "billconflict"=rawvote_conflict,
            "concernfield"="",
            "positionmeter"=""
          )
          #if (nrow(overallvotedf)==0) {
          #  overallvotedf<-testdf
          #} else {
          #  num_of_testdf<-1
          #  for (num_of_testdf in 1:nrow(testdf)) {
          #    custominsertRow(testdf[num_of_testdf,],overallvotedf)
          #  }
          #}
          #apply(testdf[,1:10],1,custommessage,overallvotedf)
          #apply(testdf[,1:10],1,function(x,overallvotedf) custommessage(x,overallvotedf)) #existingDF=overallvotedf
          overallvotedf<-rbind(overallvotedf,testdf)
        }
      }
    }
  }  
}

newvotingdf<-distinct(overallvotedf, votecontent, billuid, billterm, billperiod, billresult, billconflict)#
newvotingdf$votecontent<-as.character(newvotingdf$votecontent)
newvotingdf$billuid<-as.character(newvotingdf$billuid)
write_delim(newvotingdf, "E:\\Software\\scripts\\R\\votingdf_datafile.csv", delim = "\t", na = "NA", append = FALSE,  col_names = TRUE)
write_excel_csv(newvotingdf, "E:\\Software\\scripts\\R\\votingdf_tmpdatafile.csv", na = "NA", append = FALSE,  col_names = TRUE)
write.table(newvotingdf, file="E:\\Software\\scripts\\R\\votingdf_datafile.csv", quote = TRUE, sep = ",", na = "NA", append = FALSE,dec=".",col.names = FALSE,row.names = FALSE,fileEncoding="UTF-8")
write.foreign(newvotingdf,"E:\\Software\\scripts\\R\\votingdf_datafile.txt", "E:\\Software\\scripts\\R\\votingdf_datafile.sps", package="SPSS")



overallvotedf$votecontent<-as.character(overallvotedf$votecontent)
overallvotedf$billuid<-as.character(overallvotedf$billuid)
#save(overallvotedf, file = "E:\\Software\\scripts\\R\\voting_dataframe.RData")

#個別議案結果中的決定
#rawvote_result<-df_vote[['results']][['result']][['decision']]

#tmprawlyvotetxtfile<-"E:\\Software\\scripts\\ubuntuinwinjhome\\congress\\twly_fileHandler\\vote\\meeting_minutes\\第09屆第03會期第14次會議.txt"
#tmprawlyvotetxt<-read_file(tmprawlyvotetxtfile)
#votediscussresult_simp<-GetVoteContent(9,tmprawlyvotetxt)
#附後\S[\d]+\S
