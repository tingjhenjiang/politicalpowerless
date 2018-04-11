filespath<-"E:\\Software\\scripts\\R\\"
filespath<-"/mnt/e/Software/scripts/R/"
#load("meetingdata.RData")
source(file=paste(filespath,"shared_functions.R",sep=""))
codepath<-ifelse(check_if_windows(),"D:\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\","/mnt/d/OneDrive/OnedriveDocuments/NTU/Work/thesis/dataset(2004-2016)/")
codebook<-read.xlsx(file=paste0(codepath,"all_survey_questions.xlsx",collapse=""),sheetIndex=7,startRow = 1,endRow =54620,header = T,encoding = "UTF-8")
newcodebook<-mutate(codebook,value_to_label=paste(VALUE,LABEL,sep="",collapse=""))
