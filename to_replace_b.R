t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
filepath<-switch(
  paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model),
  "Windows8x64build9200Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="E:\\Software\\scripts\\R\\",
  "Windows10x64build17763Intel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="E:\\Software\\scripts\\R\\",
  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-4210U CPU @ 1.70GHz"="/mnt/e/Software/scripts/R/",
  "Ubuntu18.04.1LTSIntel(R) Core(TM) i5-7400 CPU @ 3.00GHz"="/mnt/d/Software/scripts/",
  "Windows7x64build7601ServicePack1Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz"="C:\\Users\\r03a21033\\DOWNLOADS\\"
)
source(file = paste(filespath, "shared_functions.R", sep = ""))
#選舉資料
overall_elec_dist_types<-c('區域','山原','平原','不分區政黨')
supplement_election_termseven<-c('補選2009苗栗縣1','補選2009南投縣1','補選2009雲林縣2','補選2009臺北市6','補選2010台中縣3','補選2010花蓮縣','補選2010桃園縣2','補選2010桃園縣3','補選2010新竹縣','補選2010嘉義縣2','補選2010臺東縣','補選2011台南市4','補選2011高雄市4')
terms<-c(7,9)


to_replace_b_file <- "to_replace_b.xlsx"
to_replace_b <- read.xlsx(to_replace_b_file, sheet = 1) %>%
  select(SURVEYANSWERVALUE,opinionfromconstituent,opinionfrombill)
intervals_bg<-which(to_replace_b$SURVEYANSWERVALUE==1)
intervals_end<-which(to_replace_b$SURVEYANSWERVALUE==99)
for (i in 1:length(intervals_end)) {
  range<-intervals_bg[i]:intervals_end[i]
  if (to_replace_b$opinionfromconstituent[range]==c("mm",	"m",	"b",	"n",	"nn",	"x",	"x",	"x",	"x",	"x",	"x",	"x")
      ) {
    if (to_replace_b$opinionfrombill[range]==c("mm",	"m",	"m",	"m",	"m",	"m",	"m",	"m",	"m",	"m",	"m",	"m")) {
      to_replace_b$opinionfrombill[range]<-replace(to_replace_b$opinionfrombill[range],to_replace_b$opinionfrombill[range]=='x','m') %>%
        replace(to_replace_b$opinionfrombill[range]=='n','m') %>%
        replace(to_replace_b$opinionfrombill[range]=='nn','m') %>%
        replace(to_replace_b$opinionfrombill[range]=='mm','m')
    } else if(to_replace_b$opinionfrombill[range]==c("n",	"n",	"n",	"n",	"n",	"x",	"x",	"x",	"x",	"x",	"x",	"x")) {
      to_replace_b$opinionfrombill[range]<-replace(to_replace_b$opinionfrombill[range],to_replace_b$opinionfrombill[range]=='x','n') %>%
        replace(to_replace_b$opinionfrombill[range]=='m','n') %>%
        replace(to_replace_b$opinionfrombill[range]=='mm','n') %>%
        replace(to_replace_b$opinionfrombill[range]=='nn','n')
    }
  } 
  #done above
  #currentrow<-to_replace_b[i,]
  #nextrow<-to_replace_b[i+1,]
  #previousrow<-to_replace_b[i-1,]
  
  #two_row_before<-to_replace_b[i-2,]
  #op_from_consti_orb<-two_row_before$opinionfromconstituent
  #op_from_bill_orb<-two_row_before$opinionfrombill
  
  #one_row_before<-to_replace_b[i-1,]
  #op_from_consti_trb<-one_row_before$opinionfromconstituent
  #op_from_bill_trb<-one_row_before$opinionfrombill
  
  #if (op_from_consti_orb=='mm' & op_from_bill_orb=='nn' & currentrow$opinionfrombill=='b') {
  #  to_replace_b$opinionfrombill[i]=nextrow$opinionfrombill
  #} else if (op_from_consti_orb=='mm' & op_from_bill_orb=='mm' & currentrow$opinionfrombill=='b') {
  #  to_replace_b$opinionfrombill[i]=previousrow$opinionfrombill
  #}
}
write_csv(to_replace_b[,c("opinionfromconstituent","opinionfrombill")],"new_to_replace_b.csv")
