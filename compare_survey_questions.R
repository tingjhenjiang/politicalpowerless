filespath <- "E:\\Software\\scripts\\R\\"
filespath <- "/mnt/e/Software/scripts/R/"
source(file = paste(filespath, "shared_functions.R", sep = ""))
surveyquestionfile <- "D:\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\all_survey_questions.xlsx"
#survey_q_man_classified <- read.xlsx(surveyquestionfile, 1, header=TRUE, startRow=1, encoding="UTF-8")
#survey_q_machine_classified <- read.xlsx(surveyquestionfile, 3, header=TRUE, startRow=1, encoding="UTF-8")
#survey_q <- rbind(survey_q_man_classified, survey_q_machine_classified)
#survey_codebook <-read.xlsx(surveyquestionfile, 6, header=TRUE, startRow=1, encoding="UTF-8")
#save(survey_q,file = "survey_q.RData")
#save(survey_codebook,file = "survey_codebook.RData")
load("survey_q.RData")
load("survey_codebook.RData")
survey_q <- filter(survey_q, SURVEY %in% c("2010第六期第一次：綜合組", "2010六期一次：環境", "2016臺灣選舉與民主化調查：總統與立法委員選舉面訪案", "2016七期二次：公民與國家"))
survey_codebook_new<-left_join(survey_codebook,survey_q)
#pos <- agrep("", survey_q$QUESTION, max.distance = 0.02)
#df <- survey_q[pos, c("SURVEY", "QUESTION","ID")]

pos <- agrep("自己是老闆", survey_codebook_new$LABEL, max.distance = 0.02)
df <- survey_codebook_new[pos, c("SURVEY", "QUESTION", "ID", "LABEL")]
df <- filter(survey_codebook_new,grepl("j15",LABEL))
df <- filter(survey_codebook_new,ID == "v103b5")
df <- filter(survey_codebook_new,grepl("配偶(或同居伴侶)現在(以前/退休前)的工作上,有沒有管理其他員工?",QUESTION))

View(df)

