# 第Ｏ部份：環境設定 --------------------------------
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
source(file = "shared_functions.R")
terms<-c(5,6,7,8,9)
gc(verbose=TRUE)
#options(scipen = 999)

model_indp_var <- c("myown_selfid", "myown_factoredses", "myown_factoredparticip", "gap_sex", "gap_eduyr", "gap_ses", "gap_ethnicity", "gap_age", "days_diff_survey_bill")
power_analysis_general_formula_suffix <- paste0(model_indp_var,collapse="+")
respondopinion_by_district_legislator_formula <- paste0("respondopinion ~ ",power_analysis_general_formula_suffix)
pass_on_bill_formula <- paste0("pass_on_bill ~ ",power_analysis_general_formula_suffix)

names(overall_district_legislators_only_power_dfdata[[1]])
if ({USING_ridge<-TRUE; USING_ridge}) {
  #library(glmnet)
  set.seed(22)
  overall_district_legislators_only_power_split <- rsample::initial_split(data = overall_district_legislators_only_power_dfdata[[1]], prop = 0.7, strata = "respondopinion")
  overall_district_legislators_only_power_train <- rsample::training(overall_district_legislators_only_power_split)
  overall_district_legislators_only_power_test <- rsample::testing(overall_district_legislators_only_power_split)
  
  overall_district_legislators_only_power_train_x <- model.matrix(object = as.formula(respondopinion_by_district_legislator_formula),
                                                                  data =  overall_district_legislators_only_power_dfdata[[1]])[, -1]
  overall_district_legislators_only_power_train_y <- log(overall_district_legislators_only_power_train$respondopinion)
  overall_district_legislators_only_power_test_x <- model.matrix(as.formula(respondopinion_by_district_legislator_formula), overall_district_legislators_only_power_test[[1]])[, -1]
  overall_district_legislators_only_power_test_y <- log(overall_district_legislators_only_power_test$respondopinion)
  # 使用rcorr()產生Matrix of correlations and P-values
  res2 <- filter(complete_survey_dataset,SURVEY=="2010overall") %>%
    distinct(myown_wsel, myown_sex, myown_age, myown_dad_ethgroup, myown_mom_ethgroup, myown_selfid, myown_eduyr, myown_factoredses, myown_factoredparticip) %>%
    {Hmisc::rcorr(as.matrix(.[,sapply(., is.numeric)]))}
}
#Ordered Logistic
if ({ordered_logistic_to_analy_repre_response<-TRUE; ordered_logistic_to_analy_repre_response}) {
  result_repre_response_on_all_bills <- lapply(overall_district_legislators_only_power_dfdata,function(dataset_for_analysis) {
    #dataset_for_analysi <- overall_district_legislators_only_power_dfdata
    model_respondopinion_by_district_legislator <- dataset_for_analysis %$%
      MASS::polr(as.formula(respondopinion_by_district_legislator_formula), Hess=TRUE) %>%
      MASS::stepAIC()
    ctable <- coef(summary(model_respondopinion_by_district_legislator))
    pvaluetable <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    confi_table <- confint.default(model_respondopinion_by_district_legislator) %>%
      rbind(matrix(rep(NA,4),nrow=2,ncol=2))
    odds_ratio_table <- c(exp(coef(model_respondopinion_by_district_legislator)),NA,NA)
    #predict_value_mean <- MASS::polr.predicts(model_respondopinion_by_district_legislator, values="mean")
    final_ctable <- cbind(ctable, "p value" = pvaluetable, confi_table, "Odds Ratio"=odds_ratio_table)#, "CI" = 
    return(final_ctable)
  })
}
model_ses_to_particip <- overall_district_legislators_only_power_dfdata[[1]] %$%
   glm(myown_factoredparticip ~ myown_factoredses, family=gaussian)
  


##測試參加政治與階級間關係
(ggplot(complete_survey_dataset[complete_survey_dataset$SURVEY=="2010overall",],
        aes(x = myown_factoredclass,
            y = (myown_factored_nonpartcip_se.z1)
        )
) + labs(title = "各個階級的不參與政治程度") + geom_point()+geom_smooth(method="lm"))
##2010env不參與程度不好、2010overall不參與程度也不好


##############################################################################
# 第O部份：分析前處理資料
##############################################################################
#如果有串選區，處理選區意見人數
glmdata %<>% dplyr::group_by( SURVEYQUESTIONID,electionarea ) %>%
  dplyr::mutate("all_pos_on_same_q_by_electionarea"=n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(SURVEYQUESTIONID,SURVEYANSWERVALUE,electionarea) %>%
  dplyr::mutate("same_pos_on_same_q_by_electionarea"=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate("same_pos_to_all_ratio_by_electionarea"=same_pos_on_same_q_by_electionarea/all_pos_on_same_q_by_electionarea*100)

#分組處理政黨成員投票情形

#group_by(billid_myown,legislator_party) %>%
#mutate("total_votes_from_same_party"=n()) %>%
#ungroup() %>%
#group_by(votedecision,billid_myown,legislator_party) %>%
#mutate("same_votes_from_same_party"=n()) %>%
#ungroup() %>%
#mutate("percent_of_same_votes_from_same_party"=same_votes_from_same_party/total_votes_from_same_party*100) %>%
#mutate("vote_along_with_majority_in_party"=ifelse(percent_of_same_votes_from_same_party>50,1,0)) %>%

#%>%
#dplyr::group_by( SURVEYQUESTIONID ) %>%
#dplyr::mutate("all_pos_on_same_q_by_nation"=n()) %>%
#dplyr::ungroup() %>%
#dplyr::group_by(SURVEYQUESTIONID,SURVEYANSWERVALUE) %>%
#dplyr::mutate("same_pos_on_same_q_by_nation"=n()) %>%
#dplyr::ungroup() %>%
#dplyr::mutate("same_pos_to_all_ratio_by_nation"=same_pos_on_same_q_by_nation/all_pos_on_same_q_by_nation*100)


#計算出同立場的人數
glmdata <- testdf %>%
  group_by(billid_myown,variable_on_q,value_on_q_variable,opiniondirectionfromconstituent) %>%
  mutate("same_opiniondirection_from_constituent_by_nation"=n()) %>%
  ungroup() %>%
  group_by(billid_myown,variable_on_q,value_on_q_variable) %>%
  mutate("all_opiniondirection_from_constituent_by_nation"=n()) %>%
  ungroup() %>%
  mutate("opinion_pressure_from_constituent_by_nation"=same_opiniondirection_from_constituent_by_nation/all_opiniondirection_from_constituent_by_nation) %>%
  mutate("majority_opinion_from_constituent_by_nation"=ifelse(opinion_pressure_from_constituent_by_nation>=0.5,1,0)) %>%
  mutate_at("majority_opinion_from_constituent_by_nation",funs(as.factor)) %>%
  group_by(billid_myown,variable_on_q,value_on_q_variable,electionarea,opiniondirectionfromconstituent) %>%
  mutate("same_opiniondirection_from_constituent_by_electionarea"=n()) %>%
  ungroup() %>%
  group_by(billid_myown,variable_on_q,value_on_q_variable,electionarea) %>%
  mutate("all_opiniondirection_from_constituent_by_electionarea"=n()) %>%
  ungroup() %>%
  mutate("opinion_pressure_from_constituent_by_electionarea"=same_opiniondirection_from_constituent_by_electionarea/all_opiniondirection_from_constituent_by_electionarea) %>%
  mutate("majority_opinion_from_constituent_by_electionarea"=ifelse(opinion_pressure_from_constituent_by_electionarea>=0.5,1,0)) %>%
  mutate_at("majority_opinion_from_constituent_by_electionarea",funs(as.factor)) %>%
  filter(!(value_on_q_variable %in% c("2016citizen@d5a","2016citizen@d6a","2016citizen@d6b","2016citizen@d6d","2016citizen@d6g","2016citizen@d6h"))) %>%
  extract(,setdiff(colnames(.),c("same_opiniondirection_from_constituent_by_nation",
                                 "all_opiniondirection_from_constituent_by_nation",
                                 "same_opiniondirection_from_constituent_by_electionarea",
                                 "all_opiniondirection_from_constituent_by_electionarea",
                                 "ballotid","leaveReason","leaveDate","leaveFlag","picUrl",
                                 "committee","ename","billcontent","pp_ignored","pp_res_notjudged",
                                 "pp_res_bycompete","pp_res_bynew","pp_enforcement","pp_duplicated_item",
                                 "votecontent","pp_committee","billcontent.y","same_votes_from_same_party",
                                 "total_votes_from_same_party","date","urln","url","billcontent.x",
                                 "same_pos_on_same_q_by_electionarea","all_pos_on_same_q_by_electionarea",
                                 "same_pos_on_same_q_by_nation","all_pos_on_same_q_by_nation",
                                 "zip3rocyear","qtype"))) #%>%   #忽略預算支出題組
#filter(issue_field1=='公民與政治權' | issue_field2=='公民與政治權')
#scale()
#group_by(billid_myown,variable_on_q,respondopinion) %>%
#mutate("same_opinion_on_same_bill_and_interest"=n()) %>%
#ungroup() %>%
#group_by(billid_myown,term,SURVEY,variable_on_q,SURVEYQUESTIONID) %>%
#mutate("all_direction_on_same_bill_and_interest"=n()) %>%
#ungroup() %>%
#mutate("same_direction_on_same_bill_and_interest_toall_ratio"=same_direction_on_same_bill_and_interest/all_direction_on_same_bill_and_interest) %>%
#group_by(name,billid_myown,term,SURVEY,variable_on_q,SURVEYQUESTIONID,respondopinion) %>%
#mutate("same_direction_on_same_bill_and_interest_in_same_legislator"=n()) %>%
#ungroup() %>%
#group_by(name,billid_myown,term,SURVEY,variable_on_q,SURVEYQUESTIONID) %>%
#mutate("all_direction_on_same_bill_and_interest_in_same_legislator"=n()) %>%
#ungroup() %>%
#mutate("same_direction_on_same_bill_and_interest_in_same_legislator_toall_ratio"=same_direction_on_same_bill_and_interest_in_same_legislator/all_direction_on_same_bill_and_interest_in_same_legislator) #%>%
#magrittr::extract(1:30,)# %>%
#View()






glmdata$respondopinion[glmdata$respondopinion==1]<-2
#contrasts(glmdata$respondopinion)<-contr.treatment(4, base=1)
glmdata$respondopinion<-ordered(glmdata$respondopinion,levels=c(0,1,2,3),labels=c("Reject","Ignore","Giveup","Respond"))
glmdata$respondopinion<-ordered(glmdata$respondopinion,levels=c(0,2,3),labels=c("Reject","Giveup","Respond"))
glmdata$myown_dad_ethgroup<-factor(glmdata$myown_dad_ethgroup,levels=c(1,2,3,4,5,6),labels=c("閩","客","原","外省","移民","其他臺灣人"))
glmdata$myown_mom_ethgroup<-factor(glmdata$myown_mom_ethgroup,levels=c(1,2,3,4,5,6),labels=c("閩","客","原","外省","移民","其他臺灣人"))
glmdata$myown_selfid<-factor(glmdata$myown_selfid,levels=c(1,2,3,4,5,6),labels=c("閩","客","原","外省","移民","其他臺灣人"))
glmdata$myown_vote<-factor(glmdata$myown_vote,levels=c(1,2,3),labels=c("有投","沒投","沒有投票權"))
glmdata$myown_protest<-factor(glmdata$myown_protest,levels=c(0,1),labels=c("沒抗議","有抗議"))
glmdata$myown_approach_to_politician_or_petition<-factor(glmdata$myown_approach_to_politician_or_petition,levels=c(0,1),labels=c("沒請願或遊說","有請願或遊說"))

contrasts(glmdata$rulingparty)<-contr.treatment(2, base=2)
contrasts(glmdata$myown_approach_to_politician_or_petition)<-contr.treatment(2, base=2)
contrasts(glmdata$myown_protest)<-contr.treatment(2, base=2)
contrasts(glmdata$myown_ext_pol_efficacy)<-contr.treatment(5, base=5)
contrasts(glmdata$myown_int_pol_efficacy)<-contr.treatment(5, base=5)

glmdata$percent_of_same_votes_from_same_party<-scale(glmdata$percent_of_same_votes_from_same_party)
glmdata$myown_family_income<-scale(glmdata$myown_family_income)
glmdata$percent_of_same_votes_from_same_party<-glmdata$percent_of_same_votes_from_same_party/100


##############################################################################
# 第O部份：產出報告
##############################################################################



library(rmarkdown)
render(input='analysis_result.Rmd',output_dir=getwd(),encoding="UTF-8")
render(input='analysis_result_on_bill_passed.Rmd',output_dir=getwd(),encoding="UTF-8")
getwd()

##############################################################################
# 第O部份：分析資料
##############################################################################



###備份

##探索性資料分析
library(ggplot2)
library(plotly)
(ggplot(dplyr::filter(glmdata,!is.na(respondopinion)),
        aes(x = respondopinion,
            y = (myown_ses)
        )
) + labs(title = "社經地位") + facet_grid(term+issue_field1 ~ party) + geom_boxplot()) %>%
  ggplotly(width=700,height=1600)


ggplot(filter(glmdata,!is.na(respondopinion)),
       aes(x = respondopinion,
           y = (myown_protest)
       )
) + labs(title = "有無抗議") + facet_grid(term ~ party) + geom_count()

#Feature Selection
library(party)
compactglmdata<-dplyr::select(glmdata,term,respondopinion,myown_areakind,myown_sex,myown_age,myown_dad_ethgroup,myown_mom_ethgroup,myown_eduyr,myown_int_pol_efficacy,myown_ext_pol_efficacy,myown_approach_to_politician_or_petition,myown_protest,myown_vote,myown_constituency_party_vote,myown_working_status,myown_ses,myown_family_income,myown_family_income_ranking,myown_family_income_stdev,percent_of_same_votes_from_same_party,rulingparty,opinionstrength,eduyrgap,sesgap,sexgap,agegap,opinion_pressure_from_constituent_by_nation,opinion_pressure_from_constituent_by_electionarea) %>%
  dplyr::filter(respondopinion %in% c("Reject","Giveup","Respond")) %>%
  mutate_at("respondopinion",funs(ordered))


gc(reset=TRUE)
cf1 <- cforest(respondopinion ~ . , data= compactglmdata, control=cforest_unbiased(mtry=2,ntree=50)) # fit the random forest
varimp(cf1)

#累積迴歸
library(ordinal)
model <- clm(respondopinion~scale(sesgap),
             data=glmdata)
summary(model)
#glmdata$vote_along_with_majority_in_party
glmdata$percent_of_same_votes_from_same_party %>% scale() %>% table()

#決策樹
require(rpart)
require(rpart.plot)
compactglmdata<-dplyr::filter(glmdata,respondopinion %in% c("Reject","Giveup","Respond")) %>%
  dplyr::select(term,respondopinion,myown_areakind,myown_sex,myown_age,myown_dad_ethgroup,myown_mom_ethgroup,myown_eduyr,myown_int_pol_efficacy,myown_ext_pol_efficacy,myown_approach_to_politician_or_petition,myown_protest,myown_vote,myown_working_status,myown_ses,myown_family_income,myown_family_income_ranking,myown_family_income_stdev,percent_of_same_votes_from_same_party,rulingparty,eduyrgap,sesgap,sexgap,agegap,opinion_pressure_from_constituent_by_nation,opinion_pressure_from_constituent_by_electionarea,issue_field1,party) %>%
  mutate_at("respondopinion",funs(ordered))
set.seed(22)
train.index <- sample(x=1:nrow(compactglmdata), size=ceiling(0.8*nrow(compactglmdata) ))
train <- compactglmdata[train.index,1:26]
test <- compactglmdata[-train.index,1:26]
cart.model<- rpart(respondopinion ~ ., 
                   data=train)
rpart.plot::prp(cart.model,         # 模型
                faclen=3,           # 呈現的變數不要縮寫
                fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
                shadow.col="gray",  # 最下面的節點塗上陰影
                # number of correct classifications / number of observations in that node
                extra=2,
                tweak=2)
rpart.plot::prp(cart.model, faclen = 0, cex = 0.8, extra = 1)
rattle::fancyRpartPlot(cart.model, cex=0.5)

only_count <- function(x, labs, digits, varlen)
{
  paste(x$frame$n)
}

boxcols <- c("pink", "palegreen3")[cart.model$frame$yval]

par(xpd=TRUE)
rpart.plot::prp(cart.model, faclen = 0, cex = 0.8, node.fun=only_count, box.col = boxcols)
legend("bottomleft", legend = c("died","survived"), fill = c("pink", "palegreen3"),
       title = "Group")


#累積迴歸
require(MASS)
##檢定挑選變數
binaryglmdata<-dplyr::filter(glmdata,respondopinion %in% c("Reject","Giveup","Respond"),myown_mom_ethgroup!="其他臺灣人") %>%
  #dplyr::filter(respondopinion %in% c("Reject","Respond")) %>%
  dplyr::select(term,respondopinion,myown_sex,myown_selfid,myown_approach_to_politician_or_petition,myown_protest,myown_vote,myown_factoredclass,percent_of_same_votes_from_same_party,rulingparty,sesgap,sexgap,opinion_pressure_from_constituent_by_electionarea,issue_field1,party) %>%
  mutate_if(is.numeric,scale) %>%
  mutate_at("respondopinion",funs(ordered))
model <- MASS::polr(respondopinion ~ .,
                    data = binaryglmdata[,2:15],
                    na.action=na.omit,
                    Hess=TRUE)
selectedMod<-MASS::stepAIC(model)
model<-glm(
  formula = respondopinion ~ .,
  family = binomial(
    link = "logit"),
  data = binaryglmdata[,2:24])
selectedMod <- step(model)
gc(reset=TRUE)
#挑出共線性有問題的 the linearly dependent variables
ld.vars <- attributes(alias(model)$Complete)$dimnames[[1]]

summary(selectedMod)
summary(model)
car::vif(model)
car::vif(selectedMod)
mctest
## view a summary of the model

## 分段
model<-MASS::polr(respondopinion ~ myown_factoredclass,data = 
                    dplyr::filter(glmdata,term==7, respondopinion %in% c(0,2,3), myown_family_income_ranking>5, myown_family_income_ranking<95) %>%
                    mutate_at("respondopinion",funs(ordered)),
                  na.action=na.omit,Hess=TRUE)
#myown_sex+myown_selfid+myown_approach_to_politician_or_petition+myown_protest+myown_vote+myown_factoredclass+percent_of_same_votes_from_same_party+rulingparty+sesgap+sexgap+opinion_pressure_from_constituent_by_electionarea
summary(model)
#view coef and pvalue
model.coef <- data.frame(coef(summary(model))) %>%
  tibble::rownames_to_column('gene') %>%
  mutate("pval"=round((pnorm(abs(t.value),lower.tail= FALSE) * 2), 4)) %>%
  tibble::column_to_rownames('gene')
#model.coef$pval <- round((pnorm(abs(model.coef$t.value),lower.tail= FALSE) * 2), 4)
model.coef

model<-filter(glmdata,term==7) %$%
  lm(myown_factored_nonpartcip_se.z1~myown_factoredclass)
summary(model)


#check validity
pscl::pR2(model)


#"myown_areakind"
#"myown_sex"
#"myown_age"
#"myown_dad_ethgroup"
#"myown_mom_ethgroup"
#"myown_marriage"
#"myown_religion"
#"myown_eduyr"   
#"myown_int_pol_efficacy"
#"myown_ext_pol_efficacy"
#"myown_approach_to_politician_or_petition"  
#"myown_protest" 
#"myown_vote"
#"myown_constituency_party_vote" 
#"myown_working_status"  
#"myown_industry"
#"myown_occp"
#"myown_ses"
#"myown_workers_numbers"
#"myown_family_income"   
#"myown_family_income_ranking"   
#"myown_family_income_stdev"  
#"total_votes_from_same_party"   
#"same_votes_from_same_party"
#"percent_of_same_votes_from_same_party" 
#"vote_along_with_majority_in_party" 
#"seats"
#"rulingparty"   
#"seatsgaptorulingparty"  
#"opinionstrength"  
#"eduyrgap" 
#"sesgap"   
#"sexgap"   
#"agegap"   
#"opinion_pressure_from_constituent_by_nation"   
#"majority_opinion_from_constituent_by_nation" 
#"opinion_pressure_from_constituent_by_electionarea"
#"majority_opinion_from_constituent_by_electionarea" 


selectedglmdata<-glmdata %>%
  select(success_on_bill,term,myown_areakind,myown_wsel,myown_sex,myown_vote,myown_selfid,myown_selfid_population,myown_factoredclass,myown_factored_nonpartcip_se.z1,opinion_pressure_from_constituent_by_nation,opinionstrength,myown_family_income,myown_family_income_ranking,myown_ses,myown_approach_to_politician_or_petition,myown_protest,myown_eduyr) %>%
  mutate_if(is.numeric,scale) %>%
  mutate_if(is.factor,factor) %>%
  na.omit()
## 分段：只看有沒有通過
#myown_sex+myown_selfid+myown_approach_to_politician_or_petition+myown_protest+myown_vote+myown_factoredclass+
model.null <- selectedglmdata  %$%
  glm(success_on_bill ~ 1,
      family = binomial(link="logit"),
      na.action=na.omit
  )
model.full <- selectedglmdata %$%
  glm(success_on_bill ~ myown_sex+myown_vote+myown_selfid+myown_selfid_population+myown_factoredclass+myown_factored_nonpartcip_se.z1+opinion_pressure_from_constituent_by_nation,
      family = binomial(link="logit"),
      na.action=na.omit
  )
selectedglmdata %>% na.omit() %$%
  step(model.null,
       scope = list(upper=model.full),
       direction="both",
       test="Chisq",
       na.action=na.omit)
#篩選結果是 opinion_pressure_from_constituent_by_nation+myown_factoredclass+myown_selfid+myown_factored_nonpartcip_se.z1+myown_vote
model.final<- selectedglmdata  %$%
  glm(formula = success_on_bill ~ (opinion_pressure_from_constituent_by_nation+myown_factored_nonpartcip_se.z1+myown_factoredclass+myown_selfid+myown_sex+myown_vote),
      family = binomial(
        link = "logit"),
      na.action=na.omit
  )
car::Anova(model.final, type="II", test="Wald")
rcompanion::nagelkerke(model.final)
anova(model.final,model.null,test="Chisq")
lmtest::lrtest(model.final)
car::vif(model.final)
summary(model.final)
write_csv(selectedglmdata,path=paste0(dataset_file_directory,"rdata",slash,"selected_bill_passed.csv"),na="")


#HLM
model<- glmdata[glmdata$term==9,]  %$%
  lme4::glmer(formula = success_on_bill ~ (myown_selfid|myown_areakind),
              family = binomial(
                link = "logit"),
              na.action=na.omit
  )


library(spatialEco)
model<- glmdata[glmdata$term==7,]  %>%
  logistic.regression(y = "success_on_bill", x= c("opinion_pressure_from_constituent_by_nation","myown_sex","myown_selfid","myown_ses","myown_approach_to_politician_or_petition","myown_vote"), penalty = F  )
print(model$model)
print(model$diagTable)
print(model$estimate)
summary(model)
pscl::pR2(model)
#$model, $diagTable, coefTable
#myown_factored_nonpartcip_Exp
#myown_factored_nonpartcip_z1
#myown_factored_nonpartcip_se.z1

#+rulingparty

binaryglmdata<-filter(glmdata,term==9,rulingparty==1,respondopinion %in% c("Reject","Respond")) %>%
  select(term,respondopinion,myown_sex,myown_selfid,myown_approach_to_politician_or_petition,myown_protest,myown_vote,myown_factoredclass,percent_of_same_votes_from_same_party,rulingparty,sesgap,sexgap,opinion_pressure_from_constituent_by_electionarea,issue_field1,party) %>%
  mutate_if(is.numeric,scale) %>%
  mutate_if(is.factor,factor) #,term==7,party=="中國國民黨"
#contrasts(binaryglmdata$respondopinion)<-contr.treatment(2, base=2) #
model<-glm(
  #myown_areakind+myown_sex+myown_dad_ethgroup+myown_mom_ethgroup+myown_marriage+myown_religion+myown_pol_efficacy+myown_approach_to_politician_or_petition+myown_protest+myown_working_status+myown_age+myown_eduyr+myown_occp+myown_family_income+opinionstrength+opinion_pressure_from_party
  formula = respondopinion ~ .,
  family = binomial(
    link = "logit"),
  data = binaryglmdata)
summary(model)


#計算預測能力
fitted.results <- predict(model,newdata=
                            dplyr::filter(glmdata,term==9) %>%
                            mutate_at("success_on_bill",funs(dplyr::recode)) %>%
                            sample_n(30000),
                          type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))


#可以看到沒有繼續當的立委沒串到
distinct(legislators_with_elections, term, name) %>%
  filter(customgrepl(name, "廖國棟|簡東明|鄭天財|秀霞|高潞"))
distinct(mergedf_votes_bills_surveyanswer, term, name) %>%
  filter(customgrepl(name, "廖國棟|簡東明|鄭天財|秀霞|高潞"))


setdiff(distinct(legislators_with_elections, term, name), distinct(mergedf_votes_bills_surveyanswer)) %>%
  filter(customgrepl(name, "廖國棟|簡東明|鄭天財|秀霞|高潞")) #%>%
#%>% View()
setdiff(distinct(mergedf_votes_bills_surveyanswer, term, name), distinct(legislators_with_elections, term, name)) %>%
  filter(customgrepl(name, "廖國棟|簡東明|鄭天財|秀霞|高潞")) #%>%
#%>% View()
distinct(legislators_with_elections, term, name) %>% View()
#廖國棟,簡東明,鄭天財,陳秀霞,高潞‧以用‧巴魕剌Kawlo．Iyun．Pacidal
#簡東明Uliw．Qaljupayare,#廖國棟Sufin．Siluko,#鄭天財Sra．Kacaw,#周陳秀霞,#高潞．以用．巴魕剌Kawlo．Iyun．Pacidal
filter(mergedf_votes_bills_surveyanswer, customgrepl(name, "高潞")) %>%
  distinct(name)

ggplot(glmdata, aes(x = myown_family_income, y = respondopinion)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  #facet_grid(pared ~ public, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#TO SPSS
library(foreign)
write.foreign(glmdata, "glmdata.txt", "glmdata.sps",   package="SPSS")






