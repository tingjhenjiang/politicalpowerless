# 第Ｏ部份：環境設定 --------------------------------
if (!("benchmarkme" %in% rownames(installed.packages()))) try(install.packages("benchmarkme"))
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-try(paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model))
t_sessioninfo_running_with_cpu_locale<-try(gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2])))
source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/shared_functions.R"), encoding="UTF-8")
gc(verbose=TRUE)

imputation_sample_i_s <- imputation_sample_i_s

# 第五部份：factor analysis 環境設定 -------------------------------------------
#reset
#load(paste0(dataset_in_scriptsfile_directory, "all_survey_combined.RData"))
#load imputed survey
load(paste0(dataset_file_directory,"rdata",slash,"miced_survey_9_Ubuntu18.04.3LTSdf.RData"), verbose=TRUE)

# 第五-1部份：IRT latent variables 將職業社經地位、家庭收入、教育程度萃取成為階級  =================================
#library(survey)
library(lavaan)


survey_data_imputed <- lapply(survey_data_imputed,function(X,imps) {
  #need_ses_var_assigned %<>% extract2(X$SURVEY[1]) %>%
  #  intersect(names(X))
  need_ses_var_assigned<-c("myown_eduyr","myown_ses","myown_income","myown_family_income")
  message("need ses var is ", X$SURVEY[1], " ", need_ses_var_assigned)
  for (checkvar in need_ses_var_assigned) {
    message("nrows of empty ", checkvar, " are ", sum(is.na(X$checkvar)) )
  }
  for (impn in imps) {
    need_ses_var_assigned<-c("myown_eduyr","myown_ses","myown_income","myown_family_income")#
    dat<-data.frame(X[X$.imp==impn,c("myown_wr",need_ses_var_assigned)])
    dat<-mutate_at(dat, need_ses_var_assigned, scale)
    mod<-'myown_factoredses=~myown_eduyr+myown_ses+myown_income+myown_family_income'
    lavaan.fit<-cfa(mod, data = dat, sampling.weights="myown_wr")
    #summary(lavaan.fit)
    #df_svydesign <- svydesign(ids = ~1, data=dat, weights = X[X$.imp==impn,]$myown_wr)
    #survey.fit <- lavaan.survey(lavaan.fit=lavaan.fit, survey.design=df_svydesign)
    #summary(survey.fit)
    X$myown_factoredses[X$.imp==impn]<-lavPredict(lavaan.fit)[,1]
    
    
    #https://jiaxiangli.netlify.com/2018/10/21/factor-analysis/
    
    if ({oldfactorses<-FALSE;oldfactorses}) {
      df_svydesign <- X[X$.imp==impn,need_ses_var_assigned] %>%
        svydesign(ids = ~1, data = ., weights = X[X$.imp==impn,]$myown_wr)
      needformula <- as.formula(paste0("~",need_ses_var_assigned,collapse = "+"))
      svy.efa.results<-svyfactanal(formula=needformula,
                                   #x=X[X$.imp==impn,need_ses_var_assigned],
                                   design=df_svydesign, factors=1, 
                                   n="sample", #n = c("none", "sample", "degf","effective", "min.effective"),
                                   rotation="promax"
                                   #,scores="Bartlett"
      )
      efa.results<-factanal(
        X[X$.imp==impn,need_ses_var_assigned],
        data=X[X$.imp==impn,need_ses_var_assigned],
        factors=1,
        rotation="promax",
        scores=c("regression")
      )
      X$myown_factoredses[X$.imp==impn]<-efa.results$scores[,1]      
    }

  }
  return(X)
}, imps=imputation_sample_i_s)

#install.packages("psy")
#library(psy)
#psy::scree.plot(fa.class$correlation)

# 第五-2部份：IRT latent variables 政治效能感 =================================
library(ltm)
library(eRm)
library(mirt)


if ({analysingefficacy <- TRUE;analysingefficacy}) {
  need_efficacy_recode_var_detail_surveyid <- list(
    "2004citizen@v47"=c(1,2,3,4,5),
    "2004citizen@v48"=c(1,2,3,4,5),
    "2004citizen@v49"=c(5,4,3,2,1),
    "2004citizen@v50"=c(1,2,3,4,5),
    "2004citizen@v51"=c(4,3,2,1),
    "2004citizen@v52"=c(4,3,2,1),
    "2010env@v21a"=c(1,2,3,4,5),
    "2010env@v21b"=c(1,2,3,4,5),
    "2010env@v70a"=c(1,2,3,4,5),
    "2010env@v70c"=c(5,4,3,2,1),
    "2010env@v70d"=c(5,4,3,2,1),
    "2010env@v70e"=c(1,2,3,4,5),
    "2010env@v78"=c(4,3,2,1),
    "2010env@v79"=c(4,3,2,1),
    "2010env@v26a"=c(1,2,3,4,5),
    "2010env@v26d"=c(1,2,3,4,5),
    "2010env@v26f"=c(1,2,3,4,5),
    "2010overall@v67d"=c(6,5,4,3,2,1),
    "2010overall@v67e"=c(6,5,4,3,2,1),
    "2010overall@v67f"=c(1,2,3,4,5,6),
    "2010overall@v67g"=c(1,2,3,4,5,6),
    "2010overall@v67h"=c(6,5,4,3,2,1),
    "2010overall@v67i"=c(6,5,4,3,2,1),
    "2016citizen@d16a"=c(1,2,3,4,5),
    "2016citizen@d16b"=c(5,4,3,2,1),
    "2016citizen@d16c"=c(5,4,3,2,1),
    "2016citizen@d16d"=c(1,2,3,4,5)
    )
  survey_data_imputed <- lapply(survey_data_imputed,function(X,need_efficacy_recode_var_detail_surveyid,imps) {
    needlist <- rlist::list.match(need_efficacy_recode_var_detail_surveyid, X$SURVEY[1])
    needvars <- names(needlist) %>%
      sapply(customgsub,paste0(X$SURVEY[1],"@"),"") %>%
      unname()
    needlist %<>% setNames(needvars)
    irt_target_d <- X[,c(".imp",needvars)]
    for (needvar in needvars) {
      #needvar<-"v49"
      #ori: [2] 同意 [4] 不同意 [4] 不同意 [2] 同意 [4] 不同意 [1] 非常同意
      #should be: 422425
      #needvar<-"v47"
      #ori: [4] 不同意 [4] 不同意 [2] 同意 [2] 同意 [3] 既不同意也不反對 [2] 同意
      #should be: 442232
      irt_target_d[,needvar] %<>% factor(.,levels(.)[extract2(needlist,needvar)]) %>%
        seq(from=1,to=length(extract2(needlist,needvar)))[.]
    }
    for (imp in imps) {
      estimatemodel<-mirt::mirt(
        data=irt_target_d[irt_target_d$.imp==imp,needvars],
        model=1,
        itemtype = "graded",
        technical = list("NCYCLES"=40000),
        survey.weights = irt_target_d[irt_target_d$.imp==imp,c("myown_wr")])
      poliefficacy<-mirt::fscores(estimatemodel,method="EAP") %>%
        as.data.frame() %>%
        set_colnames(c("myown_factoredefficacy"))
      X[irt_target_d$.imp==imp,c("myown_factoredefficacy")]<-poliefficacy$myown_factoredefficacy#bind_cols(X,poliefficacy)
      
    }
    #need_efficacy_var<-list(
    #  "2004citizen"=c("v47","v48","v49","v50","v51","v52"),
    #  "2010env"=c("v70a","v70c","v70d","v70e","v78","v21a","v21b","v26a","v26d","v26f","v78","v79"), #"v61",
    #  "2010overall"=c("v67d","v67h","v67i","v67f"),
    #  "2016citizen"=c("d16a","d16b","d16c","d16d")
    #)
    #need_efficacy_recode_var<-list(#效能感越高要重編碼為數字越大
    #  "onetofour"=list(
    #    "2004citizen"=c("v52","v51"),
    #    "2010env"=c("v61","v78"),
    #    "2010overall"=c(),
    #    "2016citizen"=c()
    #  ),
    #  "onetofive"=list(#效能感越高要重編碼為數字越大
    #    "2004citizen"=c("v49","v50"),
    #    "2010env"=c("v26b","v79"),
    #    "2010overall"=c(),
    #    "2016citizen"=c("d16b","d16c")
    #  ),
    #  "onetosix"=list(#效能感越高要重編碼為數字越大
    #    "2004citizen"=c(),
    #    "2010env"=c(),
    #    "2010overall"=c("v67d","v67h","v67i"),
    #    "2016citizen"=c()
    #  )
    #)
    #recode_list<-list(
    #  list("1"=4,"2"=3,"3"=2,"4"=1),
    #  list("1"=5,"2"=4,"4"=2,"5"=1),
    #  list("1"=6,"2"=5,"3"=4,"4"=3,"5"=1,"6"=1)
    #)
    #for (alteri in 1:3) {
    #  need_efficacy_recode_var <- extract2(need_efficacy_recode_var_assigned[[alteri]],X$SURVEY[1]) %>%
    #    intersect(names(X))
    #  X %<>% mutate_at(need_efficacy_recode_var, dplyr::recode, !!!recode_list[[alteri]])
    #}
    #irt_target_d <- X[,needvars] %>%
    #  dplyr::mutate_all(.funs=function(f) {
    #    #return(as.numeric(levels(f))[f])
    #    (seq(from=1,to=length(f)))[f] %>% return()
    #  })
    return(X)
  },
  need_efficacy_recode_var_detail_surveyid=need_efficacy_recode_var_detail_surveyid,
  #need_efficacy_var_assigned=need_efficacy_var,
  #need_efficacy_recode_var_assigned=need_efficacy_recode_var,
  imps=imputation_sample_i_s)
}
# 第五-3部份：IRT latent variables  latent variables 政治參與；用item respond抓出隱藏變數「政治參與程度」 =================================
# https://www.researchgate.net/post/How_to_conduct_item_analysis_with_a_likert_scale_questionaire
# mirt help: https://github.com/philchalmers/mirt/wiki
# http://moodle.ncku.edu.tw/pluginfile.php/977679/mod_resource/content/1/item_response_theory.pdf
library(ltm)
library(eRm)
library(mirt)


#2004citizen: v28 v29 v30 v31 v32 v33 v34 v35 v36 v37 v38 v39 v40 v59
#2016citizen-fit2-z1: b4 h2a h2b h2c h2d h2e h2f h2g h2h h3a h3b h3c h4
#2010overall-fit2: v79a v79b v79c v79d 
#2010env-fit1: v34 v35a v35b v35c ( v33f v75 v76 v77-為了環保而刻意不買某些產品,常不常參與社區的環保工作,常不常反應社區中容易造成天災危險的情況,常不常反應社區中造成環境污染的情況)
#load(paste0(dataset_in_scriptsfile_directory, "all_survey_combined.RData"))
#load imputed survey

need_particip_var<-list(
  "2004citizen"=c("v28","v29","v30","v31","v32","v33","v34","v35","v36","v37","v38","v39","v40"), #,"v59",v88 v90
  "2010env"=c("v34","v35a","v35b","v35c"), #投票"v104",
  "2010overall"=c("v79a","v79b","v79c","v79d"),
  "2016citizen"=c("h2a","h2b","h2c","h2d","h2e","h2f","h2g","h2h","h3a","h3b","h3c")#h4 投票
)
survey_data_imputed <- lapply(survey_data_imputed,function(X,need_particip_var_assigned) {
  #X<-lapply(X,function(X,need_particip_var_assigned) {
  #for testng prupose
  #X<-dummyremoved_imputed_survey_data[[1]][[1]]
  #need_particip_var_assigned<-need_particip_var
  need_particip_var_assigned %<>% extract2(X$SURVEY[1]) %>%
    intersect(names(X))
  customreordercatbylabelname<-function(X,desc=FALSE) {
    forcats::fct_reorder(X,as.character(X),.fun=unique,.desc=desc) %>%
      return()
  }
  X <- mutate_at(X,need_particip_var_assigned, customreordercatbylabelname, desc=TRUE)
  #forcats::fct_reorder(f,sort(levels(f),decreasing=FALSE))
  #forcats::fct_reorder(f,sort(levels(f),decreasing=TRUE))
  #recode_list<-list( #把越參與的答案改為數字越多，比較好解釋
  #  "2004citizen"=list("1"=4,"2"=3,"3"=2,"4"=1),
  #  "2010env"=list("1"=2,"2"=1,"是"=2,"否"=1,"有"=2,"沒有"=1),
  #  "2010overall"=list("1"=3,"2"=2,"3"=1),
  #  "2016citizen"=list("1"=4,"2"=3,"3"=2,"4"=1)
  #) %>%
  #  extract2(X$SURVEY[1])
  #X %<>% mutate_at(need_particip_var_assigned, dplyr::recode,!!!recode_list) %>%
  #  mutate_at(need_particip_var_assigned,funs(as.ordered))
  return(X)
},need_particip_var_assigned=need_particip_var)


# 第五-3-1部份：parametric IRT non-Rasch models - GRM Model ####################
# mirt::mirt by 'graded'
# ltm:grm
survey_data_imputed <- lapply(survey_data_imputed, function(X,need_particip_var_assigned,imps) {
  #X<-survey_data_test[[4]]
  #need_particip_var_assigned<-need_particip_var
  needparticip_surveyi<-X$SURVEY[1]
  need_detailed_particip_var<-extract2(need_particip_var_assigned,needparticip_surveyi)
  irt_target_d<-X[,c(".imp",need_detailed_particip_var)] %>%
    dplyr::mutate_at(.vars=need_detailed_particip_var, .funs=function(f) {
      #return(as.numeric(levels(f))[f])
      (seq(from=1,to=length(f)))[f] %>% return()
    })
  for (imp in imps) {
    estimatemodel<-mirt::mirt(
      data=irt_target_d[irt_target_d$.imp==imp,need_detailed_particip_var],
      model=1,
      itemtype = "graded",
      technical = list("NCYCLES"=40000),
      survey.weights = irt_target_d[irt_target_d$.imp==imp,c("myown_wr")]
      )
    poliparticipt<-mirt::fscores(estimatemodel,method="EAP") %>%
      as.data.frame() %>%
      set_colnames(c("myown_factoredparticip"))
    X[X$.imp==imp,c("myown_factoredparticip")]<-poliparticipt$myown_factoredparticip #bind_cols(X,poliparticipt)
  }
  #X<-estimatemodel
  #View(X[,c(need_detailed_particip_var,"myown_factoredparticip")])
  return(X)
},need_particip_var_assigned=need_particip_var,
imps=imputation_sample_i_s)


if ({using_ltm_package <- FALSE;using_ltm_package}) {
  survey_data_with_particip <- lapply(survey_data_test,function(X,need_particip_var_assigned) {
    #for testing purpose
    X<-survey_data_test[[1]]
    need_particip_var_assigned<-need_particip_var
    
    need_particip_var_assigned %<>% extract2(X$SURVEY[1]) %>%
      intersect(names(X))
    fit1 <- ltm::grm(X[,need_particip_var_assigned], constrained = TRUE, start.val = "random")
    fit2 <- ltm::grm(X[,need_particip_var_assigned], na.action = na.omit, start.val = "random")
    fit_testresult<-anova(fit1, fit2)
    if ((fit_testresult$p.value<=0.05) & (fit_testresult$L0 < fit_testresult$L1) ) {
      fit<-fit2
    } else {
      fit<-fit1
    }
    margins(fit)
    summary(fit)
    coef(fit)
    #if (fit_testresult$aic0>fit_testresult$aic1 & fit_testresult$bic0>fit_testresult$bic1) {
    #  fit<-fit2
    #} else {
    #  fit<-fit1
    #}
    X %<>% left_join(
      fit %>%
        factor.scores() %>%
        use_series("score.dat") %>%
        dplyr::select(-contains("Exp"),-contains("Obs")) %>%
        rename(myown_factored_partcip=z1,myown_factored_partcip.se=se.z1)
    )
    X$myown_factored_partcip %<>% scale() %>% as.numeric()
    X
  },need_particip_var)
  
  information(fit, c(-4, 4))
  sapply(1:length(participation_var[[itrn]]),function (X) information(fit, c(-4, 4), items = c(X)) )
  #characteristic curve for each item
  plot(fit, lwd = 0.8, cex = 0.8, legend = TRUE, cx = "left", xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
  #information curve
  plot(fit, type = "IIC", lwd = 0.8, cex = 0.5, legend = TRUE, cx = "topleft",xlab = "Latent Trait", cex.main = 0.8, cex.lab = 1, cex.axis = 1)
  #test information curve
  plot(fit, type = "IIC", items = 0, lwd = 2, xlab = "Latent Trait",cex.main = 1, cex.lab = 1, cex.axis = 1)
  info1 <- information(fit, c(-4, 0))
  info2 <- information(fit, c(0, 4))
  text(-2.5, 8, labels = paste("Information in (-4, 0):",paste(round(100 * info1$PropRange, 1), "%", sep = ""),"\n\nInformation in (0, 4):",paste(round(100 * info2$PropRange, 1), "%", sep = "")), cex = 0.7)
  par(mfrow = c(1, 1)) #configure how many figures would show in row and column
  #characteristic curve overall in different category
  #plot(fit, category = 1, lwd = 0.8, cex = 0.8, legend = TRUE, cx = -0.8,cy = 0.85, xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8,cex.axis = 0.8)
  for (ctg in 1:4) {
    plot(fit, category = ctg, lwd = 0.8, cex = 0.8, annot = TRUE,
         xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8,
         cex.axis = 0.8)
    Sys.sleep(2)
  }
}
# 第五-3-2部份：non-parametric IRT Mokken scale analysis Model ####################
#################### mokken, Mokken Scale Analysis in R
#################### read: https://www.jstatsoft.org/article/view/v020i11/v20i11.pdf

if ({using_IRT_Mokken <- FALSE;using_IRT_Mokken}) {
  mokken::coefH(as.data.frame(X[,need_particip_var_assigned]))
  checkmokkenresult<-mokken::check.monotonicity(as.data.frame(X[,need_particip_var_assigned]))
  summary(checkmokkenresult)
  plot(checkmokkenresult)
  scale.checkmokkenresult <- mokken::aisp(as.data.frame(X[,need_particip_var_assigned]))
}
# 第五-3-3部份：parametric IRT Rasch models - Partial Credit Model ####################
# mirt::Rasch
# eRm::PCM
# 第五-3-4部份：parametric IRT Rasch models - Rating Scale Model ####################
# eRm::RSM
# mirt:mirt
# 'grsm' and 'grsmIRT' - graded ratings scale model in the slope-interceptand classical IRT parameterization.
# 'grsmIRT'is restricted to unidimensional models (Muraki, 1992)

if ({usinggrsm <- FALSE;usinggrsm}) {
  rst_mirt1 <- mirt::mirt(data = X[,need_particip_var_assigned], model = 1, verbose = T, itemtype= "grsmIRT")
  coef(rst_mirt1)
  for (itemplotn in 1:length(need_particip_var_assigned)) {
    mirt::itemplot(rst_mirt1, itemplotn)
    Sys.sleep(1)
  }
  summary(rst_mirt1)
  residuals(rst_mirt1)
  mirt::fscores(rst_mirt1,method = "EAP") %>% View()
}
# 第五-3-5部份：parametric IRT non-Rasch models - Generalized Partial Credit Model - Polytomous IRT ####################
#################### Finch, W. Holmes＆French, Brian F. (2015). Latent Variable Modeling with R. Florence: Taylor and Francis
## ltm::gpcm
## mirt::mirt by gpcmIRT
## 2016 not fit: gpcm, rasch 1PL all not fit;

if ({usinggpcm <- FALSE;usinggpcm}) {
  gpcmconstraint<-"gpcm" #c("gpcm", "1PL", "rasch")
  X.gpcm<-ltm::gpcm(X[,need_particip_var_assigned],constraint=gpcmconstraint,start.val="random")
  summary(X.gpcm)
  plot(survey_data.gpcm, lwd = 0.8, cex = 0.8, legend = TRUE, cx = "left", xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
  plot(survey_data.gpcm,type=c("IIC"), lwd = 0.8, cex = 0.8, legend = TRUE, cx = "left", xlab = "Latent Trait", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
  ltm::GoF.gpcm(X.gpcm)
}
#margins(fit1)


save(survey_data_imputed, file=paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt.RData"))
