running_platform<-"guicluster"
running_platform<-"computecluster"
running_bigdata_computation<-FALSE
running_bigdata_computation<-TRUE

source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/13_merge_all_datasets.R"), encoding="UTF-8")

# modeling on brm --------------------------------

#Handle Missing Values with brms
#https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html
#DEM 7473 - Bayesian Regression using the INLA Approach
#https://rpubs.com/corey_sparks/431920
#DEM 7473 - Week 7: Bayesian modeling part 1
#https://rpubs.com/corey_sparks/431913
#DEM 7473 - Week 5: Hierarchical Models - Cross level interactions & Contextual Effects
#https://rpubs.com/corey_sparks/424927
#DEM 7473 - Week 3: Basic Hierarchical Models - Random Intercepts and Slopes
#https://rpubs.com/corey_sparks/420770
#DEM 7283 Example 10 - Survey Information and Small Area Estimation
#https://rpubs.com/corey_sparks/484730
#Example of using survey design weights Bayesian regression models for survey data
#https://rpubs.com/corey_sparks/157901
#HLM! 想聽不懂,很難!
#https://www.slideshare.net/beckett53/hlm-20140929
#第 60 章 隨機截距模型中加入共變量 random intercept model with covariates
#https://wangcc.me/LSHTMlearningnote/%E9%9A%A8%E6%A9%9F%E6%88%AA%E8%B7%9D%E6%A8%A1%E5%9E%8B%E4%B8%AD%E5%8A%A0%E5%85%A5%E5%85%B1%E8%AE%8A%E9%87%8F-random-intercept-model-with-covariates.html
#https://bookdown.org/wangminjie/R4SS/
#R-Sessions 16: Multilevel Model Specification (lme4)
#http://www.rensenieuwenhuis.nl/r-sessions-16-multilevel-model-specification-lme4/
#Bayesian mixed effects (aka multi-level) ordinal regression models with brms
#https://kevinstadler.github.io/blog/bayesian-ordinal-regression-with-random-effects-using-brms/
#Building a Multilevel Model in BRMS Tutorial: Popularity Data
#https://www.rensvandeschoot.com/tutorials/brms-started/
#Advanced Bayesian Multilevel Modeling with the R Package brms
#https://cran.r-project.org/web/packages/brms/vignettes/brms_multilevel.pdf
# 58 for dependent data
#https://wangcc.me/LSHTMlearningnote/Hierarchical.html
#the ordinal package, via the clmm and clmm2 functions (clmm = Cumulative Link Mixed Model)
#nests: id
#Hausman test
#https://bookdown.org/tpemartin/econometric_analysis/r-for-panel-data.html#hausman-1
#linear growth model
#https://quantdev.ssri.psu.edu/sites/qdev/files/GCM_Chp3_Tutorial_2.html
#Using R and lme/lmer to fit different two- and three-level longitudinal models
#https://rpsychologist.com/r-guide-longitudinal-lme-lmer

respondmodels_file<-paste0(save_dataset_in_scriptsfile_directory,"analyse_res/respondmodels.RData")
load(file=respondmodels_file, verbose=TRUE)
all_respondmodels_keys<-try(names(all_respondmodels))
all_respondmodels_keys<-if (is(all_respondmodels_keys,'try-error')) c() else all_respondmodels_keys
try(rm(all_respondmodels))
gcreset()

usingpackage<-"ordinal"

if (usingpackage=="brms" & running_bigdata_computation) {
  #load(file=paste0(dataset_in_scriptsfile_directory, "overall_nonagenda_df_sampled.RData"), verbose=TRUE)
  # Ordinal regression modeling patient's rating of inhaler instructions
  # category specific effects are estimated for variable 'treat'
  #要把屆次加入群
  #modelformula<-argrow$formula
  modelformula<-"respondopinion | weights(myown_wr)~1+days_diff_survey_bill_overallscaled+issuefield+(1||issuefield)+(1+issuefield+days_diff_survey_bill_overallscaled+party_pressure_overallscaled||billid_myown)+myown_areakind+(1+days_diff_survey_bill_overallscaled+myown_factoredses_overallscaled+myown_marriage+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+myown_sex+myown_selfid+myown_religion+myown_factoredparticip_overallscaled+similarity_distance_overallscaled+cluster_kamila*myown_factoredparticip_overallscaled||myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+myown_factoredses_overallscaled+myown_marriage+(1|myown_marriage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+myown_sex+myown_selfid+(1|myown_selfid)+myown_religion+(1|myown_religion)+myown_factoredparticip_overallscaled+similarity_distance_overallscaled+(similarity_distance_overallscaled|legislator_name)+cluster_kamila*myown_factoredparticip_overallscaled+cluster_kamila+(1|cluster_kamila)+elec_dist_type+(1+elec_dist_type+seniority_overallscaled||partyGroup:legislator_name)+seniority_overallscaled+party_pressure_overallscaled+(1+elec_dist_type+seniority_overallscaled+party_pressure_overallscaled+partysize+adminparty||partyGroup)+partysize+adminparty+SURVEY" %>%
    brms::brmsformula()
  modelformula<-"respondopinion | weights(myown_wr)~1+days_diff_survey_bill_overallscaled*myown_factoredparticip_overallscaled+days_diff_survey_bill_overallscaled*similarity_distance_overallscaled+days_diff_survey_bill_overallscaled*myown_factoredses_overallscaled+days_diff_survey_bill_overallscaled*cluster_kamila+days_diff_survey_bill_overallscaled*myown_sex+days_diff_survey_bill_overallscaled*myown_selfid+(1+days_diff_survey_bill_overallscaled||admindistrict/id_wth_survey)+(1|billid_myown)+issuefield+myown_factoredses_overallscaled+myown_sex+myown_selfid+similarity_distance_overallscaled*myown_factoredparticip_overallscaled+elec_dist_type+seniority_overallscaled+cluster_kamila+(1|partyGroup/legislator_name)+party_pressure_overallscaled+partysize+SURVEY" %>%
    brms::brmsformula()

  #tprior<-brms::get_prior(formula=modelformula,data = overall_nonagenda_df[sample(nrow(overall_nonagenda_df), 100000), ])
  #tprior<-dplyr::filter( overall_nonagenda_df, billid_myown %in% !!c("7-6-0-14-3","7-6-0-14-9","7-6-0-14-24","7-6-0-14-27","9-2-0-13-1","9-2-0-13-2","9-2-0-13-4","9-2-0-13-5","9-2-0-13-6","9-2-0-13-7","9-2-0-13-8","9-2-0-13-9","9-2-0-16-12","9-2-0-16-15","9-2-0-16-16","9-2-0-16-53","9-2-0-16-57","9-2-0-16-58","9-2-0-16-62","9-2-0-16-64","9-2-0-16-65","9-2-0-16-66","9-2-0-16-67","9-2-0-16-68","9-2-0-16-70","9-2-0-16-72","9-2-0-16-80","9-2-0-16-96","9-2-0-16-98")  ) %>%
  #  brms::get_prior(formula=modelformula,data=.)
  prior1 <- c(brms::set_prior("normal(0.5085828514,0.1)", class = "b", coef = "cluster_kamila.C"),
              brms::set_prior("normal(0.3333249552,0.1)", class = "b", coef = "cluster_kamila.L"),
              brms::set_prior("normal(-0.4338196234,0.1)", class = "b", coef = "cluster_kamila.Q"),
              brms::set_prior("normal(-0.3058793592,0.1)", class = "b", coef = "cluster_kamilaE4"),
              brms::set_prior("normal(1.4566205110,0.1)", class = "b", coef = "cluster_kamilaE5"),
              brms::set_prior("normal(-0.0569702140,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled"),
              brms::set_prior("normal(0.2178754552,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:cluster_kamila.C"),
              brms::set_prior("normal(-0.2320120817,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:cluster_kamila.L"),
              brms::set_prior("normal(0.3002668657,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:cluster_kamila.Q"),
              brms::set_prior("normal(-0.1600304458,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:cluster_kamilaE4"),
              brms::set_prior("normal(0.8573951154,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:cluster_kamilaE5"),
              brms::set_prior("normal(0.1403038135,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:myown_factoredparticip_overallscaled"),
              brms::set_prior("normal(0.5074425821,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:myown_factoredses_overallscaled"),
              brms::set_prior("normal(1.3914550996,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:myown_selfidaboriginal"),
              brms::set_prior("normal(-1.0652859391,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:myown_selfidforeignstate"),
              brms::set_prior("normal(0.4531094029,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:myown_selfidhakka"),
              brms::set_prior("normal(0.01,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:myown_selfidnewresid"),
              brms::set_prior("normal(-0.0586894384,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:myown_sexfemale"),
              brms::set_prior("normal(0.1450243475,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled:similarity_distance_overallscaled"),
              brms::set_prior("normal(-0.0228590321,0.1)", class = "b", coef = "elec_dist_typepartylist"),
              brms::set_prior("normal(0.0048392311,0.1)", class = "b", coef = "issuefieldeco"),
              brms::set_prior("normal(0.4706332133,0.1)", class = "b", coef = "issuefieldesc"),
              brms::set_prior("normal(-0.3639829927,0.1)", class = "b", coef = "issuefieldinteraff"),
              #brms::set_prior("normal(-0.24,0.1)", class = "b", coef = "issuefieldpocivright"),
              #brms::set_prior("normal(-0.24,0.1)", class = "b", coef = "issuefielddiplo"),
              #brms::set_prior("normal(-0.24,0.1)", class = "b", coef = "issuefieldlawaff"),
              #brms::set_prior("normal(-0.004,0.1)", class = "b", coef = "issuefieldsocialwelfare"),
              #brms::set_prior("normal(-0.05,0.1)", class = "b", coef = "issuefieldseright"),
              #brms::set_prior("normal(0.05,0.1)", class = "b", coef = "issuefieldenv"),
              #brms::set_prior("normal(-0.02,0.1)", class = "b", coef = "issuefieldfinance"),
              #brms::set_prior("normal(-0.02,0.1)", class = "b", coef = "issuefieldindp"),
              brms::set_prior("normal(0.2403056082,0.1)", class = "b", coef = "myown_factoredparticip_overallscaled"),
              brms::set_prior("normal(-0.0002353101,0.1)", class = "b", coef = "myown_factoredparticip_overallscaled:similarity_distance_overallscaled"),
              brms::set_prior("normal(0.005,0.1)", class = "b", coef = "myown_factoredses_overallscaled"),
              brms::set_prior("normal(-0.005,0.1)", class = "b", coef = "myown_selfidaboriginal"),
              brms::set_prior("normal(0.005,0.1)", class = "b", coef = "myown_selfidforeignstate"),
              brms::set_prior("normal(0.8124392212,0.1)", class = "b", coef = "myown_selfidhakka"),
              brms::set_prior("normal(-1.8443427853,0.1)", class = "b", coef = "myown_selfidnewresid"),
              brms::set_prior("normal(-0.0535753347,0.1)", class = "b", coef = "myown_sexfemale"),
              brms::set_prior("normal(0.0614349248,0.1)", class = "b", coef = "party_pressure_overallscaled"),
              brms::set_prior("normal(-0.1308563682,0.1)", class = "b", coef = "partysizesmall"),
              brms::set_prior("normal(-0.0114244127,0.1)", class = "b", coef = "seniority_overallscaled"),
              brms::set_prior("normal(-0.2567981031,0.1)", class = "b", coef = "similarity_distance_overallscaled"),
              #brms::set_prior("normal(0.01,0.1)", class = "b", coef = "myown_areakindindustrial"),
              #brms::set_prior("normal(0.01,0.1)", class = "b", coef = "myown_areakindnewlydeveloped"),
              #brms::set_prior("normal(0.01,0.1)", class = "b", coef = "myown_areakindtraditional"),
              #brms::set_prior("normal(-0.01,0.1)", class = "b", coef = "myown_areakindunderdev"),
              #brms::set_prior("normal(-0.01,0.1)", class = "b", coef = "myown_areakindoldandfar"),
              #brms::set_prior("normal(0.01,0.1)", class = "b", coef = "seniority_overallscaled"),
              brms::set_prior("normal(-0.0373555187,0.1)", class = "b", coef = "SURVEY2016citizen"))
  #brms::set_prior("normal(0.005,0.1)", class = "b", coef = "myown_age_overallscaled"),
  #brms::set_prior("normal(0.01,0.1)", class = "b", coef = "myown_marriagemarriaged"),
  #brms::set_prior("normal(-0.01,0.1)", class = "b", coef = "myown_marriagemarriaged_nolivtog"),
  #brms::set_prior("normal(0.01,0.1)", class = "b", coef = "myown_marriagelivtog"),
  #brms::set_prior("normal(0.01,0.1)", class = "b", coef = "myown_marriagedivorced"),
  #brms::set_prior("normal(0.01,0.1)", class = "b", coef = "myown_marriagesep"),
  #brms::set_prior("normal(0.02,0.1)", class = "b", coef = "myown_marriagespousedead"),
  #brms::set_prior("normal(0.01,0.1)", class = "b", coef = "adminparty1"),
  #overall_nonagenda_df_sampled<-overall_nonagenda_df[sample_n_for_df,] %>%
  respondmodels <- dplyr::select(overall_nonagenda_df, -tidyselect::ends_with("NA")) %>%
    dplyr::filter(billid_myown %in% !!c("7-6-0-14-3","7-6-0-14-9","7-6-0-14-24","7-6-0-14-27","9-2-0-13-1","9-2-0-13-2","9-2-0-13-4","9-2-0-13-5","9-2-0-13-6","9-2-0-13-7","9-2-0-13-8","9-2-0-13-9","9-2-0-16-12","9-2-0-16-15","9-2-0-16-16","9-2-0-16-53","9-2-0-16-57","9-2-0-16-58","9-2-0-16-62","9-2-0-16-64","9-2-0-16-65","9-2-0-16-66","9-2-0-16-67","9-2-0-16-68","9-2-0-16-70","9-2-0-16-72","9-2-0-16-80","9-2-0-16-96","9-2-0-16-98")  ) %>%
    droplevels() %>%
    brms::brm(modelformula, data = ., family = brms::cumulative(link = "logit"),
              prior=prior1, chains = 2, cores = parallel::detectCores(), iter = 1500) %>%
    try() %>%
    list() %>%
    magrittr::set_names("brms_responsive")
  #brms:::summary.brmsfit(brmmodelonrespondopinion)
  #brms:::plot.brmsfit(brmmodelonrespondopinion, ask = FALSE)
  #brms::WAIC(brmmodelonrespondopinion)
  #save(brmmodelonrespondopinion, file=paste0(dataset_in_scriptsfile_directory, "brmmodelonrespondopinion.RData"))


} else if (usingpackage=="glmmlasso" & running_bigdata_computation) {
  respondmodel_args<-data.frame("rnd"=c(
    "+(days_diff_survey_bill_overallscaled|admindistrict/id_wth_survey)+(1|billid_myown)+(1|admindistrict/id_wth_survey)+(1|partyGroup/legislator_name)"
  ),stringsAsFactors=FALSE) %>%
    cbind(., withindays = rep(c(1095,183), each = nrow(.) )) %>%
    dplyr::mutate(storekey=paste0(c("1stcondense_timevarying_lasso",withindays),withindays)) %>%
    dplyr::mutate(file = paste0(respondmodels_file,storekey,".RData")) %>%
    dplyr::filter(!(formula %in% !!all_respondmodels_keys))
  overall_nonagenda_df_small<-overall_nonagenda_df[sample(nrow(overall_nonagenda_df), 10000), ] %>%
    dplyr::mutate_if(is.factor, droplevels)
  t<-glmmLasso::glmmLasso(
    fix=respondopinion~1+days_diff_survey_bill_overallscaled*myown_factoredparticip_overallscaled+days_diff_survey_bill_overallscaled*similarity_distance_overallscaled+days_diff_survey_bill_overallscaled*myown_factoredses_overallscaled+days_diff_survey_bill_overallscaled*as.factor(cluster_kamila)+as.factor(issuefield)+myown_factoredses_overallscaled+as.factor(myown_sex)+as.factor(myown_selfid)+similarity_distance_overallscaled*myown_factoredparticip_overallscaled+as.factor(elec_dist_type)+as.factor(cluster_kamila)+party_pressure_overallscaled+partysize+SURVEY,
    rnd=list(admindistrict=~1+days_diff_survey_bill_overallscaled,id_wth_survey=~1+days_diff_survey_bill_overallscaled,billid_myown=~1,legislator_name=~1),
    family=glmmLasso::cumulative(), data = overall_nonagenda_df_small, lambda=10,
    switch.NR=TRUE, control=list(print.iter=TRUE)
    )
} else {
  # "respondopinion~1+(1|billid_myown)",
  # "respondopinion~1+(1|id_wth_survey)",
  # "respondopinion~1+(1|psu)",
  # "respondopinion~1+(1|ssu)",
  # "respondopinion~1+(1|adminvillage)",
  # "respondopinion~1+(1|admindistrict)",
  # "respondopinion~1+(1|admincity)",
  # "respondopinion~1+(1|myown_areakind)",
  # "respondopinion~1+(1|cluster_kamila)",
  # "respondopinion~1+(1|legislator_name)",
  # "respondopinion~1+(1|partyGroup)",
  # "respondopinion~1+(1|SURVEY)",
  #"respondopinion~1+(1|partyGroup/legislator_name)",
  #"respondopinion~1+(1|id_wth_survey/adminvillage/admindistrict/admincity/myown_areakind)",
  #"respondopinion~1+(1|admincity/admindistrict/adminvillage/id_wth_survey)"#,
  #"respondopinion~1+(1|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)",
  #complete
  #"respondopinion~1+days_diff_survey_bill_overallscaled+(days_diff_survey_bill_overallscaled|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+(days_diff_survey_bill_overallscaled|billid_myown)+(1|billid_myown)+issuefield+(1|issuefield)+(issuefield|billid_myown)+myown_factoredses_overallscaled+(myown_factoredses_overallscaled|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+myown_marriage+(1|myown_marriage)+(myown_marriage|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+myown_age_overallscaled+(myown_age_overallscaled|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+myown_age_overallscaled*myown_age_overallscaled+(myown_age_overallscaled*myown_age_overallscaled|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+myown_sex+(myown_sex|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+myown_selfid+(1|myown_selfid)+(myown_selfid|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+myown_religion+(1|myown_religion)+(myown_religion|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+similarity_distance_overallscaled+(similarity_distance_overallscaled|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+(similarity_distance_overallscaled|legislator_name)+myown_factoredparticip_overallscaled+(myown_factoredparticip_overallscaled|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+(1|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+cluster_kamila*myown_factoredparticip_overallscaled+(cluster_kamila*myown_factoredparticip_overallscaled|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+cluster_kamila+(1|cluster_kamila)+elec_dist_type+(elec_dist_type|partyGroup/legislator_name)+seniority_overallscaled+(seniority_overallscaled|partyGroup/legislator_name)+(1|partyGroup/legislator_name)+party_pressure_overallscaled+(party_pressure_overallscaled|partyGroup)+(party_pressure_overallscaled|billid_myown)+partysize+(partysize|partyGroup)+adminparty+(adminparty|partyGroup)+SURVEY",
  #"respondopinion~1+(1|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+(1|issuefield)+(1|cluster_kamila)+(1|elec_dist_type)"
  
  #c("7-6-0-14-3","7-6-0-14-9","7-6-0-14-24","7-6-0-14-27","7-6-0-15-8","7-6-0-15-11","7-6-0-15-12")
  # 7-6-0-14-3
  # 7-6-0-14-9
  # 7-6-0-14-24
  # 7-6-0-14-27
  # 7-6-0-15-8
  # 7-6-0-15-11
  # 7-6-0-15-12 half yr to here
  # 7-7-0-17-3
  # 7-7-0-17-4
  # 7-7-0-17-22
  # 7-7-0-17-27
  # 7-7-0-17-29
  # 7-7-0-17-30
  #c("9-2-0-13-1","9-2-0-13-2","9-2-0-13-4","9-2-0-13-5","9-2-0-13-6","9-2-0-13-7","9-2-0-13-8","9-2-0-13-9","9-2-0-16-12","9-2-0-16-15","9-2-0-16-16","9-2-0-16-53","9-2-0-16-57","9-2-0-16-58","9-2-0-16-62","9-2-0-16-64","9-2-0-16-65","9-2-0-16-66","9-2-0-16-67","9-2-0-16-68","9-2-0-16-70","9-2-0-16-72","9-2-0-16-80","9-2-0-16-96","9-2-0-16-98","9-2-0-17-34","9-2-0-17-35","9-2-0-17-36","9-2-0-17-37","9-2-0-17-38","9-2-0-17-39","9-2-0-17-41","9-2-0-17-61")
  # 9-2-0-13-1
  # 9-2-0-13-2
  # 9-2-0-13-4
  # 9-2-0-13-5
  # 9-2-0-13-6
  # 9-2-0-13-7
  # 9-2-0-13-8
  # 9-2-0-13-9
  # 9-2-0-16-12
  # 9-2-0-16-15
  # 9-2-0-16-16
  # 9-2-0-16-53
  # 9-2-0-16-57
  # 9-2-0-16-58
  # 9-2-0-16-62
  # 9-2-0-16-64
  # 9-2-0-16-65
  # 9-2-0-16-66
  # 9-2-0-16-67
  # 9-2-0-16-68
  # 9-2-0-16-70
  # 9-2-0-16-72
  # 9-2-0-16-80
  # 9-2-0-16-96
  # 9-2-0-16-98
  # 9-2-0-17-34
  # 9-2-0-17-35
  # 9-2-0-17-36
  # 9-2-0-17-37
  # 9-2-0-17-38
  # 9-2-0-17-39
  # 9-2-0-17-41
  # 9-2-0-17-61
  # 9-2-1-1-2
  # 9-2-1-1-4
  # 9-2-1-1-5
  # 9-2-1-2-1
  # 9-2-1-2-2
  # 9-2-1-2-3
  # 9-2-1-2-4
  # 9-2-1-2-5
  # 9-2-1-2-14
  # 9-2-1-2-18
  # 9-2-1-2-42
  # 9-2-1-2-47
  # 9-2-1-2-53
  # 9-2-1-2-55
  # 9-2-1-2-58 half yr to here
  # 9-3-0-10-1
  # 9-3-0-15-1
  # 9-3-1-2-1
  # 9-3-1-2-2
  # 9-3-1-2-3
  # 9-3-1-2-4
  # 9-3-1-2-5
  # 9-3-1-2-6
  # 9-3-1-2-7
  # 9-3-1-2-8
  # 9-3-1-3-4
  # 9-3-1-3-5
  # 9-3-1-3-6
  # 9-3-1-3-8
  # 9-3-1-3-11
  # 9-3-1-3-12
  # 9-3-1-3-13
  # 9-3-1-3-14
  # 9-3-1-3-15
  # 9-3-1-3-16
  # 9-3-1-3-17
  # 9-3-1-3-18
  # 9-3-1-3-20
  # 9-3-1-3-21
  # 9-3-1-3-22
  # 9-3-1-3-23
  # 9-3-1-3-24
  # 9-3-1-3-25
  # 9-3-1-3-26
  # 9-3-1-3-28
  # 9-3-3-2-629
  # 9-3-3-2-630
  # 9-3-3-2-631
  # 9-3-3-2-632
  # 
  #"7-6-0-14-3","7-6-0-14-9","7-6-0-14-24","7-6-0-14-27","9-2-0-13-1","9-2-0-13-2","9-2-0-13-4","9-2-0-13-5","9-2-0-13-6","9-2-0-13-7","9-2-0-13-8","9-2-0-13-9","9-2-0-16-12","9-2-0-16-15","9-2-0-16-16","9-2-0-16-53","9-2-0-16-57","9-2-0-16-58","9-2-0-16-62","9-2-0-16-64","9-2-0-16-65","9-2-0-16-66","9-2-0-16-67","9-2-0-16-68","9-2-0-16-70","9-2-0-16-72","9-2-0-16-80","9-2-0-16-96","9-2-0-16-98"
  respondmodel_args<-data.frame("formula"=c(
    "respondopinion~1+days_diff_survey_bill_overallscaled*myown_factoredparticip_overallscaled+days_diff_survey_bill_overallscaled*similarity_distance_overallscaled+days_diff_survey_bill_overallscaled*myown_factoredses_overallscaled+days_diff_survey_bill_overallscaled*cluster_kamila+days_diff_survey_bill_overallscaled*myown_sex+days_diff_survey_bill_overallscaled*myown_selfid+(days_diff_survey_bill_overallscaled|admindistrict/id_wth_survey)+(1|billid_myown)+issuefield+myown_factoredses_overallscaled+myown_sex+myown_selfid+similarity_distance_overallscaled*myown_factoredparticip_overallscaled+(1|admindistrict/id_wth_survey)+elec_dist_type+seniority_overallscaled+cluster_kamila+(1|partyGroup/legislator_name)+party_pressure_overallscaled+partysize+SURVEY",
    "1+(1|billid_myown)+(1|issuefield)+(1|legislator_name)+(1|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+(1|myown_marriage)+(1|myown_selfid)+(1|myown_religion)+(1|cluster_kamila)+(1|partyGroup/legislator_name)+(1|partyGroup)+SURVEY"
  ),stringsAsFactors=FALSE) %>%
    cbind(., withindays = rep(c(1095,80), each = nrow(.) )) %>% #183
    dplyr::mutate(storekey=paste0(c("1stcondense_timevarying_halfyr","null"),withindays)) %>%
    dplyr::mutate(file = paste0(respondmodels_file,storekey,".RData")) %>%
    dplyr::filter(!(formula %in% !!all_respondmodels_keys))
  if (FALSE) { #test
    overall_nonagenda_df_small<-overall_nonagenda_df[sample(nrow(overall_nonagenda_df), 100000), ] %>%
      dplyr::mutate(respondopinion_conti=as.integer(respondopinion)) %>%
      dplyr::mutate_if(is.factor, droplevels)
  }
  fi<-3
  fikey<-respondmodel_args$storekey[fi]
  loopargdf<-respondmodel_args
  #respondmodels<-custom_apply_thr_argdf(respondmodel_args, "storekey", function(fikey, loopargdf, datadf, ...) {
  argrow<-dplyr::filter(loopargdf, storekey==!!fikey)
  
  respondmodels<-dplyr::select(overall_nonagenda_df, -tidyselect::ends_with("NA")) %>%
    #dplyr::filter(overall_nonagenda_df) %>% #, days_diff_survey_bill<=!!argrow$withindays
    dplyr::filter( billid_myown %in% !!c("7-6-0-14-3","7-6-0-14-9","7-6-0-14-24","7-6-0-14-27","9-2-0-13-1","9-2-0-13-2","9-2-0-13-4","9-2-0-13-5","9-2-0-13-6","9-2-0-13-7","9-2-0-13-8","9-2-0-13-9","9-2-0-16-12","9-2-0-16-15","9-2-0-16-16","9-2-0-16-53","9-2-0-16-57","9-2-0-16-58","9-2-0-16-62","9-2-0-16-64","9-2-0-16-65","9-2-0-16-66","9-2-0-16-67","9-2-0-16-68","9-2-0-16-70","9-2-0-16-72","9-2-0-16-80","9-2-0-16-96","9-2-0-16-98")  ) %>%
    droplevels() %>%
    {list(formula=as.formula(argrow$formula), data=., weights=magrittr::use_series(., "myown_wr"), 
        Hess=TRUE, model = TRUE, link = "logit", threshold = "flexible")} %>%
    do.call(ordinal::clmm, args=.) %>%
    # {ordinal::clmm(formula=f, data=., weights=magrittr::use_series(., "myown_wr"), Hess=TRUE, model = TRUE, link = "logit",
    #               threshold = "flexible")} %>%#c("flexible", "symmetric", "symmetric2", "equidistant")
    try() 
  tryn<-1
  while (TRUE) {
    loadsavestatus<-try({
      load(file=argrow$file, verbose=TRUE)
      all_respondmodels<- respondmodels %>%
        list() %>%
        magrittr::set_names(argrow$storekey) %>%
        rlist::list.merge(all_respondmodels, .)
      save(all_respondmodels, file=argrow$file)
    })
    tryn<-tryn+1
    if(!is(loadsavestatus, 'try-error') | tryn>10) break
  }
  
  # return(retmodel[[1]]) }, datadf=overall_nonagenda_df, mc.cores=1)
  
  if (FALSE) {
    lapply(all_respondmodels, function(X) {try(ordinal:::summary.clmm(X))})
    ordinal:::summary.clmm(all_respondmodels[[1]])
  }
}




  

load(file=respondmodels_file, verbose=TRUE)
if (length(all_respondmodels)==0 | identical(all_respondmodels, list(a=1))) {
  all_respondmodels<-respondmodels
} else {
  all_respondmodels<-rlist::list.merge(all_respondmodels,respondmodels)
}
save(all_respondmodels,file=respondmodels_file)
message(respondmodel_args$file[fi])
save(respondmodels,file=respondmodel_args$file[fi])