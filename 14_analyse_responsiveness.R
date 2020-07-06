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

respondmodel_args<-data.frame("formula"=c(
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
  "respondopinion~1+days_diff_survey_bill_overallscaled+issuefield+myown_factoredses_overallscaled+myown_marriage+myown_areakind+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+myown_sex+myown_selfid+myown_religion+similarity_distance_overallscaled+similarity_distance_overallscaled*(-1)*myown_factoredparticip_overallscaled+myown_factoredparticip_overallscaled+(1|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+cluster_kamila*myown_factoredparticip_overallscaled+(cluster_kamila*myown_factoredparticip_overallscaled|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+cluster_kamila+(1|cluster_kamila)+elec_dist_type+seniority_overallscaled+(1|partyGroup/legislator_name)+party_pressure_overallscaled+partysize+adminparty+SURVEY",
  "1+(1|billid_myown)+(1|issuefield)+(1|legislator_name)+(1|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+(1|myown_marriage)+(1|myown_selfid)+(1|myown_religion)+(1|cluster_kamila)+(1|partyGroup/legislator_name)+(1|partyGroup)+SURVEY"
  #"respondopinion~1+(1|myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+(1|issuefield)+(1|cluster_kamila)+(1|elec_dist_type)"
),stringsAsFactors=FALSE) %>%
  cbind(., withindays = rep(c(1095,365), each = nrow(.) )) %>%
  cbind(., file = rep(respondmodels_file, each = nrow(.) ), stringsAsFactors=FALSE) %>%
  dplyr::mutate(storekey=paste0(c("eff","null"),withindays)) %>%
  dplyr::filter(!(formula %in% !!all_respondmodels_keys))

usingpackage<-"ordinal"

if (usingpackage=="brms" & running_bigdata_computation) {
  #load(file=paste0(dataset_in_scriptsfile_directory, "overall_nonagenda_df_sampled.RData"), verbose=TRUE)
  # Ordinal regression modeling patient's rating of inhaler instructions
  # category specific effects are estimated for variable 'treat'
  #要把屆次加入群
  #modelformula<-argrow$formula
  modelformula<-"respondopinion | weights(myown_wr)~1+days_diff_survey_bill_overallscaled+issuefield+(1||issuefield)+(1+issuefield+days_diff_survey_bill_overallscaled+party_pressure_overallscaled||billid_myown)+myown_areakind+(1+days_diff_survey_bill_overallscaled+myown_factoredses_overallscaled+myown_marriage+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+myown_sex+myown_selfid+myown_religion+myown_factoredparticip_overallscaled+similarity_distance_overallscaled+cluster_kamila*myown_factoredparticip_overallscaled||myown_areakind/admincity/admindistrict/adminvillage/id_wth_survey)+myown_factoredses_overallscaled+myown_marriage+(1|myown_marriage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+myown_sex+myown_selfid+(1|myown_selfid)+myown_religion+(1|myown_religion)+myown_factoredparticip_overallscaled+similarity_distance_overallscaled+(similarity_distance_overallscaled|legislator_name)+cluster_kamila*myown_factoredparticip_overallscaled+cluster_kamila+(1|cluster_kamila)+elec_dist_type+(1+elec_dist_type+seniority_overallscaled||partyGroup:legislator_name)+seniority_overallscaled+party_pressure_overallscaled+(1+elec_dist_type+seniority_overallscaled+party_pressure_overallscaled+partysize+adminparty||partyGroup)+partysize+adminparty+SURVEY" %>%
    brms::brmsformula()

  #tprior<-brms::get_prior(formula=modelformula,data = overall_nonagenda_df[sample(nrow(overall_nonagenda_df), 100000), ])
  prior1 <- c(brms::set_prior("normal(-0.005,0.1)", class = "b", coef = "similarity_distance_overallscaled"),
              brms::set_prior("normal(0.005,0.1)", class = "b", coef = "myown_age_overallscaled"),
              brms::set_prior("normal(0.005,0.1)", class = "b", coef = "myown_factoredses_overallscaled"),
              brms::set_prior("normal(-0.005,0.1)", class = "b", coef = "myown_sexfemale"),
              brms::set_prior("normal(-0.005,0.1)", class = "b", coef = "myown_selfidhakka"),
              brms::set_prior("normal(-0.005,0.1)", class = "b", coef = "myown_selfidaboriginal"),
              brms::set_prior("normal(0.005,0.1)", class = "b", coef = "myown_selfidforeignstate"),
              brms::set_prior("normal(-0.005,0.1)", class = "b", coef = "myown_selfidnewresid"),
              brms::set_prior("normal(0.01,0.1)", class = "b", coef = "myown_marriagemarriaged"),
              brms::set_prior("normal(-0.01,0.1)", class = "b", coef = "myown_marriagemarriaged_nolivtog"),
              brms::set_prior("normal(0.01,0.1)", class = "b", coef = "myown_marriagelivtog"),
              brms::set_prior("normal(0.01,0.1)", class = "b", coef = "myown_marriagedivorced"),
              brms::set_prior("normal(0.01,0.1)", class = "b", coef = "myown_marriagesep"),
              brms::set_prior("normal(0.02,0.1)", class = "b", coef = "myown_marriagespousedead"),
              brms::set_prior("normal(0.01,0.1)", class = "b", coef = "myown_areakindindustrial"),
              brms::set_prior("normal(0.01,0.1)", class = "b", coef = "myown_areakindnewlydeveloped"),
              brms::set_prior("normal(0.01,0.1)", class = "b", coef = "myown_areakindtraditional"),
              brms::set_prior("normal(-0.01,0.1)", class = "b", coef = "myown_areakindunderdev"),
              brms::set_prior("normal(-0.01,0.1)", class = "b", coef = "myown_areakindoldandfar"),
              brms::set_prior("normal(0.01,0.1)", class = "b", coef = "cluster_kamila.C"),
              brms::set_prior("normal(0.01,0.1)", class = "b", coef = "cluster_kamila.L"),
              brms::set_prior("normal(-0.01,0.1)", class = "b", coef = "cluster_kamila.Q"),
              brms::set_prior("normal(0.01,0.1)", class = "b", coef = "cluster_kamilaE4"),
              brms::set_prior("normal(-0.02,0.1)", class = "b", coef = "cluster_kamilaE5"),
              brms::set_prior("normal(0.1,0.1)", class = "b", coef = "myown_factoredparticip_overallscaled"),
              brms::set_prior("normal(-0.2,0.1)", class = "b", coef = "party_pressure_overallscaled"),
              brms::set_prior("normal(0.01,0.1)", class = "b", coef = "seniority_overallscaled"),
              brms::set_prior("normal(0.01,0.1)", class = "b", coef = "days_diff_survey_bill_overallscaled"),
              brms::set_prior("normal(-0.01,0.1)", class = "b", coef = "elec_dist_typepartylist"),
              brms::set_prior("normal(0.01,0.1)", class = "b", coef = "adminparty1"),
              brms::set_prior("normal(-0.4,0.1)", class = "b", coef = "issuefieldinteraff"),
              brms::set_prior("normal(-0.24,0.1)", class = "b", coef = "issuefieldpocivright"),
              brms::set_prior("normal(-0.24,0.1)", class = "b", coef = "issuefielddiplo"),
              brms::set_prior("normal(-0.24,0.1)", class = "b", coef = "issuefieldlawaff"),
              brms::set_prior("normal(0.24,0.1)", class = "b", coef = "issuefieldesc"),
              brms::set_prior("normal(-0.004,0.1)", class = "b", coef = "issuefieldsocialwelfare"),
              brms::set_prior("normal(0.05,0.1)", class = "b", coef = "issuefieldeco"),
              #brms::set_prior("normal(-0.05,0.1)", class = "b", coef = "issuefieldseright"),
              brms::set_prior("normal(0.05,0.1)", class = "b", coef = "issuefieldenv"),
              brms::set_prior("normal(-0.02,0.1)", class = "b", coef = "issuefieldfinance"),
              brms::set_prior("normal(-0.02,0.1)", class = "b", coef = "issuefieldindp"),
              brms::set_prior("normal(-0.04,0.1)", class = "b", coef = "SURVEY2016citizen"))
  #overall_nonagenda_df_sampled<-overall_nonagenda_df[sample_n_for_df,] %>%
  respondmodels <- brms::brm(modelformula, data = overall_nonagenda_df, family = brms::cumulative(link = "logit"),
                             prior=prior1, chains = 2, cores = parallel::detectCores(), iter = 1500) %>%
    try() %>%
    list() %>%
    magrittr::set_names("brms_responsive")
  #brms:::summary.brmsfit(brmmodelonrespondopinion)
  #brms:::plot.brmsfit(brmmodelonrespondopinion, ask = FALSE)
  #brms::WAIC(brmmodelonrespondopinion)
  #save(brmmodelonrespondopinion, file=paste0(dataset_in_scriptsfile_directory, "brmmodelonrespondopinion.RData"))


} else {
  
  if (FALSE) { #test
    overall_nonagenda_df_small<-overall_nonagenda_df[sample(nrow(overall_nonagenda_df), 100000), ] %>%
      dplyr::mutate(respondopinion_conti=as.integer(respondopinion)) %>%
      dplyr::mutate_if(is.factor, droplevels)
  }
  
  fikey<-respondmodel_args$storekey[1]
  loopargdf<-respondmodel_args
  #respondmodels<-custom_apply_thr_argdf(respondmodel_args, "storekey", function(fikey, loopargdf, datadf, ...) {
  argrow<-dplyr::filter(loopargdf, storekey==!!fikey)
  respondmodels<- dplyr::filter(overall_nonagenda_df, days_diff_survey_bill<=!!argrow$withindays) %>%
  {list(formula=as.formula(argrow$formula), data=., weights=magrittr::use_series(., "myown_wr"), 
        Hess=TRUE, model = TRUE, link = "logit", threshold = "flexible")} %>%
    do.call(ordinal::clmm, args=.) %>%
    # {ordinal::clmm(formula=f, data=., weights=magrittr::use_series(., "myown_wr"), Hess=TRUE, model = TRUE, link = "logit",
    #               threshold = "flexible")} %>%#c("flexible", "symmetric", "symmetric2", "equidistant")
    try() 
  tryn<-1
  while (FALSE) {
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