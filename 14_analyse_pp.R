running_platform<-"guicluster"
running_platform<-"computecluster"
running_bigdata_computation<-FALSE
#running_bigdata_computation<-TRUE

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
#Advanced Bayesian Multilevel Modelingwith the R Package brms (syntax)
#https://arxiv.org/pdf/1705.11123.pdf
#fixed effect v. random effect
#https://zhuanlan.zhihu.com/p/60528092
#/pkg/rproject/R-latest/bin/R
#ploting analysing result
#https://biol609.github.io/lectures/23c_brms_prediction.html#243_assessing_fit
#easy bayes
#https://m-clark.github.io/easy-bayes/posterior-predictive-checks.html
#calc p-value hypothesis testing
#https://www.rensvandeschoot.com/tutorials/brms-started/
#https://bookdown.org/content/4253/introducing-the-multilevel-model-for-change.html
#https://m-clark.github.io/mixed-models-with-R/
#svylme
#https://rdrr.io/github/tslumley/svylme/
#multiple imputation https://cran.r-project.org/web/packages/merTools/vignettes/imputation.html
#time to use random effect https://ah.nccu.edu.tw/bitstream/140.119/37427/8/503008.pdf
#lmerTest
#merTools
#sjstats
#data transformation
#https://molecular-service-science.com/2012/02/18/data-transformation/
#r-glmmadaptive

#nests: id

# * check kamila result ----------------------------------------------


# prepare data: matching different surveys and centering ----------------------------
survey_with_idealpoint_name<-paste0(save_dataset_in_scriptsfile_directory, "miced_survey_2surveysonly_mirt_lca_clustering_idealpoints.RData")
load(file=survey_with_idealpoint_name, verbose=TRUE)
merged_acrossed_surveys_list<-ret_merged_for_idealpoint_and_pp_df_list(survey_data_imputed, dataset_in_scriptsfile_directory, minuspolicy=TRUE)
adopting_transformation_method<-try(lapply(merged_acrossed_surveys_list_with_normality, function(X) {X[[2]]$chosen_transform}))

# inspecting data distributions ------------
if ({plotting_to_inspect_distribution<-FALSE;plotting_to_inspect_distribution}) {
  merged_acrossed_surveys_overall<-dplyr::bind_rows(merged_acrossed_surveys_list)
  plotsvykey<-"2016citizen"
  plotsvykey<-"2010overall"
  dplyr::filter(merged_acrossed_surveys_overall, .imp==1) %>%
    #magrittr::use_series("myown_factoredparticip") %>%
    #shapiro.test()
    custom_plot("myown_factoredparticip_scaled","myown_wr")
  #dplyr::select(!!need_particip_var[[plotsvykey]], myown_factoredparticip) %>%
  #View()
  merged_acrossed_surveys_overall %>%
    #dplyr::filter(merged_acrossed_surveys_overall, .imp==1, SURVEY==!!plotsvykey) %>%
  {
    custom_plot(., "myown_factoredparticip","myown_wr") %>% print()
    custom_plot(., "myown_factoredparticip_scaled","myown_wr") %>% print()
    #t<-magrittr::use_series(., "myown_factoredparticip") %>%
    #  bestNormalize::bestNormalize()
  }
  t<-bestNormalize::bestNormalize(merged_acrossed_surveys_overall$myown_factoredparticip)
  
}


# all possible models ----------------------------
ppmodels_file<-paste0(save_dataset_in_scriptsfile_directory,"/analyse_res/ppmodels.RData")
load(file=ppmodels_file, verbose=TRUE)
ppmodel_args<-data.frame("formula"=c(
  #deduct myown_marriage myown_religion myown_areakind
  "myown_factoredparticip_ordinal~1+SURVEY+cluster_kamila+(1|cluster_kamila)+myown_factoredses_overallscaled+myown_sex+myown_selfid+myown_religion+myown_age_overallscaled+myown_areakind+(1|myown_areakind/admindistrict/adminvillage)+(1|admincity)"
  #"myown_factoredparticip_ordinal~1+(1|myown_areakind/admincity/admindistrict/adminvillage)+(1|cluster_kamila)+(1|SURVEY)"
  #"myown_factoredparticip_ordinal~1+(1|myown_areakind/admincity/admindistrict/adminvillage)",
  # "myown_factoredparticip_ordinal~((1+cluster_kamila)|SURVEY)",
  # "myown_factoredparticip_ordinal~1+(1|myown_areakind/admincity/admindistrict/adminvillage)",
  # "myown_factoredparticip_ordinal~1+(1|admincity/admindistrict/adminvillage)",
  # "myown_factoredparticip_ordinal~1+myown_factoredses_overallscaled+(myown_factoredses_overallscaled|myown_areakind/admincity/admindistrict/adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+myown_selfid+myown_areakind+cluster_kamila+myown_religion+(1|cluster_kamila)+(1|myown_areakind/admincity/admindistrict/adminvillage)",
  # "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+myown_age_overallscaled+(1|adminvillage)",
  # "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+(1|adminvillage)",
  # "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled*myown_age_overallscaled+SURVEY+(1|adminvillage)",
  # "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+(1|adminvillage)",
  # "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+(1|adminvillage)",
  # "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+(1|adminvillage)",
  # "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+(1|adminvillage)",
  # "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+myown_selfid+(1|adminvillage)",
  # "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+myown_selfid+myown_areakind+(1|adminvillage)",
  # "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+myown_selfid+myown_areakind+cluster_kamila+(1|adminvillage)",
  # "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+myown_selfid+myown_areakind+cluster_kamila+myown_religion+(1|adminvillage)",
  # "myown_factoredparticip_ordinal~(myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+myown_selfid+myown_areakind+cluster_kamila+myown_religion)|adminvillage"
  #complete
  
  #"myown_factoredparticip_ordinal~myown_factoredses_overallscaled+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+myown_selfid+myown_areakind+cluster_kamila",
  #"myown_factoredparticip_ordinal~myown_factoredses_overallscaled+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+myown_selfid+myown_areakind+cluster_kamila+(1|cluster_kamila)",
  #"myown_factoredparticip_ordinal~myown_factoredses_overallscaled+myown_age_overallscaled+SURVEY+cluster_kamila+(1|cluster_kamila)",
  #"myown_factoredparticip_ordinal~myown_factoredses_overallscaled+myown_age_overallscaled+myown_marriage+myown_sex+myown_selfid+cluster_kamila+myown_areakind+SURVEY+1+(1|cluster_kamila)",
  #FULL
  #"myown_factoredparticip_ordinal~myown_factoredses_overallscaled+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+myown_marriage+myown_sex+myown_selfid+cluster_kamila+myown_areakind+SURVEY+1+(1|cluster_kamila)"#,
  #"myown_factoredparticip_ordinal~myown_factoredses_overallscaled+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+myown_marriage+myown_sex+myown_selfid+cluster_kamila+myown_areakind+SURVEY+1+(1|cluster_kamila)"#,
  #"myown_factoredparticip~myown_factoredses_overallscaled+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+myown_marriage+myown_sex+myown_selfid+cluster_kamila+myown_areakind+SURVEY+cluster_kamila*SURVEY+1+(1|cluster_kamila)"#,
  #"myown_factoredparticip~(1|adminvillage/admindistrict/admincity/myown_areakind/cluster_kamila/SURVEY)"
), stringsAsFactors=FALSE) %>%
  cbind(., needimp = rep(1:6, each = nrow(.)), stringsAsFactors=FALSE) %>%
  dplyr::mutate(storekey=paste0(formula,needimp)) %>%
  dplyr::filter(!(formula %in% !!names(all_ppmodels)))

savemodelfilename<-"pp_efficient_full"
#merged_acrossed_surveys_list[[1]]$
#library(lme4)
ppmodels<-custom_apply_thr_argdf(ppmodel_args, "storekey", function(fikey, loopargdf, datadf, ...) {
  needrow<-dplyr::filter(loopargdf, storekey==!!fikey)
  f<-needrow$formula %>% as.formula()
  needimp<-needrow$needimp
    # lme4::glmer(formula=., data={
    #     dplyr::mutate_at(datadf, "myown_factoredparticip", ~myown_factoredparticip-min(myown_factoredparticip)+1 )
    # }, family = Gamma) %>% #, family = poisson(link = "log")
    #robustlmm::rlmerRcpp(formula=., data=datadf) %>%
    #lme4::lmer(formula=., data=datadf) %>%
    #lme4::bootMer(x=., data=datadf) %>%
  retmodel<-datadf[[needimp]] %>%
    {list(
      formula=as.formula(needrow$formula),
      data=., weights=.$myown_wr, Hess=TRUE, model = TRUE, link = "logit",
      threshold = "flexible"
    )} %>%
    do.call(ordinal::clmm, args=.) %>%
    try()
    #@() %>% #c("flexible", "symmetric", "symmetric2", "equidistant")
  return(retmodel)
}, datadf=merged_acrossed_surveys_list) #

try({
  load(file=ppmodels_file, verbose=TRUE)
  if (length(all_ppmodels)==0 | identical(all_ppmodels, list(a=1))) {
    all_ppmodels<-ppmodels
  } else {
    all_ppmodels<-rlist::list.merge(all_ppmodels,ppmodels)
  }
  try(save(all_ppmodels,file=ppmodels_file))
})
try(save(ppmodels, file=paste0(save_dataset_in_scriptsfile_directory,"analyse_res/",savemodelfilename,".RData")))

if (FALSE) {
  lapply(all_ppmodels, function(X) {try(ordinal:::summary.clmm(X))})
  lapply(ppmodels, lme4:::summary.merMod, signif.stars=TRUE)
  lapply(ppmodels, summary, signif.stars=TRUE)
  lapply(ppmodels, function(X) {sum(lme4:::residuals.merMod(X))} )
  lapply(ppmodels, function(X) {hist(lme4:::residuals.merMod(X), breaks = 100)} )
  lapply(ppmodels, function(X) {shapiro.test(lme4:::residuals.merMod(X))} )
  lapply(ppmodels, robustlmm:::summary.rlmerMod)
  lapply(ppmodels, function(X) { sum(robustlmm:::residuals.rlmerMod(X))  } )
  lapply(ppmodels, function(X) { hist(robustlmm:::residuals.rlmerMod(X), breaks = 100)  } )
  lapply(ppmodels, function(X) { shapiro.test(robustlmm:::residuals.rlmerMod(X))  } )
}



if ({bayesian<-FALSE;bayesian}) {
  #ses efficacy marriage age sex selfid psu ssu areakind clusterkamila SURVEY
  pp_to_cluster_nullmod <-
    brms::brm(#
      brms::bf(myown_factoredparticip_scaled|weights(myown_wr)~(1|adminvillage/admindistrict/admincity/myown_areakind/cluster_kamila/SURVEY) ),
      prior=prior1,
      family=brms::dskew_normal(
        x,
        mu = 0,
        sigma = 1,
        alpha = -2,
        xi = NULL,
        omega = NULL,
        log = FALSE
      ), #brms::student,
      data = merged_acrossed_surveys_list[[1]],
      chains=4,
      cores=parallel::detectCores(),
      iter = 2000,
      sample_prior = TRUE
      #,file = here::here("data/policyidealpoint_cos_similarity_to_median_to_kamila-robust")
    )
}




if (FALSE) {
  brms::get_prior(formula=brms::bf(myown_factoredparticip_scaled|weights(myown_wr)~(1|adminvillage/admindistrict/admincity/myown_areakind/cluster_kamila/SURVEY)),data = merged_acrossed_surveys_list[[1]])
  brms::get_prior(formula=brms::bf(myown_factoredparticip_scaled|weights(myown_wr)~((myown_factoredses_overallscaled+myown_marriage+myown_age_overallscaled+myown_age*myown_age+myown_sex+myown_selfid+myown_areakind+cluster_kamila+cluster_kamila*SURVEY)|SURVEY) +(1|cluster_kamila/SURVEY) ),data = merged_acrossed_surveys_list[[1]])
  
  prior1 <- c(brms::set_prior("normal(-10,100)", class = "b", coef = "extrav"),
              brms::set_prior("normal(10,100)", class = "b", coef = "extrav:texp"),
              brms::set_prior("normal(-5,100)", class = "b", coef = "sex"),
              brms::set_prior("normal(-5,100)", class = "b", coef = "texp"),
              brms::set_prior("normal(10,100)", class = "b", coef = "intercept" ))
  
  pp_to_cluster_mod1 <-
    brms::brm(#
      brms::bf(myown_factoredparticip_scaled|weights(myown_wr)~(myown_factoredses_overallscaled+myown_marriage+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+myown_sex+myown_selfid+myown_areakind+cluster_kamila+cluster_kamila*SURVEY)|SURVEY+(1|cluster_kamila/SURVEY) ),
      prior=prior1,
      family=brms::dskew_normal(
        x,
        mu = 0,
        sigma = 1,
        alpha = -2,
        xi = NULL,
        omega = NULL,
        log = FALSE
      ), #brms::student,
      data = merged_acrossed_surveys_list[[1]],
      chains=4,
      cores=parallel::detectCores(),
      iter = 2000,
      sample_prior = TRUE
      #,file = here::here("data/policyidealpoint_cos_similarity_to_median_to_kamila-robust")
    )
  
  brms:::summary.brmsfit(cossim_to_cluster_mod_robusts)
  brms::pp_check(cossim_to_cluster_mod_robusts)
  brms::pp_check(cossim_to_cluster_mod_robusts, group="cluster_kamila")
  
  #load(file=paste0(dataset_in_scriptsfile_directory,"brms/test_cossim_to_cluster_mod_robusts.RData"), verbose=TRUE)
  save(pp_to_cluster_nullmod, file=paste0(dataset_in_scriptsfile_directory,"brms/pp_to_cluster_nullmod.RData"))
  
  
}




if (FALSE) {
  modeltype<-"(very_precious_efficient_full)"
  modeltype<-"(very_precious_efficient_without_marriage)"
  modeltype<-"(very_precious_efficient_without_religion)"
  modeltype<-"(very_precious_efficient_without_areakind)"
  modeltype<-"(very_precious_efficient_without_age)"
  modeltype<-"(very_precious_efficient_without_age_areakind)"
  modeltype<-"(very_precious_efficient_without_areakind_marriage)"
  modeltype<-"(very_precious_efficient_without_areakind_religion)"
  modeltype<-""
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/ppmodels",modeltype,".RData"), verbose=TRUE)
  #all_ppmodels<-all_ppmodels[7:12] names(all_ppmodels)
  #save(all_ppmodels, file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/ppmodels",modeltype,".RData"))
  t<-all_ppmodels[[1]]
  t<-lapply(all_ppmodels[1], function(X) {ordinal:::summary.clmm(X)})
  t<-t[[1]]
  t$coefficients
  t$ST
  t$optRes
  t$formula
  t$info
  #pooling https://rdrr.io/github/DaanNieboer/ordinalimputation/api/
  t<-mice::as.mira(ppmodels)
  t<-mice::as.mira(all_ppmodels)
  pv<-mice::pool(t)
  pooledppres<-pooling.clmm(t$analyses)
  summarytable<-cbind(summary(pv), pooledppres$fixed_effects)
  print(summarytable)
  write.csv(summarytable, "TMP.csv")
  print(pooledppres$random_dist)
  
  get_vcov(t$analyses[[1]])
  pp_fullmod<-ppmodels[[1]]
  pp_noagemod<-ppmodels[[1]]
  pp_noareakindmod<-ppmodels[[1]]
  pp_noreligionmod<-ppmodels[[1]]
  MuMIn::model.sel(pp_fullmod,pp_noagemod,pp_noareakindmod,pp_noreligionmod)
}

#pick parameters
#https://easystats.github.io/parameters/index.html
#parameters::model_parameters(model)
