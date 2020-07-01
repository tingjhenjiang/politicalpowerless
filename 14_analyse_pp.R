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
needimps<-custom_ret_appro_kamila_clustering_parameters()


# prepare data: matching different surveys and centering ----------------------------
survey_with_idealpoint_name<-paste0(save_dataset_in_scriptsfile_directory, "miced_survey_2surveysonly_mirt_lca_clustering_idealpoints.RData")
load(file=survey_with_idealpoint_name, verbose=TRUE)
doneimps<-unique(survey_data_imputed$`2016citizen`$.imp)
#survey_with_idealpoint_name<-paste0(dataset_in_scriptsfile_directory, "miced_survey_9_with_mirt_lca_clustering_idealpoints.RData")
#load(file=survey_with_idealpoint_name, verbose=TRUE)
#load(file=paste0(save_dataset_in_scriptsfile_directory,"miced_survey_2surveysonly_mirt_lca_clustering.RData"), verbose=TRUE)
#survey_data_imputed$`2010overall`$cluster_kamila %<>% as.factor()
#survey_data_imputed$`2016citizen`$cluster_kamila %<>% as.factor()

# custom_plot(survey_data_imputed$`2010overall`,"policyidealpoint_cos_similarity_to_median","myown_wr")
# custom_plot(survey_data_imputed$`2016citizen`,"policyidealpoint_eucli_distance_to_median","myown_wr")

need_svytitle<-names(survey_data_imputed) #c("2010overall","2016citizen")
analysis_idealpoint_to_median_args<-data.frame("survey"=survey_data_title) %>%
  cbind(., imp = rep(doneimps, each = nrow(.))) %>%
  dplyr::mutate(store_key=paste0(survey,"_imp",imp)) %>%
  dplyr::filter(survey %in% !!need_svytitle) %>%
  dplyr::mutate_at("survey", as.character) %>%
  dplyr::mutate_at("imp", as.integer) %>%
  dplyr::arrange(survey, imp)

common_names <- survey_data_imputed[need_svytitle] %>%
  lapply(FUN=names) %>%
  purrr::reduce(base::intersect)

merged_acrossed_surveys_list_with_normality_filepath<-paste0(dataset_in_scriptsfile_directory,"merged_acrossed_surveys_list_with_normality.RData")
if (FALSE) {
  transform_pp_data_to_normal<-TRUE
  transform_pp_data_to_normal<-FALSE
  merged_acrossed_surveys_list_with_normality<-lapply(doneimps, function(imp, normalize=FALSE, ...) {
    needdf<-survey_data_imputed[need_svytitle] %>%
      lapply(FUN=function(X,nimp) {dplyr::filter(X, .imp==!!nimp) %>% dplyr::select(-myown_indp_atti)}, nimp=imp) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate_at("SURVEY", as.factor) %>%
      dplyr::mutate_at("cluster_kamila", as.ordered)  %>%
      dplyr::select(-dplyr::contains("policy")) %>%
      dplyr::mutate(myown_factoredses_overallscaled=as.numeric(scale(myown_factoredses)) ) %>%
      dplyr::mutate(myown_age_overallscaled=as.numeric(scale(myown_age)) ) %>%
      dplyr::mutate(myown_factoredparticip_ordinal=cut(myown_factoredparticip,breaks=c(-10,-2,-1.5,-1.3,-0.65,-0.4,-0.15,0.15,0.7,1.3,1.8,10),right=TRUE,include.lowest=TRUE,ordered_result=TRUE))
      #C L Q E4 E5
      #dplyr::mutate_at("cluster_kamila", ~dplyr::recode_factor(., `1` = "A", `2` = "B", `3` = "C", `4` = "D", `5` = "E", `6` = "F", .ordered =TRUE) ) %>%
    if (normalize==TRUE) {
      transform_normality<-bestNormalize::bestNormalize(needdf$myown_factoredparticip)
      needdf$original_pp<-needdf$myown_factoredparticip
      needdf$myown_factoredparticip<-transform_normality$x.t
    } else {
      transform_normality<-NA
    }
    return(list(needdf, transform_normality))
  }, survey_data_imputed=survey_data_imputed, need_svytitle=need_svytitle, normalize=transform_pp_data_to_normal, needimps=needimps)
  merged_acrossed_surveys_list<-lapply(merged_acrossed_surveys_list_with_normality, function(X) {X[[1]]})
  save(merged_acrossed_surveys_list, merged_acrossed_surveys_list_with_normality,file=merged_acrossed_surveys_list_with_normality_filepath)
} else {
  load(merged_acrossed_surveys_list_with_normality_filepath,verbose=TRUE)
}
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


# null models ----------------------------

if ({nullmodel_icc_test<-FALSE;nullmodel_icc_test}) {
  pp_nullmodel_args<-data.frame("formula"=c(
    "myown_factoredparticip~1+(1|adminvillage/admindistrict/myown_areakind/cluster_kamila/SURVEY)",
    "myown_factoredparticip~1+(1|cluster_kamila/SURVEY)",
    "myown_factoredparticip~1+(1|cluster_kamila)",
    "myown_factoredparticip~1+(1|SURVEY)",
    "myown_factoredparticip~1+(1|adminvillage)",
    "myown_factoredparticip~1+(1|admindistrict)",
    "myown_factoredparticip~1+(1|admincity)",
    "myown_factoredparticip~1+(1|myown_areakind)"
  ))
  pp_nullmodels<-custom_apply_thr_argdf(pp_nullmodel_args, "formula", function(fikey, loopargdf, datadf, ...) {
    dplyr::filter(loopargdf, formula==!!fikey) %>%
      magrittr::use_series("formula") %>%
      as.character() %>%
      as.formula() %>%
      # lme4::glmer(formula=., data={
      #     dplyr::mutate_at(datadf, "myown_factoredparticip", ~myown_factoredparticip-min(myown_factoredparticip)+1 )
      # }, family = Gamma) %>% #, family = poisson(link = "log")
      #robustlmm::rlmerRcpp(formula=., data=datadf) %>%
      #lme4::lmer(formula=., data=datadf) %>%
      #lme4::bootMer(x=., data=datadf) %>%
      # ordinal::clmm(formula=., data=datadf, weights=datadf$myown_wr, Hess=TRUE, model = TRUE, link = "logit",
      #               threshold = "symmetric") %>% #c("flexible", "symmetric", "symmetric2", "equidistant")
      return()
  }, datadf=merged_acrossed_surveys_list[[1]])
  
  #lme4:::summary.merMod(nullmodel1)
  lapply(nullmodels, try(performance::icc))
  #sum(residuals(nullmodels$`myown_factoredparticip~1+(1|cluster_kamila)`))
  #shapiro.test(residuals(nullmodels$`myown_factoredparticip~1+(1|cluster_kamila)`))
}


ppmodels_file<-paste0(save_dataset_in_scriptsfile_directory,"/analyse_res/ppmodels.RData")
load(file=ppmodels_file, verbose=TRUE)
ppmodel_args<-data.frame("formula"=c(
  "myown_factoredparticip_ordinal~1+(1|adminvillage)",
  "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+myown_age_overallscaled+(1|adminvillage)",
  "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+(1|adminvillage)",
  "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled*myown_age_overallscaled+SURVEY+(1|adminvillage)",
  "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+(1|adminvillage)",
  "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+(1|adminvillage)",
  "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+(1|adminvillage)",
  "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+(1|adminvillage)",
  "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+myown_selfid+(1|adminvillage)",
  "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+myown_selfid+myown_areakind+(1|adminvillage)",
  "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+myown_selfid+myown_areakind+cluster_kamila+(1|adminvillage)",
  "myown_factoredparticip_ordinal~myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+myown_selfid+myown_areakind+cluster_kamila+myown_religion+(1|adminvillage)",
  "myown_factoredparticip_ordinal~(myown_factoredses_overallscaled+(myown_factoredses_overallscaled|adminvillage)+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+myown_selfid+myown_areakind+cluster_kamila+myown_religion)|adminvillage"
  #"myown_factoredparticip_ordinal~myown_factoredses_overallscaled+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+myown_selfid+myown_areakind+cluster_kamila",
  #"myown_factoredparticip_ordinal~myown_factoredses_overallscaled+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+SURVEY+myown_marriage+myown_sex+myown_selfid+myown_areakind+cluster_kamila+(1|cluster_kamila)",
  #"myown_factoredparticip_ordinal~myown_factoredses_overallscaled+myown_age_overallscaled+SURVEY+cluster_kamila+(1|cluster_kamila)",
  #"myown_factoredparticip_ordinal~myown_factoredses_overallscaled+myown_age_overallscaled+myown_marriage+myown_sex+myown_selfid+cluster_kamila+myown_areakind+SURVEY+1+(1|cluster_kamila)",
  #FULL
  #"myown_factoredparticip_ordinal~myown_factoredses_overallscaled+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+myown_marriage+myown_sex+myown_selfid+cluster_kamila+myown_areakind+SURVEY+1+(1|cluster_kamila)"#,
  #"myown_factoredparticip_ordinal~myown_factoredses_overallscaled+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+myown_marriage+myown_sex+myown_selfid+cluster_kamila+myown_areakind+SURVEY+1+(1|cluster_kamila)"#,
  #"myown_factoredparticip~myown_factoredses_overallscaled+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+myown_marriage+myown_sex+myown_selfid+cluster_kamila+myown_areakind+SURVEY+cluster_kamila*SURVEY+1+(1|cluster_kamila)"#,
  #"myown_factoredparticip~(1|adminvillage/admindistrict/admincity/myown_areakind/cluster_kamila/SURVEY)"
)) %>%
  dplyr::filter(!(formula %in% !!names(all_ppmodels)))

#merged_acrossed_surveys_list[[1]]$
#library(lme4)
ppmodels<-custom_apply_thr_argdf(ppmodel_args, "formula", function(fikey, loopargdf, datadf, ...) {
  dplyr::filter(loopargdf, formula==!!fikey) %>%
    magrittr::use_series("formula") %>%
    as.character() %>%
    as.formula() %>%
    # lme4::glmer(formula=., data={
    #     dplyr::mutate_at(datadf, "myown_factoredparticip", ~myown_factoredparticip-min(myown_factoredparticip)+1 )
    # }, family = Gamma) %>% #, family = poisson(link = "log")
    #robustlmm::rlmerRcpp(formula=., data=datadf) %>%
    #lme4::lmer(formula=., data=datadf) %>%
    #lme4::bootMer(x=., data=datadf) %>%
    ordinal::clmm(formula=., data=datadf, weights=datadf$myown_wr, Hess=TRUE, model = TRUE, link = "logit",
         threshold = "symmetric") %>% #c("flexible", "symmetric", "symmetric2", "equidistant")
    return()
}, datadf=merged_acrossed_surveys_list[[1]])

all_ppmodels<-rlist::list.merge(all_ppmodels, ppmodels)
save(all_ppmodels,file=ppmodels_file)

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
}


#load(file=paste0(dataset_in_scriptsfile_directory,"brms/test_cossim_to_cluster_mod_robusts.RData"), verbose=TRUE)
save(pp_to_cluster_nullmod, file=paste0(dataset_in_scriptsfile_directory,"brms/pp_to_cluster_nullmod.RData"))


if (FALSE) {
  brms:::summary.brmsfit(cossim_to_cluster_mod_robusts)
  brms::pp_check(cossim_to_cluster_mod_robusts)
  brms::pp_check(cossim_to_cluster_mod_robusts, group="cluster_kamila")
}

if (FALSE) {
  
}


#pick parameters
#https://easystats.github.io/parameters/index.html
#parameters::model_parameters(model)
