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

#nests: id
survey_with_idealpoint_name<-paste0(dataset_in_scriptsfile_directory, "miced_survey_9_with_mirt_lca_clustering_idealpoints.RData")
load(file=survey_with_idealpoint_name, verbose=TRUE)

custom_plot(survey_data_imputed$`2010overall`,"policyidealpoint_cos_similarity_to_median","myown_wr")
custom_plot(survey_data_imputed$`2016citizen`,"policyidealpoint_eucli_distance_to_median","myown_wr")

need_svytitle<-c("2010overall","2016citizen")
analysis_idealpoint_to_median_args<-data.frame("survey"=survey_data_title) %>%
  cbind(., imp = rep(imputation_sample_i_s, each = nrow(.))) %>%
  dplyr::mutate(store_key=paste0(survey,"_imp",imp)) %>%
  dplyr::filter(survey %in% !!need_svytitle) %>%
  dplyr::mutate_at("survey", as.character) %>%
  dplyr::mutate_at("imp", as.integer) %>%
  dplyr::arrange(survey, imp)

#brm_multiple
common_names <- survey_data_imputed[need_svytitle] %>%
  lapply(FUN=names) %>%
  purrr::reduce(base::intersect)
merged_acrossed_surveys<-lapply(imputation_sample_i_s, function(imp, ...) {
  needdf<-survey_data_imputed[need_svytitle] %>%
    lapply(FUN=function(X,nimp) {dplyr::filter(X, .imp==!!nimp) %>% dplyr::select(-myown_indp_atti)}, nimp=imp) %>%
    dplyr::bind_rows()
  needdf$cluster_kamila<-as.ordered(needdf$cluster_kamila)
  levels(needdf$cluster_kamila)<-c(levels(needdf$cluster_kamila), 4)
  return(needdf)
}, survey_data_imputed=survey_data_imputed, need_svytitle=need_svytitle)

mod_robusts <- lapply(imputation_sample_i_s, function(imp,...) {
  brms::brm(#_multiple
    brms::bf(policyidealpoint_cos_similarity_to_median | weights(myown_wr) ~ cluster_kamila, sigma ~ cluster_kamila),
    family=brms::student,
    data = merged_acrossed_surveys[[imp]],
    cores=parallel::detectCores()
    #,file = here::here("data/policyidealpoint_cos_similarity_to_median_to_kamila-robust")
  )
}, merged_acrossed_surveys=merged_acrossed_surveys)
save(mod_robusts, file=here::here("data/mod_robusts.RData"))
load(file=here::here("data/policyidealpoint_cos_similarity_to_median_to_kamila-robust"), verbose=TRUE)
unique(merged_acrossed_surveys[[1]]$cluster_kamila)

s<-analysis_idealpoint_to_median_args$store_key[1:10] %>%
  magrittr::set_names(., lapply(., function(fikey, ...) {
    needrow<-dplyr::filter(analysis_idealpoint_to_median_args, store_key==!!fikey)
    svytitle<-needrow$survey
    imp<-needrow$imp
    needdf<-survey_data_imputed %>%
      magrittr::extract2(svytitle) %>%
      dplyr::filter(.imp==!!imp) %>%
      mutate(new_policyidealpoint_cos_similarity_to_median=policyidealpoint_cos_similarity_to_median+10)
    t<-magrittr::use_series(needdf, "policyidealpoint_cos_similarity_to_median") %>%
      bestNormalize::bestNormalize()
    mod_robust <- brms::brm(
      brms::bf(policyidealpoint_cos_similarity_to_median ~ cluster_kamila, sigma ~ cluster_kamila),
      family=brms::student,
      data = needdf, 
      cores=parallel::detectCores(),
      file = here::here("data/iqgroup-robust")
    )
    
    return(t$chosen_transform)
    #shapiro.test(t$x.t)
    if ({boxcox<-FALSE;boxcox}) {
      r<-MASS::boxcox(new_policyidealpoint_cos_similarity_to_median~cluster_kamila, data=needdf)
      bestpower <- cbind("lambda"=r$x, "lik"=r$y) %>%
        .[order(-lik),] %>%
        .[1,1]
      f1 <- lm(new_policyidealpoint_cos_similarity_to_median^1.030303 ~ cluster_kamila, data=needdf)
      summary(f1)
      shapiro.test(f1$res)
      data.frame(t=t$x.t) %>%
        custom_plot("t")
    }
    
    #Ladder.x(t)
    #r<-car::boxCoxVariable(t)
    #r<-car::powerTransform(t)
    #shapiro.test(r)
    #r<-MASS::boxcox(policyidealpoint_cos_similarity_to_median~)
    # brms::brm(modelformula,
    #           data = ., family = brms::cumulative(link = "logit"),
    #           chains = 2, cores = parallel::detectCores())
  }, survey_data_imputed=survey_data_imputed,
    analysis_idealpoint_to_median_args=analysis_idealpoint_to_median_args)
  )


