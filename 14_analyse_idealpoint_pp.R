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

#nests: id
reskamilaclusterfile <- paste0(dataset_in_scriptsfile_directory, "kamilacluster.Rdata")
load(file=reskamilaclusterfile, verbose=TRUE)
load(file=paste0(save_dataset_in_scriptsfile_directory,"miced_survey_2surveysonly_mirt_lca_clustering.RData"), verbose=TRUE)
survey_with_idealpoint_name<-paste0(dataset_in_scriptsfile_directory, "miced_survey_9_with_mirt_lca_clustering_idealpoints.RData")
load(file=survey_with_idealpoint_name, verbose=TRUE)

kamila_clustering_parameters<-lapply(names(kamila_results), function(fikey, ...) {
  needkamilamodel<-kamila_results[[fikey]]
  catgnames<-colnames(needkamilamodel$input$catFactor) %>%
    magrittr::set_names(names(needkamilamodel$finalProbs))
  catglevels<-lapply(needkamilamodel$input$catFactor,levels)
  continames<-colnames(needkamilamodel$input$conVar)
  probtable<-lapply(names(needkamilamodel$finalProbs), function(X, ...) {
    matchcatgvarname<-catgnames[[X]]
    catgprob<-needkamilamodel$finalProbs[[X]] %>%
      magrittr::set_colnames(catglevels[[matchcatgvarname]])
  },needkamilamodel=needkamilamodel, catgnames=catgnames, catglevels=catglevels, continames=continames) %>%
    do.call(cbind,.) %>%
    cbind(needkamilamodel$finalCenters %>%
            magrittr::set_colnames(continames), .)
  data.frame("fikey"=fikey,"totlclusters"=needkamilamodel$nClust$bestNClust,"clustn"=1:needkamilamodel$nClust$bestNClust) %>%
    cbind(probtable) %>%
    return()
}, kamila_results=kamila_results) %>%
  dplyr::bind_rows()

# custom_plot(survey_data_imputed$`2010overall`,"policyidealpoint_cos_similarity_to_median","myown_wr")
# custom_plot(survey_data_imputed$`2016citizen`,"policyidealpoint_eucli_distance_to_median","myown_wr")

# now in 2010overall_1
# [1] 1 3 4 2
# now in 2010overall_2
# [1] 1 2 3 4
# now in 2010overall_3
# [1] 3 4 1 2
# now in 2010overall_4
# [1] 2 4 1 3
# now in 2010overall_5
# [1] 3 2 4 1
# now in 2010overall_6
# [1] 4 2 1 3
# now in 2010overall_7
# [1] 3 4 2 1
# now in 2010overall_8
# [1] 4 3 1 2
# now in 2010overall_9
# [1] 3 2 4 1
# now in 2010overall_10
# [1] 3 4 1 2
# now in 2010overall_11
# [1] 2 3 1 4
# now in 2010overall_12
# [1] 4 3 1 2
# now in 2010overall_13
# [1] 2 1 3 4
# now in 2010overall_14
# [1] 3 2 1 4
# now in 2010overall_15
# [1] 2 4 3 1
# now in 2010overall_16
# [1] 1 3 2 4
# now in 2010overall_17
# [1] 4 2 3 1
# now in 2010overall_18
# [1] 1 2 3 4
# now in 2010overall_19
# [1] 2 1 3 4
# now in 2010overall_20
# [1] 4 2 1 3
# now in 2010overall_21
# [1] 1 4 2 3
# now in 2010overall_22
# [1] 1 3 2 4
# now in 2010overall_23
# [1] 1 4 5 3 2
# now in 2010overall_24
# [1] 3 4 2 1
# now in 2016citizen_1
# [1] 2 1 3 5 4
# now in 2016citizen_2
# [1] 4 1 3 2 5
# now in 2016citizen_3
# [1] 2 1 4 3
# now in 2016citizen_4
# [1] 3 5 1 4 2
# now in 2016citizen_5
# [1] 2 4 1 3 5
# now in 2016citizen_6
# [1] 3 1 2 4
# now in 2016citizen_7
# [1] 1 5 4 2 3
# now in 2016citizen_8
# [1] 4 2 5 1 3
# now in 2016citizen_9
# [1] 2 5 3 1 4
# now in 2016citizen_10
# [1] 2 1 3 4
# now in 2016citizen_11
# [1] 3 4 1 2
# now in 2016citizen_12
# [1] 3 4 1 5 2
# now in 2016citizen_13
# [1] 3 2 1 4
# now in 2016citizen_14
# [1] 3 2 1 4
# now in 2016citizen_15
# [1] 2 1 3 4
# now in 2016citizen_16
# [1] 4 2 3 5 1
# now in 2016citizen_17
# [1] 5 4 2 3 1
# now in 2016citizen_18
# [1] 3 1 2 4
# now in 2016citizen_19
# [1] 1 3 4 2
# now in 2016citizen_20
# [1] 2 5 4 3 1
# now in 2016citizen_21
# [1] 3 4 1 2 5
# now in 2016citizen_22
# [1] 2 4 3 1 5
# now in 2016citizen_23
# [1] 4 1 2 5 3
# now in 2016citizen_24
# [1] 2 5 3 1 4
matched_pairs_id<-3:4



need_svytitle<-c("2010overall","2016citizen")
analysis_idealpoint_to_median_args<-data.frame("survey"=survey_data_title) %>%
  cbind(., imp = rep(matched_pairs_id, each = nrow(.))) %>%
  dplyr::mutate(store_key=paste0(survey,"_imp",imp)) %>%
  dplyr::filter(survey %in% !!need_svytitle) %>%
  dplyr::mutate_at("survey", as.character) %>%
  dplyr::mutate_at("imp", as.integer) %>%
  dplyr::arrange(survey, imp)

#brm_multiple
common_names <- survey_data_imputed[need_svytitle] %>%
  lapply(FUN=names) %>%
  purrr::reduce(base::intersect)
merged_acrossed_surveys<-lapply(matched_pairs_id, function(imp, ...) {
  needdf<-survey_data_imputed[need_svytitle] %>%
    lapply(FUN=function(X,nimp) {dplyr::filter(X, .imp==!!nimp) %>% dplyr::select(-myown_indp_atti)}, nimp=imp) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate_at("SURVEY", as.factor) %>%
    dplyr::mutate_at("cluster_kamila", as.ordered)
  #levels(needdf$cluster_kamila)<-c(levels(needdf$cluster_kamila), 4)
  return(needdf)
}, survey_data_imputed=survey_data_imputed, need_svytitle=need_svytitle)

cossim_to_cluster_mod_robusts <- #lapply(1:length(merged_acrossed_surveys), function(imp,...) {
  brms::brm_multiple(#
    brms::bf(policyidealpoint_cos_similarity_to_median | weights(myown_wr) ~ cluster_kamila + SURVEY + (1 | cluster_kamila), sigma ~ cluster_kamila),
    family=brms::student,
    data = merged_acrossed_surveys,
    cores=parallel::detectCores(),
    iter = 2000
    #,file = here::here("data/policyidealpoint_cos_similarity_to_median_to_kamila-robust")
  )
#}, merged_acrossed_surveys=merged_acrossed_surveys)
brms:::summary.brmsfit(cossim_to_cluster_mod_robusts)
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

#pick parameters
#https://easystats.github.io/parameters/index.html
#parameters::model_parameters(model)
