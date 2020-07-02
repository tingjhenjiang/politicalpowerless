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
#not normal https://rdrr.io/cran/clubSandwich/f/vignettes/panel-data-CRVE.Rmd
#https://www.researchgate.net/publication/251965897_Cluster-robust_standard_errors_using_R
#merDeriv
#clubSandwich
#https://wangcc.me/LSHTMlearningnote/random-intercept.html
#Does it make sense to include a factor as both fixed and random factor in a Linear Mixed Effects Model?
#https://stats.stackexchange.com/questions/263194/does-it-make-sense-to-include-a-factor-as-both-fixed-and-random-factor-in-a-line
#nests: id

# * check kamila result ----------------------------------------------
needimps<-custom_ret_appro_kamila_clustering_parameters()

# * try analysing ----------------------------
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
      #dplyr::select(-dplyr::contains("policy")) %>%
      dplyr::mutate(myown_factoredses_overallscaled=as.numeric(scale(myown_factoredses)) ) %>%
      dplyr::mutate(myown_age_overallscaled=as.numeric(scale(myown_age)) )
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
  }, survey_data_imputed=survey_data_imputed, need_svytitle=need_svytitle, normalize=transform_pp_data_to_normal)
  merged_acrossed_surveys_list<-lapply(merged_acrossed_surveys_list_with_normality, function(X) {X[[1]]})
  save(merged_acrossed_surveys_list, merged_acrossed_surveys_list_with_normality,file=merged_acrossed_surveys_list_with_normality_filepath)
} else {
  load(merged_acrossed_surveys_list_with_normality_filepath,verbose=TRUE)
}

if ({plotting_inspection<-FALSE;plotting_inspection}) {
  merged_acrossed_surveys_overall<-dplyr::bind_rows(merged_acrossed_surveys_list)
  dplyr::filter(merged_acrossed_surveys_overall, .imp==1) %>%
  {
    custom_plot(., "policyidealpoint_cos_similarity_to_median","myown_wr") %>% print()
    custom_plot(., "policyidealpoint_eucli_distance_to_median","myown_wr") %>% print()
  }
  for (svytitle in names(survey_data_imputed)) {
    targetplotting_policy_idealpoint_colnames<-dplyr::filter(merged_acrossed_surveys_overall, SURVEY==!!svytitle) %>%
      names() %>%
      grep(pattern="policyidealpoint", x=., value=TRUE)
    for (colname in targetplotting_policy_idealpoint_colnames) {
      targetsavefilename<-here::here(paste0("plot/idealpoints/",svytitle,colname,".png"))
      distplot<-dplyr::filter(merged_acrossed_surveys_overall, SURVEY==!!svytitle) %>%
        custom_plot(colname, "myown_wr")
      print(distplot)
      ggplot2::ggsave(filename=targetsavefilename, plot=distplot)
      readline(paste("now in ",targetsavefilename," continue?"))
    }
  }
  idealpoints_model_arguments_df<-data.frame(
    modelformula=c("policyidealpoint_cos_similarity_to_median|weights(myown_wr)~cluster_kamila+SURVEY+(1|cluster_kamila)"),
    responsefamily=c("gaussian","student")
  )
}

all_idealpoint_models_file<-paste0(save_dataset_in_scriptsfile_directory,"analyse_res/idealpoint_models.RData")
load(file=all_idealpoint_models_file, verbose=TRUE)

#library(lme4)
# * modeling ------------------
idealpoint_models_args<-data.frame("formula"=c(
  "policyidealpoint_cos_similarity_to_median~(1|SURVEY)",
  "policyidealpoint_cos_similarity_to_median~(1|cluster_kamila)",
  "policyidealpoint_cos_similarity_to_median~(1|myown_areakind)",
  "policyidealpoint_cos_similarity_to_median~(1|admincity)",
  "policyidealpoint_cos_similarity_to_median~(1|admindistrict)",
  "policyidealpoint_cos_similarity_to_median~(1|adminvillage)"#,
  # "policyidealpoint_cos_similarity_to_median~cluster_kamila+(1|cluster_kamila)",
  # "policyidealpoint_cos_similarity_to_median~cluster_kamila+(1|cluster_kamila)+(1|SURVEY)",
  # "policyidealpoint_cos_similarity_to_median~cluster_kamila+(1|cluster_kamila)+(1|adminvillage)",
  # "policyidealpoint_cos_similarity_to_median~cluster_kamila+(1|cluster_kamila)+(1|SURVEY)+(1|adminvillage)",
  # "policyidealpoint_cos_similarity_to_median~cluster_kamila+(1|adminvillage/cluster_kamila)",
  # "policyidealpoint_cos_similarity_to_median~1+(1|adminvillage/admindistrict/admincity/myown_areakind/cluster_kamila/SURVEY)",
  # "policyidealpoint_cos_similarity_to_median~1+(1|adminvillage/cluster_kamila)",
  # "policyidealpoint_cos_similarity_to_median~(1|myown_areakind)",
  # "policyidealpoint_cos_similarity_to_median~(1|adminvillage)",
  # "policyidealpoint_cos_similarity_to_median~(1|admindistrict)",
  # "policyidealpoint_cos_similarity_to_median~(1|admincity)",
  # "policyidealpoint_eucli_distance_to_median~(1|SURVEY)",
  # "policyidealpoint_eucli_distance_to_median~(1|cluster_kamila)",
  # "policyidealpoint_eucli_distance_to_median~(1|cluster_kamila)+(1|SURVEY)",
  # "policyidealpoint_eucli_distance_to_median~(1|cluster_kamila)+(1|adminvillage)",
  # "policyidealpoint_eucli_distance_to_median~(1|cluster_kamila)+(1|SURVEY)+(1|adminvillage)",
  # "policyidealpoint_eucli_distance_to_median~(1|adminvillage/cluster_kamila)",
  # "policyidealpoint_eucli_distance_to_median~cluster_kamila+(1|cluster_kamila)",
  # "policyidealpoint_eucli_distance_to_median~cluster_kamila+(1|cluster_kamila)+(1|SURVEY)",
  # "policyidealpoint_eucli_distance_to_median~cluster_kamila+(1|cluster_kamila)+(1|adminvillage)",
  # "policyidealpoint_eucli_distance_to_median~cluster_kamila+(1|cluster_kamila)+(1|SURVEY)+(1|adminvillage)",
  # "policyidealpoint_eucli_distance_to_median~cluster_kamila+(1|adminvillage/cluster_kamila)",
  # "policyidealpoint_eucli_distance_to_median~(1|myown_areakind)",
  # "policyidealpoint_eucli_distance_to_median~(1|adminvillage)",
  # "policyidealpoint_eucli_distance_to_median~(1|admindistrict)",
  # "policyidealpoint_eucli_distance_to_median~(1|admincity)"#,
)) %>%
  dplyr::filter(!(formula %in% !!names(all_idealpoint_models)))
idealpoint_models<-custom_apply_thr_argdf(idealpoint_models_args, "formula", function(fikey, loopargdf, datadf, ...) {
  f<-dplyr::filter(loopargdf, formula==!!fikey) %>%
    magrittr::use_series("formula") %>%
    as.character() %>%
    as.formula()
  #datadf %<>% dplyr::mutate(secondweight=1)
  t<-try(
    #WeMix::mix(formula=f, data=datadf, weights=c("myown_wr","secondweight"))
    #robustlmm::rlmer(data=datadf, weights=datadf$myown_wr) %>%
    lmerTest::lmer(formula=f, data=datadf, weights=datadf$myown_wr)
    #lme4::lmer(formula=f, data=datadf, weights=datadf$myown_wr)
  )
  return(t)
}, datadf=merged_acrossed_surveys_list[[1]], mc.cores=1)

breads<-custom_parallel_lapply(idealpoint_models, merDeriv::bread.lmerMod, method=parallel_method)
#estfuns:Models with weights specification is currently not supported
#estfuns<-custom_parallel_lapply(idealpoint_models, merDeriv::estfun.lmerMod, method=parallel_method)

load(file=all_idealpoint_models_file, verbose=TRUE)
if (length(all_idealpoint_models)==0) {
  all_idealpoint_models<-idealpoint_models
} else {
  all_idealpoint_models<-rlist::list.merge(all_idealpoint_models,idealpoint_models)
}
save(all_idealpoint_models, file=all_idealpoint_models_file)

lapply(all_idealpoint_models, try(performance::icc))
lapply(all_idealpoint_models, function(X) {try(lmerTest:::summary.lmerModLmerTest(X))})

lapply(all_idealpoint_models, function(X) {try(lmerTest:::anova.lmerModLmerTest(X, type="I", ddf="Kenward-Roger"))})
lapply(all_idealpoint_models, try(lme4:::summary.merMod))
lapply(all_idealpoint_models, function(X) {X})
lapply(all_idealpoint_models, try(lme4:::VarCorr.merMod))
lapply(all_idealpoint_models, try(lme4:::anova.merMod))
library(afex)
lapply(all_idealpoint_models, try(afex:::lmerTest_anova))


lapply(idealpoint_models, lmerTest:::summary.lmerModLmerTest)
lapply(idealpoint_models, function(X) {sum(lme4:::residuals.merMod(X))} )
lapply(idealpoint_models, function(X) {hist(lme4:::residuals.merMod(X), breaks = 100)} )
lapply(idealpoint_models, function(X) {shapiro.test(lme4:::residuals.merMod(X))} )

lapply(idealpoint_models, lme4:::summary.merMod, signif.stars=TRUE)
lapply(idealpoint_models, summary, signif.stars=TRUE)
lapply(idealpoint_models, lme4::confint.merMod)

lapply(idealpoint_models, robustlmm:::summary.rlmerMod)
lapply(idealpoint_models, robustlmm:::plot.rlmerMod)
lapply(idealpoint_models, confint.rlmerMod)
lapply(idealpoint_models, function(X) { sum(robustlmm:::residuals.rlmerMod(X))  } )
lapply(idealpoint_models, function(X) { hist(robustlmm:::residuals.rlmerMod(X), breaks = 100)  } )
lapply(idealpoint_models, function(X) { shapiro.test(robustlmm:::residuals.rlmerMod(X))  } )


lapply(idealpoint_models, car::linearHypothesis)

# * brms backup --------------

if (FALSE) {
  cossim_to_cluster_mod_robusts2 <-
    brms::brm_multiple(#
      brms::bf(policyidealpoint_cos_similarity_to_median|weights(myown_wr)~(cluster_kamila|SURVEY)+SURVEY+cluster_kamila*SURVEY+(1|cluster_kamila/SURVEY)),
      #brms::bf(policyidealpoint_cos_similarity_to_median|weights(myown_wr)~(cluster_kamila|SURVEY)+SURVEY+(1|cluster_kamila/SURVEY)), #, sigma~cluster_kamila
      family=gaussian(), #brms::student,
      data = merged_acrossed_surveys_list,
      chains=4,
      cores=parallel::detectCores(),
      iter = 2000
      #,file = here::here("data/policyidealpoint_cos_similarity_to_median_to_kamila-robust")
    )
  
  cossim_to_cluster_mod_robusts3 <-
    brms::brm_multiple(#
      brms::bf(policyidealpoint_cos_similarity_to_median|weights(myown_wr)~(cluster_kamila|SURVEY)+SURVEY+cluster_kamila*SURVEY+(1|cluster_kamila/SURVEY), sigma~cluster_kamila+SURVEY),
      #brms::bf(policyidealpoint_cos_similarity_to_median|weights(myown_wr)~(cluster_kamila|SURVEY)+SURVEY+(1|cluster_kamila/SURVEY)), #
      family=brms::student,
      data = merged_acrossed_surveys_list,
      chains=4,
      cores=parallel::detectCores(),
      iter = 2000
      #,file = here::here("data/policyidealpoint_cos_similarity_to_median_to_kamila-robust")
    )
}

load(file=paste0(dataset_in_scriptsfile_directory,"brms/test_cossim_to_cluster_mod_robusts.RData"), verbose=TRUE)
save(cossim_to_cluster_mod_robusts1, cossim_to_cluster_mod_robusts2, cossim_to_cluster_mod_robusts3, file=paste0(dataset_in_scriptsfile_directory,"brms/test_cossim_to_cluster_mod_robusts.RData"))


if (FALSE) {
  brms:::summary.brmsfit(cossim_to_cluster_mod_robusts)
  brms::pp_check(cossim_to_cluster_mod_robusts)
  brms::pp_check(cossim_to_cluster_mod_robusts, group="cluster_kamila")
}

if (FALSE) {
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
}


#pick parameters
#https://easystats.github.io/parameters/index.html
#parameters::model_parameters(model)
