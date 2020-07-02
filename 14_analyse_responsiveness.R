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

respondmodels_file<-paste0(save_dataset_in_scriptsfile_directory,"/analyse_res/respondmodels.RData")
load(file=respondmodels_file, verbose=TRUE)

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
  # "respondopinion~1+(1|SURVEY)"
)) %>%
  dplyr::filter(!(formula %in% !!names(all_respondmodels)))

respondmodels<-custom_apply_thr_argdf(respondmodel_args, "formula", function(fikey, loopargdf, datadf, ...) {
  dplyr::filter(loopargdf, formula==!!fikey) %>%
    magrittr::use_series("formula") %>%
    as.character() %>%
    as.formula() %>%
    ordinal::clmm(formula=., data=datadf, weights=datadf$myown_wr, Hess=TRUE, model = TRUE, link = "logit",
                  threshold = "symmetric") %>% #c("flexible", "symmetric", "symmetric2", "equidistant")
    return()
}, datadf=overall_nonagenda_df)

load(file=respondmodels_file, verbose=TRUE)
all_respondmodels<-rlist::list.merge(all_respondmodels, respondmodels)
save(all_respondmodels,file=respondmodels_file)



if ({running_brms_model<-FALSE; running_brms_model & running_bigdata_computation}) {
  #load(file=paste0(dataset_in_scriptsfile_directory, "overall_nonagenda_df_sampled.RData"), verbose=TRUE)
  # Ordinal regression modeling patient's rating of inhaler instructions
  # category specific effects are estimated for variable 'treat'
  #要把屆次加入群
  sample_n_for_df<-sample(1:nrow(overall_nonagenda_df),60000)
  #overall_nonagenda_df_sampled<-overall_nonagenda_df[sample_n_for_df,] %>%
  overall_nonagenda_df_sampled<- overall_nonagenda_df_sampled %>%
    dplyr::group_by(imp) %>%
    {
      targetfreq<-{as.data.frame(table(.$imp)) %>% .$Freq %>% min()}
      dplyr::slice(., 1:targetfreq)
    }
  modelformula<-c(modelvars_ex_conti, modelvars_ex_catg, modelvars_latentrelated, modelvars_clustervars[1], modelvars_controllclustervars) %>%
    paste0(., collapse="+") %>%
    paste0("respondopinion~",.)
  message(modelformula)
  paste0(modelvars_clustervars,collapse="|") %>%
    paste0("(",.,")") %>%
    grep(pattern=., x=names(overall_nonagenda_df_sampled), value=TRUE) %>%
    paste0(collapse="+")
  modelformula<-"
    respondopinion | weights(myown_wr)~(myown_age+myown_sex+myown_selfid+myown_marriage+myown_factoredses+myown_factoredefficacy+myown_factoredparticip|myown_areakind)+similarity_distance+days_diff_survey_bill+(party_pressure+seniority+adminparty+elec_dist_type+1|term)+issuefield+cluster_varsellcm2+cluster_varsellcm3+cluster_varsellcm4+cluster_varsellcm5+cluster_varsellcm6
  " %>%
    brms::brmsformula()
  brmmodelonrespondopinion <- overall_nonagenda_df_sampled %>%
    brms::brm(modelformula,
              data = ., family = brms::cumulative(link = "logit"),
              chains = 2, cores = parallel::detectCores())
  brms:::summary.brmsfit(brmmodelonrespondopinion)
  #brms:::plot.brmsfit(brmmodelonrespondopinion, ask = FALSE)
  #brms::WAIC(brmmodelonrespondopinion)
  save(brmmodelonrespondopinion, file=paste0(dataset_in_scriptsfile_directory, "brmmodelonrespondopinion.RData"))
}