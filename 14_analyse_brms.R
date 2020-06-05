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

if ({running_brms_model<-FALSE; running_brms_model & running_bigdata_computation}) {
  
  # Ordinal regression modeling patient's rating of inhaler instructions
  # category specific effects are estimated for variable 'treat'
  #要把屆次加入群
  modelformula<-c(modelvars_indo, modelvars_latentrelated, modelvars_clustervars[1], "myown_areakind") %>%
    paste0(., collapse="+") %>%
    paste0("respondopinion~",.)
  brmmodelonrespondopinion <- dplyr::group_by(overall_nonagenda_df, imp) %>%
    {
      targetfreq<-{as.data.frame(table(.$imp)) %>% .$Freq %>% min()}
      dplyr::slice(., 1:targetfreq)
    } %>%
    #lapply(., usinglib="lavaan") %>%
    brms::brm(modelformula,
              data = ., family = brms::sratio("logit"), chains = 1)
  summary(fit2)
  plot(fit2, ask = FALSE)
  WAIC(fit2)
}