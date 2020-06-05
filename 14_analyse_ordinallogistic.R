running_platform<-"guicluster"
running_platform<-"computecluster"
running_bigdata_computation<-FALSE
running_bigdata_computation<-TRUE

source_mergalldf_r_path<-try(here::here())
if(is(source_mergalldf_r_path, 'try-error')) source_mergalldf_r_path<-"/home/u4/dowbatw1133/Documents/vote_record"
source(file = paste0(source_mergalldf_r_path,"/13_merge_all_datasets.R"), encoding="UTF-8")


# modeling on survey Ordinal Logistic --------------------------------

#DEM 7283 - Example 2 - Logit and Probit Models
#https://rpubs.com/corey_sparks/577954
#DEM 7283 - Example 7 Multiple Imputation & Missing Data
#https://rpubs.com/corey_sparks/477390
#DEM 7283 - Example 1 - Survey Statistics using BRFSS data
#https://rpubs.com/corey_sparks/571267
#DEM 7283 Example 10 - Survey Information and Small Area Estimation
#https://rpubs.com/corey_sparks/484730  
#DEM 7283 - Example 3 - Ordinal & Multinomial Logit Models
#https://rpubs.com/corey_sparks/356551
#calculate p-value
#https://www.researchgate.net/post/p_value_calculator
#weight assigning method
#https://cran.r-project.org/web/packages/survey/vignettes/pps.pdf

if ({running_ordinal_logistic_model<-TRUE; running_ordinal_logistic_model & running_bigdata_computation}) {
  afterdummyc_vars<- c(modelvars_ex_catg,modelvars_controllclustervars,modelvars_clustervars[1]) %>% #only choose one cluster variable
    paste0(collapse="|") %>%
    paste0("(",.,")") %>%
    grep(pattern=.,x=names(overall_nonagenda_df),value=TRUE)# %>%
  #.[!(. %in% modelvars_controllclustervars)]
  paste0(afterdummyc_vars,collapse="+")
  modelformula<-c(modelvars_ex_conti, modelvars_latentrelated) %>%
    c(afterdummyc_vars) %>%
    #c("myown_sex.2..女+myown_selfid.2..台灣客家人+myown_selfid.3..台灣原住民+myown_selfid.4..大陸各省市.含港澳金馬.+myown_selfid.5..新移民+myown_selfid.6..其他臺灣人+myown_marriage.2..已婚且與配偶同住+myown_marriage.3..已婚但沒有與配偶同住+myown_marriage.4..同居+myown_marriage.5..離婚+myown_marriage.6..分居+myown_marriage.7..配偶去世+cluster_varsellcm2+cluster_varsellcm3+cluster_varsellcm4+cluster_varsellcm5+cluster_varsellcm6+elec_dist_typepartylist+adminparty1+issuefield公民與政治權+issuefield環境+issuefield教育+issuefield經濟+issuefield經濟社會文化權+issuefield兩岸+issuefield內政+issuefield社會福利") %>%
    paste0(., collapse="+") %>%
    paste0("respondopinion~",.) %>%
    as.formula()
  options(survey.multicore = TRUE)
  #des <- overalldf_to_implist_func(overall_nonagenda_df, usinglib="survey") %>%
  #  survey::svydesign(ids=~1, weight=~myown_wr, data=.)
  load(file=paste0(dataset_in_scriptsfile_directory, "ordinallogisticmodelonrespondopinion_des.RData"))
  ordinallogisticmodelonrespondopinion<-survey:::with.svyimputationList(des,survey::svyolr(modelformula),multicore=TRUE)
  while (TRUE) {
    savestatus<-try({save(ordinallogisticmodelonrespondopinion, file=paste0(save_dataset_in_scriptsfile_directory, "ordinallogisticmodelonrespondopinion.RData"))})
    if(!is(savestatus, 'try-error')) break
  }
  #load(file=paste0(dataset_in_scriptsfile_directory, "ordinallogisticmodelonrespondopinion.RData"), verbose=TRUE)
  #summary(mitools::MIcombine(ordinallogisticmodelonrespondopinion))
  #poolresult<-micombineresult(ordinallogisticmodelonrespondopinion)
  #View(poolresult)
}