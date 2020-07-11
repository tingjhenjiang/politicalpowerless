running_platform<-"guicluster"
running_platform<-"computecluster"
running_bigdata_computation<-FALSE
running_bigdata_computation<-TRUE

source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/13_merge_all_datasets.R"), encoding="UTF-8")

# plotting for responsive data --------------------------------

#des <- overalldf_to_implist_func(overall_nonagenda_df, usinglib="survey") %>%
#  survey::svydesign(ids=~1, weight=~myown_wr, data=.)

if (FALSE) {
  load(file=paste0(save_dataset_in_scriptsfile_directory, "ordinallogisticmodelonrespondopinion_des.RData"), verbose=TRUE)
  load(file=paste0(save_dataset_in_scriptsfile_directory, "survey_summary_statistics.RData"), verbose=TRUE)
  allmodelvars<-c(modelvars_ex_conti,modelvars_ex_catg,modelvars_latentrelated,modelvars_clustervars,modelvars_controllclustervars,"respondopinion")
  allmodelvars_catg<-c(modelvars_ex_catg,modelvars_clustervars,modelvars_controllclustervars,"respondopinion")
  allmodelvars_catg_formula<-paste0(allmodelvars_catg,collapse="+") %>%
    paste0("~",.) %>% as.formula()
  allmodelvars_numeric<-c(modelvars_ex_conti,modelvars_latentrelated)
  allmodelvars_numeric_formula<-paste0(allmodelvars_numeric,collapse="+") %>%
    paste0("~",.) %>% as.formula()
  res.survey_summary_statistics<-list()
  #categorical var
  res.survey_summary_statistics[["catgvars_mean"]]<-survey:::with.svyimputationList(des,survey::svymean(allmodelvars_catg_formula),multicore=TRUE)
  res.survey_summary_statistics[["catgvars_total"]]<-survey:::with.svyimputationList(des,survey::svytotal(allmodelvars_catg_formula),multicore=TRUE)
  #conti var
  res.survey_summary_statistics[["contivars_mean"]]<-survey:::with.svyimputationList(des,survey::svymean(allmodelvars_numeric_formula),multicore=TRUE)
  res.survey_summary_statistics[["contivars_quantile"]]<-survey:::with.svyimputationList(des,survey::svyquantile(allmodelvars_numeric_formula, quantiles=c(.25,.5,.75), ci=TRUE),multicore=TRUE)
  save(res.survey_summary_statistics, file=paste0(save_dataset_in_scriptsfile_directory, "survey_summary_statistics.RData"))
  
}


if (FALSE) {
  plotvars<-base::setdiff(
    names(overall_nonagenda_df), 
    c("myown_wr","billid_myown","days_diff_survey_bill","id_wth_survey")
  )
  for (plotvar in plotvars) {
    message(plotvar)
    resplot<-custom_plot(overall_nonagenda_df, fvar=plotvar, weightvar="")
    targetsavefilename<-here::here(paste0("plot/responsiveness/",plotvar,".png"))
    ggplot2::ggsave(filename=targetsavefilename, plot=resplot)
    print(resplot)
  }
}

# plotting for idealpoint and participation data --------------------------------

needimps<-custom_ret_appro_kamila_clustering_parameters()
survey_with_idealpoint_name<-paste0(save_dataset_in_scriptsfile_directory, "miced_survey_2surveysonly_mirt_lca_clustering_idealpoints.RData")
load(file=survey_with_idealpoint_name, verbose=TRUE)
merged_acrossed_surveys_list<-ret_merged_for_idealpoint_and_pp_df_list(survey_data_imputed, dataset_in_scriptsfile_directory, minuspolicy=FALSE)
plotvars<-base::intersect(plotvars, names(merged_acrossed_surveys_list[[1]])) %>%
  c("policyidealpoint_cos_similarity_to_median_scaled", "policyidealpoint_eucli_distance_to_median_scaled")
for (plotvar in plotvars) {
  message(plotvar)
  resplot<-dplyr::filter(merged_acrossed_surveys_list[[1]]) %>%
    custom_plot(., fvar=plotvar, weightvar="")#myown_wr
  targetsavefilename<-here::here(paste0("plot/idp_pp/",plotvar,"_before_wr.png"))
  ggplot2::ggsave(filename=targetsavefilename, plot=resplot)
  print(resplot)
}

# plotting for different issues ----------------
df <- data.frame(y=abs(rnorm(8)),
                 x=as.factor(rep(c(0,100,200,500),times=2))) 
ggplot(aes(y=y,x=x), data=df) + 
  geom_boxplot()