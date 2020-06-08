running_platform<-"guicluster"
running_platform<-"computecluster"
running_bigdata_computation<-FALSE
running_bigdata_computation<-TRUE
loadbigdatadf<-FALSE

source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/13_merge_all_datasets.R"), encoding="UTF-8")

# plotting --------------------------------

#des <- overalldf_to_implist_func(overall_nonagenda_df, usinglib="survey") %>%
#  survey::svydesign(ids=~1, weight=~myown_wr, data=.)
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


if (FALSE) {
  for (plotvar in allmodelvars) {
    message(plotvar)
    custom_plot(overall_nonagenda_df, fvar=plotvar, weightvar="myown_wr") %>%
      print()
  }
}



