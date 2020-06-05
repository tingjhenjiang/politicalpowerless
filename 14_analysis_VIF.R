running_platform<-"guicluster"
running_platform<-"computecluster"
running_bigdata_computation<-FALSE
running_bigdata_computation<-TRUE

source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/13_merge_all_datasets.R"), encoding="UTF-8")

# VIF test --------------------------------

custom_vif<-function(df,vifcheckvars=c()) {
  vifcheckvars %>%
    { magrittr::set_rownames(
      miceFast::VIF(df, posit_y="respondopinion", posit_x=.), .)
    } %>% t() %>% return()
}


sample_n_for_df<-sample(1:nrow(overall_nonagenda_df),50000)
overall_nonagenda_df_sampled<-overall_nonagenda_df[sample_n_for_df,] %>%
  overalldf_to_implist_func(usinglib="survey")
mitools:::with.imputationList(overall_nonagenda_df_sampled, expr=miceFast::VIF() )

custom_parallel_lapply() lapply(overall_nonagenda_df_sampled$imputations, function(df, checkvifvars) {
  custom_vif(df,checkvifvars)
}, checkvifvars=c(modelvars_latentrelated,modelvars_ex_conti,modelvars_ex_catg,modelvars_controllclustervars) )
