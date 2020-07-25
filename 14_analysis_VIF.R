running_platform<-"guicluster"
running_platform<-"computecluster"
running_bigdata_computation<-FALSE
running_bigdata_computation<-TRUE

source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/13_merge_all_datasets.R"), encoding="UTF-8")

# VIF test --------------------------------

custom_vif<-function(df,vifcheckvars=c(),...) {
  vifcheckvars %>%
    { magrittr::set_rownames(
      miceFast::VIF(df, posit_y="respondopinion", posit_x=.), .)
    } %>% t() %>% return()
}

# sample_n_for_df<-sample(1:nrow(overall_nonagenda_df),50000)
# overall_nonagenda_df_sampled<-overall_nonagenda_df[sample_n_for_df,] %>%

res.custom_vif<-overall_nonagenda_df %>%
  overalldf_to_implist_func(usinglib="survey") %>%
  magrittr::use_series("imputations") %>%
  custom_parallel_lapply(custom_vif, vifcheckvars=c(modelvars_latentrelated,modelvars_ex_conti,modelvars_ex_catg,modelvars_controllclustervars), custom_vif=custom_vif, method=parallel_method)
save(res.custom_vif, file = paste0(dataset_in_scriptsfile_directory,"res.custom_vif.RData"))


inmodel_vars<-Reduce(base::union, c(modelvars_clustervars,modelvars_latentrelated,modelvars_ex_conti,modelvars_ex_catg,modelvars_controllclustervars) ,c()) %>%
  base::setdiff("partyGroup")
dummycoded_inmodel_vars_pattern<-paste0(inmodel_vars, collapse="|") %>%
  paste0("(",.,")")

# VIF test on pp and idealpoint--------------------------------
if (FALSE) {
  inmodel_vars %<>% c("policyidealpoint_cos_similarity_to_median_scaled")
  t<-dplyr::select(merged_acrossed_surveys_list[[1]], tidyselect::any_of(c(inmodel_vars)), -tidyselect::ends_with("NA")) %>%
    #dplyr::mutate_at("myown_marriage", dplyr::recode_factor, `[2] 已婚且與配偶同住`="0已婚且與配偶同住") %>%
    dummycode_of_a_dataframe() %>%
    cbind(myown_factoredparticip_ordinal=merged_acrossed_surveys_list[[1]]$myown_factoredparticip_ordinal)
  dummycoded_inmodel_vars<-dummycoded_inmodel_vars_pattern %>%
    grep(pattern=., x=names(t), value=TRUE)
  dummycoded_inmodel_vars %<>% Reduce(base::setdiff, c("myown_factoredparticip_overallscaled"), init=.) %>%
    grep(pattern="myown_marriage", x=., value=TRUE, invert=TRUE)
  
  #policyidealpoint_cos_similarity_to_median myown_factoredparticip_overallscaled
  targetcheckdf<- miceFast::VIF(t, posit_y="myown_factoredparticip_ordinal", posit_x=dummycoded_inmodel_vars  ) %>%
    data.frame(dummycoded_inmodel_vars,.)
  print(targetcheckdf)
}

# VIF test on responsiveness--------------------------------
if (FALSE) {
  t<-dplyr::mutate_at(overall_nonagenda_df, "myown_selfid", dplyr::recode_factor, `fulo`="1fulo", `hakka`="2hakka", `foreignstate`="3foreignstate", `aboriginal`="4aboriginal", `newresid`="5newresid") %>%
    dplyr::mutate_at("issuefield", dplyr::recode_factor, `seright`="1seright")
  t<-dplyr::select(t, tidyselect::any_of(inmodel_vars), -tidyselect::ends_with("NA")) %>%
    dummycode_of_a_dataframe() %>%
    #fastDummies::dummy_cols(remove_first_dummy = TRUE) %>%
    dplyr::bind_cols(
      dplyr::select(t, -tidyselect::any_of(inmodel_vars) )
      ) %>%
    dplyr::select(-tidyselect::contains(c("_mean","_inverse")))
  
  dummycoded_inmodel_vars<-dummycoded_inmodel_vars_pattern %>%
    grep(pattern=., x=names(t), value=TRUE) %>%
    Reduce(base::setdiff, c("issuefieldenv","issuefieldlawaff","myown_factoredparticip_overallscaled_inverse","respondopinion") , init=.) %>%
    grep(pattern="(myown_marriage|myown_areakind|myown_religion|myown_age)", x=., value=TRUE, invert=TRUE)
    #.[c(1:31,33:36,38:39)] #problem at 38issuefieldenv 43issuefieldlawaff
  
  targetcheckdf<-dplyr::filter(t, billid_myown %in% !!c("7-6-0-14-3","7-6-0-14-9","7-6-0-14-24","7-6-0-14-27",
                                                                           "7-6-0-15-8","7-6-0-15-11","7-6-0-15-12",
                                                                           "9-2-0-13-1","9-2-0-13-2","9-2-0-13-4","9-2-0-13-5","9-2-0-13-6","9-2-0-13-7","9-2-0-13-8","9-2-0-13-9","9-2-0-16-12","9-2-0-16-15","9-2-0-16-16","9-2-0-16-53","9-2-0-16-57","9-2-0-16-58","9-2-0-16-62","9-2-0-16-64","9-2-0-16-65","9-2-0-16-66","9-2-0-16-67","9-2-0-16-68","9-2-0-16-70","9-2-0-16-72","9-2-0-16-80","9-2-0-16-96","9-2-0-16-98","9-2-0-17-34","9-2-0-17-35","9-2-0-17-36","9-2-0-17-37","9-2-0-17-38","9-2-0-17-39","9-2-0-17-41","9-2-0-17-61","9-2-1-1-2","9-2-1-1-4","9-2-1-1-5","9-2-1-2-1","9-2-1-2-2","9-2-1-2-3","9-2-1-2-4","9-2-1-2-5","9-2-1-2-14","9-2-1-2-18","9-2-1-2-42","9-2-1-2-47","9-2-1-2-53","9-2-1-2-55","9-2-1-2-58"
  )) %>%
    droplevels() %>%
    miceFast::VIF(posit_y="respondopinion", posit_x=dummycoded_inmodel_vars  ) %>%
    data.frame(dummycoded_inmodel_vars, .)
  print(targetcheckdf)
}