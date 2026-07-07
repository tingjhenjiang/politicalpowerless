source(file = "13_preprocessing_merge_all_datasets.R")
# VIF test --------------------------------
analyse_vif_class <- R6::R6Class("analyse_vif", inherit=merge_all_datasets_class, public = list(
  res_custom_vif_filepath = NULL,
  initialize = function(dataset_in_scriptsfile_directory="/mnt", filespath="/mnt", dataset_file_directory="/mnt", debug_func_mode=TRUE) {
    super$initialize(dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory, filespath=filespath, dataset_file_directory=dataset_file_directory, debug_func_mode=debug_func_mode)
    self$res_custom_vif_filepath <- file.path(dataset_in_scriptsfile_directory, "res.custom_vif.RData")
  },
  custom_vif = function(df,vifcheckvars=c(),...) {
    vifcheckvars %>%
      { magrittr::set_rownames(
        miceFast::VIF(df, posit_y="respondopinion", posit_x=.), .)
      } %>% t() %>% return()
  },
  run_vif_test = function(overall_nonagenda_df, save=TRUE) {
    custom_vif<-self$custom_vif
    # sample_n_for_df<-sample(1:nrow(overall_nonagenda_df),50000)
    # overall_nonagenda_df_sampled<-overall_nonagenda_df[sample_n_for_df,] %>%
    res.custom_vif<-overall_nonagenda_df %>%
      self$overalldf_to_implist_func(usinglib="survey") %>%
      magrittr::use_series("imputations") %>%
      custom_parallel_lapply(custom_vif, vifcheckvars=c(self$modelvars_latentrelated,self$modelvars_ex_conti,self$modelvars_ex_catg,self$modelvars_controllclustervars), custom_vif=custom_vif, method=parallel_method)
    if (save==TRUE) {
      save(res.custom_vif, file = self$res_custom_vif_filepath)
    }
    res.custom_vif
  },
  get_inmodel_vars = function() {
    Reduce(base::union, c(self$modelvars_clustervars,self$modelvars_latentrelated,self$modelvars_ex_conti,self$modelvars_ex_catg,self$modelvars_controllclustervars) ,c()) %>%
      base::setdiff("partyGroup")
  },
  get_dummycoded_inmodel_vars_pattern = function(inmodel_vars=self$get_inmodel_vars()) {
    paste0(inmodel_vars, collapse="|") %>%
      paste0("(",.,")")
  },
  # VIF test on pp and idealpoint--------------------------------
  run_vif_test_on_pp_idealpoint = function(merged_acrossed_surveys_list) {
    inmodel_vars<-self$get_inmodel_vars() %>% c("policyidealpoint_cos_similarity_to_median_scaled")
    dummycoded_inmodel_vars_pattern<-self$get_dummycoded_inmodel_vars_pattern(inmodel_vars)
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
    targetcheckdf
  },
  # VIF test on responsiveness--------------------------------
  run_vif_test_on_responsiveness = function(overall_nonagenda_df) {
    inmodel_vars<-self$get_inmodel_vars()
    dummycoded_inmodel_vars_pattern<-self$get_dummycoded_inmodel_vars_pattern(inmodel_vars)
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
    targetcheckdf
  }
))
