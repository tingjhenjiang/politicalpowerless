source(file = "13_preprocessing_merge_all_datasets.R")
# plotting for responsive data --------------------------------
analyse_plot_class <- R6::R6Class("analyse_plot", inherit=merge_all_datasets_class, public = list(
  survey_summary_statistics_filepath = NULL,
  ordinallogistic_des_filepath = NULL,
  initialize = function(dataset_in_scriptsfile_directory="/mnt", filespath="/mnt", dataset_file_directory="/mnt", debug_func_mode=TRUE) {
    super$initialize(dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory, filespath=filespath, dataset_file_directory=dataset_file_directory, debug_func_mode=debug_func_mode)
    self$survey_summary_statistics_filepath <- file.path(save_dataset_in_scriptsfile_directory, "survey_summary_statistics.RData")
    self$ordinallogistic_des_filepath <- file.path(save_dataset_in_scriptsfile_directory, "ordinallogisticmodelonrespondopinion_des.RData")
  },
  # 以survey design計算加權敘述統計（類別變數mean/total、連續變數mean/quantile）
  compute_survey_summary_statistics = function(des=NULL, save=TRUE) {
    if (is.null(des)) {
      load_env <- new.env()
      load(file=self$ordinallogistic_des_filepath, envir=load_env, verbose=TRUE)
      des <- load_env$des
    }
    allmodelvars<-c(self$modelvars_ex_conti,self$modelvars_ex_catg,self$modelvars_latentrelated,self$modelvars_clustervars,self$modelvars_controllclustervars,"respondopinion")
    allmodelvars_catg<-c(self$modelvars_ex_catg,self$modelvars_clustervars,self$modelvars_controllclustervars,"respondopinion")
    allmodelvars_catg_formula<-paste0(allmodelvars_catg,collapse="+") %>%
      paste0("~",.) %>% as.formula()
    allmodelvars_numeric<-c(self$modelvars_ex_conti,self$modelvars_latentrelated)
    allmodelvars_numeric_formula<-paste0(allmodelvars_numeric,collapse="+") %>%
      paste0("~",.) %>% as.formula()
    res.survey_summary_statistics<-list()
    #categorical var
    res.survey_summary_statistics[["catgvars_mean"]]<-survey:::with.svyimputationList(des,survey::svymean(allmodelvars_catg_formula),multicore=TRUE)
    res.survey_summary_statistics[["catgvars_total"]]<-survey:::with.svyimputationList(des,survey::svytotal(allmodelvars_catg_formula),multicore=TRUE)
    #conti var
    res.survey_summary_statistics[["contivars_mean"]]<-survey:::with.svyimputationList(des,survey::svymean(allmodelvars_numeric_formula),multicore=TRUE)
    res.survey_summary_statistics[["contivars_quantile"]]<-survey:::with.svyimputationList(des,survey::svyquantile(allmodelvars_numeric_formula, quantiles=c(.25,.5,.75), ci=TRUE),multicore=TRUE)
    if (save==TRUE) {
      save(res.survey_summary_statistics, file=self$survey_summary_statistics_filepath)
    }
    res.survey_summary_statistics
  },
  get_responsiveness_plotvars = function(overall_nonagenda_df) {
    base::setdiff(
      names(overall_nonagenda_df),
      c("myown_wr","billid_myown","days_diff_survey_bill","id_wth_survey")
    )
  },
  plot_responsiveness_vars = function(overall_nonagenda_df) {
    plotvars<-self$get_responsiveness_plotvars(overall_nonagenda_df)
    for (plotvar in plotvars) {
      message(plotvar)
      n_bins<-if(plotvar %in% c("party_pressure_overallscaled","seniority_overallscaled")) 80 else ""
      n_bins<-if(plotvar %in% c("myown_age_overallscaled")) 80 else ""
      for (weightvar in c("myown_wr","")) {
        filename_prefix<-if(weightvar=="") paste0(plotvar,"_before_wr") else plotvar
        resplot<-custom_plot(overall_nonagenda_df, fvar=plotvar, weightvar=weightvar, fillvar="respondopinion", n_bins=n_bins)
        targetsavefilename<-here::here(paste0("plot/responsiveness/",filename_prefix,".png"))
        ggplot2::ggsave(filename=targetsavefilename, plot=resplot)
        print(resplot)
      }
    }
  },
  # plotting for idealpoint and participation data --------------------------------
  get_merged_acrossed_surveys_list = function() {
    needimps<-custom_ret_appro_kamila_clustering_parameters()
    survey_data_imputed <- self$get_survey_data_imputed_stage(stage="mirt_lca_clustering_idealpoints")
    ret_merged_for_idealpoint_and_pp_df_list(survey_data_imputed, dataset_in_scriptsfile_directory, minuspolicy=FALSE)
  },
  get_idp_pp_sourcedatadf = function(merged_acrossed_surveys_list=self$get_merged_acrossed_surveys_list()) {
    lapply(merged_acrossed_surveys_list, dplyr::select, -policyidealpoint_cos_similarity_to_median_ordinal) %>%
      plyr::rbind.fill()  %>%
      dplyr::mutate(policyidealpoint_cos_similarity_to_median_ordinal=cut(policyidealpoint_cos_similarity_to_median,breaks=17,right=TRUE,include.lowest=TRUE,ordered_result=TRUE))
  },
  plot_idp_pp_distribution = function(sourcedatadf, plotvars) {
    #https://cran.r-project.org/web/packages/srvyr/vignettes/srvyr-vs-survey.html
    #https://dcava.github.io/wpp/production_code.html
    plotvars %<>% base::intersect(names(sourcedatadf)) %>%
      c("policyidealpoint_cos_similarity_to_median_scaled", "policyidealpoint_eucli_distance_to_median_scaled")
    for (plotvar in plotvars) {
      message(plotvar)
      for (fillvar in c("policyidealpoint_cos_similarity_to_median_ordinal","myown_factoredparticip_ordinal")) {
        savepath_after<-if (fillvar=="policyidealpoint_cos_similarity_to_median_ordinal") "idp/" else "pp/"
        for (usingweightvar in c("myown_wr","")) {
          if_wr_filename_suffix<-if(usingweightvar=="myown_wr") "" else "_before_wr"
          n_bins<-if(plotvar %in% c("myown_age_overallscaled")) 80 else ""
          resplot<-sourcedatadf %>%
            custom_plot(., fvar=plotvar, weightvar=usingweightvar, fillvar=fillvar, n_bins=n_bins)
          targetsavefilename<-here::here(paste0("plot/idp_pp/",savepath_after,plotvar,"_fill_",fillvar,if_wr_filename_suffix,".png"))
          message(paste("saving to",targetsavefilename))
          ggplot2::ggsave(filename=targetsavefilename, plot=resplot)
          print(resplot)
        }
      }
    }
  },
  # boxplot and scatter and trend plot --------
  plot_idp_pp_boxplot_scatter = function(sourcedatadf, plotvars) {
    for (plotvar in plotvars) {
      for (fillvar in c("myown_factoredparticip_overallscaled","policyidealpoint_cos_similarity_to_median")) {
        savepath_after<-if (fillvar=="myown_factoredparticip_overallscaled") "pp/" else "idp/"
        fillvar_title<-if(fillvar=="myown_factoredparticip_overallscaled") "political participation" else "ideal point(cosine similarity to L1median)"
        plotvar_title<-gsub(pattern="myown_",replacement="",plotvar)
        for (usingweightvar in c("","myown_wr")) {
          if_wr_filename_suffix<-if(usingweightvar=="myown_wr") "" else "_before_wr"
          if (usingweightvar!="") {
            df_weight<-sourcedatadf %>%
              magrittr::extract2(.,usingweightvar)
            sum_df_weight<-sum(df_weight)
            ggplotweight<-df_weight/sum_df_weight
          }
          message(paste("now in",plotvar,"and weight is",usingweightvar))
          if ("factor" %in% class(sourcedatadf[,plotvar])) {
            outputplot<-ggplot2::ggplot(sourcedatadf,ggplot2::aes(
              x=.data[[plotvar]],
              y=.data[[fillvar]],
              colour=.data[[plotvar]],
              weight={if (usingweightvar=="") 1 else ggplotweight}
            ))+#,weight=.data[[usingweightvar]]
              ggplot2::geom_boxplot(width=.5,outlier.shape = 1)+
              ggplot2::labs(title=paste("Boxplot of",fillvar_title,"by",plotvar_title))+
              ggplot2::theme(plot.title=ggplot2::element_text(hjust = 0.5,face="bold",size=11))
          } else if ("numeric" %in% class(sourcedatadf[,plotvar])) {
            outputplot<-sourcedatadf %>%
              ggplot2::ggplot(.,ggplot2::aes(
                x=.data[[plotvar]],
                y=.data[[fillvar]],
                weight={if (usingweightvar=="") 1 else ggplotweight}
              ))+
              ggplot2::geom_point()+
              ggplot2::geom_smooth()+
              ggplot2::labs(title=paste("Scatter plot of",fillvar_title,"and",plotvar_title))+
              ggplot2::theme(plot.title=ggplot2::element_text(hjust = 0.5,face="bold",size=12))
          }
          targetsavefilename<-here::here(paste0("plot/idp_pp/inf_plot_",savepath_after,"infplot_",fillvar,"_by_",plotvar,if_wr_filename_suffix,".png"))
          message(paste("saving to",targetsavefilename))
          ggplot2::ggsave(filename=targetsavefilename, plot=outputplot)
          print(outputplot)
        }
      }
    }
  },
  plot_idealpoint_similarity = function(sourcedatadf) {
    for (needfvar in c("policyidealpoint_cos_similarity_to_median","policyidealpoint_euclid_distance_to_median")) {
      outputplot<-custom_plot(sourcedatadf, fvar=needfvar, weightvar="myown_wr")
      targetsavefilename<-here::here(paste0("plot/idealpoints/",needfvar,".png"))
      ggplot2::ggsave(filename=targetsavefilename, plot=outputplot)
      print(outputplot)
    }
  }
))

if (FALSE) { #svyhist/svyboxplot試驗（僅保留參考用）
  implist<-mitools::imputationList(merged_acrossed_surveys_list)
  des<-survey::svydesign(ids=~1,weight=~myown_wr,data=implist)

  custom_return_var_from_svyimp<-function (formula, design, breaks = "Sturges", include.lowest = TRUE,
                                           right = TRUE, xlab = NULL, main = NULL, probability = TRUE,
                                           freq = !probability, ...)  {
    mf <- stats::model.frame(
      formula,
      stats::model.frame(design),
      na.action = na.pass)
    #if (ncol(mf) > 1)
    #  stop("Only one variable allowed.")
    variable <- mf[, 1]
    varname <- names(mf)
    return(variable)
  }
  t<-survey:::with.svyimputationList(des,custom_return_var_from_svyimp(~myown_age_overallscaled))
  t<-survey:::with.svyimputationList(des,survey::svyhist(~policyidealpoint_cos_similarity_to_median,freq=TRUE))
  t<-survey:::with.svyimputationList(des,survey::svyboxplot(myown_factoredparticip~myown_factoredses_scaled))
  plot(t)
}

if (FALSE) { #ggplot boxplot試驗（僅保留參考用）
  df <- data.frame(y=abs(rnorm(8)),
                   x=as.factor(rep(c(0,100,200,500),times=2)))
  ggplot(aes(y=y,x=x), data=df) +
    geom_boxplot()
}
