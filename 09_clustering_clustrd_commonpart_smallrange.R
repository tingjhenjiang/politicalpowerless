

# clustrd clustering single result --------------------------------


#repeat {
#  if (nrow(already_in_sqltable_clustrd_records)>=nrow(already_in_sqltable_clustrd_records)) {
#    break
#  }
#}
for_checking_clustrd_processed_vars<-c('survey', 'imp', 'method', 'criterion', 'dst', 'alpha', 'rotation', 'nclus', 'ndim', 'weight')
if (!exists("clustrd_using_surveyweight")) clustrd_using_surveyweight<-FALSE
if (!exists("target_impn")) target_impn<-1
if ({migrating_clustrd_table<-FALSE;migrating_clustrd_table}) {
  con <- do.call(DBI::dbConnect, dbconnect_info)
  already_in_sqltable_clustrd_records<-DBI::dbReadTable(con, 'demographic_clusters_ranged')
  DBI::dbDisconnect(con)
  migrate_clustrd_processed_vars<-c(for_checking_clustrd_processed_vars, 'critbest', 'nclusbest', 'ndimbest')
  check_if_processed_clustrd_df<-already_in_sqltable_clustrd_records[c(),] %>%
    dplyr::select(!!for_checking_clustrd_processed_vars)
  for (rowi in 1:nrow(already_in_sqltable_clustrd_records)) {
    tp_singlerow<-already_in_sqltable_clustrd_records[rowi,]
    checkclustrange<-seq(from=tp_singlerow$nclusmin, to=tp_singlerow$nclusmax)
    checkdimrange<-seq(from=tp_singlerow$ndimmin, to=tp_singlerow$ndimmax)
    check_if_processed_clustrd_df %<>% dplyr::bind_rows({
      dplyr::select(tp_singlerow, !!migrate_clustrd_processed_vars) %>%
        cbind(., nclus = rep(checkclustrange, each = nrow(.))) %>%
        cbind(., ndim = rep(checkdimrange, each = nrow(.))) %>%
        dplyr::mutate(imp=1)
    })
  }
  con <- do.call(DBI::dbConnect, dbconnect_info)
  DBI::dbWriteTable(con, db_table_name, check_if_processed_clustrd_df, append=TRUE)
  DBI::dbDisconnect(con)
}

# clustrd clustering: weighting data --------------------------------
#setting rep times
if (clustrd_using_surveyweight==TRUE) {
  weighted_adj_survey_data<-mclapply(names(survey_data_imputed), function (survey, tp_weight_reptimes_n_integer, tp_needvars) {
    message(survey)
    generate_weight_repeated_data(
      survey_data_imputed[[survey]],
      weight_reptimes_n_integer=tp_weight_reptimes_n_integer, surveyweightvar="myown_wr",
      needvars=c(tp_needvars[[survey]], ".imp")) %>%
      return()
  }, tp_weight_reptimes_n_integer=weight_reptimes_n_integer, tp_needvars=clustering_var, mc.cores = detectedcores) %>% #
    set_names(names(survey_data_imputed))
}

if ({checkdata_dist_plot<-FALSE;checkdata_dist_plot}) {
  test_plot_df<-dplyr::filter(survey_data_imputed[[1]], .imp==1)
  test_plot_df2<-dplyr::filter(weighted_adj_survey_data[[1]], .imp==1)
  fvar<-"myown_age"
  fvar<-"myown_selfid"
  fvar<-"myown_marriage"
  fvar<-"myown_areakind"
  fvar<-"myown_factoredses"
  #"myown_age" "myown_marriage" "myown_areakind" "myown_factoredses" ".imp"
  custom_plot(test_plot_df, fvar, "myown_wr", TRUE)
  custom_plot(test_plot_df, fvar, "myown_wr", FALSE)
  custom_plot(test_plot_df, fvar)
  custom_plot(test_plot_df2, fvar)
}


con <- do.call(DBI::dbConnect, dbconnect_info)
already_in_sqltable_clustrd_records<-DBI::dbReadTable(con, db_table_name)
DBI::dbDisconnect(con)
need_clustrd_arguments_df <- dplyr::distinct(clustrd_arguments_df, survey, imp, nclus, ndim, alpha, method, criterion, rotation, dst) %>%
  dplyr::mutate(title=paste(survey, imp, nclus, ndim, alpha, method, criterion, rotation, dst, sep="_")) %>%
  dplyr::filter(imp %in% target_impn) %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  dplyr::mutate(weight={ if(clustrd_using_surveyweight) 1 else 0}) %>%
  dplyr::anti_join(already_in_sqltable_clustrd_records, by=for_checking_clustrd_processed_vars)


idx<-1
#idx_process_ratio<-0.001
idx<-round(nrow(need_clustrd_arguments_df)/10*idx_process_ratio,0)
if (idx<1) idx<-1
needmodelrange<-seq(idx,nrow(need_clustrd_arguments_df))
need_clustrd_arguments_df<-need_clustrd_arguments_df[needmodelrange,]
message(paste(length(needmodelrange)," models to go"))
mclapply(need_clustrd_arguments_df$title, function (need_clustrd_title) {
  message("idx is ", which(need_clustrd_arguments_df$title==need_clustrd_title, arr.ind = TRUE), " and argument is ", need_clustrd_title)
  singlerow_need_clustrd_arguments_df<-dplyr::filter(need_clustrd_arguments_df, title==!!need_clustrd_title)
  #checking if already existed in db
  #survey, alpha, method, rotation, dst, nclusmin, nclusmax
  needdata<- {
    if (clustrd_using_surveyweight==TRUE) {
      #surveyweight<-needdata$myown_wr
      weighted_adj_survey_data[[singlerow_need_clustrd_arguments_df$survey]]
    } else {
      survey_data_imputed[[singlerow_need_clustrd_arguments_df$survey]]
    }
  } %>% dplyr::filter(.imp==singlerow_need_clustrd_arguments_df$imp) %>%
    dplyr::select(!!!clustering_var[[singlerow_need_clustrd_arguments_df$survey]])
  clustrd_result<-dplyr::select(singlerow_need_clustrd_arguments_df, -survey, -imp, -title, -weight) %>%
    dplyr::rename(nclusrange=nclus, ndimrange=ndim) %>%
    as.list() %>%
    rlist::list.append(center = TRUE, scale = TRUE, data=needdata) %>%
    tryCatch(
      {return(do.call(clustrd::tuneclus, .))},
      error=function(e) {message(e)},
      finally = message("complete for ", singlerow_need_clustrd_arguments_df$title)
    )
  failedonassemble<-FALSE
  tryCatch({single_clustering_infotable <- dplyr::bind_cols(singlerow_need_clustrd_arguments_df, 
    data.frame(
        "nclusbest"=clustrd_result$nclusbest,
        "ndimbest"=clustrd_result$ndimbest,
        "critbest"=clustrd_result$critbest,
        "scale"=clustrd_result$clusobjbest$scale,
        "center"=clustrd_result$clusobjbest$center,
        "nstart"=clustrd_result$clusobjbest$nstart,
        "criterion_in_obj"=clustrd_result$clusobjbest$criterion
      )
    )
  }, error=function(e) {
    cat("Failed on assemble info table")
    cat(str(e))
    failedonassemble<-TRUE
  })
  if (failedonassemble==TRUE) {
    writeinresult<-paste0("Failed on assemble info table at ",need_clustrd_title, collapse="")
  } else {
    writeinresult<-tryCatch({
      con <- do.call(DBI::dbConnect, dbconnect_info)
      DBI::dbWriteTable(con, db_table_name, single_clustering_infotable, append=TRUE)
      DBI::dbDisconnect(con)
      #custom_df_replaceinto_db(dbconnect_info, db_table_name, with_df, columns=c())
    }, error=function(e) {
      cat("Failed on dbWriteTable")
      cat(str(e))
    })
  }
  
  return(writeinresult)
}, mc.cores = detectedcores)#
