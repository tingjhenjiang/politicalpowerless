

# clustrd clustering single result --------------------------------


generate_weight_repeated_data<-function(single_survey_df, weight_reptimes_n_integer=0.5, surveyweightvar="myown_wr", needvars="") {
  #message(survey)
  min_myownwr<-min(extract2(single_survey_df, surveyweightvar))
  min_repeat_times<-1/min_myownwr #要讓最權重最小的觀察值出現一次的重複row倍數
  min_rep_weighted_myownwr<-extract2(single_survey_df, surveyweightvar)*min_repeat_times #所有觀察值的重複倍數
  #因為要讓最權重最小的觀察值出現一次的重複row倍數 套用在其他觀察值上 會出現小數，所以現在要計算重複row倍數的最小公倍數
  n_digits<-max(sapply(min_rep_weighted_myownwr,function (x) nchar(sub('^0+','',sub('\\.','',x)))))
  #n_digits<-9 #大於9似乎會有問題
  repeat { #為了防止求最小公倍數時出現問題所以往下求整數
    #message("n_digits is now ", n_digits)
    continue_to_minus<-FALSE
    ten_multiplier<-10^n_digits 
    tryCatch({
      integer_min_repeat_times<-min_rep_weighted_myownwr*ten_multiplier
      integer_min_repeat_times<-round(integer_min_repeat_times,digits = 0)
      integer_min_repeat_times<-as.integer(integer_min_repeat_times)
      lcm<-Reduce(f=DescTools::LCM, x=integer_min_repeat_times)
    }, warning=function (war) {
      #message(war)
      continue_to_minus<-TRUE
      #return(TRUE)
    }, error=function (err) {
      #message(err)
      continue_to_minus<-TRUE
      #return(TRUE)
    })
    n_digits<-n_digits-1
    if (continue_to_minus==TRUE) next
    if (is.numeric(lcm) | is.integer(lcm)) break
  }
  #message("continue_to_minus is ", continue_to_minus, " and lcm is ", class(lcm))
  lcm<-abs(lcm)
  adj_min_repeat_times<-lcm/min_myownwr
  adj_all_sample_rep_times<-extract2(single_survey_df, surveyweightvar)*adj_min_repeat_times
  single_survey_df$repeat_sample_times<-adj_all_sample_rep_times
  #message("adj_all_sample_rep_times is now ", length(adj_all_sample_rep_times))
  #message("uniq adj_all_sample_rep_times is now ", length(unique(adj_all_sample_rep_times)))
  weighted_adj_survey_data<-lapply(unique(adj_all_sample_rep_times), function(repeattimes, needvars, weight_reptimes_n_integer=1) {
    needrows<-which(adj_all_sample_rep_times==repeattimes)
    log_part<-log10(repeattimes)
    integers_of_log_part<-trunc(log_part)
    reptimes<-10^(log_part-integers_of_log_part+weight_reptimes_n_integer)
    reptimes<-round(reptimes,digits = 0)
    return(dplyr::slice(single_survey_df[,needvars], rep(needrows,reptimes)))
  },needvars=needvars, weight_reptimes_n_integer=weight_reptimes_n_integer) %>% dplyr::bind_rows()
  #needvars=c(clustering_var[[survey]],".imp")
  #message("weighted_adj_survey_data has ", nrow(weighted_adj_survey_data), " rows")
  return(weighted_adj_survey_data)
}


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
