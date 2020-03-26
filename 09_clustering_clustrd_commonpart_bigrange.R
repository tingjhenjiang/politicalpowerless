
clustering_result_to_infotable<-function(title=c(),listelement=list(),nclusrange=c(),ndimrange=c()) {
  df1<-data.frame("title"=as.character(title))
  df2<-{if(length(title)==0) {
    data.frame(character(),numeric(),character(),character(),character(),character())
  } else {
    stri_split_fixed(title, "_",simplify = TRUE) %>%
      as.data.frame(stringsAsFactors=FALSE)
  }}  %>%
    set_colnames(c("survey","alpha","method","rotation","criterion","dst"))
  if (length(listelement)>0) {
    df3<-listelement[c("nclusbest","ndimbest", "crit", "critbest")] %>%
      as.data.frame()
    df4<-listelement$clusobjbest[c("scale","center","nstart")] %>% #"criterion",
      as.data.frame()
    df5<-data.frame("criterion_in_obj"=as.character(listelement$clusobjbest$criterion))
  } else {
    df3<-data.frame(integer(),integer(),character(),numeric()) %>% set_colnames(c("nclusbest","ndimbest", "crit", "critbest"))
    df4<-data.frame(logical(),logical(),integer()) %>% set_colnames(c("scale","center","nstart"))
    df5<-data.frame(numeric()) %>% set_colnames("criterion_in_obj")
  }
  df6<-data.frame("nclusmin"=switch(as.character(length(nclusrange)>0),"TRUE"=min(nclusrange),"FALSE"=c()) %>% as.integer(),
                  "nclusmax"=switch(as.character(length(nclusrange)>0),"TRUE"=max(nclusrange),"FALSE"=c()) %>% as.integer(),
                  "ndimmin"=switch(as.character(length(nclusrange)>0),"TRUE"=min(ndimrange),"FALSE"=c()) %>% as.integer(),
                  "ndimmax"=switch(as.character(length(nclusrange)>0),"TRUE"=max(ndimrange),"FALSE"=c()) %>% as.integer()
  )
  cbind(df1,df2,df3,df4,df5,df6) %>% #
    mutate_at(c("title","survey","method","rotation","criterion","dst","crit"), as.character) %>%
    mutate_at(c("nclusmin","nclusmax","ndimmin","ndimmax","nclusbest","ndimbest","nstart"), as.integer) %>%
    mutate_at(c("alpha","critbest","criterion_in_obj"), as.numeric) %>%
    mutate_at(c("scale","center"), as.logical) %>%
    return()
}


# Establishing connections --------------------------------
db_table_name<-"demographic_clusters"
tryCatch({
  con <- do.call(DBI::dbConnect, dbconnect_info)
  clustering_result_to_infotable() %>%
    DBI::dbWriteTable(con, db_table_name, .)
  DBI::dbDisconnect(con)}, error=function(e) {
    cat("Failed on dbWriteTable")
    cat(str(e))
  }
)



# Loading previous results --------------------------------
if ({usingFTP<-FALSE; usingFTP}) {
  target_clustrd_save_name<-"clustrd_small_results.RData"
  tmpdetect_best_results<-list()
  for (name_i in c("colab_","colab_tj_","colab_worldhero_","colab_taiwanhao_","")) {
    clustrd_result_filename<-paste0(dataset_in_scriptsfile_directory,name_i,target_clustrd_save_name)
    if (file.exists(clustrd_result_filename)) {
      load(file=clustrd_result_filename,verbose=TRUE)
      tmpdetect_best_results <- c(tmpdetect_best_results, detect_best_results)
    }
  }
  all_processed_results<-names(tmpdetect_best_results)
  target_clustrd_save_name<-paste0("",target_clustrd_save_name, sep='')
  load(file=target_clustrd_save_name,verbose=TRUE)
}

# Choosing the number of clusters and dimensions --------------------------------
need_clustrd_arguments_df <- distinct(clustrd_arguments_df, surveyidx, survey, alpha, method, rotation, criterion, dst) %>%
  mutate(name=paste(survey, alpha, method, rotation, criterion, dst, sep="_"))


idx<-1
idx<-round(nrow(need_clustrd_arguments_df)/10*idx_process_ratio,0)
while (idx<=nrow(need_clustrd_arguments_df)) {
  
  tryCatch({
    con <- do.call(DBI::dbConnect, dbconnect_info)
    clustering_previous_results_df <- DBI::dbReadTable(con, name=db_table_name)
    DBI::dbDisconnect(con)}, error=function(e) {
      cat("Failed on dbReadTable")
      cat(str(e))
    }
  )
  
  nclusrange<-nclusrange#2#
  ndimrange<-ndimrange#1#
  singlerow_need_clustrd_arguments_df<-need_clustrd_arguments_df[idx,] %>%
    mutate_at(.var=c("method","rotation","criterion","dst"), .funs = as.character)
  name_for_detect_best_result<-singlerow_need_clustrd_arguments_df$name
  idx<-idx+1
  if (nrow(dplyr::inner_join(clustering_previous_results_df,singlerow_need_clustrd_arguments_df))>0) {next}
  print(paste0("idx is ", idx-1, " and argument is ", name_for_detect_best_result))
  detect_best_result<-select(singlerow_need_clustrd_arguments_df, -surveyidx, -survey, -name) %>%
    as.list() %>%
    rlist::list.append(nclusrange=nclusrange, ndimrange=ndimrange, data={ #ndimrange=1:8
      survey_data_imputed[[singlerow_need_clustrd_arguments_df$surveyidx]] %>%
        filter(.imp==1) %>%
        select(!!!clustering_var[[singlerow_need_clustrd_arguments_df$survey]])
    }) %>%
    tryCatch(
      {return(do.call(clustrd::tuneclus, .))},
      error=function(e) {message(e)},
      finally = message("complete for ", singlerow_need_clustrd_arguments_df$name)
    )
  single_clutering_infotable<-clustering_result_to_infotable(
    title=name_for_detect_best_result,
    listelement=detect_best_result,
    nclusrange=nclusrange,
    ndimrange=ndimrange)
  tryCatch({
    con <- do.call(DBI::dbConnect, dbconnect_info)
    DBI::dbWriteTable(con, db_table_name, single_clutering_infotable, append=TRUE)
    DBI::dbDisconnect(con)
    #custom_df_replaceinto_db(dbconnect_info, db_table_name, with_df, columns=c())
  }, error=function(e) {
    cat("Failed on dbWriteTable")
    cat(str(e))
  })
  #detect_best_results[[name_for_detect_best_result]]<-detect_best_result
  #save(detect_best_results, file=paste0(target_clustrd_save_name))
}

