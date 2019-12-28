# 第Ｏ部份：環境設定 --------------------------------
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
source(file = "shared_functions.R")

gc(verbose=TRUE)

t_sessioninfo_running_with_cpu_locale<-sessionInfo()$locale %>% stringi::stri_split(regex=";") %>% unlist() %>% getElement(1) %>% stringi::stri_split(regex="=") %>% unlist() %>% getElement(2) %>%
  paste0(t_sessioninfo_running_with_cpu, .) %>% gsub(pattern=" ",replacement = "", x=.)
# 第九部份：clustering  =================================

#clustering
#clustrd
#https://cran.r-project.org/web/packages/clustrd/clustrd.pdf
#http://www.amarkos.gr/clustrd/
#https://www.jamleecute.com/hierarchical-clustering-%E9%9A%8E%E5%B1%A4%E5%BC%8F%E5%88%86%E7%BE%A4/

load(paste0(dataset_in_scriptsfile_directory,"miced_survey_9_Ubuntu18.04.3LTSdf_with_mirt.RData"), verbose=TRUE)
imps<-imputation_sample_i_s

clustering_var <- list(
  "2004citizen"=c("myown_sex","myown_age","myown_selfid","myown_marriage","myown_areakind","myown_factoredses"),
  "2010env"=c("myown_sex","myown_age","myown_selfid","myown_marriage","myown_areakind","myown_factoredses"),
  "2010overall"=c("myown_sex","myown_age","myown_selfid","myown_marriage","myown_areakind","myown_factoredses"),
  "2016citizen"=c("myown_sex","myown_age","myown_selfid","myown_marriage","myown_areakind","myown_factoredses")
)
#"myown_dad_ethgroup","myown_mom_ethgroup","myown_working_status","myown_occp","myown_ses","myown_income","myown_family_income",
library(future)
library(future.apply)
reset_multi_p()
arguments_df<-data.frame("surveyidx"=names(survey_data_imputed),"survey"=stri_replace_all_fixed(names(survey_data_imputed), pattern=".sav", replacement="")) %>%
  cbind(., imp = rep(imps, each = nrow(.))) %>%
  cbind(., nclus = rep(2:6, each = nrow(.))) %>%
  cbind(., ndim = rep(1:5, each = nrow(.))) %>%
  cbind(., alpha = rep(c(0.25,0.5,0.75), each = nrow(.))) %>%
  cbind(., method = rep(c("mixedRKM","mixedFKM"), each = nrow(.))) %>%
  cbind(., rotation = rep(c("none","varimax","promax"), each = nrow(.))) %>%
  cbind(., criterion = rep(c("asw","ch","crit"), each = nrow(.))) %>%
  cbind(., dst = rep(c("low","full"), each = nrow(.))) %>%
  filter(nclus>ndim)

# Loading previous results --------------------------------
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
# Choosing the number of clusters and dimensions --------------------------------
need_arguments_df <- distinct(arguments_df, surveyidx, survey, alpha, method, rotation, criterion, dst) %>%
  mutate(name=paste(survey, alpha, method, rotation, criterion, dst, sep="_"))

idx<-1
while (idx<=nrow(need_arguments_df)) {
  singlerow_need_arguments_df<-extract(need_arguments_df,idx,) %>%
    mutate_at(.var=c("method","rotation","criterion","dst"), .funs = as.character)
  name_for_detect_best_result<-singlerow_need_arguments_df$name
  idx<-idx+1
  if (name_for_detect_best_result %in% all_processed_results) {next}
  message("idx is ", idx, " and argument is ", name_for_detect_best_result)
  detect_best_result<-select(singlerow_need_arguments_df, -surveyidx, -survey, -name) %>%
    as.list() %>%
    rlist::list.append(nclusrange=2:8, ndimrange=1:6, data={ #ndimrange=1:8
      survey_data_imputed[[singlerow_need_arguments_df$surveyidx]] %>%
        filter(.imp==1) %>%
        select(!!!clustering_var[[singlerow_need_arguments_df$survey]])
    }) %>%
    tryCatch(
      {return(do.call(clustrd::tuneclus, .))},
      error=function(e) {message(e)},
      finally = message("complete for ", singlerow_need_arguments_df$name)
    )
  detect_best_results[[name_for_detect_best_result]]<-detect_best_result
  save(detect_best_results, file=paste0(target_clustrd_save_name))
}

# Assessing Cluster Stability --------------------------------
#load(file=paste0(dataset_in_scriptsfile_directory,"clustrd_results.RData"))
#load(file=paste0(dataset_in_scriptsfile_directory,"small_clustrd_results.RData"))
clustering_result_compare_table<-future_mapply(function(title,listelement) {
  df1<-data.frame("title"=c(title))
  df2<-stri_split_fixed(title, "_",simplify = TRUE) %>%
    as.data.frame() %>%
    set_colnames(c("survey","alpha","method","rotation","criterion","dst"))
  df3<-listelement[c("nclusbest","ndimbest", "crit", "critbest")] %>%
    as.data.frame()
  df4<-listelement$clusobjbest[c("scale","center","nstart")] %>% #"criterion",
    as.data.frame()
  df5<-data.frame("criterion_in_obj"=as.character(listelement$clusobjbest$criterion))
  cbind(df1,df2,df3,df4,df5)
},title=names(tmpdetect_best_results), listelement=tmpdetect_best_results,SIMPLIFY = FALSE) %>%
  dplyr::bind_rows() %>%
  dplyr::arrange(survey, desc(critbest), crit, desc(criterion_in_obj))
View(clustering_result_compare_table)

#The ASW index, which ranges from −1 to 1, reflects the compactness of the clusters and indicates whether a cluster structure is well separated or not.
#The CH index is the ratio of between-cluster variance to within-cluster variance, corrected according to the number of clusters, and takes values between 0 and infinity.
#In general, the higher the ASW and CH values, the better the cluster separation.


for (i in 1:length(detect_best_results)) {
  message(names(detect_best_results)[[i]])
  print(detect_best_results[[i]])
}
# old methods --------------------------------
multiplier<-0
increment<-12
outcluspcamix<-list()
while (multiplier<=nrow(arguments_df)) {
  message("multiplier is ",multiplier)
  idx <- (1:increment)+multiplier
  idx <- idx[idx<=nrow(arguments_df)]
  if (length(idx)>0) {
    part_of_arguments_df <- slice(arguments_df,idx)
    tp_outcluspcamix<-future_mapply( function(surveyidx,survey,imp,nclus,ndim,alpha,rotation,survey_data_imputed_list){
      targetdf <- (extract2(survey_data_imputed_list, surveyidx)) %>%
        filter(.imp==imp) %>%
        select(!!!extract2(clustering_var,survey))
      return(tryCatch(
        {clustrd::cluspcamix(targetdf, nclus=nclus, ndim=ndim, method="mixedRKM", center = TRUE, scale = TRUE, alpha=alpha, rotation=rotation,nstart = 100, smartStart=NULL, seed=NULL, inboot = FALSE)},
        error=function(e) {
          message(e)
          return(as.character(e))
        }))
    }, surveyidx=part_of_arguments_df$surveyidx, survey=part_of_arguments_df$survey, imp=part_of_arguments_df$imp, nclus=part_of_arguments_df$nclus, ndim=part_of_arguments_df$ndim, alpha=part_of_arguments_df$alpha, rotation=part_of_arguments_df$rotation,
    MoreArgs=list(survey_data_imputed_list=survey_data_imputed), SIMPLIFY = FALSE
    ) %>%
      set_names(part_of_arguments_df$name)
    outcluspcamix<-c(outcluspcamix,tp_outcluspcamix)
    save(outcluspcamix, multiplier, file=paste0(dataset_in_scriptsfile_directory,"clustrd_results.RData"))
  }
  multiplier<-multiplier+increment
}







if (FALSE) {
  readme="
method = mixedRKM, criterion = asw, dst = low
2004citizen
The best solution was obtained for 2 clusters of sizes 995 (55.9%), 786 (44.1%) in 1 dimensions, for an average Silhouette width value of 0.603.
Cluster quality criterion values across the specified range of clusters (rows) and dimensions (columns):
       1     2     3     4     5     6     7     8     9
2  0.603                                                
3  0.565 0.599                                          
4  0.535 0.558 0.584                                    
5   0.54 0.519 0.531 0.567                              
6  0.533 0.523  0.52  0.53 0.558                        
7  0.542 0.512 0.483  0.52 0.501  0.52                  
8  0.534 0.511 0.492 0.482 0.503 0.459 0.479            
9  0.542 0.466 0.488 0.489 0.469 0.459 0.452 0.472      
10 0.535 0.461 0.461 0.462 0.461 0.408 0.452 0.425 0.419

The average Silhouette width values of each cluster are:
[1] 0.59 0.62

Cluster centroids:
            Dim.1
Cluster 1 -1.5626
Cluster 2  1.9781

Within cluster sum of squares by cluster:
[1] 1448.1244  936.1581
 (between_SS / total_SS =  69.78 %) 

Objective criterion value: 34648.3618 
------------------------------------------------------------------------------------------------------------
2010env
The best solution was obtained for 2 clusters of sizes 1376 (62.3%), 833 (37.7%) in 1 dimensions, for an average Silhouette width value of 0.591.

Cluster quality criterion values across the specified range of clusters (rows) and dimensions (columns):
       1     2     3     4     5     6     7     8     9
2  0.591                                                
3  0.543 0.585                                          
4  0.531 0.535 0.574                                    
5  0.542 0.517 0.516 0.562                              
6  0.537 0.525 0.497 0.514 0.544                        
7  0.545 0.518 0.501 0.485 0.493 0.477                  
8  0.532 0.522   0.5 0.496 0.482 0.491 0.475            
9  0.527 0.501   0.5 0.495 0.478 0.488 0.403 0.399      
10 0.534 0.496 0.484 0.494 0.463 0.483 0.416 0.405 0.401

The average Silhouette width values of each cluster are:
[1] 0.60 0.57

Cluster centroids:
            Dim.1
Cluster 1 -1.3744
Cluster 2  2.2703

Within cluster sum of squares by cluster:
[1] 2008.807 1430.564
 (between_SS / total_SS =  66.71 %) 

Objective criterion value: 46256.0183
------------------------------------------------------------------------------------------------------------
2010overall
The best solution was obtained for 4 clusters of sizes 1332 (70.3%), 327 (17.3%), 223 (11.8%), 13 (0.7%) in 3 dimensions, for an average Silhouette width value of 0.858.

Cluster quality criterion values across the specified range of clusters (rows) and dimensions (columns):
       1     2     3     4     5     6     7     8     9
2  0.601                                                
3  0.549 0.598                                          
4   0.53 0.542 0.858                                    
5  0.535 0.521 0.511 0.551                              
6  0.523 0.523 0.521 0.506 0.546                        
7  0.535 0.508 0.496 0.513 0.501 0.497                  
8  0.545 0.514 0.486 0.485 0.509 0.442 0.397            
9  0.532 0.524  0.48 0.486 0.501 0.446 0.384 0.414      
10 0.533 0.526 0.458 0.467 0.483 0.483  0.35 0.414 0.343

The average Silhouette width values of each cluster are:
[1] 0.91 0.77 0.73 0.88

Cluster centroids:
            Dim.1   Dim.2   Dim.3
Cluster 1 -0.0796 -0.8854  0.5621
Cluster 2 -0.3642  3.5154  0.6282
Cluster 3 -0.1644  0.0607 -4.2737
Cluster 4 20.1380  1.2489 -0.0849

Within cluster sum of squares by cluster:
[1] 146.0244 221.3986 291.0812  67.1648
 (between_SS / total_SS =  95.4 %) 

Objective criterion value: 32265.4593
------------------------------------------------------------------------------------------------------------
2016citizen
The best solution was obtained for 2 clusters of sizes 1214 (61.7%), 752 (38.3%) in 1 dimensions, for an average Silhouette width value of 0.591.

Cluster quality criterion values across the specified range of clusters (rows) and dimensions (columns):
       1     2     3     4     5     6     7     8     9
2  0.591                                                
3  0.562 0.587                                          
4  0.521  0.56 0.581                                    
5  0.526 0.518 0.549 0.544                              
6  0.539 0.524 0.504   0.5 0.548                        
7  0.535 0.535 0.505 0.489 0.494 0.546                  
8  0.536  0.53 0.509 0.489 0.503 0.549 0.479            
9  0.536 0.529 0.498 0.488  0.46 0.491 0.455 0.479      
10 0.535 0.529   0.5 0.487 0.454  0.49 0.486 0.457 0.383

The average Silhouette width values of each cluster are:
[1] 0.60 0.57

Cluster centroids:
            Dim.1
Cluster 1 -1.3709
Cluster 2  2.2131

Within cluster sum of squares by cluster:
[1] 1658.743 1135.321
 (between_SS / total_SS =  68.1 %) 

Objective criterion value: 43218.6061
＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝

"
}