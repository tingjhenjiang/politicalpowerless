# 第Ｏ部份：環境設定 --------------------------------
if (!("benchmarkme" %in% rownames(installed.packages()))) install.packages("benchmarkme")
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
t_sessioninfo_running_with_cpu_locale<-gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2]))
source(file = "shared_functions.R")

gc(verbose=TRUE)
if (!file.exists(paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt.RData"))) {
  dir.create(dataset_in_scriptsfile_directory,recursive=TRUE)
  download.file(
    "http://homepage.ntu.edu.tw/~r03a21033/voterecord/miced_survey_9_with_mirt_lca.RData",
    paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt_lca.RData"))
}
load(paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt_lca.RData"), verbose=TRUE)
imps<-imputation_sample_i_s
detectedcores <- ifelse(check_if_windows(),1,parallel::detectCores())

#"myown_dad_ethgroup","myown_mom_ethgroup","myown_working_status","myown_occp","myown_ses","myown_income","myown_family_income",

#clustering
#clustrd
#https://cran.r-project.org/web/packages/clustrd/clustrd.pdf
#http://www.amarkos.gr/clustrd/
#https://www.jamleecute.com/hierarchical-clustering-%E9%9A%8E%E5%B1%A4%E5%BC%8F%E5%88%86%E7%BE%A4/

load_lib_or_install(c("rvest","rlist","RMariaDB","getPass","parallel","itertools","ggplot2")) #,"future","future.apply"

## Establishing connections --------------------------------
db_table_name<-"demographic_clusters"
message(myremoteip)
dbtype <- RMariaDB::MariaDB() #RSQLite::SQLite()
dbname <- "thesis"
dbhost <- mysqldbhost
dbusername <- "j"
dbpassword <- ifelse(exists("dbpassword"),dbpassword,getPass::getPass("Please enter your password: ")) #rstudioapi::askForPassword("input password")
dbport <- 3306
dbconnect_info <- list(
  "drv"=dbtype,
  "host"=dbhost,
  "dbname"=dbname,
  "username"=dbusername,
  "password"=dbpassword,
  "port"=dbport
)
con <- do.call(DBI::dbConnect, dbconnect_info)
DBI::dbDisconnect(con)

# 第九部份：clustering  =================================

clustering_var <- list(
  "2004citizen"=c("myown_sex","myown_age","myown_selfid","myown_marriage","myown_areakind","myown_factoredses"),
  "2010env"=c("myown_sex","myown_age","myown_selfid","myown_marriage","myown_areakind","myown_factoredses"),
  "2010overall"=c("myown_sex","myown_age","myown_selfid","myown_marriage","myown_areakind","myown_factoredses"),
  "2016citizen"=c("myown_sex","myown_age","myown_selfid","myown_marriage","myown_areakind","myown_factoredses")
)

nclusrange<-2:12
ndimrange<-1:6
clustrd_arguments_df<-data.frame("survey"=names(survey_data_imputed)) %>%
  cbind(., imp = rep(imps, each = nrow(.))) %>%
  cbind(., nclus = rep(nclusrange, each = nrow(.))) %>%
  cbind(., ndim = rep(ndimrange, each = nrow(.))) %>%
  cbind(., alpha = rep(c(0,0.25,0.5,0.75,1), each = nrow(.))) %>%
  cbind(., method = rep(c("mixedRKM"), each = nrow(.))) %>% #,"mixedFKM"
  cbind(., rotation = rep(c("none","varimax","promax"), each = nrow(.))) %>%
  cbind(., criterion = rep(c("asw"), each = nrow(.))) %>%
  cbind(., dst = rep(c("low"), each = nrow(.))) %>% #,"full"
  filter(nclus>ndim)
clustrd_using_surveyweight<-FALSE
clustrd_using_surveyweight<-TRUE
target_impn<-imputation_sample_i_s
weight_reptimes_n_integer<-1
weight_reptimes_n_integer<-0.15



# clustrd clustering single result --------------------------------

tryCatch({
  con <- do.call(DBI::dbConnect, dbconnect_info)
  clustering_result_to_infotable() %>%
    DBI::dbWriteTable(con, db_table_name, .)
  DBI::dbDisconnect(con)}, error=function(e) {
    cat("Failed on dbWriteTable")
    cat(str(e))
  }
)

load_lib_or_install(c("clustrd")) #,"future","future.apply"
idx_process_ratio<-8
idx_process_ratio<-9.2
source("09_clustering_clustrd_commonpart_smallrange.R")
stop() #setwd('/mnt/e/Software/scripts/R/vote_record')
# clustrd clustering ranging assement --------------------------------

# My own Using WeightedCluster Examples --------------------------------

load_lib_or_install(c("rvest","rlist","RMariaDB","getPass","parallel","itertools","WeightedCluster","cluster","ggplot2")) #,"future","future.apply"


k_range<-1:2#30
hclustermethods<-c("single", "mcquitty", "complete", "ward.D", "ward.D2", "median", "centroid", "average")
pclustermethods<-c("KMedoids", "PAMonce")
needindicator<-c("ASWw", "HG", "PBC", "HC")
cluster_quality_resultsdf<-data.frame()
combine_hclust_pam_results<-list()
hclustrange_detects<-list()
pamrange_detects<-list()
load(file=(paste0(dataset_in_scriptsfile_directory,"weightedclustering_detect_results.RData")), verbose=TRUE )
it <- ihasNext(product(needsurveykey = 1:4, needimp = 1:5))
while(hasNext(it)) {
  iterx <- nextElem(it)
  print(iterx)
  if (iterx$needsurveykey %in% c(1,2)) {next}
  inputData<-survey_data_imputed[[iterx$needsurveykey]]
  survey<-inputData$SURVEY[1]
  surveyweight<-inputData$myown_wr[inputData$.imp==iterx$needimp]
  inputData<-inputData[inputData$.imp==iterx$needimp,clustering_var[[iterx$needsurveykey]]]
  inputData$myown_age %<>% scale() %>% .[,1]
  if ({plotingattr_distribution<-FALSE;plotingattr_distribution}) {
    gplotd <- ggplot(inputData)
    gplotd+geom_density(aes(myown_age),kernel = "gaussian")
    gplotd+geom_density(aes(myown_factoredses),kernel = "gaussian")
  }
  G.dist <- cluster::daisy(x = inputData, metric = "gower")
  gower_mat <- as.matrix(G.dist)
  cluster_arguments_df<-data.frame("survey"=survey,"imp"=iterx$needimp,"hmethod"=hclustermethods) %>%
    cbind(., k = rep(k_range, each = nrow(.))) %>%
    cbind(., pmethod = rep(pclustermethods, each = nrow(.))) %>%
    dplyr::mutate(title=paste0(survey,"_","imp",imp,"_",hmethod,"_",pmethod,"_",k)) %>%
    dplyr::mutate_at(c("hmethod","pmethod"),.funs=as.character)
  hclusterresults<-mclapply(hclustermethods, function(hclustermethod) {
    signlehclustresult<-hclust(G.dist,method=hclustermethod,members=surveyweight)
    return(signlehclustresult)
  },mc.cores = parallel::detectCores()) %>%
    set_names(hclustermethods)
  hcluster_cor_results<-mclapply(names(hclusterresults), function(signlehclustresult_idx) {
    signlehclustresult<-extract2(hclusterresults,signlehclustresult_idx)
    #fviz_dend(hclusterresults[[hclustermethod]], cex = 0.5)
    res.coph <- cophenetic(signlehclustresult)
    cophentic_distance_relation<-cor(G.dist, res.coph)
    #grp <- cutree(signlehclustresult, k = k)
    #head(grp, n = k) %>% print()
    #table(grp) %>% print()
    return(data.frame(hmethod=as.character(signlehclustresult_idx),cor=as.numeric(cophentic_distance_relation)))
  },mc.cores = parallel::detectCores()) %>% #
    dplyr::bind_rows()
  hcluster_quality_results<-mcmapply(function(hmethod,k,hclusterresults) {
    singlehclusterresult<-extract2(hclusterresults, hmethod)
    clust4 <- cutree(singlehclusterresult, k=k)
    clustqual4 <- wcClusterQuality(G.dist, clust4, weights=surveyweight)
    df1<-data.frame("survey"=survey,"imp"=iterx$needimp,"by"="hierarchical","indicator"=names(clustqual4$stats), "stats"=clustqual4$stats) %>%
      cbind(., hmethod = rep(hmethod, each = nrow(.))) %>%
      cbind(., k = rep(k, each = nrow(.)))
    rownames(df1) <- NULL
    return(df1)
  }, hmethod=cluster_arguments_df$hmethod, k=cluster_arguments_df$k, SIMPLIFY = FALSE, MoreArgs = list(hclusterresults=hclusterresults),mc.cores = parallel::detectCores()) %>%
    set_names(cluster_arguments_df$title) %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(hcluster_cor_results)
  new_combine_hclust_pam_results<-mcmapply(
    function(hmethod, pmethod, k, hclusterresults) {
      message("hmethod is ", hmethod, " and pmethod is ", pmethod, " and k is ", k)
      singlehclusterresult<-extract2(hclusterresults, hmethod)
      singlepamresult<-wcKMedoids(G.dist, k=k, weights=surveyweight, initialclust=singlehclusterresult, method=pmethod)
      #return(clustqual4$stats)
      #signleclustresult<-singlepamresult
      signleclustresult<-singlepamresult
      return(signleclustresult)
    }, hmethod=cluster_arguments_df$hmethod, pmethod=cluster_arguments_df$pmethod, k=cluster_arguments_df$k, SIMPLIFY = FALSE, MoreArgs = list(hclusterresults=hclusterresults), mc.cores = parallel::detectCores()) %>%
    set_names(cluster_arguments_df$title)
  combine_hclust_pam_results<-c(combine_hclust_pam_results, new_combine_hclust_pam_results)
  combine_hclust_pam_results_stats<-mclapply(names(new_combine_hclust_pam_results), function(idx) {
    single_combine_hclust_pam_result<-extract2(combine_hclust_pam_results,idx)
    title<-strsplit(idx,"_") %>% unlist()
    survey<-title[1]
    imp<-as.integer(stri_replace(str=title[2],replacement="",fixed="imp"))
    hmethod<-title[3]
    pmethod<-title[4]
    kv<-as.integer(title[5])
    data.frame(
      "survey"=survey,
      "imp"=imp,
      "by"="Partition",
      "indicator"=names(single_combine_hclust_pam_result$stats),
      "stats"=unname(single_combine_hclust_pam_result$stats),
      "hmethod"=hmethod,
      "pmethod"=pmethod,
      "k"=kv
    ) %>%
      return()
  },mc.cores = parallel::detectCores()) %>%
    dplyr::bind_rows()
  
  cluster_quality_resultsdf<-dplyr::bind_rows(cluster_quality_resultsdf,{
    dplyr::bind_rows(hcluster_quality_results,combine_hclust_pam_results_stats) %>%
      dplyr::filter(indicator %in% needindicator) %>%
      dplyr::arrange(by, indicator, stats)}
  )
  if ({using_default_range_detects<-FALSE;using_default_range_detects}) {
    hclustrange_detects<-c(hclustrange_detects,mclapply(hclusterresults, function(single_hclusterresult) {
      as.clustrange(single_hclusterresult, diss=G.dist, weights=surveyweight, ncluster=max(k_range))
    },mc.cores = parallel::detectCores()) %>%
      set_names(paste0(survey, "_", "imp", iterx$needimp, "_", names(.) ) ) )
    pamrange_detects <- c(pamrange_detects,
                          list(wcKMedRange(G.dist, kvals=k_range, weights=surveyweight)) %>% set_names(paste0(survey, "_imp", iterx$needimp))
    )
  }
  if ({printingandsummary_detects<-FALSE;printingandsummary_detects}) {
    for (idx in names(hclustrange_detects)) {
      message(idx)
      print(summary(hclustrange_detects[[idx]], max.rank=3))
      plot(hclustrange_detects[[idx]], stat=needindicator, norm="zscore" , main=idx)
    }
    summary(pamrange_detects, max.rank=3)
    plot(pamrange_detects, stat=needindicator, norm="zscore" , main="wcKMedRange")
  }
  tryCatch(save(
    combine_hclust_pam_results, cluster_quality_resultsdf, file=paste0(dataset_in_scriptsfile_directory,"weightedclustering_detect_results.RData")
  ), error=function(e) {
      message(e)
  })
  
}



# loading processed hierarchical Kmed cluster quality examination ------------------------
load(file=(paste0(dataset_in_scriptsfile_directory,"weightedclustering_detect_results.RData")), verbose=TRUE )
cluster_quality_resultsdf %<>% dplyr::mutate(title=paste0(survey, "_", "imp", imp, "_", hmethod, "_", pmethod, "_", k))
View(dplyr::distinct(dplyr::arrange(cluster_quality_resultsdf, survey, imp, by, indicator, desc(stats), hmethod),survey, imp, by, indicator, stats, hmethod, k, cor, pmethod))

"
1569	2004citizen	1	Partition	ASWw	0.6220064	ward.D	46	NA	PAMonce
1570	2004citizen	1	Partition	ASWw	0.6185019	ward.D2	50	NA	KMedoids
1571	2004citizen	1	Partition	ASWw	0.6168894	complete	45	NA	PAMonce
1572	2004citizen	1	Partition	ASWw	0.6168894	ward.D	45	NA	PAMonce
1573	2004citizen	1	Partition	ASWw	0.6167973	average	45	NA	PAMonce
6273	2004citizen	2	Partition	ASWw	0.6228385	ward.D2	50	NA	KMedoids
6274	2004citizen	2	Partition	ASWw	0.6187022	ward.D	44	NA	PAMonce
6275	2004citizen	2	Partition	ASWw	0.6187022	ward.D2	44	NA	PAMonce
6276	2004citizen	2	Partition	ASWw	0.6169955	ward.D2	49	NA	KMedoids
6277	2004citizen	2	Partition	ASWw	0.6147430	ward.D	43	NA	PAMonce
10977	2004citizen	3	Partition	ASWw	0.6215120	ward.D2	45	NA	PAMonce
10978	2004citizen	3	Partition	ASWw	0.6214041	mcquitty	45	NA	PAMonce
10979	2004citizen	3	Partition	ASWw	0.6209833	average	45	NA	PAMonce
10980	2004citizen	3	Partition	ASWw	0.6204365	ward.D2	50	NA	KMedoids
10981	2004citizen	3	Partition	ASWw	0.6180696	mcquitty	44	NA	PAMonce
15681	2004citizen	4	Partition	ASWw	0.6225769	ward.D2	50	NA	KMedoids
15682	2004citizen	4	Partition	ASWw	0.6187395	ward.D	44	NA	PAMonce
15683	2004citizen	4	Partition	ASWw	0.6187395	ward.D2	44	NA	PAMonce
15684	2004citizen	4	Partition	ASWw	0.6186682	average	44	NA	PAMonce
15685	2004citizen	4	Partition	ASWw	0.6186682	median	44	NA	PAMonce
20385	2004citizen	5	Partition	ASWw	0.6331580	average	47	NA	PAMonce
20386	2004citizen	5	Partition	ASWw	0.6331580	ward.D2	47	NA	PAMonce
20387	2004citizen	5	Partition	ASWw	0.6283608	ward.D	46	NA	PAMonce
20388	2004citizen	5	Partition	ASWw	0.6283608	ward.D2	46	NA	PAMonce
20389	2004citizen	5	Partition	ASWw	0.6280319	average	46	NA	PAMonce

25089	2010env	1	Partition	ASWw	0.6204924	complete	50	NA	PAMonce
25090	2010env	1	Partition	ASWw	0.6204924	mcquitty	50	NA	PAMonce
25091	2010env	1	Partition	ASWw	0.6204924	ward.D	50	NA	PAMonce
25092	2010env	1	Partition	ASWw	0.6204924	ward.D2	50	NA	PAMonce
25093	2010env	1	Partition	ASWw	0.6167166	complete	49	NA	PAMonce
29793	2010env	2	Partition	ASWw	0.6225807	complete	50	NA	PAMonce
29794	2010env	2	Partition	ASWw	0.6225807	mcquitty	50	NA	PAMonce
29795	2010env	2	Partition	ASWw	0.6225807	ward.D	50	NA	PAMonce
29796	2010env	2	Partition	ASWw	0.6225807	ward.D2	50	NA	PAMonce
29797	2010env	2	Partition	ASWw	0.6183137	complete	49	NA	PAMonce
34497	2010env	3	Partition	ASWw	0.6213428	complete	50	NA	PAMonce
34498	2010env	3	Partition	ASWw	0.6213428	mcquitty	50	NA	PAMonce
34499	2010env	3	Partition	ASWw	0.6213428	ward.D	50	NA	PAMonce
34500	2010env	3	Partition	ASWw	0.6213428	ward.D2	50	NA	PAMonce
34501	2010env	3	Partition	ASWw	0.6174344	complete	49	NA	PAMonce
39201	2010env	4	Partition	ASWw	0.6239977	ward.D	50	NA	PAMonce
39202	2010env	4	Partition	ASWw	0.6239977	ward.D2	50	NA	PAMonce
39203	2010env	4	Partition	ASWw	0.6238165	complete	50	NA	PAMonce
39204	2010env	4	Partition	ASWw	0.6238165	mcquitty	50	NA	PAMonce
39205	2010env	4	Partition	ASWw	0.6195707	average	49	NA	PAMonce
43905	2010env	5	Partition	ASWw	0.6216690	complete	50	NA	PAMonce
43906	2010env	5	Partition	ASWw	0.6216690	ward.D	50	NA	PAMonce
43907	2010env	5	Partition	ASWw	0.6216690	ward.D2	50	NA	PAMonce
43908	2010env	5	Partition	ASWw	0.6213576	mcquitty	50	NA	PAMonce
43909	2010env	5	Partition	ASWw	0.6171964	complete	49	NA	PAMonce

48609	2010overall	1	Partition	ASWw	0.5779079	average	50	NA	PAMonce
48610	2010overall	1	Partition	ASWw	0.5779079	mcquitty	50	NA	PAMonce
48611	2010overall	1	Partition	ASWw	0.5779079	ward.D	50	NA	PAMonce
48612	2010overall	1	Partition	ASWw	0.5775569	complete	50	NA	PAMonce
48613	2010overall	1	Partition	ASWw	0.5775569	ward.D2	50	NA	PAMonce
53313	2010overall	2	Partition	ASWw	0.5799655	median	50	NA	PAMonce
53314	2010overall	2	Partition	ASWw	0.5795645	average	50	NA	PAMonce
53315	2010overall	2	Partition	ASWw	0.5795645	complete	50	NA	PAMonce
53316	2010overall	2	Partition	ASWw	0.5795645	mcquitty	50	NA	PAMonce
53317	2010overall	2	Partition	ASWw	0.5795645	ward.D	50	NA	PAMonce
58017	2010overall	3	Partition	ASWw	0.5923895	median	50	NA	PAMonce
58018	2010overall	3	Partition	ASWw	0.5923895	ward.D	50	NA	PAMonce
58019	2010overall	3	Partition	ASWw	0.5923895	ward.D2	50	NA	PAMonce
58020	2010overall	3	Partition	ASWw	0.5917583	average	50	NA	PAMonce
58021	2010overall	3	Partition	ASWw	0.5917583	complete	50	NA	PAMonce
62721	2010overall	4	Partition	ASWw	0.5775224	average	50	NA	PAMonce
62722	2010overall	4	Partition	ASWw	0.5775224	complete	50	NA	PAMonce
62723	2010overall	4	Partition	ASWw	0.5775224	mcquitty	50	NA	PAMonce
62724	2010overall	4	Partition	ASWw	0.5775224	ward.D	50	NA	PAMonce
62725	2010overall	4	Partition	ASWw	0.5775224	ward.D2	50	NA	PAMonce
67425	2010overall	5	Partition	ASWw	0.5794807	ward.D	50	NA	PAMonce
67426	2010overall	5	Partition	ASWw	0.5793507	average	50	NA	PAMonce
67427	2010overall	5	Partition	ASWw	0.5793507	centroid	50	NA	PAMonce
67428	2010overall	5	Partition	ASWw	0.5793507	complete	50	NA	PAMonce
67429	2010overall	5	Partition	ASWw	0.5793507	mcquitty	50	NA	PAMonce

72129	2016citizen	1	Partition	ASWw	0.5525072	complete	50	NA	PAMonce
72130	2016citizen	1	Partition	ASWw	0.5525072	mcquitty	50	NA	PAMonce
72131	2016citizen	1	Partition	ASWw	0.5509503	ward.D	50	NA	PAMonce
72132	2016citizen	1	Partition	ASWw	0.5509503	ward.D2	50	NA	PAMonce
72133	2016citizen	1	Partition	ASWw	0.5477929	complete	49	NA	PAMonce
76833	2016citizen	2	Partition	ASWw	0.5521460	median	50	NA	PAMonce
76834	2016citizen	2	Partition	ASWw	0.5499016	ward.D2	50	NA	PAMonce
76835	2016citizen	2	Partition	ASWw	0.5494650	ward.D	50	NA	PAMonce
76836	2016citizen	2	Partition	ASWw	0.5463512	median	49	NA	PAMonce
76837	2016citizen	2	Partition	ASWw	0.5447200	ward.D2	49	NA	PAMonce
81537	2016citizen	3	Partition	ASWw	0.5541979	complete	50	NA	PAMonce
81538	2016citizen	3	Partition	ASWw	0.5541979	ward.D	50	NA	PAMonce
81539	2016citizen	3	Partition	ASWw	0.5541979	ward.D2	50	NA	PAMonce
81540	2016citizen	3	Partition	ASWw	0.5495602	ward.D2	49	NA	PAMonce
81541	2016citizen	3	Partition	ASWw	0.5494957	complete	49	NA	PAMonce
86241	2016citizen	4	Partition	ASWw	0.5530140	ward.D	50	NA	PAMonce
86242	2016citizen	4	Partition	ASWw	0.5530140	ward.D2	50	NA	PAMonce
86243	2016citizen	4	Partition	ASWw	0.5474496	ward.D	49	NA	PAMonce
86244	2016citizen	4	Partition	ASWw	0.5474496	ward.D2	49	NA	PAMonce
86245	2016citizen	4	Partition	ASWw	0.5426122	average	48	NA	PAMonce
90945	2016citizen	5	Partition	ASWw	0.5524685	mcquitty	50	NA	PAMonce
90946	2016citizen	5	Partition	ASWw	0.5512658	complete	50	NA	PAMonce
90947	2016citizen	5	Partition	ASWw	0.5512658	ward.D	50	NA	PAMonce
90948	2016citizen	5	Partition	ASWw	0.5512658	ward.D2	50	NA	PAMonce
90949	2016citizen	5	Partition	ASWw	0.5472830	mcquitty	49	NA	PAMonce
"

# DBSCAN ----------------

load_lib_or_install(c("fpc","dbscan","factoextra"))
clustrd_results_with_best_argument<-list()
it <- ihasNext(product(needsurvey = survey_data_title, needimp = 1))
while(hasNext(it)) {
  iterx <- nextElem(it)
  needdf<-dplyr::filter(survey_data_imputed[[iterx$needsurvey]], .imp==!!iterx$needimp) %>%
    dplyr::mutate_at("myown_age",scale) %>%
    dplyr::mutate_at("myown_age",function (X) {X[,1]})
  surveyweight<-needdf$myown_wr
  inputData<-dplyr::select(needdf, !!clustering_var[[iterx$needsurvey]])
  G.dist <- cluster::daisy(x = inputData, metric = "gower")
  gower_mat <- as.matrix(G.dist)
  #finding epsilon distance(eps)
  repeat {
    targetminpts <- as.integer(readline(paste("assigning k value for detecting survey",iterx$needsurvey, "imp", iterx$needimp, ":")))
    dbscan::kNNdistplot(G.dist, k = targetminpts)
    repeat {
      tryplotaxisylocation<-readline("try plot axis y location/or stop at:")
      if (tryplotaxisylocation=="") {
        break
      } else {
        assigned_dbscan_optimal_eps<-as.numeric(tryplotaxisylocation)
        title(main = paste("survey",iterx$needsurvey, "imp", iterx$needimp))
        abline(h = assigned_dbscan_optimal_eps, lty = 2)
      }
    }
    #generating clusters
    dbscan_optimal_eps<-assigned_dbscan_optimal_eps
    dbcluster_obj<-dbscan::dbscan(gower_mat, eps=dbscan_optimal_eps, minPts = targetminpts, weights=surveyweight)
    print(unique(dbcluster_obj$cluster))
    print(table(dbcluster_obj$cluster))
    silhouetteresult<-cluster::silhouette(dbcluster_obj$cluster, dmatrix=gower_mat)
    message("Silhouette is :")
    print(summary(silhouetteresult))
    if (readline("continue to change k-value?")=="N") break
  }
  clustrd_results_with_best_argument[[iterx$needsurvey]]$cluster<-dbcluster_obj$cluster
}

# model-based clustering by VarSelLCM ----------------
load_lib_or_install(c("VarSelLCM"))
clustrd_results_with_best_argument<-list()
it <- ihasNext(product(needsurvey = survey_data_title[1], needimp = 1))
while(hasNext(it)) {
  iterx <- nextElem(it)
  needdf<-dplyr::filter(survey_data_imputed[[iterx$needsurvey]], .imp==!!iterx$needimp) %>%
    dplyr::mutate_at("myown_age",scale) %>%
    dplyr::mutate_at("myown_age",function (X) {X[,1]}) %>%
    dplyr::mutate_at("myown_age",as.numeric)
  surveyweight<-needdf$myown_wr
  inputData<-dplyr::select(needdf, !!clustering_var[[iterx$needsurvey]])
  n_select_components<-2:6
  # Cluster analysis without variable selection
  res_without <- VarSelCluster(inputData, gvals = n_select_components, nbcores = detectedcores, vbleSelec = FALSE, crit.varsel = "BIC")
  # Cluster analysis with variable selection (with parallelisation)
  res_with <- VarSelCluster(inputData, gvals = n_select_components, nbcores = detectedcores, crit.varsel = "BIC")
  for (res_modelsel in list(res_without,res_with) ) {
    if (readline("next model:")=="N") break
    BIC(res_modelsel) %>% print()
    summary(res_modelsel) %>% print()
    # Estimated probabilities of classification
    head(fitted(res_modelsel, type="probability")) %>% print()
    print(res_modelsel)
    coef(res_modelsel) %>% print()
    table(fitted(res_modelsel)) %>% print()
    for (interpretvar in clustering_var[[iterx$needsurvey]]) {
      if (readline("next var:")=="N") break
      plot(x=res_modelsel, y=interpretvar)
    }
  }
  factorlevelseq <- as.character(sort(unique(fitted(res_with))))
  factored_cluster<-fitted(res_with) %>%
    as.factor() %>%
    forcats::fct_infreq() %>%
    forcats::fct_recode(., !!!{
      set_names(levels(.), as.list(factorlevelseq))
      })
  message("orig distribution of cluster is:")
  table(fitted(res_with)) %>% print()
  
  clustrd_results_with_best_argument[[iterx$needsurvey]]$cluster<-fitted(res_with)
}

# model-based clustering by clustMD ----------------
load_lib_or_install(c("clustMD"))

clustrd_results_with_best_argument<-list()
it <- ihasNext(product(needsurvey = survey_data_title[1], needimp = 1))
while(hasNext(it)) {
  iterx <- nextElem(it)
  needdf<-dplyr::filter(survey_data_imputed[[iterx$needsurvey]], .imp==!!iterx$needimp)
  surveyweight<-needdf$myown_wr
  needdf<-dplyr::select(needdf, !!clustering_var[[iterx$needsurvey]])
  inputData <- dplyr::bind_cols(dplyr::select_if(needdf, is.numeric),dplyr::select_if(needdf, is.factor))
  #measure skewness
  #If skewness value lies above +1 or below -1, data is highly skewed. If it lies between +0.5 to -0.5, it is moderately skewed. If the value is 0, then the data is symmetric
  numericvars<-lapply(inputData, is.numeric) %>%
    .[.==TRUE] %>%
    names()
  to_skew_num_var<-lapply(dplyr::select_if(inputData, is.numeric), e1071::skewness) %>%
    {is_greater_than(.,0.05) | is_less_than(.,-0.05)} %>%
    .[.==TRUE] %>%
    names()
  inputData %<>% dplyr::mutate_at(to_skew_num_var,custom_shift_sqrt) %>%
    dplyr::mutate_at(numericvars,scale) %>%
    dplyr::mutate_at(numericvars,function (X) {X[,1]})
  #ploting var to check the skewness
  for (numericvar in numericvars) {
    custom_plot(cbind(inputData,surveyweight), numericvar, weightvar="surveyweight") %>% print()
    readline("wait")
  }
  factorvars<-sapply(inputData, class) %>%
    .[.=="factor"] %>%
    names()
  previous_levellables<-lapply(inputData[,factorvars],levels) %>%
    lapply(sort)
  afterward_levellables<-lapply(previous_levellables, function(x) {
    x<-gsub("]\\s{1}.+","",x,perl=TRUE)
    x<-gsub("\\[{1}","",x,perl=TRUE)
    return(x)
  })
  recode_factorvar_levellabels<-lapply(names(previous_levellables), function(factorvar) {
    to_labels<- previous_levellables[[factorvar]] %>%
      set_names(afterward_levellables[[factorvar]])
    return(to_labels)
  }) %>% unlist()
  inputData <- dplyr::mutate_if(inputData, is.factor, forcats::fct_recode, !!!recode_factorvar_levellabels)
}



# Assessing Cluster clustrd Stability --------------------------------
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



# Apply Clustrd results according to previously retrieved information db--------------------------------
con <- do.call(DBI::dbConnect, dbconnect_info)
already_in_sqltable_clustrd_records<-DBI::dbReadTable(con, db_table_name)
DBI::dbDisconnect(con)

nclusbest_threshold<-3
comparing_appro_clustrd_result<-lapply(names(clustering_var), function(survey) {
  dplyr::filter(already_in_sqltable_clustrd_records, survey==!!survey, criterion=="asw") %>% #, imp==needimp
    dplyr::filter(nclusbest>=!!nclusbest_threshold) %>%
    dplyr::filter(critbest==max(critbest)) %>%
    dplyr::distinct(alpha, method, rotation, dst, nclusbest, ndimbest) %>%
    dplyr::arrange(nclusbest, ndimbest, alpha, method, rotation, dst) %>%
    return()
}) %>% set_names(names(clustering_var))
#try finding common clustrd argument
for (i in 2:3) {
  if (i==2) common_argument_for_clustrd_results<-comparing_appro_clustrd_result[[i]]
  common_argument_for_clustrd_results %<>% dplyr::semi_join(comparing_appro_clustrd_result[[i+1]])
}
#if no common clustrd argument, using single best argument
if (nrow(common_argument_for_clustrd_results)>0) {
  common_argument_for_clustrd_results<-common_argument_for_clustrd_results[1,]
  bestarguments_for_clustrd<-list(
    "2004citizen"=comparing_appro_clustrd_result$`2004citizen`,
    "2010env"=common_argument_for_clustrd_results,
    "2010overall"=common_argument_for_clustrd_results,
    "2016citizen"=common_argument_for_clustrd_results
  )
} else {
  bestarguments_for_clustrd<-lapply(names(clustering_var), function(survey) {
    extract2(comparing_appro_clustrd_result,survey) %>%
      .[1,]
  }) %>% set_names(names(clustering_var))
}

#for (survey in names(bestarguments_for_clustrd)) {
clustrd_results_with_best_argument<-parallel::mclapply(names(bestarguments_for_clustrd), function(survey) {
  needdata<-extract2(survey_data_imputed,survey) %>%
    dplyr::filter(.imp==1)
  surveyweight<-needdata$myown_wr
  needdata<-needdata %>%
    extract(clustering_var[[survey]]) %>%
    dplyr::mutate_at("myown_age",scale) %>%
    dplyr::mutate_at("myown_age",function (X) {X[,1]})
  extract2(bestarguments_for_clustrd,survey) %>%
    dplyr::rename(nclus=nclusbest, ndim=ndimbest) %>%
    dplyr::select(-dst) %>%
    as.list() %>%
    rlist::list.append(data=needdata, center=TRUE, scale=TRUE, smartStart=surveyweight) %>%
    {
      if (clustrd_using_surveyweight) {
        rlist::list.append(smartStart=surveyweight)
      } else {
        .
      }
    } %>%
    do.call(clustrd::cluspcamix,.) %>%
    return()
}, mc.cores=detectedcores) %>%
  set_names(names(clustering_var))
save(clustrd_results_with_best_argument,file=paste0(dataset_in_scriptsfile_directory,"clustrd_results_with_best_argument_nclus-threshold-",nclusbest_threshold,"_with_weight.RData"))

load(file=paste0(dataset_in_scriptsfile_directory,"clustrd_results_with_best_argument_nclus-threshold-",nclusbest_threshold,".RData"), verbose=TRUE)
# for using in plotting
it <- ihasNext(product(needsurveykey = survey_data_title, needimp = 1))
while(hasNext(it)) {
  iterx <- nextElem(it)
  clustrd_results_with_best_argument[[iterx$needsurveykey]]$cluster<-clustrd_results_with_best_argument[[iterx$needsurveykey]]$cluster
  needdf<-dplyr::filter(survey_data_imputed[[iterx$needsurvey]], .imp==!!iterx$needimp)
  surveyweight<-needdf$myown_wr
  inputData<-dplyr::select(needdf, !!clustering_var[[iterx$needsurvey]]) %>%
    dplyr::mutate_at("myown_age",scale) %>%
    dplyr::mutate_at("myown_age",function (X) {X[,1]})
  G.dist <- cluster::daisy(x = inputData, metric = "gower")
  gower_mat <- as.matrix(G.dist)
  silhouetteresult<-cluster::silhouette(clustrd_results_with_best_argument[[iterx$needsurveykey]]$cluster, dmatrix=gower_mat)
  message(iterx$needsurveykey)
  print(summary(silhouetteresult))
}

# Factor analysis of mixed data and Graphing cluster result --------------------------------
load_lib_or_install(c("FactoMineR","factoextra","parallel","magrittr"))

it <- ihasNext(product(needsurveykey = 1:4, needimp = 1))
while(hasNext(it)) {
  iterx <- nextElem(it)
  if (readline(paste0("Continue to next imputation where imp=",iterx$needimp," ? (Y/N)"))=="N") {
    next
  }
  df<-survey_data_imputed[[iterx$needsurveykey]] %>%
    .[.$.imp==iterx$needimp,]
  survey<-df$SURVEY[1]
  surveyweight<-df$myown_wr
  df<-df[,extract2(clustering_var,iterx$needsurveykey)]
  df$myown_age %<>% scale() %>% .[,1]
  res.famd <- FAMD(df, graph = FALSE, row.w=surveyweight)
  summary.FAMD(res.famd)
  eig.val <- get_eigenvalue(res.famd)
  head(eig.val) #要選擇特徵值大於一的
  fviz_screeplot(res.famd)
  if ({usingpamresult<-exists("cluster_quality_resultsdf");usingpamresult}) {
    needclusteringmethod<-dplyr::arrange(cluster_quality_resultsdf, survey, imp, by, indicator, desc(stats), hmethod) %>%
      dplyr::filter(!is.na(pmethod)) %>%
      dplyr::distinct(survey, imp, by, indicator, stats, hmethod, k, cor, pmethod, title) %>%
      dplyr::filter(survey==!!survey) %>%
      head() %>%
      extract2("title") %>%
      .[1] %>%
      extract2(combine_hclust_pam_results, .)
  }
  clusterf<-switch(as.character(exists("clustrd_results_with_best_argument")),
                   "TRUE"=clustrd_results_with_best_argument[[iterx$needsurveykey]]$cluster,
                   "FALSE"=needclusteringmethod$clustering) %>%
    as.factor() %>%
    forcats::fct_infreq()
  table(clusterf)
  var <- get_famd_var(res.famd)
  #weired: 2010env dim 4
  for(dim in combn(1:5,2,simplify = FALSE)) {#ucscgb #simpsons
    fviz_famd_ind(res.famd, axes=dim, geom = "point", ggtheme = theme_classic(), legend = "bottom",
                  title = paste0("FAMD ", survey, " imp ", iterx$needimp," dim ", dim),
                  habillage=clusterf,
                  palette = "jco") %>%
      print()
    # Coordinates of variables
    print("Coordinates")
    head(var$coord[,dim]) %>% print()
    # Cos2: quality of representation on the factore map
    print("quality")
    head(var$cos2[,dim]) %>% print()
    # Contributions to the dimensions
    print("Contributions")
    head(var$contrib[,dim]) %>% print()
    if (iterx$needimp==1) {
      if (readline("Continue to show dim contribution graph? (Y/N)")=="Y") {
        # Plot of variables
        fviz_famd_var(res.famd, axes=dim, repel = TRUE)
        # Contribution to the ?th dimension
        print(fviz_contrib(res.famd, "var", axes = dim))
      }
    }
    if (readline("Continue to next dim graph? (Y/N)")=="N") {
      break
    }
  }
  #break
}

# VAT (Visual Assessment of Tendency) assessing clustering tendency --------------------------------

load_lib_or_install(c("itertools","cluster","factoextra","dendextend","clustertend","factoextra","NbClust"))
it <- ihasNext(product(needsurveykey = 1:4, needimp = 1)) #:5
while(hasNext(it)) {
  iterx <- nextElem(it)
  inputData<-survey_data_imputed[[iterx$needsurveykey]]
  survey<-inputData$SURVEY[1]
  surveyweight<-inputData$myown_wr[inputData$.imp==iterx$needimp]
  inputData<-inputData[inputData$.imp==iterx$needimp,clustering_var[[iterx$needsurveykey]]]
  inputData$myown_age %<>% scale() %>%
    .[,1]
  #hopkins(inputData, n = nrow(inputData)-1)
  #get_clust_tendency(inputData, n = nrow(inputData)-1)
  G.dist <- cluster::daisy(x = inputData, metric = "gower")
  gower_mat <- as.matrix(G.dist)
  print(
  fviz_dist(G.dist, show_labels = FALSE)+
    labs(title = survey)
  )
  if (readline("Continue to next VAT? (Y/N)")=="N") {
    break
  }
}



save(survey_data_imputed,file=paste0(dataset_in_scriptsfile_directory,"miced_survey_9_Ubuntu18.04.3LTSdf_with_mirt_clustering_",".RData"))
