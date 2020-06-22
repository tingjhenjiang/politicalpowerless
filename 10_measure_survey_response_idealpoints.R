# 第Ｏ部份：環境設定 --------------------------------
if (!("benchmarkme" %in% rownames(installed.packages()))) try(install.packages("benchmarkme"))
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-try(paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model))
t_sessioninfo_running_with_cpu_locale<-try(gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2])))
source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/shared_functions.R"), encoding="UTF-8")
load(file=paste0(dataset_in_scriptsfile_directory, "miced_survey_9_with_mirt_lca_clustering.RData"), verbose=TRUE)
load(file=paste0(dataset_in_scriptsfile_directory, "bills_answer_to_bill_bills_billcontent.RData"), verbose=TRUE)
survey_codebook_file<-paste0(dataset_file_directory,"all_survey_questions_englished.xlsx")
survey_keys <- c("2010overall","2016citizen")
survey_question_category_df<-lapply(c(1,3), function(fi,...) {
  openxlsx::read.xlsx(survey_codebook_file,sheet = fi)
},survey_codebook_file=survey_codebook_file) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(grepl(pattern="民主價值", x=CATEGORY, perl=TRUE) | CATEGORY=="議題") %>%
  dplyr::filter(SURVEY %in% !!survey_keys) %>%
  dplyr::filter(MEASUREMENT %in% c("nominal","ordinal")) %>%
  dplyr::filter(IMPUTATION!="ignore" & ID!="myown_indp_atti") %>%
  dplyr::mutate(itemtype=NA) %>%
  mutate_cond(MEASUREMENT=="nominal", itemtype="2PL") %>%
  mutate_cond((grepl(pattern=";3", x=ANSWER) & itemtype=="2PL"), itemtype="nominal") %>%
  mutate_cond(MEASUREMENT=="ordinal", itemtype="graded") %>%
  mutate_cond(grepl(pattern="(1分;22分|22分;33分)", x=ANSWER), itemtype="grsm") %>%
  dplyr::arrange(SURVEYCOMPLETEID) %>%
  lapply(survey_keys, function(k, data) {
    return(dplyr::filter(data, SURVEY==!!k))
  }, data=.) %>%
  magrittr::set_names(survey_keys)

reduce_levels_from_ten_to_seven <- function(X) {
  if (length(unique(X)) %in% c(10,11)) {
    newlist<- list(0,1,2,2,3,3,3,4,4,5,6) %>%
      magrittr::set_names(1:11)
    X<-dplyr::recode(X, !!!newlist)
    #X<-dplyr::recode(X, `0`=,`1`=,`2`=,`3`=,``=,``=,``=,``=,``=,``=,``=,``=,``=,)
  }
  return(X)
}

# fa parallel探測因素數目 --------------------------------
survey_parallelfa_arguments_df<-data.frame("survey"=survey_keys) %>%
  cbind(., imp = rep(imputation_sample_i_s, each = nrow(.))) %>%
  dplyr::left_join(data.frame(
    survey=c("2004citizen","2004citizen","2004citizen","2010env","2010env","2010env","2010overall","2010overall","2010overall","2016citizen"),
    term=c(5,6,"5&6",7,8,"7&8",7,8,"7&8",9)
    )) %>%
  cbind(., fm = rep(c("minres", "ml", "wls", "pa"), each = nrow(.))) %>%
  cbind(., needvars = rep(c("limited", "unlimited"), each = nrow(.))) %>%
  dplyr::mutate(store_key=paste0(survey, "_imp", imp, "_term", term, "_fm", fm, "_needvars", needvars)) %>%
  dplyr::arrange(needvars, survey, imp) %>%
  dplyr::filter(needvars=="unlimited", survey %in% c("2010overall","2016citizen"), term %in% c(7,9))
survey_parallelfa_n_factors_file <- paste0(dataset_in_scriptsfile_directory, "survey_parallelfa_n_factors.RData")
random.polychor.survey_parallelfa_n_factors_file <- paste0(dataset_in_scriptsfile_directory, "random.polychor.survey_parallelfa_n_factors.RData") 
if (FALSE) {
  survey_parallelfa_n_factors <- survey_parallelfa_arguments_df$store_key %>%
    magrittr::set_names(custom_parallel_lapply(., function(fikey, ...) {
      fi<-which(survey_parallelfa_arguments_df$store_key==fikey)
      message(paste("now in", survey_parallelfa_arguments_df[fi,"store_key"],"and fi is",fi))
      argument_imp<-survey_parallelfa_arguments_df$imp[fi]
      parallel_fa_method_fm<-as.character(survey_parallelfa_arguments_df$fm[fi])
      surveyvars<-survey_question_category_df[[survey_parallelfa_arguments_df$survey[fi]]] %>%
        dplyr::filter(SURVEY==!!survey_parallelfa_arguments_df$survey[fi]) %>%
        magrittr::use_series("ID")
      ordinalsurveyvars<-dplyr::filter(survey_question_category_df[[survey_parallelfa_arguments_df$survey[fi]]], MEASUREMENT=="ordinal") %>%
        magrittr::use_series("ID") %>%
        c("myown_indp_atti")
      targetsurveydf<-as.character(survey_parallelfa_arguments_df$survey[fi]) %>%
        magrittr::extract2(survey_data_imputed, .) %>%
        {.[.$.imp==argument_imp, surveyvars]} %>%
        dplyr::mutate_all(unclass)
      notemptyvars<-sapply(targetsurveydf, is.na) %>% colSums() %>% .[.==0] %>% names()
      fewerthan8catgsvars<-sapply(targetsurveydf, function(X) {
        length(unique(X))
      }) %>% .[.<=8] %>% names()
      largerthan8catgsvars<-dplyr::setdiff(names(targetsurveydf), fewerthan8catgsvars)
      targetsurveydf <- dplyr::mutate_at(targetsurveydf, .vars=largerthan8catgsvars, .funs=~reduce_levels_from_ten_to_seven(.))
      finalneedvars<-notemptyvars
      #finalneedvars<-dplyr::intersect(notemptyvars, fewerthan8catgsvars)
      #reduce_levels_from_ten_to_seven(targetsurveydf$d12)
      if (as.character(survey_parallelfa_arguments_df$needvars[fi])=="limited") {
        argdf_term <- as.character(survey_parallelfa_arguments_df$term[fi])
        if (grepl(pattern="&", x=argdf_term) ) {
          argdf_term <- unlist(stringr::str_split(argdf_term, pattern="&"))
        }
        extractedneedvars <- magrittr::extract(term_related_q, argdf_term) %>%
          unlist() %>%
          unique()
        finalneedvars <- extractedneedvars %>%
          gsub(pattern=paste0(as.character(survey_parallelfa_arguments_df$survey[fi]),"@"),"",.) %>%
          dplyr::intersect(finalneedvars) %>%
          unique()
      }
      targetsurveydf<-targetsurveydf[,finalneedvars]
      res_n_factors <- try({
        #https://rmc.ehe.osu.edu/files/2018/08/Parallel-AnalysisOct2017.pdf
        psych::fa.parallel(targetsurveydf, fm=parallel_fa_method_fm, main="Parallel Analysis Scree Plots", cor="poly")
        #random.polychor.pa::random.polychor.pa(nrep=50, data.matrix = targetsurveydf, q.eigen=.99)
      })
      res_n_factors <- hullEFA(X=dplyr::mutate_all(targetsurveydf, as.numeric),
        extr="ML", index_hull="CAF")
      res_n_factors <- try({ dplyr::mutate_all(targetsurveydf, as.numeric) %>%
          EFA.MRFA::parallelMRFA(corr="Polychoric")})
      if(is(res_n_factors, 'try-error')) {
        message(paste0("error at ",survey_parallelfa_arguments_df[fi,"store_key"]))
      }
      while (TRUE) {
        tryloadsaveresult<-try({
          load(file=survey_parallelfa_n_factors_file, verbose=TRUE)
          survey_parallelfa_n_factors[[as.character(survey_parallelfa_arguments_df$store_key[fi])]]<-res_n_factors
          save(survey_parallelfa_n_factors, file=survey_parallelfa_n_factors_file)
        }) #, envir = .GlobalEnv
        if(!is(tryloadsaveresult, 'try-error')) {
          break
        }
      }
      return(res_n_factors)
    }, survey_question_category_df=survey_question_category_df,
    survey_data_imputed=survey_data_imputed,
    survey_parallelfa_arguments_df=survey_parallelfa_arguments_df,
    survey_question_category_df=survey_question_category_df,
    term_related_q=term_related_q,
    survey_parallelfa_n_factors_file=survey_parallelfa_n_factors_file,
    reduce_levels_from_ten_to_seven=reduce_levels_from_ten_to_seven,
    method=parallel_method, mc.cores=1), .)
  
  save(survey_parallelfa_n_factors, file=survey_parallelfa_n_factors_file)
  ##save(random.polychor.survey_parallelfa_n_factors, file=random.polychor.survey_parallelfa_n_factors_file)
}

load(file=survey_parallelfa_n_factors_file, verbose=TRUE)

survey_parallelfa_arguments_df<-lapply(names(survey_parallelfa_n_factors), function(store_key_of_survey_parallelfa_n_factors_args,...) {
  obj_parallel_analysis_result<-survey_parallelfa_n_factors[[store_key_of_survey_parallelfa_n_factors_args]]
  data.frame(store_key=store_key_of_survey_parallelfa_n_factors_args,
             nfact=tryCatch(obj_parallel_analysis_result$nfact, error=function(msg) {return(NA)}),
             ncomp=tryCatch(obj_parallel_analysis_result$ncomp, error=function(msg) {return(NA)})
             )
},survey_parallelfa_n_factors=survey_parallelfa_n_factors) %>%
  dplyr::bind_rows() %>%
  dplyr::left_join(survey_parallelfa_arguments_df,.)

# bills_answer_to_bill
# bills_billcontent
# bills_billcontent_with_relatedq
# term_related_q
# bills_billid_to_relatedq_pairs

distincted_survey_parallelfa_arguments_df<-survey_parallelfa_arguments_df %>%
  dplyr::distinct(survey, imp, term, needvars, nfact, ncomp) %>%
  dplyr::arrange(survey, term, imp, needvars, ncomp) %>%
  reshape2::melt(., id.vars=c("survey","imp","term","needvars")) %>%
  dplyr::rename(ncompnfact=value, reduct_type=variable) %>%
  dplyr::filter(!is.na(ncompnfact)) %>%
  dplyr::distinct(survey, term, imp, reduct_type, ncompnfact)
  #dplyr::filter(needvars=="limited")  %>%
  #dplyr::filter(!(term %in% c(5,6,7,8))) %>%

distincted_survey_parallelfa_arguments_df %<>% dplyr::bind_rows(
  data.frame("survey"="2016citizen", "term"="9") %>%
    cbind(., imp = rep(imputation_sample_i_s, each = nrow(.))) %>%
    cbind(., ncompnfact = rep(c(12,21:23), each = nrow(.))) %>%
    cbind(., reduct_type = rep(c("nfact"), each = nrow(.)))
) %>%
  dplyr::bind_rows(
    data.frame("survey"="2010overall", "term"="7") %>%
      cbind(., imp = rep(imputation_sample_i_s, each = nrow(.))) %>%
      cbind(., ncompnfact = rep(5:8, each = nrow(.))) %>%
      cbind(., reduct_type = rep(c("nfact"), each = nrow(.)))
  ) %>%
  dplyr::distinct_all()

# explore data distributions on issues --------------------------------

if (FALSE) {
  for (surveytitle in survey_keys) {
    needqsdf <- magrittr::extract2(survey_question_category_df,surveytitle)
    needqs <- needqsdf$ID
    needdf <- magrittr::extract2(survey_data_imputed,surveytitle)
    for (needq in needqs) {
      question_detail<-dplyr::filter(needqsdf,ID==!!needq) %>% magrittr::use_series("QUESTION")
      custom_plot(needdf, fvar=needq, weightvar="myown_wr", usingsurveypkg=FALSE) %>% print()
      if (readline(paste("now in",surveytitle,needq,question_detail,"continue?"))=="N") break
    }
  }
}

# exploratory IRT 探測問卷因素結構 --------------------------------

distincted_survey_parallelfa_arguments_df_runonly<- distincted_survey_parallelfa_arguments_df %>%
  dplyr::distinct(survey, imp, term, ncompnfact) %>%
  dplyr::mutate(runmirt_store_key=paste0(survey,"_imp",imp,"_ncompnfact",ncompnfact)) #%>%
  #dplyr::filter(imp==1)

survey_idealpoints_mirt_models_file <- paste0(save_dataset_in_scriptsfile_directory, "survey_idealpoints_mirt_models.RData")
if ({avoid_run_duplicated_models<-TRUE;avoid_run_duplicated_models}) {
  load(file=survey_idealpoints_mirt_models_file, verbose=TRUE)
  distincted_survey_parallelfa_arguments_df_runonly<-dplyr::filter(distincted_survey_parallelfa_arguments_df_runonly, !(runmirt_store_key %in% !!names(survey_idealpoints_mirt_models)) )
}
if (FALSE) {
  survey_idealpoints_mirt_models<-distincted_survey_parallelfa_arguments_df_runonly$runmirt_store_key %>%
    magrittr::set_names(custom_parallel_lapply(., function(fikey, ...) {
      needrow<-dplyr::filter(distincted_survey_parallelfa_arguments_df_runonly, runmirt_store_key==!!fikey)
      argdf_term <- as.character(needrow$term)
      if (grepl(pattern="&", x=argdf_term) ) {
        argdf_term <- unlist(stringr::str_split(argdf_term, pattern="&"))
      }
      if (FALSE) {
        extractedneedvars <- magrittr::extract(term_related_q, argdf_term) %>%
          unlist() %>%
          unique() %>%
          sort()
        extractedneedvars_without_survey <- gsub(pattern=paste0(as.character(needrow$survey),"@"), replacement="", x=extractedneedvars)
      } else {
        extractedneedvars <- magrittr::extract2(survey_question_category_df, needrow$survey) %>%
          magrittr::use_series("SURVEYCOMPLETEID")
        extractedneedvars_without_survey <- magrittr::extract2(survey_question_category_df, needrow$survey) %>%
          magrittr::use_series("ID")
      }
      to_explor_IRT_itemtypes<-data.frame(SURVEYCOMPLETEID=extractedneedvars, ID=extractedneedvars_without_survey, SURVEY=needrow$survey) %>%
        dplyr::left_join(survey_question_category_df[[needrow$survey]]) %>%
        mutate_cond(grepl(pattern="myown_indp_atti", x=SURVEYCOMPLETEID), itemtype="graded")
      needsurveydatadf <- survey_data_imputed[[as.character(needrow$survey)]] %>%
        {.[.$.imp==needrow$imp,]}
      to_explor_IRT_data<-needsurveydatadf[,to_explor_IRT_itemtypes$ID] %>%
        dplyr::mutate_all(unclass)
      mirtmethod<-if (as.integer(needrow$ncomp)>=3) "QMCEM" else "EM"
      explor_mirt_model<-mirt::mirt(
        to_explor_IRT_data,
        model=as.integer(needrow$ncomp),
        itemtype=to_explor_IRT_itemtypes$itemtype,
        technical=list(NCYCLES=250,MAXQUAD=40000),
        survey.weights = needsurveydatadf[,c("myown_wr")],
        method=mirtmethod , SE=TRUE
      )
      while (TRUE) {
        tryloadsaveresult<-try({
          load(file=survey_idealpoints_mirt_models_file, verbose=TRUE)
          survey_idealpoints_mirt_models[[fikey]]<-explor_mirt_model
          save(survey_idealpoints_mirt_models, file=survey_idealpoints_mirt_models_file)
        }) #, envir = .GlobalEnv
        if(!is(tryloadsaveresult, 'try-error')) {
          break
        }
      }
      return(explor_mirt_model)
    }, survey_question_category_df=survey_question_category_df,
    survey_data_imputed=survey_data_imputed,
    survey_parallelfa_arguments_df=survey_parallelfa_arguments_df,
    survey_question_category_df=survey_question_category_df,
    term_related_q=term_related_q,
    distincted_survey_parallelfa_arguments_df_runonly=distincted_survey_parallelfa_arguments_df_runonly,
    survey_idealpoints_mirt_models_file=survey_idealpoints_mirt_models_file,
    method=parallel_method, mc.cores=5
    ), .) 
}

#save(survey_idealpoints_mirt_models, file=survey_idealpoints_mirt_models_file)

# Checking factor scores and factor structure and goodness of fit --------------------------------
load(file=survey_idealpoints_mirt_models_file, verbose=TRUE)
complete_inf_mirt_models<-lapply(survey_idealpoints_mirt_models,tidymirt:::glance.SingleGroupClass) %>%
  dplyr::bind_rows() %>%
  data.frame(runmirt_store_key=names(survey_idealpoints_mirt_models)) %>%
  dplyr::left_join(distincted_survey_parallelfa_arguments_df_runonly, .) %>%
  dplyr::arrange(survey, SABIC, BIC, AIC, imp)
write.csv(complete_inf_mirt_models, "TMP.csv")
mirtmodelinfindicators<-c("AIC","AICc","SABIC","HQ","BIC")

#check which model is better among different dimensions
if (FALSE) {
  for (survey_title in as.character(unique(complete_inf_mirt_models$survey))) {
    for (needimp in 1:5) {
      mirtcomparei_df<-dplyr::filter(complete_inf_mirt_models, survey==!!survey_title, imp==!!needimp)
      for (baseline_key in 1:nrow(mirtcomparei_df)) {
        baselinerow<-mirtcomparei_df[baseline_key, ]
        mirtcomparei_a<-baselinerow$runmirt_store_key
        othercomparemodel_keys<-dplyr::filter(mirtcomparei_df, runmirt_store_key!=!!mirtcomparei_a) %>%
          magrittr::use_series("runmirt_store_key")
        for (mirtcomparei_b in othercomparemodel_keys) {
          modela<-survey_idealpoints_mirt_models[[mirtcomparei_a]]
          modela_bic<-mirt::extract.mirt(modela, "BIC")
          modelb<-survey_idealpoints_mirt_models[[mirtcomparei_b]]
          bettermodel_text<-data.frame(modela_ind=sapply(mirtmodelinfindicators, function (X,m) {mirt::extract.mirt(m,X)}, m=modela),
                                       modelb_ind=sapply(mirtmodelinfindicators, function (X,m) {mirt::extract.mirt(m,X)}, m=modelb)) %>%
            dplyr::mutate(smaller=magrittr::is_less_than(modela_ind,modelb_ind)) %>%
            magrittr::use_series("smaller") %>%
            sum() %>%
            {if (.>=3) mirtcomparei_a else mirtcomparei_b}
          mirt::anova(modela,modelb) %>%
            print()
          message(paste("better model is",bettermodel_text))
          if (readline(paste("now in",mirtcomparei_b,"and basis is",mirtcomparei_a,"(whose BIC is",modela_bic,") continue?"))=="N") break
        } #end othercomparemodel_keys
      } #end baseline key
    } #end needimp
  } #end survey title
}

#merge fscore data
if (FALSE) {
  loopmirtmodellist_keys<-dplyr::filter(complete_inf_mirt_models, ncompnfact %in% c(6,12)) %>%
    dplyr::arrange(survey,imp) %>%
    magrittr::use_series("runmirt_store_key")
  policy_idealpoint_colname_header<-"policyidealpoint"
  for (mirt_model_on_survey_key in loopmirtmodellist_keys) {
    #mirt_model_on_survey_key <- loopmirtmodellist_keys[1]
    message(paste("now in",mirt_model_on_survey_key))
    needrow<-dplyr::filter(complete_inf_mirt_models, runmirt_store_key==!!mirt_model_on_survey_key)
    needsurvey<-as.character(needrow$survey) 
    surveydataids<-dplyr::filter(survey_data_imputed[[needsurvey]], .imp==!!needrow$imp) %>%
      dplyr::select(SURVEY, id, .imp)
    mirtsurveyresult<-custom_mirt_coef_to_df(survey_idealpoints_mirt_models[[mirt_model_on_survey_key]], printSE = TRUE)
    write.csv(mirtsurveyresult, file="TMP.csv")
    #View(mirtsurveyresult)
    #mirt:::summary(survey_idealpoints_mirt_models[[mirt_model_on_survey_key]], rotate="varimax") %>% print()
    #mirt::itemfit(survey_idealpoints_mirt_models[[mirt_model_on_survey_key]], QMC=TRUE) %>% print()
    if (TRUE) {
      mirtfscoresdf<- mirt::fscores(survey_idealpoints_mirt_models[[mirt_model_on_survey_key]], QMC=TRUE) %>%
        {magrittr::set_colnames(., paste0(policy_idealpoint_colname_header, colnames(.)))} %>%
        data.frame()
      median_policy_idealpoint<-SpatialNP::spatial.location(mirtfscoresdf, shape=TRUE,score="signrank") %>%
        as.numeric()
      cos_similarity_to_median_policy_idealpoint<-apply(mirtfscoresdf, 1, FUN=lsa::cosine, y=median_policy_idealpoint)
      euclid_distance_to_median_policy_idealpoint<-apply(mirtfscoresdf, 1, FUN=custom_eucli_similarity, y=median_policy_idealpoint)
      more_similar_cos<-quantile(cos_similarity_to_median_policy_idealpoint)[4] %>%
        magrittr::is_weakly_greater_than(cos_similarity_to_median_policy_idealpoint,.) %>%
        as.numeric()
      more_similar_euclid<-quantile(euclid_distance_to_median_policy_idealpoint)[2] %>%
        magrittr::is_weakly_less_than(euclid_distance_to_median_policy_idealpoint, .) %>%
        as.numeric()
      
      mirtfscoresdf<-data.frame(
        cos_similarity_to_median_policy_idealpoint,
        euclid_distance_to_median_policy_idealpoint,
        more_similar_cos,
        more_similar_euclid) %>%
        magrittr::set_colnames(c(
          paste0(policy_idealpoint_colname_header,"_cos_similarity_to_median"),
          paste0(policy_idealpoint_colname_header,"_eucli_distance_to_median"),
          paste0(policy_idealpoint_colname_header,"_more_similar_cos"),
          paste0(policy_idealpoint_colname_header,"_more_similar_eucli")
        )) %>%
        dplyr::bind_cols(mirtfscoresdf, .) %>%
        dplyr::bind_cols(surveydataids, .)
      
      survey_data_imputed[[needsurvey]] <- dplyr::bind_rows(
        dplyr::semi_join(survey_data_imputed[[needsurvey]], mirtfscoresdf, by = c(".imp", "id", "SURVEY")) %>%
          dplyr::select(-dplyr::starts_with(!!policy_idealpoint_colname_header)) %>%
          dplyr::left_join(mirtfscoresdf, by = c(".imp", "id", "SURVEY")),
        dplyr::anti_join(survey_data_imputed[[needsurvey]], mirtfscoresdf, by = c(".imp", "id", "SURVEY") )  
      )
    }
    if (readline(paste("now in",mirt_model_on_survey_key,"continue?"))=="N") break
  }
  survey_data_imputed <- lapply(survey_data_imputed, function(X) {
    dplyr::mutate_at(X, dplyr::vars(dplyr::contains("_more_similar")), as.factor)
  })
  survey_with_idealpoint_name<-paste0(dataset_in_scriptsfile_directory, "miced_survey_9_with_mirt_lca_clustering_idealpoints.RData")
  #save(survey_data_imputed, file=survey_with_idealpoint_name)
  load(file=survey_with_idealpoint_name, verbose=TRUE)
  if (readline(paste("now in",mirt_model_on_survey_key,"continue?"))=="N") break
}


# Testing Multivariate Normality of policy preference using R --------------------------------
#MVN package
#mvnormtest::mshapiro.test()
if (FALSE) {
  multivariate_test_args <- data.frame(survey_key=survey_keys) %>%
    cbind(., imp = rep(imputation_sample_i_s, each = nrow(.))) %>%
    cbind(., testm = rep(c("mardia", "hz", "royston", "dh","energy"), each = nrow(.))) %>%
    dplyr::mutate_at(c("survey_key","testm"), as.character) %>%
    dplyr::arrange(survey_key, imp, testm)
  multivariate_test_res <- custom_parallel_lapply(1:nrow(multivariate_test_args), function(rowi, ...) {
    needrow<-multivariate_test_args[rowi,]
    t<-dplyr::filter(survey_data_imputed[[needrow$survey_key]], .imp==!!needrow$imp) %>%
      dplyr::select(dplyr::starts_with("policy"))
    res<-MVN::mvn(t,mvnTest=as.character(needrow$testm))
    dplyr::bind_cols(needrow, res$multivariateNormality) %>%
      return()
  }, multivariate_test_args=multivariate_test_args, survey_data_imputed=survey_data_imputed,
  method=parallel_method) %>%
    plyr::rbind.fill()
  write.csv(multivariate_test_res, "TMP.csv")
  
  for (n in names(t)) {
    custom_plot(t,n) %>% print()
  }
  custom_plot(t,"policyidealpointF1")
  # ANOVA, mixed ANOVA, ANCOVA and MANOVA, Kruskal-Wallis test and Friedman test
  # Nonparametric Inference for Multivariate Data:
  #   The R Package npmv
  #https://cran.r-project.org/web/packages/MultNonParam/MultNonParam.pdf
  # Testing Mean Differences among Groups: Multivariate and Repeated Measures Analysis with Minimal Assumptions
  #MNM https://cran.r-project.org/web/packages/MNM/index.html
  #SpatialNP https://cran.r-project.org/web/packages/SpatialNP/index.html
}

# Testing Normality of political participation using R --------------------------------

if (FALSE) {
  pp_normality_test_args <- data.frame(survey_key=survey_keys) %>%
    cbind(., imp = rep(imputation_sample_i_s, each = nrow(.))) %>%
    cbind(., testm = rep(c("Shapiro-Wilk", "Anderson-Darling", "Cramer-vonMises", "Lilliefors","PearsonChi-Squared","Shapiro-Francia"), each = nrow(.))) %>%
    dplyr::mutate(store_key=paste0(survey_key,"_imp",imp,"_",testm)) %>%
    dplyr::mutate_at(c("survey_key","testm","store_key"), as.character) %>%
    dplyr::mutate_at(c("imp"), as.integer) %>%
    dplyr::arrange(survey_key, imp, testm)
  pp_normality_test_res<-custom_parallel_lapply(1:nrow(pp_normality_test_args), function(rowi, ...) {
    needrow<-pp_normality_test_args[rowi,]
    needv<-survey_data_imputed[[needrow$survey_key]] %>%
      dplyr::filter(.imp==!!needrow$imp) %>%
      magrittr::use_series("myown_factoredparticip")
    switch(needrow$testm, 
           "Shapiro-Wilk"=shapiro.test(needv),
           "Anderson-Darling"=nortest::ad.test(needv),
           "Cramer-vonMises"=nortest::cvm.test(needv),
           "Lilliefors"=nortest::lillie.test(needv),
           "PearsonChi-Squared"=nortest::pearson.test(needv),
           "Shapiro-Francia"=nortest::sf.test(needv)
    ) %>%
      magrittr::use_series("p.value") %>%
      data.frame("pvalue"=.) %>%
      dplyr::bind_cols(needrow, .)
  },pp_normality_test_args=pp_normality_test_args,survey_data_imputed=survey_data_imputed,
  method=parallel_method) %>%
    dplyr::bind_rows() %>%
    dplyr::select(-store_key) %>%
    dplyr::mutate(TF=pvalue<=0.05)
  write.csv(pp_normality_test_res,"TMP.csv")
}


# CFA IRT 驗證性因素分析 問卷因素結構 --------------------------------
#mirt example https://philchalmers.github.io/mirt/html/mirt.html
#https://www.yongxi-stat.com/explore-factor-analysis-introduction/

#2010overall
if (FALSE) {
  needimp<-1
  surveydata<-dplyr::filter(survey_data_imputed$`2010overall`, .imp==!!needimp)
  testvars<-c("v39d","v39e","v40")
  testvars<-c("v27b")
  testvars<-c("v78a","v78b","v78c","v78d","v78e","v78f","v78g","v78h","v78i") #baseline 0.315 
  summaryofmirt_results<-list()
  for (i in 1:length(testvars)) {
    needtestvars <- base::setdiff(testvars, testvars[i])
    if (length(needtestvars)<3) next
    testmodel<-mirt::mirt(
      data=dplyr::mutate_all(surveydata[,needtestvars, drop=FALSE], .funs=unclass) ,
      model=1,
      itemtype = "graded",
      technical = list("NCYCLES"=40000),
      survey.weights = surveydata[,c("myown_wr")],
      SE=TRUE
    )
    summaryofmirt_results[[i]]<-capture.output(mirt:::summary(testmodel)) %>%
      grep(pattern="Proportion|SS loadings", x=., value=TRUE) %>%
      {data.frame(drop=testvars[i],ss=.[1], prop=.[2])}
  }
  summaryofmirt_result<-dplyr::bind_rows(summaryofmirt_results)
}


# v40
# myown_indp_atti
# v80
# v68g
# v27b
# 
# #2016citizen
# c1a+c1b+c1c+c1d+c1e
# c2+c3+c4+c5+c6
# ? c10+c11+c12+c13+c14
# d1+d2a+d2b+d3a+d3b+d4
# d5a+d5b+d5c+d5d+d5e+d5f
# d7a+d7b+d7c+d7d+d7e+d7f+d7g+d7h+d7i+d7j+d7k+d8a+d8b+d8c  +f9
# d11a+d11b+d12+d13a+d13b+d14a+d14b+d14c
# d17a+d17b+d17c
# f3+f4+f5
# myown_indp_atti
