source(file = "13_preprocessing_merge_all_datasets.R")
# modeling on idealpoint（理想點與集群／人口變項模型） --------------------------------
#Handle Missing Values with brms
#https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html
#DEM 7473 - Bayesian Regression using the INLA Approach
#https://rpubs.com/corey_sparks/431920
#DEM 7473 - Week 7: Bayesian modeling part 1
#https://rpubs.com/corey_sparks/431913
#DEM 7473 - Week 5: Hierarchical Models - Cross level interactions & Contextual Effects
#https://rpubs.com/corey_sparks/424927
#DEM 7473 - Week 3: Basic Hierarchical Models - Random Intercepts and Slopes
#https://rpubs.com/corey_sparks/420770
#DEM 7283 Example 10 - Survey Information and Small Area Estimation
#https://rpubs.com/corey_sparks/484730
#Example of using survey design weights Bayesian regression models for survey data
#https://rpubs.com/corey_sparks/157901
#HLM! 想聽不懂,很難!
#https://www.slideshare.net/beckett53/hlm-20140929
#第 60 章 隨機截距模型中加入共變量 random intercept model with covariates
#https://wangcc.me/LSHTMlearningnote/%E9%9A%A8%E6%A9%9F%E6%88%AA%E8%B7%9D%E6%A8%A1%E5%9E%8B%E4%B8%AD%E5%8A%A0%E5%85%A5%E5%85%B1%E8%AE%8A%E9%87%8F-random-intercept-model-with-covariates.html
#https://bookdown.org/wangminjie/R4SS/
#R-Sessions 16: Multilevel Model Specification (lme4)
#http://www.rensenieuwenhuis.nl/r-sessions-16-multilevel-model-specification-lme4/
#Bayesian mixed effects (aka multi-level) ordinal regression models with brms
#https://kevinstadler.github.io/blog/bayesian-ordinal-regression-with-random-effects-using-brms/
#Building a Multilevel Model in BRMS Tutorial: Popularity Data
#https://www.rensvandeschoot.com/tutorials/brms-started/
#Advanced Bayesian Multilevel Modeling with the R Package brms
#https://cran.r-project.org/web/packages/brms/vignettes/brms_multilevel.pdf
# 58 for dependent data
#https://wangcc.me/LSHTMlearningnote/Hierarchical.html
#Advanced Bayesian Multilevel Modelingwith the R Package brms (syntax)
#https://arxiv.org/pdf/1705.11123.pdf
#fixed effect v. random effect
#https://zhuanlan.zhihu.com/p/60528092
#/pkg/rproject/R-latest/bin/R
#ploting analysing result
#https://biol609.github.io/lectures/23c_brms_prediction.html#243_assessing_fit
#easy bayes
#https://m-clark.github.io/easy-bayes/posterior-predictive-checks.html
#calc p-value hypothesis testing
#https://www.rensvandeschoot.com/tutorials/brms-started/
#https://bookdown.org/content/4253/introducing-the-multilevel-model-for-change.html
#not normal https://rdrr.io/cran/clubSandwich/f/vignettes/panel-data-CRVE.Rmd
#https://www.researchgate.net/publication/251965897_Cluster-robust_standard_errors_using_R
#merDeriv
#clubSandwich
#https://wangcc.me/LSHTMlearningnote/random-intercept.html
#Does it make sense to include a factor as both fixed and random factor in a Linear Mixed Effects Model?
#https://stats.stackexchange.com/questions/263194/does-it-make-sense-to-include-a-factor-as-both-fixed-and-random-factor-in-a-line
#Test homogeneity in lmer models
#https://stats.stackexchange.com/questions/255546/test-homogeneity-in-lmer-models
#nests: id
#data analysis after multiple imputation
#https://bookdown.org/mwheymans/bookmi/data-analysis-after-multiple-imputation.html
#pool
#https://francish.netlify.app/post/multiple-imputation-in-r-with-regression-output/
#https://www.jaredknowles.com/journal/2014/5/17/mixed-effects-tutorial-2-fun-with-mermod-objects
#pick parameters
#https://easystats.github.io/parameters/index.html
#parameters::model_parameters(model)
#註：micombineresult原亦定義於本檔，與shared_functions.R重複，已移除（保留shared_functions.R版本）
analyse_idealpoint_class <- R6::R6Class("analyse_idealpoint", inherit=merge_all_datasets_class, public = list(
  all_idealpoint_models_file = NULL,
  initialize = function(dataset_in_scriptsfile_directory="/mnt", filespath="/mnt", dataset_file_directory="/mnt", debug_func_mode=TRUE) {
    super$initialize(dataset_in_scriptsfile_directory=dataset_in_scriptsfile_directory, filespath=filespath, dataset_file_directory=dataset_file_directory, debug_func_mode=debug_func_mode)
    self$all_idealpoint_models_file<-file.path(save_dataset_in_scriptsfile_directory,"analyse_res","idealpoint_models.RData")
  },
  # 常態性檢驗之各種變數轉換（from https://wangcc.me/LSHTMlearningnote/assumptions.html）
  ladder_x = function(x){
    data <- data.frame(x^3,x^2,x,sqrt(x),log(x),1/sqrt(x),1/x,1/(x^2),1/(x^3))
    names(data) <- c("cubic","square","identity","square root","log","1/(square root)",
                     "inverse","1/square","1/cubic")
    # options(scipen=5)
    test1 <- shapiro.test(data$cubic)
    test2 <- shapiro.test(data$square)
    test3 <- shapiro.test(data$identity)
    test4 <- shapiro.test(data$`square root`)
    test5 <- shapiro.test(data$log)
    test6 <- shapiro.test(data$`1/(square root)`)
    test7 <- shapiro.test(data$inverse)
    test8 <- shapiro.test(data$`1/square`)
    test9 <- shapiro.test(data$`1/cubic`)
    W.statistic <- c(test1$statistic,
                     test2$statistic,
                     test3$statistic,
                     test4$statistic,
                     test5$statistic,
                     test6$statistic,
                     test7$statistic,
                     test8$statistic,
                     test9$statistic)
    p.value <- c(test1$p.value,
                 test2$p.value,
                 test3$p.value,
                 test4$p.value,
                 test5$p.value,
                 test6$p.value,
                 test7$p.value,
                 test8$p.value,
                 test9$p.value)
    Hmisc::format.pval(p.value ,digits=5, eps = 0.00001, scientific = FALSE)
    Transformation <- c("cubic","square","identity","square root","log","1/(square root)",
                        "inverse","1/square","1/cubic")
    Formula <- c("x^3","x^2","x","sqrt(x)","log(x)","1/sqrt(x)","1/x","1/(x^2)","1/(x^3)")
    (results <- data.frame(Transformation, Formula, W.statistic, p.value))
  },
  # rlmer模型信賴區間（robustlmm無內建confint；原檔曾有另一簡化版定義，見檔尾備份）
  confint_rlmerMod = function(object, level = 0.95) {
    # Extract beta coefficients
    beta <- robustlmm:::fixef.rlmerMod(object)
    # Extract names of coefficients
    parm <- names(beta)
    # Extract standard errors for the coefficients
    #se <- sqrt(diag(robustlmm:::vcov.rlmerMod(object)))
    se<-coef(summary(object))[,2]
    # Set level of confidence interval
    z <- qnorm((1 + level) / 2)
    # Calculate CI
    ctab <- cbind(beta - (z * se),
                  beta + (z * se))
    # label column names
    colnames(ctab) <- c(paste(100 * ((1 - level) / 2), '%'),
                        paste(100 * ((1 + level) / 2), '%'))
    # Output
    return(ctab[parm, ])
  },
  # cluster-robust standard errors（clubSandwich），from https://www.jepusto.com/mi-with-clubsandwich/
  ret_robust_models = function(list_of_models, datadf, clustervar="myown_areakind", vcov="CR1", method="fork", ...) {
    coefsrobust_mods<-lapply(1:length(list_of_models), function(fi, ...) {
      list(obj=list_of_models[[fi]], vcov=vcov, cluster=datadf[[fi]][,clustervar]) %>%
        return()
    }, list_of_models=list_of_models, datadf=datadf, clustervar=clustervar, vcov=vcov) %>%
      custom_parallel_lapply(function(arg) {do.call(clubSandwich::coef_test, args=arg)}, method=method, ... )
    #function(fi, list_of_models=list_of_models, datadf=datadf, clustervar=clustervar, vcov=vcov, ...) {
    #clubSandwich::coef_test(list_of_models[[fi]], cluster=magrittr::use_series(datadf[[fi]], clustervar, vcov=vcov)) %>%
    #  return()
    #}, list_of_models=list_of_models, datadf=datadf, clustervar=clustervar, vcov=vcov, method=parallel_method, ...)
    return(coefsrobust_mods)
  },
  myown_robustlmm_as.data.frame.VarCorr.rlmerMod = function (x, row.names = NULL, optional = FALSE, order = c("cov.last", "lower.tri"), ...) {
    order <- match.arg(order)
    tmpf <- function(v, grp) {
      vcov <- c(diag(v), v[lt.v <- lower.tri(v, diag = FALSE)])
      sdcor <- c(attr(v, "stddev"), attr(v, "correlation")[lt.v])
      nm <- rownames(v)
      n <- nrow(v)
      dd <- data.frame(grp = grp, var1 = nm[c(seq(n), col(v)[lt.v])],
                       var2 = c(rep(NA, n), nm[row(v)[lt.v]]), vcov, sdcor,
                       stringsAsFactors = FALSE)
      if (order == "lower.tri") {
        m <- matrix(NA, n, n)
        diag(m) <- seq(n)
        m[lower.tri(m)] <- (n + 1):(n * (n + 1)/2)
        dd <- dd[m[lower.tri(m, diag = TRUE)], ]
      }
      dd
    }
    r <- do.call(rbind, c(mapply(tmpf, x, names(x), SIMPLIFY = FALSE),
                          deparse.level = 0))
    if (attr(x, "useSc")) {
      ss <- attr(x, "sc")
      r <- rbind(r, data.frame(grp = "Residual", var1 = NA,
                               var2 = NA, vcov = ss^2, sdcor = ss), deparse.level = 0)
    }
    rownames(r) <- NULL
    r
  },
  myown_robustlmm_vcov2 = function(object, level = 0.95, ...) {
    fit0 <- fit <- object
    object <- robustlmm:::VarCorr.rlmerMod(fit)
    vdd <- self$myown_robustlmm_as.data.frame.VarCorr.rlmerMod(object, order = "lower.tri")
    pars <- vdd[, "sdcor"]
    nms <- apply(vdd[, 1:3], 1, function(x) paste(na.omit(x),
                                                  collapse = "."))
    names(pars) <- nms
    Vcov <- as.matrix(vcov(fit0, useScale = FALSE))
    betas <- robustlmm:::fixef.rlmerMod(fit0)
    np_fixed <- length(betas)
    np_random <- length(pars)
    np <- np_fixed + np_random
    random <- list(coef = pars, vcov = NULL)
    fixed <- list(coef = betas, vcov = Vcov)
    coef1 <- c(fixed$coef, random$coef)
    ind_fixed <- 1:np_fixed
    ind_random <- ind <- np_fixed + 1:np_random
    se <- c(sqrt(diag(fixed$vcov)), rep(NA, np_random))
    np <- np_fixed + np_random
    dfr <- data.frame(index = 1:np, type = c(rep("fixed", np_fixed),
                                             rep("random", np_random)))
    s1 <- strsplit(names(random$coef), split = ".", fixed = TRUE)
    s1 <- unlist(lapply(s1, FUN = function(ll) {
      hh <- length(ll)
      label <- "SD"
      if (hh == 3) {
        label <- "Cor"
      }
      return(label)
    }))
    dfr$stat <- c(rep("Beta", np_fixed), s1)
    dfr$parm <- names(coef1)
    dfr$est <- coef1
    dfr$se <- se
    dfr <- sirt::parmsummary_extend(dfr = dfr, level = level,
                                    est_label = "est", se_label = "se")
    res <- list(par_summary = dfr, coef = coef1, se = se, fixed = fixed,
                random = random, np = np, np_random = np_random, np_fixed = np_fixed,
                ind_fixed = ind_fixed, ind_random = ind_random)
    #class(res) <- "lmer_vcov"
    return(res)
  },
  # pooling rlmer多重插補模型（仿miceadds::lmer_pool）
  myown_robustlmm_pool = function(models, level = 0.95, FUN = self$myown_robustlmm_vcov2, ...)
  {
    M <- length(models)
    qhat <- list()
    se <- list()
    NMI <- FALSE
    for (mm in 1:M) {
      args <- list(object = models[[mm]], level = level)
      res_mm <- do.call(what = FUN, args = args)
      qhat[[mm]] <- res_mm$coef
      se[[mm]] <- res_mm$se
    }
    res <- miceadds::pool_nmi(qhat = qhat, u = NULL, se = se, NMI = NMI,
                              comp_cov = TRUE, is_list = TRUE, method = 1)
    if (!NMI) {
      res$lambda_Between <- NA
      res$lambda_Within <- NA
    }
    #class(res) <- "lmer_pool"
    return(res)
  },
  myown_robustlmm_summary_pooledres = function (object, digits = 4, file = NULL, ...)
  {
    CDM::osink(file = file, suffix = paste0("__SUMMARY.Rout"))
    x <- object
    table <- data.frame(est = x$qbar)
    table$se <- sqrt(diag(x$Tm))
    table$t <- table[, 1]/table[, 2]
    table$df <- x$df
    table$p <- 2 * (1 - stats::pt(abs(table$t), x$df))
    table$`lo 95` <- table$est - stats::qt(0.975, x$df) * table$se
    table$`hi 95` <- table$est + stats::qt(0.975, x$df) * table$se
    table$fmi <- x$lambda
    table$fmi_Betw <- x$lambda_Between
    table$fmi_Within <- x$lambda_Within
    table <- as.data.frame(table)
    if (is.na(table$se)[1]) {
      table$df <- NA
    }
    if (!is.null(object$u_NULL)) {
      if (object$u_NULL) {
        table <- table[, "est", drop = FALSE]
      }
    }
    table0 <- table
    for (vv in seq(1, ncol(table))) {
      table[, vv] <- round(table[, vv], digits = digits)
    }
    print(table)
    return(table0)
    CDM::csink(file = file)
  },
  # rank-based (Jaeckel) 迴歸with cluster-correlated errors（改自jrfit::jrfit以支援大矩陣）
  customjrfit = function (x, y, block, yhat0 = NULL, scores = wscores, fitint = NULL,
                          var.type = "sandwich", fitblock = FALSE, tuser = NULL, ...)
  {
    call <- match.call()
    if (var.type == "sandwich") {
      v1 <- jrfit::tsand
    }
    if (var.type == "cs") {
      v1 <- jrfit::tcs
    }
    if (var.type == "ind") {
      v1 <- tind
    }
    if (var.type == "user") {
      if (!exists("tuser"))
        stop("tuser not defined")
      v1 <- tuser
    }
    x <- as.matrix(x)
    x1 <- as.matrix(cbind(rep(1, nrow(x)), x))
    qrx1 <- base::qr(x1)
    if (is.null(fitint)) {
      if (qrx1$rank == ncol(x1)) {
        x <- x1
        fitint <- TRUE
      }
      else {
        fitint <- FALSE
      }
    }
    else {
      if (fitint) {
        x <- x1
      }
    }
    P <- ncol(x)
    Q <- as.matrix(pbdDMAT::qr.Q(qrx1))
    q1 <- Q[, 1]
    xq <- as.matrix(Q[, 2:qrx1$rank])
    if (fitblock) {
      z <- model.matrix(~as.factor(block) - 1)
      z <- z[, 2:ncol(z)]
      QZ <- cbind(Q, z)
      qrxz <- pbdDMAT::qr(QZ)
      Qxz <- pbdDMAT::qr.Q(qrxz)
      zq <- Qxz[, (qrx1$rank + 1):qrxz$rank]
      xq <- cbind(xq, zq)
      x <- cbind(x, zq)
    }
    if (is.null(yhat0)) {
      beta0 <- suppressWarnings(quantreg::rq(y ~ xq - 1)$coef)
    }
    else {
      beta0 <- stats::lsfit(xq, yhat0, intercept = FALSE)$coef
    }
    fit <- Rfit::jaeckel(xq, y, beta0, scores = scores)
    if (fit$convergence != 0) {
      fit <- Rfit::jaeckel(xq, y, fit$par, scores = scores)
      if (fit$convergence != 0)
        warning("Convergence status not zero in jaeckel")
    }
    betahat <- fit$par
    yhat <- xq %*% betahat
    ehat <- y - yhat
    alphahat <- median(ehat)
    ehat <- ehat - alphahat
    yhat <- yhat + alphahat
    bhat <- stats::lsfit(x, yhat, intercept = FALSE)$coefficients
    tauhat <- Rfit::gettauF0(ehat, ncol(xq), scores)
    xxpxi <- x %*% base::chol2inv(base::chol(base::crossprod(x)))
    A1 <- base::crossprod(xxpxi, q1)
    A2 <- base::crossprod(xxpxi, xq)
    sigma0 <- jrfit::sigmastar(ehat, block, ncol(xq) + 1)
    taus <- Rfit::taustar(ehat, ncol(xq) + 1)
    V1 <- v1(ehat, xq, block, scores = scores)
    varhat <- sigma0 * taus * taus * Matrix::tcrossprod(A1) + tauhat *
      tauhat * Matrix::tcrossprod(A2 %*% V1, A2)
    DF <- switch(var.type, cs = length(y) - ncol(xq) - 1 - 1,
                 ind = length(y) - ncol(xq) - 1, sandwich = length(unique(block)),
                 user = length(unique(block)))
    res <- list(coefficients = bhat, residuals = ehat, fitted.values = yhat,
                varhat = varhat, x = x, y = y, block = block, tauhat = tauhat,
                tauhats = taus, qrx1 = qrx1, disp = fit$value, scores = scores,
                v1 = v1, fitint = fitint, P = P, var.type = var.type,
                DF = DF)
    res$call <- call
    class(res) <- "jrfit"
    res
  },
  # * check kamila result and prepare data ----------------------------------------------
  get_merged_acrossed_surveys_list = function() {
    needimps<-custom_ret_appro_kamila_clustering_parameters()
    survey_data_imputed <- self$get_survey_data_imputed_stage(stage="mirt_lca_clustering_idealpoints")
    ret_merged_for_idealpoint_and_pp_df_list(survey_data_imputed, dataset_in_scriptsfile_directory, minuspolicy=FALSE)
  },
  get_all_idealpoint_models_keys = function() {
    load_env <- new.env()
    all_idealpoint_models_loadsave_status<-try(load(file=self$all_idealpoint_models_file, envir=load_env, verbose=TRUE))
    all_idealpoint_models_keys<-try(names(load_env$all_idealpoint_models))
    all_idealpoint_models_keys<-if (is(all_idealpoint_models_keys,'try-error')) c() else all_idealpoint_models_keys
    all_idealpoint_models_keys
  },
  # * set model ------------------
  #full efficient
  #policyidealpoint_cos_similarity_to_median~1+SURVEY+cluster_kamila+(1|cluster_kamila)+myown_factoredses_overallscaled+myown_marriage+myown_age_overallscaled+myown_age_overallscaled*myown_age_overallscaled+myown_sex+myown_selfid+myown_religion+myown_areakind+(1|myown_areakind/admindistrict/adminvillage)+(1|admincity)
  #其他曾嘗試之隨機效果組合公式清單保留於檔尾備份區
  get_idealpoint_models_args = function(all_idealpoint_models_keys=self$get_all_idealpoint_models_keys(), needimps=1:6) {
    idealpoint_models_args<-data.frame("formula"=c(
      #"policyidealpoint_cos_similarity_to_median~1+SURVEY+cluster_kamila+(1|cluster_kamila)+myown_factoredses_overallscaled+myown_marriage+myown_age_overallscaled+myown_sex+myown_selfid+(1|myown_areakind/admindistrict/adminvillage)+(1|admincity)",
      "policyidealpoint_cos_similarity_to_median~1+SURVEY+cluster_kamila+myown_sex+myown_selfid+myown_factoredses_overallscaled+myown_age_overallscaled+(1|admincity/admindistrict/adminvillage)" #+(1|admincity) #myown_areakind/ #+(1|cluster_kamila)
    ), stringsAsFactors=FALSE) %>%
      cbind(., needimp = rep(needimps, each = nrow(.)), stringsAsFactors=FALSE) %>%
      dplyr::mutate(storekey=paste0(needimp,formula)) %>%
      dplyr::filter(!(formula %in% !!all_idealpoint_models_keys))
    idealpoint_models_args
  },
  # * modeling ------------------
  # lmerTest（可加權）或robustlmm（穩健、不加權）逐插補樣本擬合
  run_idealpoint_models_lmer = function(idealpoint_models_args, merged_acrossed_surveys_list, usingpackage=c("robustlmm","lmertest")[1], savemodelfilename="base_no_marriage_unweighted_noreligionareakind_catchadmincity", save=TRUE) {
    savemodelfilename %<>% paste0("(",usingpackage,")",.)
    idealpoint_models<-custom_apply_thr_argdf(idealpoint_models_args, "storekey", function(fikey, loopargdf, datadf, usingpackage, savemodelfilename, ...) {
      needrow<-dplyr::filter(loopargdf, storekey==!!fikey)
      #library(lme4)
      #library(lmerTest)
      t<-dplyr::select(datadf[[needrow$needimp]], -tidyselect::ends_with("NA"))
      if (usingpackage=="lmertest") {
        docallfunc<-lmerTest::lmer
        obsweight<-if (grepl(pattern="unweighted",x=savemodelfilename)) NULL else t$myown_wr
      } else if (usingpackage=="robustlmm") {
        library(robustlmm)
        docallfunc<-robustlmm::rlmer
        obsweight<-NULL
      }
      t<- list(formula=as.formula(needrow$formula), data=t, weights=obsweight) %>% #, weights=.$myown_wr
        #WeMix::mix(formula=f, data=datadf, weights=c("myown_wr","secondweight"))
        do.call(docallfunc, args=.) %>%
        #do.call(robustlmm::rlmer, args=.) %>%
        #do.call(lmerTest::lmer, args=.)
        #lmerTest::lmer(formula=f, data=datadf[[needrow$needimp]], weights=datadf[[needrow$needimp]]$myown_wr) %>%
        #do.call(lme4::lmer, args=.) %>%
        try()
    }, datadf=merged_acrossed_surveys_list, usingpackage=usingpackage, savemodelfilename=savemodelfilename)
    if (save==TRUE) {
      self$merge_idealpoint_models_into_all(idealpoint_models, savemodelfilename=savemodelfilename)
    }
    idealpoint_models
  },
  # 以survey design（svylme::svy2lme）納入抽樣權重之多層次模型
  run_idealpoint_models_svylme = function(merged_acrossed_surveys_list, save=TRUE) {
    library(svylme)
    #needformula<-as.formula(self$get_idealpoint_models_args(all_idealpoint_models_keys=c())[1,"formula"])
    needformula<-"policyidealpoint_cos_similarity_to_median~1+SURVEY+cluster_kamila+myown_factoredses_overallscaled+myown_sex+myown_selfid+myown_age_overallscaled+myown_religion+myown_areakind+(1|adminvillage)" #+(1|cluster_kamila)
    #single
    #des <- survey::svydesign(ids=~1, weight=~myown_wr, data=merged_acrossed_surveys_list[[1]])
    #t<-svylme::svy2lme(needformula, design=des, sterr=TRUE, return.devfun=FALSE, method="general")
    #multiple
    des <- mitools::imputationList(merged_acrossed_surveys_list) %>%
      survey::svydesign(ids=~1, weight=~myown_wr, data=.)
    all_idealpoint_models_svy<-survey:::with.svyimputationList(des,svylme::svy2lme(needformula, sterr=TRUE, return.devfun=FALSE, method="general"),multicore=TRUE)
    if (save==TRUE) {
      save(all_idealpoint_models_svy, file=file.path(save_dataset_in_scriptsfile_directory,"analyse_res","idealpoint_models(svylme).RData"))
    }
    all_idealpoint_models_svy
  },
  pool_idealpoint_models_svylme = function(all_idealpoint_models_svy=NULL, writecsv=TRUE) {
    if (is.null(all_idealpoint_models_svy)) {
      load_env <- new.env()
      load(file.path(save_dataset_in_scriptsfile_directory,"analyse_res","idealpoint_models(svylme).RData"), envir=load_env, verbose=TRUE)
      all_idealpoint_models_svy <- load_env$all_idealpoint_models_svy
    }
    combined_all_idealpoint_models_svy<-mitools::MIcombine(all_idealpoint_models_svy)
    mitools_summary_table<-cbind(summary(combined_all_idealpoint_models_svy), combined_all_idealpoint_models_svy$df)
    all_idealpoint_models_svy_mira<-mice::as.mira(all_idealpoint_models_svy)
    #combined_all_idealpoint_models_svy
    miceaddspooled<-miceadds::pool_mi(
      qhat=mitools::MIextract( all_idealpoint_models_svy, fun=coef),
      u=mitools::MIextract( all_idealpoint_models_svy, fun=vcov)#,
      #se="List of vector of standard errors. Either u or se must be provided.",
      #dfcom="Degrees of freedom of statistical analysis",
      #all_idealpoint_models_svy_mira$analyses
      )
    restable<-cbind(mitools_summary_table, summary(miceaddspooled))
    if (writecsv==TRUE) {
      write.csv(restable,"TMP.csv")
    }
    restable
  },
  # jrfit（rank-based穩健迴歸，village為block）之輸入建構
  build_jrfit_idealpoint_model_inputs = function(idealpoint_models_args, merged_acrossed_surveys_list) {
    custom_apply_thr_argdf(idealpoint_models_args, "storekey", function(fikey, loopargdf, datadf, modelvars, ...) {
      needrow<-dplyr::filter(loopargdf, storekey==!!fikey)
      #modelvars[["modelvars_ex_catg"]] %<>% base::setdiff("myown_marriage")
      #jrfit part
      dummyc_catg_vars<-unlist(modelvars[c("modelvars_ex_catg","modelvars_clustervars","modelvars_controllclustervars")]) %>%
        base::intersect(names(datadf[[needrow$needimp]]), .)
      greppattern_allmodelgvars<-dummyc_catg_vars %>%
        paste0(.,collapse="|") %>%
        paste0("(",.,")",collapse="|")
      allmodelvars<-base::intersect(names(datadf[[needrow$needimp]]), c(modelvars[["modelvars_ex_conti"]],modelvars[["modelvars_latentrelated"]])) %>%
        c(dummyc_catg_vars)
      t<-dplyr::select(datadf[[needrow$needimp]], -tidyselect::ends_with("NA")) %>%
        dummycode_of_a_dataframe(catgvars=dummyc_catg_vars) %>%
        dplyr::select(tidyselect::starts_with(c(allmodelvars,"policyidealpoint_cos_similarity_to_median","adminvillage")), -myown_selfid_population) %>%
        #{ .[complete.cases(.), ]} %>%
        {
          targetx<-dplyr::select(., -tidyselect::contains(c("policyidealpoint","particip")), -adminvillage) %>% # policyidealpoint_cos_similarity_to_median, -policyidealpoint_cos_similarity_to_median_ordinal
            dplyr::select(-tidyselect::contains(c("myown_marriage"))) #, "myown_age_overallscaled","myown_religion","myown_areakind"
          list(x=as.matrix(targetx) , y=.$policyidealpoint_cos_similarity_to_median, block=.$adminvillage, var.type="sandwich")
        } %>%
        try()
      return(t)
    }, datadf=merged_acrossed_surveys_list, modelvars=list(
      "modelvars_ex_conti"=self$modelvars_ex_conti,
      "modelvars_ex_catg"=self$modelvars_ex_catg,
      "modelvars_latentrelated"=self$modelvars_latentrelated,
      "modelvars_clustervars"=self$modelvars_clustervars,
      "modelvars_controllclustervars"=self$modelvars_controllclustervars ),
      mc.cores=1
    )
  },
  run_idealpoint_models_jrfit = function(jrfit_idealpoint_model_inputs) {
    customjrfit<-self$customjrfit
    idealpoint_models<-custom_parallel_lapply(jrfit_idealpoint_model_inputs, function(X) {
      library(jrfit)
      do.call(customjrfit, args=X) %>% return()
    },method=parallel_method)
    idealpoint_models
  },
  pool_idealpoint_models_jrfit = function(idealpoint_models, jrfit_idealpoint_model_inputs, writecsv=TRUE) {
    #df is weired
    library(jrfit)
    jrfitvcovs<-custom_parallel_lapply(names(idealpoint_models), function(X, ...) {
      jrfit::tcs(ehat=idealpoint_models[[X]]$residuals,
                 X=cbind("Intercept"=1, jrfit_idealpoint_model_inputs[[X]]$x),
                 block=idealpoint_models[[X]]$block)
    },idealpoint_models=idealpoint_models,jrfit_idealpoint_model_inputs=jrfit_idealpoint_model_inputs,method=parallel_method)
    jrfitcoefs<-custom_parallel_lapply(idealpoint_models, function(X) {summary(X) %>% .$coefficients %>% .[,"Estimate"]}, method=parallel_method)
    combined_all_idealpoint_models_jrfit<-mitools:::MIcombine(results=jrfitcoefs,variances=jrfitvcovs)
    mitools_summary_table_jrfit<-cbind(summary(combined_all_idealpoint_models_jrfit), combined_all_idealpoint_models_jrfit$df)
    miceaddspooled<-miceadds::pool_mi(
      qhat=lapply(idealpoint_models, function(X) { summary(X) %>% .$coefficients %>% .[,"Estimate"] }),
      u=lapply(idealpoint_models, function(X) { X$varhat }),
      se=lapply(idealpoint_models, function(X) { summary(X) %>% .$coefficients %>% .[,"Std. Error"] }),
      #dfcom=idealpoint_models[[1]]$DF, #"Degrees of freedom of statistical analysis",
      method="largesample"
    )
    if (writecsv==TRUE) {
      summary(miceaddspooled) %>% write.csv("TMP.csv")
    }
    randomeffectofjrfit<-lapply(idealpoint_models, function(X) {
      jrfit::vee(ehat=X$residuals, center=X$block)
      })
    list(mitools_summary_table=mitools_summary_table_jrfit, miceaddspooled=miceaddspooled, randomeffectofjrfit=randomeffectofjrfit)
  },
  # 合併新擬合模型入總檔並另存單獨備份
  merge_idealpoint_models_into_all = function(idealpoint_models, savemodelfilename) {
    try({
      load_env <- new.env()
      load(file=self$all_idealpoint_models_file, envir=load_env, verbose=TRUE)
      all_idealpoint_models <- load_env$all_idealpoint_models
      if (length(all_idealpoint_models)==0 | identical(all_idealpoint_models, list(a=1))) {
        all_idealpoint_models<-idealpoint_models
      } else {
        all_idealpoint_models<-rlist::list.merge(all_idealpoint_models,idealpoint_models)
      }
      save(all_idealpoint_models, file=self$all_idealpoint_models_file)
    })
    try({
      save(idealpoint_models, file=file.path(save_dataset_in_scriptsfile_directory,"analyse_res",paste0("idealpoint_models_",savemodelfilename,".RData")))
    })
    invisible(idealpoint_models)
  },
  # pooling（lmerTest與robustlmm各自作法）
  pool_idealpoint_models = function(all_idealpoint_models_lmertest=NULL, all_idealpoint_models_robust=NULL, writecsv=TRUE) {
    res<-list()
    if (!is.null(all_idealpoint_models_lmertest)) {
      lmertest_res_pooled<-miceadds:::lmer_pool(all_idealpoint_models_lmertest)
      res[["lmertest"]]<-summary(lmertest_res_pooled)
    }
    if (!is.null(all_idealpoint_models_robust)) {
      robustlmm_res_pooled<-self$myown_robustlmm_pool(all_idealpoint_models_robust)
      res[["robustlmm"]]<-self$myown_robustlmm_summary_pooledres(robustlmm_res_pooled, digits=3)
      if (writecsv==TRUE) {
        write.csv(res[["robustlmm"]], "TMP.csv")
      }
    }
    res
  }
))

if (FALSE) { #plotting inspection（僅保留參考用）
  merged_acrossed_surveys_overall<-dplyr::bind_rows(merged_acrossed_surveys_list)
  dplyr::filter(merged_acrossed_surveys_overall, .imp==1) %>%
  {
    custom_plot(., "policyidealpoint_cos_similarity_to_median","myown_wr") %>% print()
    custom_plot(., "policyidealpoint_eucli_distance_to_median","myown_wr") %>% print()
  }
  for (svytitle in names(survey_data_imputed)) {
    targetplotting_policy_idealpoint_colnames<-dplyr::filter(merged_acrossed_surveys_overall, SURVEY==!!svytitle) %>%
      names() %>%
      grep(pattern="policyidealpoint", x=., value=TRUE)
    for (colname in targetplotting_policy_idealpoint_colnames) {
      targetsavefilename<-here::here(paste0("plot/idealpoints/",svytitle,colname,".png"))
      distplot<-dplyr::filter(merged_acrossed_surveys_overall, SURVEY==!!svytitle) %>%
        custom_plot(colname, "myown_wr")
      print(distplot)
      ggplot2::ggsave(filename=targetsavefilename, plot=distplot)
      readline(paste("now in ",targetsavefilename," continue?"))
    }
  }
  idealpoints_model_arguments_df<-data.frame(
    modelformula=c("policyidealpoint_cos_similarity_to_median|weights(myown_wr)~cluster_kamila+SURVEY+(1|cluster_kamila)"),
    responsefamily=c("gaussian","student")
  )
  #常態性轉換選擇（來源物件merged_acrossed_surveys_list_with_normality建構程序已不存在，僅留紀錄）
  adopting_transformation_method<-try(lapply(merged_acrossed_surveys_list_with_normality, function(X) {X[[2]]$chosen_transform}))
}

if (FALSE) { #曾嘗試之模型公式清單（僅保留參考用）
  #"policyidealpoint_cos_similarity_to_median~(1|SURVEY)",
  # "policyidealpoint_cos_similarity_to_median~(1|cluster_kamila)",
  # "policyidealpoint_cos_similarity_to_median~(1|myown_areakind)",
  # "policyidealpoint_cos_similarity_to_median~(1|admincity)",
  # "policyidealpoint_cos_similarity_to_median~(1|admindistrict)",
  #"policyidealpoint_cos_similarity_to_median~(1|admincity)"#,
  # "policyidealpoint_cos_similarity_to_median~cluster_kamila+(1|cluster_kamila)",
  # "policyidealpoint_cos_similarity_to_median~cluster_kamila+(1|cluster_kamila)+(1|SURVEY)",
  # "policyidealpoint_cos_similarity_to_median~cluster_kamila+(1|cluster_kamila)+(1|adminvillage)",
  # "policyidealpoint_cos_similarity_to_median~cluster_kamila+(1|cluster_kamila)+(1|SURVEY)+(1|adminvillage)",
  # "policyidealpoint_cos_similarity_to_median~cluster_kamila+(1|adminvillage/cluster_kamila)",
  # "policyidealpoint_cos_similarity_to_median~1+(1|adminvillage/admindistrict/admincity/myown_areakind/cluster_kamila/SURVEY)",
  # "policyidealpoint_cos_similarity_to_median~1+(1|adminvillage/cluster_kamila)",
  # "policyidealpoint_cos_similarity_to_median~(1|myown_areakind)",
  # "policyidealpoint_cos_similarity_to_median~(1|adminvillage)",
  # "policyidealpoint_cos_similarity_to_median~(1|admindistrict)",
  # "policyidealpoint_cos_similarity_to_median~(1|admincity)",
  # "policyidealpoint_eucli_distance_to_median~(1|SURVEY)",
  # "policyidealpoint_eucli_distance_to_median~(1|cluster_kamila)",
  # "policyidealpoint_eucli_distance_to_median~(1|cluster_kamila)+(1|SURVEY)",
  # "policyidealpoint_eucli_distance_to_median~(1|cluster_kamila)+(1|adminvillage)",
  # "policyidealpoint_eucli_distance_to_median~(1|cluster_kamila)+(1|SURVEY)+(1|adminvillage)",
  # "policyidealpoint_eucli_distance_to_median~(1|adminvillage/cluster_kamila)",
  # "policyidealpoint_eucli_distance_to_median~cluster_kamila+(1|cluster_kamila)",
  # "policyidealpoint_eucli_distance_to_median~cluster_kamila+(1|cluster_kamila)+(1|SURVEY)",
  # "policyidealpoint_eucli_distance_to_median~cluster_kamila+(1|cluster_kamila)+(1|adminvillage)",
  # "policyidealpoint_eucli_distance_to_median~cluster_kamila+(1|cluster_kamila)+(1|SURVEY)+(1|adminvillage)",
  # "policyidealpoint_eucli_distance_to_median~cluster_kamila+(1|adminvillage/cluster_kamila)",
  # "policyidealpoint_eucli_distance_to_median~(1|myown_areakind)",
  # "policyidealpoint_eucli_distance_to_median~(1|adminvillage)",
  # "policyidealpoint_eucli_distance_to_median~(1|admindistrict)",
  # "policyidealpoint_eucli_distance_to_median~(1|admincity)"#,
}

if (FALSE) { #原confint.rlmerMod第一版定義（後被上方類別內版本取代，僅保留參考用）
  confint.rlmerMod <- function(object,parm,level=0.95) {
    beta <- fixef(object)
    if (missing(parm)) parm <- names(beta)
    se <- sqrt(diag(vcov(object)))
    z <- qnorm((1+level)/2)
    ctab <- cbind(beta-z*se,beta+z*se)
    colnames(ctab) <- stats:::format.perc(c((1-level)/2,(1+level)/2),
                                          digits=3)
    return(ctab[parm,])
  }
}

# * interpretation parts --------------
if (FALSE) { #模型比較與解讀（僅保留參考用；confint.rlmerMod等函數已改為類別方法）
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models.RData"), verbose=TRUE)
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models(very_precious_efficient).RData"), verbose=TRUE)
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models(lmertest_no_weight_multipleimp).RData"), verbose=TRUE)
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models(lme4_no_weight).RData"), verbose=TRUE)
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models(robustlmm).RData"), verbose=TRUE)
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models(rlmer_compare).RData"), verbose=TRUE)
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models(lmertest_compare_weighted).RData"), verbose=TRUE)
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models(robustlmm_final_efficient).RData"), verbose=TRUE)
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models(lmertest_final_efficient).RData"), verbose=TRUE)

  idpmodfilenamepattern<-"idealpoint_models_\\(lmertest\\).+_weighted.+catchadmincity"
  idpmodfilenamepattern<-"(unweighted_full|unweighted_noagereligionareakind)"
  idpmodfiles_prefix<-here::here("data/work1/analyse_res/")
  idpmodfiles<-list.files(idpmodfiles_prefix) %>%
    grep(pattern=idpmodfilenamepattern, x=., value=TRUE) %>%
    #grep(pattern="unweighted", x=., invert=FALSE, value=TRUE) %>%
    paste0(idpmodfiles_prefix,.)
  idpmods_list<-custom_parallel_lapply(idpmodfiles, function(idpmodfile) {
    load(file=idpmodfile, verbose=TRUE)
    return(idealpoint_models[[1]])
  }, method=parallel_method) %>% magrittr::set_names(idpmodfiles)
  t1<-eval(parse(text=paste("anova(",paste("idpmods_list[[",1:length(idpmods_list),"]]",sep="",collapse=","),")"))) %>%
    data.frame(name=rownames(.), .)
  t2<-eval(parse(text=paste("AIC(",paste("idpmods_list[[",1:length(idpmods_list),"]]",sep="",collapse=","),")"))) %>%
    data.frame(name=rownames(.), .)
  t3<-data.frame(name=paste0("idpmods_list[[",1:length(idpmodfiles),"]]"), filename=stringi::stri_replace(str=idpmodfiles, replacement="", regex=idpmodfiles_prefix))
  dplyr::left_join(t1, t2, by=c("name")) %>%
    dplyr::left_join(t3, ., by=c("name")) %>%
    write.csv("TMP.csv")


  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models_(lmertest)base_no_marriage_unweighted_full.RData"), verbose=TRUE)
  basemodel<-idealpoint_models[[1]]
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models_(lmertest)base_no_marriage_unweighted_noage.RData"), verbose=TRUE)
  model_noage<-idealpoint_models[[1]]
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models_(lmertest)base_no_marriage_unweighted_noageareakind.RData"), verbose=TRUE)
  model_noageareakind<-idealpoint_models[[1]]
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models_(lmertest)base_no_marriage_unweighted_noagereligion.RData"), verbose=TRUE)
  model_noagereligion<-idealpoint_models[[1]]
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models_(lmertest)base_no_marriage_unweighted_noagereligionareakind.RData"), verbose=TRUE)
  model_noagereligionareakind<-idealpoint_models[[1]]
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models_(lmertest)base_no_marriage_unweighted_noareakind.RData"), verbose=TRUE)
  model_noareakind<-idealpoint_models[[1]]
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models_(lmertest)base_no_marriage_unweighted_noreligion.RData"), verbose=TRUE)
  model_noreligion<-idealpoint_models[[1]]
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models_(lmertest)base_no_marriage_unweighted_noreligionareakind.RData"), verbose=TRUE)
  model_noreligionareakind<-idealpoint_models[[1]]
  AIC(basemodel,model_noage,model_noageareakind,model_noagereligion,model_noagereligionareakind,model_noareakind,model_noreligion,model_noreligionareakind)
  anova(basemodel,model_noage,model_noageareakind,model_noagereligion,model_noagereligionareakind,model_noareakind,model_noreligion,model_noreligionareakind)
  #save(all_idealpoint_models, file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models(lmertest_compare_weighted).RData"))

  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models_base_no_marriage_robustlmm_full.RData"), verbose=TRUE)
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models_base_no_marriage_robustlmm_noreligionareakind.RData"), verbose=TRUE)
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/lmertest_after_collinearity/idealpoint_models_base_unweighted_no_marriage.RData"), verbose=TRUE)
  load(file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/lmertest_after_collinearity/idealpoint_models_base_unweighted_no_marriage_noreligionareakind.RData"), verbose=TRUE)


  robustlmm::compare(all_idealpoint_models[[1]], all_idealpoint_models[[2]], all_idealpoint_models[[3]], all_idealpoint_models[[4]]) %>%
    xtable::xtable()
  all_idealpoint_models_lmertest<-idealpoint_models
  all_idealpoint_models_robust<-idealpoint_models
  #check distribution
  merged_acrossed_surveys_list[[1]]$policyidealpoint_cos_similarity_to_median_scaled %>%
    fitdistrplus::descdist(discrete=FALSE)

  #pooled summary
  lmertest_res_pooled<-miceadds:::lmer_pool(all_idealpoint_models_lmertest)
  lmertest_res_pooled_summary<-summary(lmertest_res_pooled)
  robustlmm_res_pooled<-myown_robustlmm_pool(all_idealpoint_models_robust)
  robustlmm_res_pooled_summary<-myown_robustlmm_summary_pooledres(robustlmm_res_pooled, digits=3)
  write.csv(robustlmm_res_pooled_summary, "TMP.csv")

  #single imp res summary
  robustlmm:::VarCorr.rlmerMod(all_idealpoint_models_robust[[1]])
  robustlmm::getME(all_idealpoint_models_robust[[1]],"theta")
  #try on rlmer
  coefs.robust <- data.frame(coef(summary(all_idealpoint_models_robust[[1]]))) %>%
    cbind(dfs=coef(summary(all_idealpoint_models_lmertest[[1]]))[,"df"]) %>%
    dplyr::mutate(pvalue=2*pt(abs(t.value), dfs, lower=FALSE) )
  coefs.robust[,"Std..Error"]<-coef(summary(all_idealpoint_models_robust[[1]]))[,2]
  coefs.robust %<>% cbind(confint.rlmerMod(all_idealpoint_models_robust[[1]]))
  write.csv(coefs.robust, "TMP.csv")
  sigma(all_idealpoint_models_robust[[1]])


  if(typeof(Model) == "S4"){
    coefs = data.frame(coef(summary(all_idealpoint_models_robust[[1]])))
    t_value = coefs["DxSchizo", "t.value"]
    Results[i,"P_value"] = 2 * (1 - pnorm(abs(t_value))) * ncol(Data)
    Results[i,"Beta"]= coefs["DxSchizo", "Estimate"]
  } else {
    Results[i,"Warning"] = as.character(Model)
    Results[i, "Beta"] = 0
    Results[i,"P_value"] = 1
  }


  if (FALSE) { #trial and backup
    imputeFEs <- ldply(mods, FEsim, nsims = 1000)
    t<-miceadds::lmer_vcov(all_idealpoint_models[[1]], level=.95, use_reml=FALSE)
    miceadds::lmer_vcov(all_idealpoint_models[[1]])
    miceadds::lmer_vcov(all_idealpoint_models_robust[[1]])
    t<-miceadds:::lmer_pool(all_idealpoint_models)
    summary(t)
    miceadds:::lmer_pool_wrapper
    t<-miceadds::lmer_vcov2(all_idealpoint_models_lmertest[[1]])
    miceadds::lmer_vcov2(all_idealpoint_models_robust[[1]])
    t<-VarCorr(all_idealpoint_models_lmertest[[1]])
    as.data.frame(t, order = "lower.tri")
    t<-VarCorr(all_idealpoint_models_robust[[1]])
    t<-as.list(t) %>%
      lapply(as.data.frame) %>%
      plyr::rbind.fill()
    vcov(all_idealpoint_models_robust[[1]])

    t<-merTools::FEsim(all_idealpoint_models[[1]])
    t<-mice::as.mira(all_idealpoint_models)
    t<-mitools::imputationList(all_idealpoint_models)

    miceadds::lmer_pool
    miceadds:::lmer_pool_wrapper
    mires<-mitools::MIcombine(t$imputations)
    #try on lme
    all_idealpoint_models<-all_idealpoint_models[7:12]
    t<-mice::as.mira(all_idealpoint_models)
    coefs <- data.frame(coef(summary(all_idealpoint_models[[1]])))
    confint(all_idealpoint_models[[1]])
    coefs<-ret_robust_models(all_idealpoint_models, merged_acrossed_surveys_list, clustervar="myown_areakind", vcov="CR1", method=parallel_method, mc.cores=1)
    mitml::testEstimates(t$analyses, var.comp=TRUE)
    pv<-mice::pool(t)
    summary(pv)
    #save(all_idealpoint_models, file=paste0(save_dataset_in_scriptsfile_directory, "analyse_res/idealpoint_models(very_precious_efficient).RData"))
    clubSandwich::coef_test


    breads<-custom_parallel_lapply(idealpoint_models, merDeriv::bread.lmerMod, method=parallel_method)
    #estfuns:Models with weights specification is currently not supported
    #estfuns<-custom_parallel_lapply(idealpoint_models, merDeriv::estfun.lmerMod, method=parallel_method)
  }





  lapply(all_idealpoint_models, try(performance::icc))
  lapply(all_idealpoint_models, function(X) {try(lmerTest:::summary.lmerModLmerTest(X))})

  lapply(all_idealpoint_models, function(X) {try(lmerTest:::anova.lmerModLmerTest(X, type="I", ddf="Kenward-Roger"))})
  lapply(all_idealpoint_models, try(lme4:::summary.merMod))
  lapply(all_idealpoint_models, function(X) {X})
  lapply(all_idealpoint_models, try(lme4:::VarCorr.merMod))
  lapply(all_idealpoint_models, try(lme4:::anova.merMod))
  library(afex)
  lapply(all_idealpoint_models, try(afex:::lmerTest_anova))


  lapply(idealpoint_models, lmerTest:::summary.lmerModLmerTest)
  lapply(idealpoint_models, function(X) {sum(lme4:::residuals.merMod(X))} )
  lapply(idealpoint_models, function(X) {hist(lme4:::residuals.merMod(X), breaks = 100)} )
  lapply(idealpoint_models, function(X) {shapiro.test(lme4:::residuals.merMod(X))} )

  lapply(idealpoint_models, lme4:::summary.merMod, signif.stars=TRUE)
  lapply(idealpoint_models, summary, signif.stars=TRUE)
  lapply(idealpoint_models, lme4::confint.merMod)

  lapply(idealpoint_models, robustlmm:::summary.rlmerMod)
  lapply(idealpoint_models, robustlmm:::plot.rlmerMod)
  lapply(idealpoint_models, confint.rlmerMod)
  lapply(idealpoint_models, function(X) { sum(robustlmm:::residuals.rlmerMod(X))  } )
  lapply(idealpoint_models, function(X) { hist(robustlmm:::residuals.rlmerMod(X), breaks = 100)  } )
  lapply(idealpoint_models, function(X) { shapiro.test(robustlmm:::residuals.rlmerMod(X))  } )


  lapply(idealpoint_models, car::linearHypothesis)
}


# * brms backup --------------

if (FALSE) {
  cossim_to_cluster_mod_robusts2 <-
    brms::brm_multiple(#
      brms::bf(policyidealpoint_cos_similarity_to_median|weights(myown_wr)~(cluster_kamila|SURVEY)+SURVEY+cluster_kamila*SURVEY+(1|cluster_kamila/SURVEY)),
      #brms::bf(policyidealpoint_cos_similarity_to_median|weights(myown_wr)~(cluster_kamila|SURVEY)+SURVEY+(1|cluster_kamila/SURVEY)), #, sigma~cluster_kamila
      family=gaussian(), #brms::student,
      data = merged_acrossed_surveys_list,
      chains=4,
      cores=parallel::detectCores(),
      iter = 2000
      #,file = here::here("data/policyidealpoint_cos_similarity_to_median_to_kamila-robust")
    )

  cossim_to_cluster_mod_robusts3 <-
    brms::brm_multiple(#
      brms::bf(policyidealpoint_cos_similarity_to_median|weights(myown_wr)~(cluster_kamila|SURVEY)+SURVEY+cluster_kamila*SURVEY+(1|cluster_kamila/SURVEY), sigma~cluster_kamila+SURVEY),
      #brms::bf(policyidealpoint_cos_similarity_to_median|weights(myown_wr)~(cluster_kamila|SURVEY)+SURVEY+(1|cluster_kamila/SURVEY)), #
      family=brms::student,
      data = merged_acrossed_surveys_list,
      chains=4,
      cores=parallel::detectCores(),
      iter = 2000
      #,file = here::here("data/policyidealpoint_cos_similarity_to_median_to_kamila-robust")
    )
  load(file=paste0(dataset_in_scriptsfile_directory,"brms/test_cossim_to_cluster_mod_robusts.RData"), verbose=TRUE)
  save(cossim_to_cluster_mod_robusts1, cossim_to_cluster_mod_robusts2, cossim_to_cluster_mod_robusts3, file=paste0(dataset_in_scriptsfile_directory,"brms/test_cossim_to_cluster_mod_robusts.RData"))

}


if (FALSE) {
  brms:::summary.brmsfit(cossim_to_cluster_mod_robusts)
  brms::pp_check(cossim_to_cluster_mod_robusts)
  brms::pp_check(cossim_to_cluster_mod_robusts, group="cluster_kamila")
}

if (FALSE) {
  s<-analysis_idealpoint_to_median_args$store_key[1:10] %>%
    magrittr::set_names(., lapply(., function(fikey, ...) {
      needrow<-dplyr::filter(analysis_idealpoint_to_median_args, store_key==!!fikey)
      svytitle<-needrow$survey
      imp<-needrow$imp
      needdf<-survey_data_imputed %>%
        magrittr::extract2(svytitle) %>%
        dplyr::filter(.imp==!!imp) %>%
        mutate(new_policyidealpoint_cos_similarity_to_median=policyidealpoint_cos_similarity_to_median+10)
      t<-magrittr::use_series(needdf, "policyidealpoint_cos_similarity_to_median") %>%
        bestNormalize::bestNormalize()
      mod_robust <- brms::brm(
        brms::bf(policyidealpoint_cos_similarity_to_median ~ cluster_kamila, sigma ~ cluster_kamila),
        family=brms::student,
        data = needdf,
        cores=parallel::detectCores(),
        file = here::here("data/iqgroup-robust")
      )

      return(t$chosen_transform)
      #shapiro.test(t$x.t)
      if ({boxcox<-FALSE;boxcox}) {
        r<-MASS::boxcox(new_policyidealpoint_cos_similarity_to_median~cluster_kamila, data=needdf)
        bestpower <- cbind("lambda"=r$x, "lik"=r$y) %>%
          .[order(-lik),] %>%
          .[1,1]
        f1 <- lm(new_policyidealpoint_cos_similarity_to_median^1.030303 ~ cluster_kamila, data=needdf)
        summary(f1)
        shapiro.test(f1$res)
        data.frame(t=t$x.t) %>%
          custom_plot("t")
      }

      #Ladder.x(t)
      #r<-car::boxCoxVariable(t)
      #r<-car::powerTransform(t)
      #shapiro.test(r)
      #r<-MASS::boxcox(policyidealpoint_cos_similarity_to_median~)
      # brms::brm(modelformula,
      #           data = ., family = brms::cumulative(link = "logit"),
      #           chains = 2, cores = parallel::detectCores())
    }, survey_data_imputed=survey_data_imputed,
    analysis_idealpoint_to_median_args=analysis_idealpoint_to_median_args)
    )
}
