# 第Ｏ部份：環境設定 --------------------------------
if (!("benchmarkme" %in% rownames(installed.packages()))) install.packages("benchmarkme")
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
t_sessioninfo_running_with_cpu_locale<-gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2]))
source(file = "shared_functions.R", encoding="UTF-8")
terms<-c(5,6,7,8,9)
survey_time_range <- list(
  "2004citizen"=data.frame("SURVEY"="2004citizen","yrmonth"=c("093/07","093/08","093/09","093/10","093/11","093/12","094/01","094/02","094/03","094/04","094/05","094/06","094/07","094/08","094/09","094/10","094/11","094/12","095/01","095/02","095/03","095/04","095/05","095/06","095/07","095/08","095/09")), #,"095/10","095/11","095/12"
  "2010env"=data.frame("SURVEY"="2010env","yrmonth"=c("099/07","099/08","099/09","099/10","099/11","099/12","100/01","100/02","100/03","100/04","100/05","100/06","100/07","100/08","100/09","100/10","100/11","100/12","101/01","101/02","101/03","101/04","101/05","101/06","101/07","101/08")),
  "2010overall"=data.frame("SURVEY"="2010overall","yrmonth"=c("099/07","099/08","099/09","099/10","099/11","099/12","100/01","100/02","100/03","100/04","100/05","100/06","100/07","100/08","100/09","100/10","100/11","100/12","101/01","101/02","101/03","101/04","101/05","101/06","101/07","101/08","101/09","101/10","101/11")),
  "2016citizen"=data.frame("SURVEY"="2016citizen","yrmonth"=c("105/09","105/11","105/12","106/01","106/04","106/05","106/06","106/08","106/10","106/11","106/12","107/01","107/03","107/04","107/05","107/06","107/07","107/08","107/09","107/10","107/11"))
)
survey_time_range_df <- dplyr::bind_rows(survey_time_range)
gc(verbose=TRUE)
non_decision <- "棄權/未投票/請假"
# 第二部分：投票及議案及「問卷答案對照意向」資料,主要也就是RData&excel檔  -------------------------------------------

if ({incoporate_party_seats<-FALSE; incoporate_party_seats}) { #舊方法暫時忽略
  rulingparty <- dplyr::bind_rows(
    data.frame(
      "term"=5,
      "party"=c("中國國民黨","民主進步黨","親民黨","台灣團結聯盟","新黨","台灣吾黨","無黨籍及未經政黨推薦"),
      "rulingparty"=factor(c(0,1,0,0,0,0,0))
    ),
    data.frame(
      "term"=6,
      "party"=c("中國國民黨","民主進步黨","親民黨","台灣團結聯盟","新黨","無黨團結聯盟","無黨籍及未經政黨推薦"),
      "rulingparty"=factor(c(0,1,0,0,0,0,0))
    ),
    data.frame(
      "term"=7,
      "party"=c("中國國民黨","民主進步黨","無黨團結聯盟","親民黨","無黨籍及未經政黨推薦"),
      "rulingparty"=factor(c(1,0,0,0,0))
    ),
    data.frame(
      "term"=8,
      "party"=c("中國國民黨","民主進步黨","無黨團結聯盟","親民黨","台灣團結聯盟","民國黨","無黨籍及未經政黨推薦"),
      "rulingparty"=factor(c(1,0,0,0,0,0,0))
    ),
    data.frame(
      "term"=9,
      "party"=c("中國國民黨","民主進步黨","時代力量","親民黨","無黨團結聯盟","無黨籍及未經政黨推薦"),
      "rulingparty"=factor(c(0,1,0,0,0,0))
    ),
  )
}

#as.character(unique(bills_billcontent$pp_related_q_1))

#讀取投票紀錄資料-此處通常預處理好，直接load下面 mergedf_votes_bills_surveyanswer
load(paste0(dataset_in_scriptsfile_directory, "myown_vote_record_df.RData"), verbose=TRUE)
load(paste0(dataset_in_scriptsfile_directory, "myown_vote_record_detailed_part_df.RData"), verbose=TRUE)

myown_vote_record_df <- list( 
    dplyr::distinct(myown_vote_record_df,term,period,temp_meeting_no,meetingno) %>% dplyr::arrange(term,period,temp_meeting_no,meetingno), 
    dplyr::distinct(myown_vote_record_detailed_part_df,term,period,temp_meeting_no,meetingno) %>% dplyr::arrange(term,period,temp_meeting_no,meetingno)
  ) %>%
  { dplyr::inner_join(getElement(., 1), getElement(., 2)) } %>%
  dplyr::anti_join(myown_vote_record_df, .) %>%
  #above are 把重複的會議刪除
  dplyr::bind_rows(myown_vote_record_detailed_part_df) %>%
  list(
    dplyr::anti_join(., dplyr::distinct(myown_vote_record_df,term,period,temp_meeting_no,meetingno)), #這邊是2004詳細版會議紀錄中有但議事錄中沒有的紀錄，也就是第5屆的資料
    dplyr::semi_join(., dplyr::distinct(myown_vote_record_df,legislator_name,term,period,temp_meeting_no,meetingno))
  ) %>%
  magrittr::extract(2:3) %>%
  dplyr::bind_rows() %>%
  #因為pipeline無法準確限於指定的兩個element，反而還是會在list傳遞第一個element，所以用extract方式處理
  #above are 把會議記錄(較多投票表決版本的紀錄)中到離職不明者以議事錄為準篩選
  dplyr::arrange(term, period, temp_meeting_no, meetingno, billn) %>%
  mutate_cond(votedecision %in% c("棄權","未投票","請假"), votedecision=non_decision) %>% 
  dplyr::mutate_at(c("votedecision","billresult"), as.factor) %>%
  dplyr::mutate_at("legislator_name", .funs = list(legislator_name = ~customgsub(legislator_name, "　", ""))) %>%#funs(customgsub(legislator_name, "　", ""))
  dplyr::mutate_at(c("billcontent","term","period","meetingno","temp_meeting_no","billn"), as.character) %>%
  dplyr::mutate_at("billcontent", trimws) %>%
  dplyr::mutate(billid_myown = paste(term, "-", period, "-", temp_meeting_no, "-", meetingno, "-", billn, sep = "")) %>%
  dplyr::mutate_at("billid_myown", as.character) %>%
  dplyr::mutate_at(c("term","period","meetingno","temp_meeting_no","billn"), as.numeric) %>%
  dplyr::select(-billcontent,-url)
#save(myown_vote_record_df,file=paste0(dataset_in_scriptsfile_directory, "myown_vote_record_df_across2004.RData"))

# 將合併好的投票紀錄進一步處理  -------------------------------------------

load(file=paste0(dataset_in_scriptsfile_directory, "myown_vote_record_df_across2004.RData"), verbose=TRUE)
load(file=paste0(dataset_in_scriptsfile_directory, "legislators_with_elections.RData"), verbose=TRUE)

myown_vote_bills_file <- paste0(dataset_file_directory, "votingdf_datafile_myown_englished.xlsx", sep="")
bills_answer_to_bill <- openxlsx::read.xlsx(myown_vote_bills_file, sheet = 4)
bills_billcontent <- openxlsx::read.xlsx(myown_vote_bills_file, sheet = 1) %>%
  dplyr::mutate_at("billcontent", as.character) %>%
  dplyr::select(-starts_with("pp_related_q_")) %>% #因為第四個表格問卷對政策實現與否表已經有了variable_on_q所以此處略過
  dplyr::mutate_at("pp_agendavoting", as.factor)


# 測試有無重複投票紀錄  -------------------------------------------
if ({testing_if_duplicated<-FALSE;testing_if_duplicated}) {
  duplicated_ind <- dplyr::distinct(myown_vote_record_df, term, legislator_name, billid_myown) %>%
    dplyr::arrange_all()
  duplicated()
  myown_vote_record_df[duplicated_ind,c("term", "legislator_name", "billid_myown")] %>%
    dplyr::inner_join(myown_vote_record_df, .) %>%
    dplyr::select_at(.vars=c("term", "legislator_name", "billid_myown", "votedecision")) %>%
    dplyr::arrange(legislator_name, billid_myown, votedecision)
  # 曾華德    6-3-0-8-1 棄權/未投票/未出席 
  # 曾華德    6-3-0-8-1               贊成 @ old version meeting record
  # 新單純議事錄 (一)「第二條照國民黨團、親民黨團修正動議條文通過」部分 也是贊成
  #正確：
  dplyr::filter(myown_vote_record_df, legislator_name=="曾華德", term==6, period==3, temp_meeting_no==0, meetingno==8, billn==1)
  #錯誤：
  dplyr::filter(myown_vote_record_detailed_part_df, legislator_name=="曾華德", term==6, period==3, temp_meeting_no==0, meetingno==8, billn==1)
  # 6          劉寬平  6-2-0-19-26               反對
  # 6          劉寬平  6-2-0-19-26 棄權/未投票/未出席
  #新單純議事錄沒有#26案
  #舊完全版會議記錄顯示劉寬平反對
  #新df顯示完全沒有表決紀錄（因為沒有第26案）
  dplyr::filter(myown_vote_record_df, legislator_name=="劉寬平", term==6, period==2, temp_meeting_no==0, meetingno==19, billn==26)
  #df顯示重複
  dplyr::filter(myown_vote_record_detailed_part_df, legislator_name=="劉寬平", term==6, period==2, temp_meeting_no==0, meetingno==19, billn==26)
  
  #檢查缺失值
  df_i<-1
  checking_missing_voterecords_df<-myown_vote_record_df_wide_billidascol[[df_i]] %>%
    .[!complete.cases(.), ] %>%
    .[,c(1:3,which(colSums(is.na(.))>0))]
  save(checking_missing_voterecords_df, file=paste0(dataset_in_scriptsfile_directory, "checking_missing_voterecords_df.RData"))
  write.xlsx(checking_missing_voterecords_df, file=paste0(dataset_in_scriptsfile_directory, "checking_missing_voterecords_df.xlsx"))
}


load_lib_or_install(c("FactoMineR","factoextra","parallel","magrittr"))
# 對投票紀錄進行探索性因素分析構面縮減檢查投票的構面，以及testing on IRT for vote decision --------------------------------

#bills_billcontent
myown_vote_record_df_wide <- dplyr::distinct(myown_vote_record_df, term, legislator_name, billid_myown, votedecision) %>%
  dplyr::left_join(dplyr::distinct(legislators_with_elections, term, legislator_name, legislator_party)) %>%#legislator_party,
  dplyr::left_join(dplyr::distinct(bills_billcontent, billid_myown, pp_agendavoting, research_period) ) %>%
  dplyr::filter(pp_agendavoting==0, research_period==1)
widedata_preserve_vars <- dplyr::setdiff(names(myown_vote_record_df_wide), c("billid_myown","votedecision"))
widedata_formula <- paste0(widedata_preserve_vars, collapse="+") %>%
  paste0("~","billid_myown") %>% as.formula()
myown_vote_record_df_wide_billidascol <- myown_vote_record_df_wide %>%
  dplyr::arrange(legislator_name, billid_myown, votedecision) %>%
  dplyr::mutate_at(.vars=c("votedecision","legislator_party"), as.character) %>%
  lapply(terms, function (term,data) { data[data$term==term,] }, data=. ) %>%
  lapply(function(data) {reshape2::dcast(data, widedata_formula, fun.aggregate=paste0, value.var="votedecision", fill="")}) %>%
  lapply(function(data) {dplyr::mutate_all(data, .funs=function (X){X<-ifelse(X=="",NA,X)} )} ) %>%
  #lapply(function(data) {t(data)}) %>%
  #lapply(function(data) {magrittr::set_colnames(data, data[2,])}) %>%
  #lapply(function(data) {as.data.frame(data[4:nrow(data),])}) %>%
  lapply(function(data) {dplyr::mutate_at(data, .vars=dplyr::setdiff(colnames(data), widedata_preserve_vars), function (X) {
    ordered(X, levels = c("反對", non_decision, "贊成"))
  })} ) %>%
  lapply(function(data) {magrittr::set_rownames(data, data$legislator_name)})

#先前parallel analysis的結果
result_n_factor<-data.frame(i=1:5,term=5:9,fa=c(3,3,3,1,4),princp=c(3,2,2,1,4)) %>%
  dplyr::mutate(need_factorn = paste0(term,"_",fa))

#MDS algorithms
#http://www.hmwu.idv.tw/web/R/C01-hmwu_R-DimensionReduction.pdf


#投票結果homals
res.homals <- list()
res.homals.accepted_ndimensions <- list()
res.homals.loadings <- list()
for (i in 1:length(myown_vote_record_df_wide_billidascol)) {
  plot_title<-terms[i]
  colname_billids <- setdiff(names(myown_vote_record_df_wide_billidascol[[i]]), widedata_preserve_vars)
  votingdfwide<-myown_vote_record_df_wide_billidascol[[i]][,colname_billids]
  n_component<-dplyr::filter(result_n_factor, term==plot_title) %>%
    magrittr::use_series(fa)
  #for (n_component in 2:8) { #repeat
    #n_component<-as.integer(readline(prompt="Enter n_component: "))
  if (is.na(n_component)) {
    break
  } else {
    #from https://alice86.github.io/2018/04/08/Factor-Analysis-on-Ordinal-Data-example-in-R-(psych,-homals)/
    residx <- paste0(plot_title,"_",n_component)
    res.homals[[residx]] <- homals::homals(votingdfwide, ndim = n_component, level = "ordinal")
    #summary(res.homals[[residx]])
    #plot(res.homals[[residx]], plot.type = "screeplot")
    res.homal.df<-data.frame("index"=residx, "eigenvalue"=res.homals[[residx]]$eigenvalues, "dimension"=1:res.homals[[residx]]$ndim)
    ggplotscreeplot(res.homal.df, "dimension", "eigenvalue")
    tmpdim <- readline(paste0("now at ", residx, ", Enter number of dimensions?"))
    if (tmpdim=="") {
      break
    } else {
      res.homals.accepted_ndimensions[[residx]]<-tmpdim
    }
    cache <- apply(votingdfwide, 2, function(x) nlevels(as.factor(x)))
    ld <- unlist(lapply(res.homals[[residx]]$loadings, function(x) x[1,]))
    loadings <- matrix(ld, byrow = T, nrow = n_component)
    colnames(loadings) <- names(cache)
    res.homals.loadings[[residx]] <- loadings
  }
}
reshomelfile <- paste0(dataset_in_scriptsfile_directory, "res.homals.RData")
save(res.homals, res.homals.accepted_ndimensions, res.homals.loadings, file = reshomelfile)
load(file = reshomelfile)
homals::plot3dstatic(res.homals[[residx]], plot.type = "screeplot")
openxlsx::write.xlsx(as.data.frame(res.homals.accepted_ndimensions), "test.xlsx")
for (idx in names(res.homals.loadings)) {
  openxlsx::write.xlsx(as.data.frame(res.homals.loadings[[idx]]), "test.xlsx")
  readline(paste0("now at ", idx))
}

#投票結果gifi homels
gifi.res.homals <- list()
gifi.res.homals.accepted_ndimensions <- list()
gifi.res.homals.loadings <- list()
for (i in 1:length(myown_vote_record_df_wide_billidascol)) {
  plot_title<-terms[i]
  colname_billids <- setdiff(names(myown_vote_record_df_wide_billidascol[[i]]), widedata_preserve_vars)
  votingdfwide<-myown_vote_record_df_wide_billidascol[[i]][,colname_billids]
  Gifi::makeNumeric(votingdfwide)
  for (n_component in 2:8) { #repeat
    if (is.na(n_component)) break
    #from https://alice86.github.io/2018/04/08/Factor-Analysis-on-Ordinal-Data-example-in-R-(psych,-homals)/
    residx <- paste0(plot_title,"_",n_component)
    gifi.res.homals[[residx]] <- Gifi::homals(votingdfwide, ndim = n_component, ordinal = TRUE)
    #summary(gifi.res.homals[[residx]])
    #plot(gifi.res.homals[[residx]], plot.type = "screeplot")
    gifi.res.homals.df<-data.frame("index"=residx, "eigenvalue"=gifi.res.homals[[residx]]$eigenvalues, "dimension"=1:gifi.res.homals[[residx]]$ndim)
    ggplotscreeplot(res.homal.df, "dimension", "eigenvalue")
    tmpdim <- readline(paste0("now at ", residx, ", Enter number of dimensions?"))
    if (tmpdim=="") {
      break
    } else {
      gifi.res.homals.accepted_ndimensions[[residx]]<-tmpdim
    }
    cache <- apply(votingdfwide, 2, function(x) nlevels(as.factor(x)))
    ld <- unlist(lapply(gifi.res.homals[[residx]]$loadings, function(x) x[1,]))
    loadings <- matrix(ld, byrow = T, nrow = n_component)
    colnames(loadings) <- names(cache)
    gifi.res.homals.loadings[[residx]] <- loadings
  }
}


#投票結果EFA by exploratory IRT using mirt
res.mirtefas <- list()
resmirtefasfile <- paste0(dataset_in_scriptsfile_directory, "res.mirtefas.RData")
custom_parallel_lapply(1:nrow(result_n_factor), function (fi, myown_vote_record_df_wide_billidascol, resmirtefasfile, result_n_factor, widedata_preserve_vars) {
  n_component_row<-dplyr::filter(result_n_factor, i==fi)
  n_component<-magrittr::use_series(n_component_row, fa)
  plot_title<-magrittr::use_series(n_component_row, term)
  residx <- paste0(plot_title,"_",n_component)
  colname_billids <- setdiff(names(myown_vote_record_df_wide_billidascol[[fi]]), widedata_preserve_vars)
  votingdfwide<-myown_vote_record_df_wide_billidascol[[fi]][,colname_billids] %>%
    dplyr::mutate_all(.funs=unclass)
  message(paste0("now in ",residx))
  if (TRUE) {
    resmodel <- mirt::mirt(data=votingdfwide, model=n_component, itemtype='graded', method='QMCEM')
    load(file=resmirtefasfile)
    res.mirtefas[[residx]] <- resmodel
    save(res.mirtefas, file = resmirtefasfile)
  }
  #remove(n_component_row, n_component, plot_title, residx, colname_billids, votingdfwide, resmodel)
},myown_vote_record_df_wide_billidascol=myown_vote_record_df_wide_billidascol,
resmirtefasfile=resmirtefasfile,
result_n_factor=result_n_factor,
widedata_preserve_vars=widedata_preserve_vars,
method="fork",
mc.cores=nrow(result_n_factor))
load(resmirtefasfile,verbose=TRUE)
result_n_factor
mirt::anova(res.mirtefas$`5_3`, res.mirtefas$`5_4`) #base 5-3 BIC 14030.53
mirt::anova(res.mirtefas$`6_3`, res.mirtefas$`6_5`) #base 6-3 BIC 8397.375
mirt::anova(res.mirtefas$`7_3`, res.mirtefas$`7_6`) #base 7-3 BIC 14218.07
mirt::anova(res.mirtefas$`8_1`, res.mirtefas$`8_6`) #base 8-1 BIC 501.2352
resmodel<-res.mirtefas$`7_3`
mirt::M2(resmodel, na.rm=TRUE)
# 5_3
#            M2   df p RMSEA RMSEA_5 RMSEA_95     SRMSR      TLI CFI
# stats 5720.34 7263 1     0       0        0 0.1922678 1.002611   1
# 6_3
# Sample size after row-wise response data removal: 218
# 錯誤: Could not invert orthogonal complement matrix
t<-mirt::itemfit(resmodel, na.rm=TRUE)
#find misfitting items
t[t[, 4] < 0.05, ]
summary(resmodel, rotate = "varimax")
mirt::itemplot(resmodel)

#投票結果polychoric correlations
#http://personality-project.org/r/psych/HowTo/factor.pdf
res.catgfas <- list()
rescatgfasfile <- paste0(dataset_in_scriptsfile_directory, "res.catgfas.RData")
custom_parallel_lapply(1:nrow(result_n_factor), function (fi, myown_vote_record_df_wide_billidascol, rescatgfasfile, result_n_factor, widedata_preserve_vars) {
  n_component_row<-dplyr::filter(result_n_factor, i==fi)
  n_component<-magrittr::use_series(n_component_row, fa)
  plot_title<-magrittr::use_series(n_component_row, term)
  colname_billids <- setdiff(names(myown_vote_record_df_wide_billidascol[[fi]]), widedata_preserve_vars)
  votingdfwide<-myown_vote_record_df_wide_billidascol[[fi]][,colname_billids] %>%
    dplyr::mutate_all(.funs=unclass)
  residx <- paste0(plot_title,"_",n_component)
  if (TRUE) {
    #from https://alice86.github.io/2018/04/08/Factor-Analysis-on-Ordinal-Data-example-in-R-(psych,-homals)/
    #https://sakaluk.wordpress.com/2016/05/26/11-make-it-pretty-scree-plots-and-parallel-analysis-using-psych-and-ggplot2/
    resmodel <- psych::fa(r=votingdfwide, nfactors=n_component, rotate="varimax", cor="poly", missing=TRUE)
    load(file=rescatgfasfile)
    res.catgfas[[residx]] <- resmodel
    save(res.catgfas, file = rescatgfasfile)
  }
},myown_vote_record_df_wide_billidascol=myown_vote_record_df_wide_billidascol,
rescatgfasfile=rescatgfasfile,
result_n_factor=result_n_factor,
widedata_preserve_vars=widedata_preserve_vars,
method="fork",
mc.cores=nrow(result_n_factor))
load(rescatgfasfile,verbose=TRUE)

res.catgfas$`7_3`$loadings %>% unclass() %>% as.data.frame() %>%
  {
    cbind(., row.names(.))
  } %>%
  openxlsx::write.xlsx("FA.xlsx")


#投票結果#fa parallel探測因素數目
parallelfa_n_factors <- list()
load(paste0(dataset_in_scriptsfile_directory, "parallelfa_n_factors.RData"), verbose=TRUE)
for (i in 5) { #1:length(myown_vote_record_df_wide_billidascol)
  plot_title<-terms[i]
  message(paste0("now in ", plot_title))
  colname_billids <- setdiff(names(myown_vote_record_df_wide_billidascol[[i]]), widedata_preserve_vars)
  votingdfwide<-myown_vote_record_df_wide_billidascol[[i]][,colname_billids] %>%
    dplyr::mutate_all(.funs=unclass) %>%
    .[complete.cases(.),] %>%
    as.matrix()
  parallelfa_n_factors[[plot_title]] <- tryCatch({
    psych::fa.parallel(votingdfwide, fm="minres", main="Parallel Analysis Scree Plots", cor="poly")
  }, error = function(msg) {
    message(paste0(msg,"\n"))
    return("ERROR")
  })
  if (parallelfa_n_factors[[plot_title]]!="ERROR") {
    plot(parallelfa_n_factors[[plot_title]])
  }
  save(parallelfa_n_factors, file=paste0(dataset_in_scriptsfile_directory, "parallelfa_n_factors.RData"))
  readline("wait for next")
}



#投票結果MCA
res.catpcas <- list()
res.mcas <- list()
for (i in 1:length(myown_vote_record_df_wide_billidascol)) {
  plot_title<-terms[i]
  colname_billids <- setdiff(names(myown_vote_record_df_wide_billidascol[[i]]), widedata_preserve_vars)
  votingdfwide<-myown_vote_record_df_wide_billidascol[[i]][,colname_billids]
  repeat {
    n_component<-as.integer(readline(prompt="Enter n_component: "))
    if (is.na(n_component)) break
    #res.catpca<- Gifi::princals(data=votingdfwide, ndim = n_component, ordinal = TRUE, verbose = TRUE)
    res.mca<-Gifi::homals(data=votingdfwide, ndim = n_component, ordinal = TRUE, verbose = TRUE)
    #plot(res.catpca, plot.type = "screeplot")
    plot(res.mca, plot.type = "screeplot")
    #summary(res.catpca)
    residx<-paste0(plot_title, "_", n_component)
    #res.catpcas[[index_res]]<-res.catpca
    res.mcas[[residx]]<-res.mca
    summary(res.mca)
  }
}

save(res.mcas, file = paste0(dataset_in_scriptsfile_directory, "res.mcas.RData"))

var <- factoextra::get_mca_var(res.mcas[[5]])
var
var$cos2

for (key in names(myown_vote_record_df_wide)) {
  billids <- setdiff(names(myown_vote_record_df_wide[[key]]), c("term","legislator_name"))
  estimatemodel<-mirt::mirt(
    data=myown_vote_record_df_wide[[key]][,billids],
    model=1,
    itemtype = "graded",
    technical = list("NCYCLES"=40000)
  )
  poliparticipt<-mirt::fscores(estimatemodel,method="EAP") %>%
    as.data.frame() %>%
    set_colnames(c("myown_factoredparticip"))
  X[X$.imp==imp,c("myown_factoredparticip")]<-poliparticipt$myown_factoredparticip #bind_cols(X,poliparticipt)
}

mergedf_votes_bills_surveyanswer <- dplyr::distinct(legislators_with_elections,term,legislator_name,legislator_party,legislator_sex,seniority,legislator_age,incumbent,elec_dist_type) %>%
  dplyr::mutate_at("term", as.numeric) %>%
  dplyr::left_join(myown_vote_record_df, ., by=c("term","legislator_name")) %>%
  dplyr::distinct(votedecision,legislator_name,billid_myown,legislator_party,legislator_sex,seniority,legislator_age,incumbent,elec_dist_type) %>%
  dplyr::left_join(dplyr::filter(myown_vote_record_df, term %in% terms), .) %>%
  dplyr::filter(!is.na(legislator_party)) %>% 
  #above are myown_vote_record_df_with_party
  
  #以下發現有人沒有黨籍，因為研究設計只涵蓋問卷後二年，所以二年之後的立法委員沒有串到資料，例如第八屆補選的
  #dplyr::distinct(myown_vote_record_df, legislator_name, term) %>% View()
  #dplyr::distinct(elections_df, term, name, party) %>% 
  #  dplyr::filter(customgrepl(name,"鄭天財|簡東明|Kolas")) %>% View()
  #dplyr::distinct(myown_vote_record_df, legislator_name, term) %>%
  #  dplyr::filter(customgrepl(legislator_name,"鄭天財|簡東明|Kolas")) %>% View()
  #dplyr::distinct(legislators_with_elections, term, legislator_name, legislator_party) %>%
  #  dplyr::filter(customgrepl(legislator_name,"鄭天財|簡東明|Kolas")) %>% View()
  #dplyr::distinct(mergedf_votes_bills_surveyanswer, term, legislator_name, legislator_party) %>%
#  dplyr::filter(customgrepl(legislator_name,"鄭天財|簡東明|Kolas")) %>% View()
#dplyr::distinct(mergedf_votes_bills_surveyanswer, term, legislator_name, legislator_party) %>% filter(is.na(legislator_party)) %>% View()

dplyr::left_join(., { #此處設計一個政黨壓力指標並串連加入
  #mergedf_votes_bills_surveyanswer %>%
  dplyr::group_by(., votedecision, billid_myown, legislator_party) %>%
    dplyr::summarise(samepartysamepositioncounts=n()) %>%
    arrange(billid_myown, legislator_party, desc(samepartysamepositioncounts), votedecision) %>%
    dplyr::group_by(billid_myown, legislator_party) %>%
    dplyr::summarise(party_pressure=(max(samepartysamepositioncounts)-sum(samepartysamepositioncounts)+max(samepartysamepositioncounts))/sum(samepartysamepositioncounts)) 
})
#dplyr::left_join(partyseats) %>%

# 第三部分：合併投票紀錄與法案資料  -------------------------------------------

mergedf_votes_bills_surveyanswer %<>%
  dplyr::right_join(bills_billcontent, by = c("billid_myown","term","period","meetingno","temp_meeting_no","billn","billresult","date")) %>% ##,"url"
  dplyr::right_join(bills_answer_to_bill, by = c("billid_myown")) %>% 
  #篩選出研究範圍
  dplyr::inner_join(distinct(survey_time_range_df,yrmonth)) %>%
  dplyr::left_join(survey_time_range_df) %>%
  dplyr::mutate(stdbilldate=as.Date(paste(
    as.integer(substr(date,0,3))+1911,
    substr(date,5,6),
    substr(date,8,9)
  ),"%Y %m %d")) %>%
  dplyr::mutate(opinionstrength=dplyr::recode(opinionfromconstituent, `n`=1, `nn`=2, `m`=1, `mm`=2, `b`=0)) %>%
  dplyr::mutate(opiniondirectionfromconstituent=dplyr::recode(opinionfromconstituent, `n`="n", `nn`="n", `nnn`="n", `m`="m", `mm`="m", `mmm`="m", `b`="b")) %>%
  dplyr::mutate(opiniondirectionfrombill=dplyr::recode(opinionfrombill,`n`="n",`nn`="n",`m`="m",`mm`="m",`b`="b")) %>%
  dplyr::mutate(opiniondirectionfromlegislator=NA,respondopinion=NA,success_on_bill=NA) %>%
  mutate_cond( (opiniondirectionfromconstituent==opiniondirectionfrombill) & (billresult=="Passed"), success_on_bill=1 ) %>%
  mutate_cond( (opiniondirectionfromconstituent==opiniondirectionfrombill) & (billresult=="NotPassed"), success_on_bill=0 ) %>%
  mutate_cond( (opiniondirectionfromconstituent!=opiniondirectionfrombill & opiniondirectionfromconstituent != "x" & opiniondirectionfromconstituent != "b") & (billresult=="Passed"), success_on_bill=0 ) %>%
  mutate_cond( (opiniondirectionfromconstituent!=opiniondirectionfrombill & opiniondirectionfromconstituent != "x" & opiniondirectionfromconstituent != "b") & (billresult=="NotPassed"), success_on_bill=1 ) %>%
  mutate_cond(votedecision=="贊成", opiniondirectionfromlegislator=opiniondirectionfrombill) %>%
  mutate_cond(votedecision=="反對", opiniondirectionfromlegislator=recode(opiniondirectionfrombill,
    "n"="m","m"="n",
    "cpg"="NOTcpg","NOTcpg"="cpg","envenerg"="NOTenvenerg","NOTenvenerg"="envenerg","nuenerg"="NOTnuenerg","NOTnuenerg"="nuenerg",
    "crop"="NOTcrop","NOTcrop"="crop","otherenerg"="NOTotherenerg","NOTotherenerg"="otherenerg",
    "NOTgovpushrichpeoplemore"="govpushrichpeoplemore","NOTgovmore"="govmore","NOTnoint"="noint","NOTpoorontheirown"="poorontheirown",
    "govpushrichpeoplemore"="NOTgovpushrichpeoplemore","govmore"="NOTgovmore","noint"="NOTnoint","poorontheirown"="NOTpoorontheirown",
    "bygov"="NOTbygov","byent"="NOTbyent","bynpo"="NOTbynpo","byrelg"="NOTbyrelg","byfamily"="NOTbyfamily",
    "NOTbygov"="bygov","NOTbyent"="byent","NOTbynpo"="bynpo","NOTbyrelg"="byrelg","NOTbyfamily"="byfamily"
  )) %>%
  # 反對核電怎麼編碼？
  mutate_cond(opiniondirectionfromconstituent!=opiniondirectionfromlegislator, respondopinion=0) %>%
  mutate_cond(opiniondirectionfromconstituent==opiniondirectionfromlegislator, respondopinion=2) %>%
  mutate_cond(votedecision==non_decision, respondopinion=1, opiniondirectionfromlegislator='ig/gu') %>% #ignore/giveup
  mutate_cond(opiniondirectionfromconstituent=='x' | opiniondirectionfromconstituent=='b' | opiniondirectionfrombill=='x', respondopinion=NA, success_on_bill=NA) %>%
  dplyr::select(-date,-urln,-pp_committee,-votecontent,-pp_enactment,-pp_enforcement,-pp_res_bynew,-pp_res_bycompete,-pp_res_notjudged,-pp_ignored,-billconflict,-pol_score,-eco_score,-SURVEYQUESTIONID) %>%
  mutate(ansv_and_label=paste0("[",SURVEYANSWERVALUE,"] ",LABEL)) %>%
  mutate_at(c("SURVEY","billresult","legislator_party","pp_agendavoting","pp_propose_advanceforagenda","value_on_q_variable","variable_on_q","pp_lawamendment","issue_field1","issue_field2","respondopinion","success_on_bill","ansv_and_label"), as.factor) %>%
  dplyr::select(-url.x,-url.y,-pp_keyword.x,-pp_keyword.y,-billcontent.x,-billcontent.y, -SURVEYANSWERVALUE, -LABEL, -QUESTION) %>%
  arrange(term, period, temp_meeting_no, meetingno, billn)

#save(mergedf_votes_bills_surveyanswer, file = paste0(dataset_in_scriptsfile_directory, "mergedf_votes_bills_surveyanswer.RData"))

#%>%
#mutate(term=stringi::stri_sub(electionname,from=1,length=1)) #%>%
#mutate(electionarea=last(unlist(strsplit(electionname,split="\\."))))
#names(bulletin_links) %>% sapply(function(X) {
#last(unlist(strsplit(X,split="\\.")))
#})
#可以看到有回應也有不回應
#distinct(mergedf_votes_bills_surveyanswer,votedecision,billid_myown,variable_on_q,value_on_q_variable,name,party,opiniondirectionfromconstituent,opiniondirectionfrombill,opiniondirectionfromlegislator,respondopinion) %>%
#  #testdf %>%
#  filter(billid_myown=="9-2-0-16-67",variable_on_q=="pp_related_q_1",value_on_q_variable=="2016citizen@c2") %>%
#  arrange(name,party) %>%
#  View()

#mergedf_votes_bills_surveyanswer %>%
#  filter(!is.na(respondopinion),billid_myown=="9-2-0-17-88",variable_on_q=="pp_related_q_1") %>%
#  distinct(name,votedecision,variable_on_q,respondopinion,billid_myown,party) %>%
#  arrange(party,billid_myown,variable_on_q,respondopinion) %>%
#  View()


