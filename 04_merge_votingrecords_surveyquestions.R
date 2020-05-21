# 第Ｏ部份：環境設定 --------------------------------
#Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
#Sys.setlocale(category = "LC_ALL", locale = "zh_TW.UTF-8")
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
adminparty <- list(
  data.frame(
    "term"=5,
    "party"=c("中國國民黨","民主進步黨","親民黨","台灣團結聯盟","新黨","台灣吾黨","無黨籍及未經政黨推薦"),
    "adminparty"=c(0,1,0,0,0,0,0)
  ),
  data.frame(
    "term"=6,
    "party"=c("中國國民黨","民主進步黨","親民黨","台灣團結聯盟","新黨","無黨團結聯盟","無黨籍及未經政黨推薦"),
    "adminparty"=c(0,1,0,0,0,0,0)
  ),
  data.frame(
    "term"=7,
    "party"=c("中國國民黨","民主進步黨","無黨團結聯盟","親民黨","無黨籍及未經政黨推薦"),
    "adminparty"=c(1,0,0,0,0)
  ),
  data.frame(
    "term"=8,
    "party"=c("中國國民黨","民主進步黨","無黨團結聯盟","親民黨","台灣團結聯盟","民國黨","無黨籍及未經政黨推薦"),
    "adminparty"=c(1,0,0,0,0,0,0)
  ),
  data.frame(
    "term"=9,
    "party"=c("中國國民黨","民主進步黨","時代力量","親民黨","無黨團結聯盟","無黨籍及未經政黨推薦"),
    "adminparty"=c(0,1,0,0,0,0)
  )
) %>%
  do.call(dplyr::bind_rows,.) %>%
  dplyr::mutate_at("adminparty", as.factor)

# 第一部分：投票及議案及「問卷答案對照意向」資料,主要也就是RData&excel檔  -------------------------------------------


# （略過）把投票紀錄整理輸出成法案紀錄 -------------

if (FALSE) {
  load(paste0(dataset_in_scriptsfile_directory, "myown_vote_record_df.RData"), verbose=TRUE)
  load(paste0(dataset_in_scriptsfile_directory, "myown_vote_record_detailed_part_df.RData"), verbose=TRUE)
  list( 
    dplyr::distinct(myown_vote_record_df,term,period,temp_meeting_no,meetingno,url) %>% dplyr::arrange(term,period,temp_meeting_no,meetingno), 
    dplyr::distinct(myown_vote_record_detailed_part_df,term,period,temp_meeting_no,meetingno,url) %>% dplyr::arrange(term,period,temp_meeting_no,meetingno)
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
    dplyr::mutate_at(c("billcontent","term","period","meetingno","temp_meeting_no","billn","url"), as.character) %>%
    dplyr::mutate_at("billcontent", trimws) %>%
    dplyr::mutate(billid_myown = paste(term, "-", period, "-", temp_meeting_no, "-", meetingno, "-", billn, sep = "")) %>%
    dplyr::mutate_at("billid_myown", as.character) %>%
    dplyr::mutate_at(c("term","period","meetingno","temp_meeting_no","billn"), as.numeric) %>%
    dplyr::distinct(url, billcontent, term, period, temp_meeting_no, meetingno, billn, billresult, date, billid_myown) %>%
    openxlsx::write.xlsx("TMP.xlsx")
}

# (通常略過)讀取投票紀錄資料(此處通常預處理好，直接load下面 mergedf_votes_bills_surveyanswer)  -------------------------------------------
if (FALSE) {
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
}

# 將合併好的投票紀錄進一步處理  -------------------------------------------

load(file=paste0(dataset_in_scriptsfile_directory, "myown_vote_record_df_across2004.RData"), verbose=TRUE)
load(file=paste0(dataset_in_scriptsfile_directory, "legislators_with_elections.RData"), verbose=TRUE)

tryCatch({
  myown_vote_bills_file <- paste0(dataset_file_directory, "votingdf_datafile_myown_englished.xlsx", sep="")
  bills_answer_to_bill <- openxlsx::read.xlsx(myown_vote_bills_file, sheet = 4)
  bills_billcontent <- openxlsx::read.xlsx(myown_vote_bills_file, sheet = 1) %>%
    dplyr::mutate_at("billcontent", as.character) %>%
    dplyr::select(-starts_with("pp_related_q_")) %>% #因為第四個表格問卷對政策實現與否表已經有了variable_on_q所以此處略過
    dplyr::mutate_at("pp_agendavoting", as.factor)
  save(bills_answer_to_bill, bills_billcontent, file=paste0(dataset_in_scriptsfile_directory, "bills_answer_to_bill_bills_billcontent.RData"))
}, error = function(msg) {
  message(paste0(msg,"\n"))
  load(file=paste0(dataset_in_scriptsfile_directory, "bills_answer_to_bill_bills_billcontent.RData"), envir = .GlobalEnv, verbose=TRUE)
})


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


# load_lib_or_install(c("FactoMineR","factoextra","parallel","magrittr"))
# 對投票紀錄進行探索性因素分析構面縮減檢查投票的構面，以及testing on IRT for vote decision --------------------------------

# 組成factor analysis專用dataset bills_billcontent ----------------
myown_vote_record_df_wide <- dplyr::distinct(myown_vote_record_df, term, legislator_name, billid_myown, votedecision) %>%
  dplyr::left_join(dplyr::distinct(legislators_with_elections, term, legislator_name, legislator_party)) %>%#legislator_party,
  dplyr::left_join(dplyr::distinct(bills_billcontent, billid_myown , pp_agendavoting, research_period) ) %>%
  {
    list("notagendavoting"=dplyr::filter(., pp_agendavoting==0, research_period==1),
    "agendavoting"=dplyr::filter(., pp_agendavoting==1, research_period==1))
  }
widedata_preserve_vars <- dplyr::setdiff(names(myown_vote_record_df_wide[[1]]), c("billid_myown","votedecision"))
widedata_formula <- paste0(widedata_preserve_vars, collapse="+") %>%
    paste0("~","billid_myown") %>%
    as.formula()
newlevel_of_voting<-c("反對", non_decision, "贊成")
myown_vote_record_df_wide_billidascol <- lapply(names(myown_vote_record_df_wide), function(key, ...) {
  dplyr::arrange(myown_vote_record_df_wide[[key]], legislator_name, billid_myown, votedecision) %>%
    dplyr::mutate_at(.vars=c("votedecision","legislator_party"), as.character) %>%
    lapply(terms, function (term,data) { data[data$term==term,] }, data=. ) %>%
    lapply(function(data, ...) {reshape2::dcast(data, widedata_formula, fun.aggregate=paste0, value.var="votedecision", fill="")}, widedata_formula=widedata_formula) %>%
    lapply(function(data) {dplyr::mutate_all(data, .funs=function (X){X<-ifelse(X=="",NA,X)} )} ) %>%
    #lapply(function(data) {t(data)}) %>%
    #lapply(function(data) {magrittr::set_colnames(data, data[2,])}) %>%
    #lapply(function(data) {as.data.frame(data[4:nrow(data),])}) %>%
    lapply(function(data) {dplyr::mutate_at(data, .vars=dplyr::setdiff(colnames(data), widedata_preserve_vars), function (X) {
      ordered(X, levels = newlevel_of_voting)
    })} ) %>%
    lapply(function(data) {magrittr::set_rownames(data, data$legislator_name)})
}, myown_vote_record_df_wide=myown_vote_record_df_wide, terms=terms, widedata_preserve_vars=widedata_preserve_vars, widedata_formula=widedata_formula) %>%
  magrittr::set_names(names(myown_vote_record_df_wide))

## Establishing connections --------------------------------
db_table_name<-"parallel_fa_result"
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


# * 先前parallel analysis的結果 --------------------------------

# * * 投票結果#fa parallel探測因素數目 --------------------------------
parallelfa_n_factors <- list()
parallelfa_n_factors_args_df <- data.frame(i=1:5, term=5:9) %>%
  cbind(., fm = rep(c("minres", "ml", "wls", "pa"), each = nrow(.))) %>%
  cbind(., completecase = rep(c(0,1), each = nrow(.))) %>%
  cbind(., agenda = rep(c("agendavoting","notagendavoting"), each = nrow(.))) %>%
  dplyr::mutate(store_key=paste0("term",term,"_",fm,"_completecase",completecase,"_",agenda) ) %>%
  dplyr::mutate_at(c("fm","store_key"), as.character)
random.polychor.parallelfa_n_factors_file<-paste0(dataset_in_scriptsfile_directory, "random.polychor.parallelfa_n_factors.RData")
parallelfa_n_factors_file<-paste0(dataset_in_scriptsfile_directory, "parallelfa_n_factors.RData")
usingrandom.polychor.pa<-FALSE
parallel_analysis_result_filename <- ifelse(usingrandom.polychor.pa==TRUE, random.polychor.parallelfa_n_factors_file, parallelfa_n_factors_file)
load(file=parallel_analysis_result_filename, envir = .GlobalEnv, verbose=TRUE)
if (FALSE) {
  parallel_analysis_looprange<-1:nrow(parallelfa_n_factors_args_df) #nrow(parallelfa_n_factors_args_df):1
  need_parallel_analysis_looprange<-lapply(parallelfa_n_factors, class) %>%
    .[!(. %in% c("try-error", "character"))] %>% names() %>%
    {which(parallelfa_n_factors_args_df$store_key %in% .)} %>%
    dplyr::setdiff(parallel_analysis_looprange, .)
  need_parallel_analysis_looprange<-1:nrow(parallelfa_n_factors_args_df)
  need_parallel_analysis_looprange<-which(!parallelfa_n_factors_args_df$store_key %in% names(parallelfa_n_factors))
  need_parallel_analysis_looprange<-sort(need_parallel_analysis_looprange, decreasing = TRUE)
  need_parallel_analysis_looprange<-parallelfa_n_factors_args_df[need_parallel_analysis_looprange,] %>%
    cbind(needi=need_parallel_analysis_looprange) %>%
    dplyr::arrange(agenda, term, dplyr::desc(completecase)) %>%
    magrittr::use_series(needi)
  parallelfa_n_factors <-custom_parallel_lapply(need_parallel_analysis_looprange, function(fi, ...) {
    #for (i in 1:length(myown_vote_record_df_wide_billidascol)) { #
    arg_row<-parallelfa_n_factors_args_df[fi,]
    i<-arg_row$i
    plot_title<-arg_row$term
    if_agenda<-arg_row$agenda %>% as.character()
    message(paste("now in term", plot_title, "fm=", arg_row$fm, "completecase=", arg_row$completecase," agenda=",if_agenda))
    colname_billids <- setdiff(names(myown_vote_record_df_wide_billidascol[[if_agenda]][[i]]), widedata_preserve_vars)
    votingdfwide<-myown_vote_record_df_wide_billidascol[[if_agenda]][[i]][,colname_billids] %>%
      {
        if(arg_row$completecase==1) .[complete.cases(.),] else .
      } %>%
      dplyr::mutate_all(.funs=unclass) %>%
      as.matrix()
    res_n_factors <- try({
      if (usingrandom.polychor.pa!=TRUE) {
        psych::fa.parallel(votingdfwide, fm=arg_row$fm, main=paste("Parallel Analysis Scree Plots with fm =", arg_row$fm), cor="poly")
      } else { #capture.output
        (random.polychor.pa::random.polychor.pa(nrep=5, data.matrix = votingdfwide, q.eigen=.99, distr="uniform", print.all=FALSE))
      }
    })
    if (res_n_factors!="ERROR") {
      #plot(res_n_factors)
    }
    tryn<-1
    while(TRUE){
      loadingstatus<-try({load(file=parallel_analysis_result_filename, envir = .GlobalEnv, verbose=TRUE)})
      tryn <- tryn+1
      if(!is(loadingstatus, 'try-error') | tryn>10) break
    }
    if (class(res_n_factors)=="psych") {
      parallelfa_n_factors[[arg_row$store_key]]<-res_n_factors
      tryn<-1
      while(TRUE){
        savingstatus<-try({save(parallelfa_n_factors, file=parallel_analysis_result_filename)})
        tryn <- tryn+1
        if(!is(savingstatus, 'try-error') | tryn>10) break
      }
    }
    return(res_n_factors)
  }, terms=terms, myown_vote_record_df_wide_billidascol=myown_vote_record_df_wide_billidascol
  , usingrandom.polychor.pa=usingrandom.polychor.pa
  , parallelfa_n_factors_args_df=parallelfa_n_factors_args_df
  , parallel_analysis_result_filename=parallel_analysis_result_filename
  , method=parallel_method
  #, mc.cores=7
  ) %>%
    magrittr::set_names(parallelfa_n_factors_args_df[need_parallel_analysis_looprange,"store_key"])
  save(parallelfa_n_factors, file=parallel_analysis_result_filename)
}
#remove(plot_title,colname_billids,votingdfwide,parallel_analysis_result_filename,res_n_factors,parallelfa_n_factors)
parallelfa_result_n_factor <- dplyr::left_join(parallelfa_n_factors_args_df,
  data.frame(
    store_key=names(parallelfa_n_factors),
    fa=sapply(parallelfa_n_factors, function (X) {if (class(X)=="psych") {return(X$nfact)} else {return(X)}}),
    comp=sapply(parallelfa_n_factors, function (X) {if (class(X)=="psych") {return(X$ncomp)} else {return(X)}})
  )
) %>%
  dplyr::arrange(term, fm, completecase)
# * MDS algorithms --------------------------------
#http://www.hmwu.idv.tw/web/R/C01-hmwu_R-DimensionReduction.pdf


# * EFA by MCMC --------------------------------
res.MCMCefas <- list()
resMCMCefasfile <- paste0(dataset_in_scriptsfile_directory, "res.MCMCefas.RData")
load(file=resMCMCefasfile, verbose=TRUE)
need_parallelfa_result_n_factor<-reshape2::melt(parallelfa_result_n_factor, id.vars=c("i","term","fm","completecase","agenda","store_key")) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::distinct(i,term,agenda,value) %>%
  dplyr::mutate_at("value", as.integer) %>%
  dplyr::bind_rows(., {
    dplyr::mutate_at(., "value", list(~.+1))
  }) %>%
  dplyr::distinct(i,term,agenda,value) %>%
  dplyr::mutate(store_key=paste0("term",term,"_",value,"_",agenda)) %>%
  dplyr::mutate(alreadyprocessed=magrittr::is_in(store_key, !!names(res.MCMCefas))) %>%
  dplyr::arrange(alreadyprocessed, dplyr::desc(term), dplyr::desc(agenda))
iterswqforres.MCMCefas<-nrow(need_parallelfa_result_n_factor):1
res.MCMCefas <- custom_parallel_lapply(iterswqforres.MCMCefas, function (fi, ...) { #1:nrow(parallelfa_result_n_factor)
  n_component_row<-need_parallelfa_result_n_factor[fi,]
  n_component<-magrittr::use_series(n_component_row, value) %>% as.character() %>% as.integer()
  need_agenda_key<-n_component_row$agenda %>% as.character()
  colname_billids <- setdiff(names(myown_vote_record_df_wide_billidascol[[need_agenda_key]][[n_component_row$i]]), widedata_preserve_vars)
  adj_colname_billids <- stringr::str_replace_all(colname_billids, "-","_") %>%
    paste0("w",.)
  votingdfwide<-myown_vote_record_df_wide_billidascol[[need_agenda_key]][[n_component_row$i]][,colname_billids] %>%
    dplyr::mutate_all(.funs=unclass) %>%
    magrittr::set_colnames(adj_colname_billids)
  residx <- magrittr::use_series(n_component_row, store_key) %>% as.character()
  mcmcformula <- paste("`",adj_colname_billids,"`", collapse=" + ", sep="") %>%
    paste0("~ ",.) %>%
    as.formula()
  lconstrains<-list()
  if (residx=="term9_4_notagendavoting") {
    lconstrains<-list(
      #PRO政治檔案條例草案立法通過 與 促轉 完全同s at dim2；大部分同at dim3,5
      #同婚與促轉 同s at dim2,5；大部分同s at dim3；
      #同婚與勞基法修惡同s at dim2 dim3 dim5(絕大部分)
      #同婚與PRO降低遺贈稅免稅額門檻 同s at dim2,3,4,5
      #同婚與
      #國安五法
      #同婚系列
      w9_7_0_14_4=list(2,"-"),w9_7_0_14_2=list(2,"-"), w9_7_0_14_26=list(2,"-"), w9_7_0_14_3=list(2,"-"), w9_7_0_14_20=list(2,"-"), w9_7_0_14_8=list(2,"-"), w9_7_0_14_4=list(2,"-"), w9_7_0_14_15=list(2,"-"), w9_7_0_14_31=list(2,"-"), w9_7_0_14_13=list(2,"-"), w9_7_0_14_18=list(2,"-"), w9_7_0_14_22=list(2,"-"), w9_7_0_14_24=list(2,"-"), w9_7_0_14_21=list(2,"-"), w9_7_0_14_30=list(2,"-"), w9_7_0_14_12=list(2,"-"), w9_7_0_14_27=list(2,"-"), w9_7_0_14_9=list(2,"-"), w9_7_0_14_25=list(2,"-"), w9_7_0_14_33=list(2,"-"), w9_7_0_14_28=list(2,"-"), w9_7_0_14_34=list(2,"-"), w9_7_0_14_17=list(2,"-"), w9_7_0_14_32=list(2,"-"), w9_7_0_14_19=list(2,"-"), w9_7_0_14_29=list(2,"-"), w9_7_0_14_14=list(2,"-"), w9_7_0_14_11=list(2,"-"), w9_7_0_14_23=list(2,"-"), w9_7_0_14_10=list(2,"-"), w9_7_0_14_16=list(2,"-"), w9_7_0_14_6=list(2,"-"), w9_7_0_14_5=list(2,"-"), w9_7_0_14_7=list(2,"-"),
      w9_7_0_14_4=list(5,"-"),w9_7_0_14_2=list(5,"-"), w9_7_0_14_26=list(5,"-"), w9_7_0_14_3=list(5,"-"), w9_7_0_14_20=list(5,"-"), w9_7_0_14_8=list(5,"-"), w9_7_0_14_4=list(5,"-"), w9_7_0_14_15=list(5,"-"), w9_7_0_14_31=list(5,"-"), w9_7_0_14_13=list(5,"-"), w9_7_0_14_18=list(5,"-"), w9_7_0_14_22=list(5,"-"), w9_7_0_14_24=list(5,"-"), w9_7_0_14_21=list(5,"-"), w9_7_0_14_30=list(5,"-"), w9_7_0_14_12=list(5,"-"), w9_7_0_14_27=list(5,"-"), w9_7_0_14_9=list(5,"-"), w9_7_0_14_25=list(5,"-"), w9_7_0_14_33=list(5,"-"), w9_7_0_14_28=list(5,"-"), w9_7_0_14_34=list(5,"-"), w9_7_0_14_17=list(5,"-"), w9_7_0_14_32=list(5,"-"), w9_7_0_14_19=list(5,"-"), w9_7_0_14_29=list(5,"-"), w9_7_0_14_14=list(5,"-"), w9_7_0_14_11=list(5,"-"), w9_7_0_14_23=list(5,"-"), w9_7_0_14_10=list(5,"-"), w9_7_0_14_16=list(5,"-"), w9_7_0_14_6=list(5,"-"), w9_7_0_14_5=list(5,"-"), w9_7_0_14_7=list(5,"-"),
      #酒駕加刑
      #w9_7_0_16_5=list(), 
      #促轉會系列
      w9_4_0_11_3=list(2,"-"), w9_4_0_11_13=list(2,"-"), w9_4_0_11_23=list(2,"-"), w9_4_0_11_11=list(2,"-"), w9_4_0_11_12=list(2,"-"), w9_4_0_11_18=list(2,"-"), w9_4_0_11_4=list(2,"-"), w9_4_0_11_15=list(2,"-"), w9_4_0_11_8=list(2,"-"), w9_4_0_11_20=list(2,"-"), w9_4_0_11_9=list(2,"-"), w9_4_0_11_10=list(2,"-"), w9_4_0_11_1=list(2,"-"), w9_4_0_11_14=list(2,"-"), w9_4_0_11_17=list(2,"-"), w9_4_0_11_24=list(2,"-"), w9_4_0_11_25=list(2,"-"), w9_4_0_11_19=list(2,"-"), w9_4_0_11_22=list(2,"-"), w9_4_0_11_2=list(2,"-"), w9_4_0_11_16=list(2,"-"),
      w9_4_0_11_3=list(5,"-"), w9_4_0_11_13=list(5,"-"), w9_4_0_11_23=list(5,"-"), w9_4_0_11_11=list(5,"-"), w9_4_0_11_12=list(5,"-"), w9_4_0_11_18=list(5,"-"), w9_4_0_11_4=list(5,"-"), w9_4_0_11_15=list(5,"-"), w9_4_0_11_8=list(5,"-"), w9_4_0_11_20=list(5,"-"), w9_4_0_11_9=list(5,"-"), w9_4_0_11_10=list(5,"-"), w9_4_0_11_1=list(5,"-"), w9_4_0_11_14=list(5,"-"), w9_4_0_11_17=list(5,"-"), w9_4_0_11_24=list(5,"-"), w9_4_0_11_25=list(5,"-"), w9_4_0_11_19=list(5,"-"), w9_4_0_11_22=list(5,"-"), w9_4_0_11_2=list(5,"-"), w9_4_0_11_16=list(5,"-"),w9_4_0_11_3=list(5,"-"), w9_4_0_11_13=list(5,"-"), w9_4_0_11_23=list(5,"-"), w9_4_0_11_11=list(5,"-"), w9_4_0_11_12=list(5,"-"), w9_4_0_11_18=list(5,"-"), w9_4_0_11_4=list(5,"-"), w9_4_0_11_15=list(5,"-"), w9_4_0_11_8=list(5,"-"), w9_4_0_11_20=list(5,"-"), w9_4_0_11_9=list(5,"-"), w9_4_0_11_10=list(5,"-"), w9_4_0_11_1=list(5,"-"), w9_4_0_11_14=list(5,"-"), w9_4_0_11_17=list(5,"-"), w9_4_0_11_24=list(5,"-"), w9_4_0_11_25=list(5,"-"), w9_4_0_11_19=list(5,"-"), w9_4_0_11_22=list(5,"-"), w9_4_0_11_2=list(5,"-"), w9_4_0_11_16=list(5,"-"),
      
      #勞基法修惡 與 勞基法PRO降低特休門檻增加特休、PRO特休勞工決定(9-2-0-13-6)	不同s at dim3,5
      #勞基法修惡 與 砍七天國定假 同s at dim2,4,5
      #勞基法修惡 與 PRO所得稅法修正；PRO有錢人、財團降稅 同s at dim4,5
      #勞基法修惡 與 PRO平均地權條例不動產交易價格資訊申報登錄免除地政士登錄責任改為買賣雙方 同s at dim4
      #勞基法修惡 與 PRO降低遺贈稅免稅額門檻(課更多稅) 不同s at dim4
      #勞基法修惡 與 Pro工廠管理輔導法輔導農地工廠合法化 增訂第二十八條之五 同s at dim2,3,4,5
      #PRO降低遺贈稅免稅額門檻 與 #PRO所得稅法修正；PRO有錢人、財團降稅 不同s at dim2,3,4,5
      #PRO推動國家住宅與都市更新 與 #PRO所得稅法修正；PRO有錢人、財團降稅 不同s at dim3
      #年改間 同s at dim2
      #PRO勞基法修惡 與 年改 不同s at dim3（一點點）dim4（一點點）dim5（一點點）
      #勞基法修惡 與 年改 不同s at no dim ;
      #PRO推動國家住宅與都市更新 and PRO長照法修正明訂遺產稅及贈與稅、菸酒稅菸品應徵稅稅率加稅作為財源 同s at dim2,4,5
      #綜合以上dim4應該是與經濟社會面向、所得重分配有關
      #PRO長照法修正增加財源
      
      #勞基法修惡系列
      w_9_4_1_2_152=list(5,"+"), w9_4_1_2_153=list(5,"+"), w9_4_1_2_154=list(5,"+"), w9_4_1_2_155=list(5,"+"), w9_4_1_2_156=list(5,"+"), w9_4_1_2_157=list(5,"+"), w9_4_1_2_158=list(5,"+"), w9_4_1_2_160=list(5,"+"),
      #砍七天國定假
      w9_2_0_13_5=list(5,"+"),
      #保障勞工的勞基法系列
      w9_2_0_13_6=list(5,"-"),
      #PRO降低遺贈稅免稅額門檻
      w9_3_0_10_1=list(5,"-"),
      #PRO所得稅法修正；PRO有錢人、財團降稅 系列
      w9_4_1_2_168=list(5,"+"), w9_4_1_2_169=list(5,"+"), w9_4_1_2_170=list(5,"+"),
      #PRO推動國家住宅與都市更新
      #w9_4_1_2_171=list(),
      #PRO長照法修正明訂遺產稅及贈與稅、菸酒稅菸品應徵稅稅率加稅作為財源(對照時代力量版本)
      #w9_2_1_1_4=list(),
      #Pro工廠管理輔導法輔導農地工廠合法化 增訂第二十八條之五
      #w_7_1_2_7=list(),
      #政務人員年改系列
      w9_3_1_3_25=list(2,"-"), w9_3_1_3_26=list(2,"-"), w9_3_1_3_27=list(2,"-"), w9_3_1_3_28=list(2,"-"),
      #公務員年改系列,
      w9_3_1_2_1=list(2,"-"), w9_3_1_2_2=list(2,"-"), w9_3_1_2_3=list(2,"-"), w9_3_1_2_4=list(2,"-"), w9_3_1_2_5=list(2,"-"), w9_3_1_2_6=list(2,"-"), w9_3_1_2_7=list(2,"-"), w9_3_1_2_8=list(2,"-"), w9_3_1_2_9=list(2,"-"), w9_3_1_2_10=list(2,"-"), w9_3_1_2_11=list(2,"-"),w9_3_1_3_1=list(2,"-"), w9_3_1_3_2=list(2,"-"), w9_3_1_3_3=list(2,"-"), w9_3_1_3_4=list(2,"-"),
      #教職員年改系列,
      w9_3_1_3_5=list(2,"-"), w9_3_1_3_6=list(2,"-"), w9_3_1_3_7=list(2,"-"), w9_3_1_3_8=list(2,"-"), w9_3_1_3_9=list(2,"-"), w9_3_1_3_10=list(2,"-"), w9_3_1_3_11=list(2,"-"), w9_3_1_3_12=list(2,"-"), w9_3_1_3_13=list(2,"-"), w9_3_1_3_14=list(2,"-"), w9_3_1_3_15=list(2,"-"), w9_3_1_3_16=list(2,"-"), w9_3_1_3_17=list(2,"-"), w9_3_1_3_18=list(2,"-"), w9_3_1_3_19=list(2,"-"), w9_3_1_3_20=list(2,"-"), w9_3_1_3_21=list(2,"-"), w9_3_1_3_22=list(2,"-"), w9_3_1_3_23=list(2,"-"), w9_3_1_3_24=list(2,"-"),
      #軍人年改系列
      w9_5_1_1_2=list(2,"-"), w9_5_1_1_3=list(2,"-"), w9_5_1_1_4=list(2,"-"), w9_5_1_1_5=list(2,"-"), w9_5_1_1_6=list(2,"-"), w9_5_1_1_7=list(2,"-"), w9_5_1_1_8=list(2,"-"), w9_5_1_1_9=list(2,"-"), w9_5_1_1_10=list(2,"-"), w9_5_1_1_11=list(2,"-"), w9_5_1_1_12=list(2,"-"), w9_5_1_1_13=list(2,"-"), w9_5_1_1_14=list(2,"-"), w9_5_1_1_15=list(2,"-"), w9_5_1_1_16=list(2,"-"), w9_5_1_1_17=list(2,"-"), w9_5_1_1_18=list(2,"-")
    )
  }
  if (TRUE) {
    resmodel <- MCMCpack::MCMCordfactanal(
      x=~. , factors=n_component,
      lambda.constraints=lconstrains,
      data=votingdfwide, verbose=0,
      l0=0, L0=0.1,
      mcmc=50000, thin=25, store.lambda=TRUE, store.scores=TRUE
    )
    tryn<-1
    while(TRUE){
      loadingsvingstatus<-try({
        load(file=resMCMCefasfile, verbose=TRUE)
        res.MCMCefas[[residx]] <- resmodel
        save(res.MCMCefas, file = resMCMCefasfile)
      })
      tryn <- tryn+1
      if(!is(loadingsvingstatus, 'try-error') | tryn>10) {break}
      Sys.sleep(10)
    }
    return(resmodel)
  }
},myown_vote_record_df_wide_billidascol=myown_vote_record_df_wide_billidascol,
resMCMCefasfile=resMCMCefasfile,
need_parallelfa_result_n_factor=need_parallelfa_result_n_factor,
widedata_preserve_vars=widedata_preserve_vars,
method=parallel_method) %>% #parallel::detectCores()
  magrittr::set_names(need_parallelfa_result_n_factor$store_key[iterswqforres.MCMCefas])
save(res.MCMCefas, file=resMCMCefasfile)

# res.MCMCefas <- names(res.MCMCefas) %>%
#   gsub(pattern="_\\d{1}","_minres",.) %>%
#   paste0("term",.) %>%
#   magrittr::set_names(res.MCMCefas,.)

load(file=resMCMCefasfile, verbose=TRUE)
for (residx in names(res.MCMCefas)) {
  residx_args<-unlist(strsplit(residx,"_"))
  term<-grep("term", residx_args, value=TRUE) %>%
    gsub("term","",.)
  if (as.character(term)!="9") next
  fi<-dplyr::filter(parallelfa_result_n_factor, term==!!term)$i[1]
  need_agenda_key<-grep("agenda", residx_args, value=TRUE)
  #fi<-parallelfa_result_n_factor$i[which(parallelfa_result_n_factor$store_key==residx)]
  #legislator_names <- myown_vote_record_df_wide_billidascol[[fi]]$legislator_name
  result<-res.MCMCefas[[residx]]
  means.sds <- summary(result)[[1]][,1:2]
  ideal.points.df <- means.sds[grepl("phi",rownames(means.sds)),] %>%
    as.data.frame() %>%
    cbind(dim={
      stringr::str_extract(row.names(.), "\\.\\d$") %>%
        unlist() %>%
        stringr::str_replace(., "\\.", "") %>%
        unlist() %>%
        paste0("dim", .)
    }) %>%
    cbind(legislator_id=as.integer({
      stringr::str_extract(row.names(.), "phi\\.\\d{1,3}") %>%
        unlist() %>%
        gsub("phi\\.","", .)
    })) %>%
    reshape2::melt(id.vars=c("legislator_id","dim")) %>%
    reshape2::dcast(legislator_id ~ dim + variable, value.var="value") %>%
    data.frame(term=term,legislator_name=myown_vote_record_df_wide_billidascol[[need_agenda_key]][[fi]]$legislator_name, agenda=need_agenda_key, .) %>%
    dplyr::select(-legislator_id)
  openxlsx::write.xlsx(ideal.points.df, file="TMP.xlsx")
  readline(paste0("now in ",residx," ideal points, continue?"))
  item.params.df <- means.sds[grepl("Lambda",rownames(means.sds)),] %>%
    as.data.frame() %>%
    cbind(dim={
      stringr::str_extract(row.names(.), "\\.\\d") %>%
        unlist() %>%
        stringr::str_replace(., "\\.", "") %>%
        unlist() %>%
        paste0("dim", .)
    }) %>%
    dplyr::mutate(billid={
      gsub("Lambdaw","",row.names(.)) %>%
        gsub("_","-",.) %>%
        stringr::str_extract(., "\\d{1,3}-\\d{1,3}-\\d{1,3}-\\d{1,3}-\\d{1,3}") %>%
        unlist()
    }) %>%
    magrittr::set_rownames(NULL) %>%
    reshape2::melt(id.vars=c("billid","dim")) %>%
    reshape2::dcast(billid ~ dim + variable, value.var="value") %>%
    data.frame(term=term, agenda=need_agenda_key, .)
  openxlsx::write.xlsx(item.params.df, file="TMP.xlsx")
  readline(paste0("now in ",residx," item loadings, continue?"))
}

# * EFA by exploratory IRT using mirt --------------------------------
res.mirtefas <- list()
resmirtefasfile <- paste0(dataset_in_scriptsfile_directory, "res.mirtefas.RData")
resmirt_itemfitefasfile <- paste0(dataset_in_scriptsfile_directory, "res.mirtitemfitefas.RData")
#nrow(parallelfa_result_n_factor)
res.mirtefas<-custom_parallel_lapply(1:nrow(parallelfa_result_n_factor), function (fi, myown_vote_record_df_wide_billidascol, resmirtefasfile, parallelfa_result_n_factor, widedata_preserve_vars) {
  n_component_row<-dplyr::filter(parallelfa_result_n_factor, i==fi)
  n_component<-magrittr::use_series(n_component_row, fa)
  plot_title<-magrittr::use_series(n_component_row, term)
  residx <- paste0(plot_title,"_",n_component)
  colname_billids <- setdiff(names(myown_vote_record_df_wide_billidascol[[fi]]), widedata_preserve_vars)
  votingdfwide<-myown_vote_record_df_wide_billidascol[[fi]][,colname_billids] %>%
    dplyr::mutate_all(.funs=unclass) %>%
    .[complete.cases(.),] #all 686 cols 685 ok
  message(paste0("now in ",residx))
  mirt::mirtCluster()
  if (TRUE) {
    resmodel <- mirt::mirt(data=votingdfwide, model=n_component, itemtype='graded', method='QMCEM', technical=list(NCYCLES=2000))
    load(file=resmirtefasfile, verbose=TRUE)
    res.mirtefas[[residx]] <- resmodel
    save(res.mirtefas, file = resmirtefasfile)
    return(resmodel)
  }
  #remove(n_component_row, n_component, plot_title, residx, colname_billids, votingdfwide, resmodel)
},myown_vote_record_df_wide_billidascol=myown_vote_record_df_wide_billidascol,
resmirtefasfile=resmirtefasfile,
parallelfa_result_n_factor=parallelfa_result_n_factor,
widedata_preserve_vars=widedata_preserve_vars,
method=parallel_method,
mc.cores=nrow(parallelfa_result_n_factor))

#load(file=paste0(dataset_in_scriptsfile_directory, "res.mirtefas.a715.MCEM.RData"), verbose=TRUE)
#load(file=paste0(dataset_in_scriptsfile_directory, "res.mirtefas.h170.RData"), verbose=TRUE)

parallelfa_result_n_factor
# mirt::anova(res.mirtefas$`5_3`, res.mirtefas$`5_4`) #base 5-3 BIC 14030.53
# mirt::anova(res.mirtefas$`6_3`, res.mirtefas$`6_5`) #base 6-3 BIC 8397.375
# mirt::anova(res.mirtefas$`7_3`, res.mirtefas$`7_6`) #base 7-3 BIC 14218.07
# mirt::anova(res.mirtefas$`8_1`, res.mirtefas$`8_6`) #base 8-1 BIC 501.2352
mirtitemfit<-list()
load(file=resmirt_itemfitefasfile, verbose=TRUE)
for (residx in names(res.mirtefas)) {
  #if (residx %in% c("7_3",names(mirtitemfit))) {next}
  resmodel<-res.mirtefas[[residx]]
  mirtitemfit[[residx]]<-tryCatch({
    mirt::itemfit(resmodel, na.rm=TRUE, QMC=TRUE)
  }, error = function(msg) {
    message(paste0(msg,"\n"))
    return("ERROR")
  } ) 
  save(mirtitemfit, file=resmirt_itemfitefasfile)
  #readline(paste0("now in ", residx,", continue?"))
}
for (residx in names(res.mirtefas)) {
  term<-parallelfa_result_n_factor$term[which(parallelfa_result_n_factor$need_factorn==residx)]
  fi<-parallelfa_result_n_factor$i[which(parallelfa_result_n_factor$need_factorn==residx)]
  colname_billids <- setdiff(names(myown_vote_record_df_wide_billidascol[[fi]]), widedata_preserve_vars)
  legislator_names<-myown_vote_record_df_wide_billidascol[[fi]][,c("legislator_name",colname_billids)] %>%
    .[complete.cases(.),] %>%
    magrittr::use_series("legislator_name")
  coefdf <- mirt::coef(res.mirtefas[[residx]], rotate="varimax", as.data.frame=TRUE) %>%
    .[grepl("Group",rownames(.))==FALSE,] %>%
    data.frame(term=term, attr={
      stringr::str_replace_all(names(.),"\\.","-") %>%
        stringr::str_replace_all(., "X", "") %>%
        stringr::str_extract_all(., "\\w\\d$") %>%
        unlist()
    }, billid={
      stringr::str_replace_all(names(.),"\\.","-") %>%
        stringr::str_replace_all(., "X", "") %>%
        stringr::str_extract_all(., "\\d{1,3}-\\d{1,3}-\\d{1,3}-\\d{1,3}-\\d{1,3}") %>%
        unlist()
    }, var=.) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate_at("var",as.numeric) %>%
    magrittr::set_rownames(NULL) %>%
    reshape2::dcast(term + billid ~ attr, value.var="var")
  openxlsx::write.xlsx(coefdf, "TMP.xlsx")
  readline(paste0("now in ", residx," coef, continue?"))
  fscoredf<-mirt::fscores(res.mirtefas[[residx]], method = "MAP", QMC=TRUE, rotate="varimax") %>%
    data.frame(name=legislator_names, term=term, .)
  openxlsx::write.xlsx(fscoredf, "TMP.xlsx")
  readline(paste0("now in ", residx," fscore, continue?"))
}
for (residx in names(res.mirtefas)) {
  #mirt::summary(res.mirtefas[[residx]], rotate = "varimax") %>% print()
  
  readline(paste0("now in ", residx," , continue?"))
}
#find misfitting items
t[t[, 4] < 0.05, ]
summary(resmodel, rotate = "varimax")
mirt::itemplot(resmodel)


# * EFA by IRT.fa --------------------------------
res.irtefas <- list()
resirtefasfile <- paste0(dataset_in_scriptsfile_directory, "res.irtefas.RData")
custom_parallel_lapply(1:nrow(parallelfa_result_n_factor), function (fi, myown_vote_record_df_wide_billidascol, resirtefasfile, parallelfa_result_n_factor, widedata_preserve_vars) {
  n_component_row<-dplyr::filter(parallelfa_result_n_factor, i==fi)
  n_component<-magrittr::use_series(n_component_row, fa)
  plot_title<-magrittr::use_series(n_component_row, term)
  colname_billids <- setdiff(names(myown_vote_record_df_wide_billidascol[[fi]]), widedata_preserve_vars)
  votingdfwide<-myown_vote_record_df_wide_billidascol[[fi]][,colname_billids] %>%
    dplyr::mutate_all(.funs=unclass)
  residx <- paste0(plot_title,"_",n_component)
  if (TRUE) {
    resmodel <- #psych::polychoric(votingdfwide, na.rm=FALSE) %>%
      psych::irt.fa(votingdfwide,nfactors=n_component, rotate="varimax", missing=TRUE)
    load(file=resirtefasfile)
    res.irtefas[[residx]] <- resmodel
    save(res.irtefas, file = resirtefasfile)
  }
},myown_vote_record_df_wide_billidascol=myown_vote_record_df_wide_billidascol,
resirtefasfile=resirtefasfile,
parallelfa_result_n_factor=parallelfa_result_n_factor,
widedata_preserve_vars=widedata_preserve_vars,
method=parallel_method,
mc.cores=1) #nrow(parallelfa_result_n_factor)
load(file=resirtefasfile, verbose=TRUE)
for (residx in names(res.irtefas)) {
  #View(res.irtefas[[residx]]$irt$difficulty)
  res.irtefas[[residx]]$irt$discrimination %>%
    {dplyr::bind_cols(
      data.frame(billid=rownames(.)), as.data.frame(.))} %>%
    openxlsx::write.xlsx("TMP.xlsx")
  readline(paste0("now in ", residx))
}
for (residx in names(res.irtefas)) {
  fi<-parallelfa_result_n_factor$i[which(parallelfa_result_n_factor$need_factorn==residx)]
  term<-parallelfa_result_n_factor$term[which(parallelfa_result_n_factor$need_factorn==residx)]
  colname_billids <- setdiff(names(myown_vote_record_df_wide_billidascol[[fi]]), widedata_preserve_vars)
  votingdfwide<-myown_vote_record_df_wide_billidascol[[fi]][,colname_billids] %>%
    dplyr::mutate_all(.funs=unclass)
  #psych::score.irt.poly(stats=res.irtefas[[residx]], items=votingdfwide)
  dplyr::select(myown_vote_record_df_wide_billidascol[[fi]] ,legislator_name) %>%
    dplyr::mutate(term=!!term) %>%
    dplyr::bind_cols(psych::scoreIrt(stats=res.irtefas[[residx]], items=votingdfwide)) %>%
    openxlsx::write.xlsx("TMP.xlsx")
  readline(paste0("now in ", residx))
}
# * EFA by polychoric correlations --------------------------------
#http://personality-project.org/r/psych/HowTo/factor.pdf
res.catgfas <- list()
rescatgfasfile <- paste0(dataset_in_scriptsfile_directory, "res.catgfas_noimp.RData")
custom_parallel_lapply(1:nrow(parallelfa_result_n_factor), function (fi, myown_vote_record_df_wide_billidascol, rescatgfasfile, parallelfa_result_n_factor, widedata_preserve_vars) {
  n_component_row<-dplyr::filter(parallelfa_result_n_factor, i==fi)
  n_component<-magrittr::use_series(n_component_row, fa)
  plot_title<-magrittr::use_series(n_component_row, term)
  colname_billids <- setdiff(names(myown_vote_record_df_wide_billidascol[[fi]]), widedata_preserve_vars)
  votingdfwide<-myown_vote_record_df_wide_billidascol[[fi]][,colname_billids] %>%
    dplyr::mutate_all(.funs=unclass)
  residx <- paste0(plot_title,"_",n_component)
  if (TRUE) {
    #from https://alice86.github.io/2018/04/08/Factor-Analysis-on-Ordinal-Data-example-in-R-(psych,-homals)/
    #https://sakaluk.wordpress.com/2016/05/26/11-make-it-pretty-scree-plots-and-parallel-analysis-using-psych-and-ggplot2/
    resmodel <- psych::fa(r=votingdfwide, nfactors=n_component, rotate="varimax", cor="poly", missing=FALSE)
    load(file=rescatgfasfile)
    res.catgfas[[residx]] <- resmodel
    save(res.catgfas, file = rescatgfasfile)
  }
},myown_vote_record_df_wide_billidascol=myown_vote_record_df_wide_billidascol,
rescatgfasfile=rescatgfasfile,
parallelfa_result_n_factor=parallelfa_result_n_factor,
widedata_preserve_vars=widedata_preserve_vars,
method=parallel_method,
mc.cores=nrow(parallelfa_result_n_factor))
load(rescatgfasfile,verbose=TRUE)
for (residx in names(res.catgfas)) {
  res.catgfas[[residx]]$loadings %>% unclass() %>% as.data.frame() %>%
    {
      cbind(., row.names(.))
    } %>%
    openxlsx::write.xlsx("TMP.xlsx") 
  readline(paste0("now in ",residx))
}
for (residx in names(res.catgfas)) {
  fi<-parallelfa_result_n_factor$i[which(parallelfa_result_n_factor$need_factorn==residx)]
  term<-parallelfa_result_n_factor$term[which(parallelfa_result_n_factor$need_factorn==residx)]
  data.frame(name=myown_vote_record_df_wide_billidascol[[fi]]$legislator_name) %>%
    dplyr::bind_cols(  as.data.frame(res.catgfas[[residx]]$scores)  ) %>%
    openxlsx::write.xlsx("TMP.xlsx")
  readline(paste0("now in term ", term))
}
# * EFA by homals --------------------------------
res.homals <- list()
# res.homals.accepted_ndimensions <- list()
res.homals.loadings <- list()
reshomalsfile <- paste0(dataset_in_scriptsfile_directory, "res.homals.RData")
res.homals<-custom_parallel_lapply(1:nrow(parallelfa_result_n_factor), function(i,...) {
  plot_title<-parallelfa_result_n_factor$term[i]
  colname_billids <- setdiff(names(myown_vote_record_df_wide_billidascol[[i]]), widedata_preserve_vars)
  votingdfwide<-myown_vote_record_df_wide_billidascol[[i]][,colname_billids] %>%
    .[complete.cases(.),]
  n_component<-parallelfa_result_n_factor$fa[i]
  message(paste("now in",plot_title,"for component in",n_component))
  #for (n_component in 2:8) { #repeat
  #n_component<-as.integer(readline(prompt="Enter n_component: "))
  #from https://alice86.github.io/2018/04/08/Factor-Analysis-on-Ordinal-Data-example-in-R-(psych,-homals)/
  residx <- paste0(plot_title,"_",n_component)
  message(paste("now in",residx))
  resmodel <- homals::homals(votingdfwide, ndim = n_component, level = "ordinal")
  res.homal.df<-data.frame("index"=residx, "eigenvalue"=resmodel$eigenvalues, "dimension"=1:resmodel$ndim)
  cache <- apply(votingdfwide, 2, function(x) nlevels(as.factor(x)))
  ld <- unlist(lapply(resmodel$loadings, function(x) x[1,]))
  loadings <- matrix(ld, byrow = T, nrow = n_component)
  colnames(loadings) <- names(cache)
  load(reshomalsfile, verbose=TRUE)
  res.homals[[residx]] <- resmodel
  res.homals.loadings[[residx]] <- loadings
  save(res.homals, res.homals.loadings, file = reshomalsfile) #res.homals.accepted_ndimensions
  return(resmodel)
  #summary(res.homals[[residx]])
  #plot(res.homals[[residx]], plot.type = "screeplot")
  #ggplotscreeplot(res.homal.df, "dimension", "eigenvalue")
  # tmpdim <- readline(paste0("now at ", residx, ", Enter number of dimensions?"))
  # if (tmpdim=="") {
  #   break
  # } else {
  #   res.homals.accepted_ndimensions[[residx]]<-tmpdim
  # }
},myown_vote_record_df_wide_billidascol=myown_vote_record_df_wide_billidascol,
parallelfa_result_n_factor=parallelfa_result_n_factor,reshomalsfile=reshomalsfile,
method=parallel_method,mc.cores=1) %>%
  magrittr::set_names(parallelfa_result_n_factor$need_factorn)
save(res.homals, res.homals.loadings, file = reshomalsfile) #res.homals.accepted_ndimensions
load(file = reshomalsfile, verbose=TRUE)
homals::plot3dstatic(res.homals[[residx]], plot.type = "screeplot")
for (residx in names(res.homals.loadings)) {
  openxlsx::write.xlsx(as.data.frame(res.homals.loadings[[residx]]), "TMP.xlsx")
  readline(paste("now at loadings", residx, "continue?"))
  res.homals[[residx]]$objscores %>%
    as.data.frame() %>%
    data.frame(name=rownames(.), term=substr(residx,1,1), .) %>%
    dplyr::mutate_at(base::setdiff(names(.), "name"), as.character) %>%
    dplyr::mutate_at(base::setdiff(names(.), "name"), as.numeric) %>%
    openxlsx::write.xlsx("TMP.xlsx")
  readline(paste("now at objscores", residx, ", continue?"))
}

# * EFA by gifi homels --------------------------------
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

# * EFA by MCA --------------------------------
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

# 第三部分：立法委員資料與投票資料合併  -------------------------------------------

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
    dplyr::group_by(., votedecision, billid_myown, legislator_party) %>%
      dplyr::summarise(samepartysamepositioncounts=n()) %>%
      arrange(billid_myown, legislator_party, desc(samepartysamepositioncounts), votedecision) %>%
      dplyr::group_by(billid_myown, legislator_party) %>%
      dplyr::summarise(party_pressure=(max(samepartysamepositioncounts)-sum(samepartysamepositioncounts)+max(samepartysamepositioncounts))/sum(samepartysamepositioncounts)) 
  }) %>%
  dplyr::left_join(adminparty)

# 第四部分：合併投票紀錄與法案資料  -------------------------------------------

mergedf_votes_bills_surveyanswer %>%
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
  dplyr::mutate(ansv_and_label=paste0("[",SURVEYANSWERVALUE,"] ",LABEL)) %>%
  dplyr::mutate_at(c("SURVEY","billresult","legislator_party","pp_agendavoting","pp_propose_advanceforagenda","value_on_q_variable","variable_on_q","pp_lawamendment","issue_field1","issue_field2","respondopinion","success_on_bill","ansv_and_label"), as.factor) %>%
  dplyr::select(-url.x,-url.y,-pp_keyword.x,-pp_keyword.y,-billcontent.x,-billcontent.y, -SURVEYANSWERVALUE, -LABEL, -QUESTION) %>%
  dplyr::arrange(term, period, temp_meeting_no, meetingno, billn)

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


