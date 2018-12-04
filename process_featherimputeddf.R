t_sessioninfo<-sessionInfo()
t_sessioninfo_running<-gsub(" ","",t_sessioninfo$running)
t_sessioninfo_running<-gsub("[>=()]","",t_sessioninfo_running)
filespath<-switch(
  t_sessioninfo_running,
  Ubuntu16.04.4LTS="/mnt/e/Software/scripts/R/",
  Windows7x64build7601ServicePack1="C:\\Users\\r03a21033\\DOWNLOADS\\",
  Windows10x64build16299 = "E:\\Software\\scripts\\R\\",
  Windows8x64build9200 = "E:\\Software\\scripts\\R\\"
)
#filespath <- "E:\\Software\\scripts\\R\\"
#filespath <- "/mnt/e/Software/scripts/R/"
source(file = paste(filespath, "shared_functions.R", sep = ""))
dataset_file_directory <- switch(
  t_sessioninfo_running,
  Windows7x64build7601ServicePack1="C:\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\",
  Windows8x64build9200 = "D:\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\",
  Windows10x64build16299 = "D:\\OneDrive\\OnedriveDocuments\\NTU\\Work\\thesis\\dataset(2004-2016)\\",
  Ubuntu16.04.4LTS="/mnt/d/OneDrive/OnedriveDocuments/NTU/Work/thesis/dataset(2004-2016)/"
)
ntuspace_file_directory <- switch(
  t_sessioninfo_running,
  Windows7x64build7601ServicePack1="C:\\NTUSpace\\",
  Windows8x64build9200 = "D:\\NTUSpace\\",
  Windows10x64build16299 = "D:\\NTUSpace\\",
  Ubuntu16.04.4LTS="/mnt/d/NTUSpace/"
)
#選舉資料
overall_elec_dist_types<-c('district','ab_m','ab_plain','partylist')
supplement_election_termseven<-c('supp2009miaoli1','supp2009nantou1','supp2009yunlin2','supp2009taipei6','supp2010taichungs3','supp2010hualian','supp2010taoyuan2','supp2010taoyuan3','supp2010hsinchus','supp2010chiayi2','supp2010taitung','supp2011tainan4','supp2011kaoshiung4')
terms<-c(5,6,7,8,9)
gc(verbose=TRUE)
############################################################
#library(feather)
featherpath<-paste0(filespath,"vote_record",slash,"imputedsurvey",slash,"survey_data_MM_NN.feather")
featherfiles<-stri_replace_first(featherpath,0:4,regex="NN") %>%
  lapply(X=1:4, function(replacement, str, regex) stri_replace_all(str=str, replacement=replacement, regex=regex), regex='MM', str=.)
dflist <- lapply(X=featherfiles,FUN=function(X) lapply(X=X,FUN=feather::read_feather))
load(paste0(dataset_file_directory,"rdata",slash,"all_survey_combined.RData"))
load(paste0(dataset_file_directory,"rdata",slash,"survey_data_labels.RData"))

list_of_dfcolname<-lapply(survey_data,names)
list_of_dfcoltype<-lapply(survey_data,sapply,class)
list_of_dfcollevel<-lapply(survey_data,sapply,levels)
list_of_dfid<-lapply(survey_data,function(X) {
  X<-getElement(X,name="id")
  })
list_of_dflabel<-survey_data_labels
#
reduce_dummy_variable<-function(X,nc,tc,lc) {
  #mapply(function(nc,tc,lc) {
  #},nc=nc,tc=tc,lc=lc)#,nc=n,tc=t,lc=l
  if (length(lc)>=2) { ##找出至少有兩個類別的類別變項
    #message(lc)
    #message("-----------")
    varname<-paste0(nc,"_",lc) ##合成出被dummy的變數
    common_varname<-dplyr::intersect(names(X),varname)
    columnnameprefix<-paste0(nc,"_")
    if (length(common_varname)>0) { ##有些變項在Google Colab填補時為了省時間忽略了
      X[,nc] <- common_varname[max.col(X[,common_varname])] %>%
        stri_replace_all(replacement="",regex=columnnameprefix) #%>%
        #as.factor()
      X[,common_varname]<-list(NULL)
      #type <- names(cars[14:18])[max.col(cars[14:18])] 
    }
    varname<-common_varname<-NULL
  } else {
    varname<-common_varname<-NULL
  }
  return(X)
}

recode_according_to_list<-function(X,list) {
  if (is.element(X,set=names(list))) {
    unlist(list[X])
  } else {
    X
  }
}


dummyremoved_imputed_survey_data<-mapply(function(n,t,l,dummieddf,id,labels,survey_data_for_loop) {
  #各自在不同問卷裡開始loop，共四個問卷,mapply may execute 4 times
  #以下在不同填補值問卷檔裡面loop
  #dummieddf<-list(dummieddf[[1]])
  #View(l)
  #message(class(l))
  dummieddf_test<-lapply(dummieddf,function(X,n,t,l,id,labels,survey_data) {
    X<-as.data.frame(X)
    for (i in 1:length(n)) {
      nc<-n[i]
      tc<-t[i]
      lc<-getElement(l,nc)
      exactlabel<-getElement(labels,nc)
      #message(i,": levels of ", nc," are ",length(lc)," and its contents are ", lc)
      #message(i,": label of ", nc, " is ",exactlabel," and name of labels are ",names(exactlabel))
      X<-reduce_dummy_variable(X,nc=nc,tc=tc,lc=lc) #把dummy variable合併
      #message("done reducing ",nc," dummy variable")
      if ((nc %in% names(X)) & (!identical(exactlabel,NULL)) ) {
        newlist_label_as_value<-split(unname(exactlabel),names(exactlabel)) %>%
          lapply(`[[`,1) ##might appear unexpected result due to duplicated value;e.g. 私立立人高中,市立中山高中 appears not only one times and makes trouble,so reduce them
        #some variable contains ordinary numerical data and categorical data(survey design, e.g. missing value), so indirectly make them converting
        #X[,nc]<-unlist(newlist_label_as_value[X[,nc]]) #this would make trouble due to reasons above
        X[,nc]<-sapply(pull(X,var=nc),recode_according_to_list,list=newlist_label_as_value, simplify = TRUE) #%>%
          #sapply(`[[`,1)
        #message("done recoding ",nc," values")
        asfun<-match.fun(paste0("as.",tc))
        X[,nc]<-asfun(X[,nc])
        #message("done setting ",nc," class")
        if (tc=="factor") {
          levels(X[,nc])<-newlist_label_as_value
        }
        #message("done setting ",nc," level")
      }
    }
    #v3 21 22 v4 myown_dad_ethgroup 24 wrong
    #message("id are ",id)
    #message("-------")
    X_columnnames<-colnames(X)
    rest_survey_data_columns<-survey_data_for_loop[,c(setdiff(names(survey_data_for_loop),X_columnnames))]
    X$id<-id
    #message("dim of X is ",dim(X)," and dim of rest_survey_data is ",dim(rest_survey_data))
    X<-X[,c("id",X_columnnames)] %>%
      left_join(rest_survey_data_columns,by=c("id"))
    X
  },n=n,t=t,l=l,id=id,labels=labels,survey_data=survey_data)
  dummieddf_test
  #message("length of dummieddf are ",length(dummieddf))
  #message("-------")
  #return(dummieddf)
  },n=list_of_dfcolname,
  t=list_of_dfcoltype,
  l=list_of_dfcollevel,
  dummieddf=dflist,
  id=list_of_dfid,
  labels=list_of_dflabel,
  survey_data_for_loop=survey_data,
  SIMPLIFY=FALSE)
save(dummyremoved_imputed_survey_data,file=paste0(dataset_file_directory,"rdata",slash,"dummyremoved_imputed_survey_data.RData"))
#lapply(dflist,)

#for testing purpose: after executing codes below, testing may begin from where out of the iteration lapply
n=(list_of_dfcolname[[1]])
t=(list_of_dfcoltype[[1]])
l=(list_of_dfcollevel[[1]])
dummieddf=(dflist[[1]])
id=list_of_dfid[[1]]
labels<-list_of_dflabel[[1]]
survey_data_for_loop<-survey_data[[1]]
X<-dummieddf[[1]]
