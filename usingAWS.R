# 第Ｏ部份：環境設定 --------------------------------
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
source(file = "shared_functions.R")

gc(verbose=TRUE)

aws_public_ip <- "1.2.3.4"
ssh_private_key_file <- "/mnt/d/Documents/.ssh/MyFirstAWSEC2.pem"
security_group <- "sg-0e24ed4eb6d7bd085"
instanceid <- "i-0ae4bf5434b0e6c6e"
volumeid <- "vol-062ae1728a51e3ac9"
type <- "t2.micro"
port <- "18787"
#rstudio_serverRStudioServer
#ref https://gist.github.com/DavisVaughan/5aac4a2757c0947a499d25d28a8ca89b
# 測試使用AWS  ================================= 
if ({usingaws_cre_ec2_startup<-FALSE; usingaws_cre_ec2_startup}) {
  library(aws.signature)
  library(aws.ec2)
  aws.signature::use_credentials()
  describe_images(image)
}
# 開始平行運算  ================================= 
library(future)
library(future.apply)
###parallel cluster test
#plan(cluster, workers = c("n1", "n2", "n3"))
pid <- Sys.getpid()
awsec2cl <- makeClusterPSOCK(
  ## Public IP number of EC2 instance
  aws_public_ip,
  ## User name (always'ubuntu')
  user = "ubuntu",
  port = port,
  ## Use private SSH key registered with AWS
  rshopts = c("-o", "StrictHostKeyChecking=no",
              "-o", "IdentitiesOnly=yes",
              "-i", ssh_private_key_file),
  ## Set up .libPaths() for the'ubuntu'user and install future package
  rscript_args = c("-e", shQuote("local({
                                 p <- Sys.getenv('R_LIBS_USER')
                                 dir.create(p, recursive = TRUE, showWarnings = FALSE)
                                 .libPaths(p)
                                 })"),
                   ), #"-e", shQuote("install.packages('future');install.packages('future.apply')")
  dryrun = TRUE,
  verbose = TRUE
)
plan(cluster, workers = awsec2cl)
workers <- c("localhost", "localhost") 
parallel::stopCluster(cl)