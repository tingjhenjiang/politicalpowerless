# 第Ｏ部份：環境設定 --------------------------------
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model)
source(file = "shared_functions.R")

gc(verbose=TRUE)

# 測試使用AWS  ================================= 

library(future)
###parallel cluster test
#plan(cluster, workers = c("n1", "n2", "n3"))
pid <- Sys.getpid()
workers <- c("localhost", "localhost")
cl <- makeClusterPSOCK(workers, dryrun = TRUE)
plan(cluster, workers = cl)
a %<-% {
  pid <- Sys.getpid()
  cat("Future 'a' ...\n")
  3.14
}
c %<-% {
  cat("Future 'c' ...\n")
  2 * a
}
aws_public_ip <- "1.2.3.4"
ssh_private_key_file <- "/mnt/d/Documents/.ssh/MyFirstAWSEC2.pem"
cl <- makeClusterPSOCK(
  ## Public IP number of EC2 instance
  aws_public_ip,
  ## User name (always'ubuntu')
  user = "ubuntu",
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
                   "-e", shQuote("install.packages('future')")),
  dryrun = TRUE
)
parallel::stopCluster(cl)