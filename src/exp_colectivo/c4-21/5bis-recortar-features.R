rm(list=ls())
require(data.table)
time
#base_path <- '~/buckets/b1/exp/colectivos/'
base_path <- '/home/user/projects/dmeyf_R/exp/colectivos' #local
setwd(base_path)
PARAM <- list()
PARAM$TS <- 2
PARAM$exp <- 1
PARAM$topN <- 10
PARAM$val <- FALSE

#--------------------------------------
with_tz(Sys.time(),"America/Buenos_Aires")

dir_in <- paste0('./EC',as.character(PARAM$TS),as.character(PARAM$exp),'-4-train_strategy')
dir_out <-paste0('./EC',as.character(PARAM$TS+3),as.character(PARAM$exp),'-4-train_strategy')

importance <- fread('importance.csv')
impo <- importance[1:PARAM$topN,Feature]
target <- c("clase_ternaria", "clase01")
cols <- c(impo,target)   

cut_features <- function(dir_in,dir_out,cols,filename){
  dataset <- fread(paste0(dir_in,'/',filename))
  
  fwrite( dataset[,colnames(dataset) %in% cols, with=FALSE],
          file= paste0(dir_out,'/',filename),
          logical01= TRUE,
          sep= "," )
}

cut_features(dir_in=dir_in,dir_out=dir_out,cols=cols,filename='dataset_train_final.csv.gz')
cut_features(dir_in=dir_in,dir_out=dir_out,cols=cols,filename='dataset_future.csv.gz')

if (PARAM$val == TRUE){
  cut_features(dir_in=dir_in,dir_out=dir_out,cols=cols,filename='dataset_validation_final.csv.gz')
}

with_tz(Sys.time(),"America/Buenos_Aires")