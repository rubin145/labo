rm( list=ls() )  #remove all objects
gc() 

require("data.table")

if (".env" %in% list.files(path=".", pattern=NULL, all.files=TRUE,full.names=FALSE)) {
  readRenviron(".env"); local <- as.logical(Sys.getenv("LOCAL")) } else {local <- FALSE}
semillas = c(539141, 746773, 448883, 190207, 982343)


PARAM  <- list()
PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"



if (local) {
  setwd("/home/user/projects/dmeyf_R") } else
    { setwd("~/buckets/b1/") 
}


dataset  <- fread( PARAM$input$dataset )

dataset_04 <- dataset [foto_mes %in% c(202104)]
dataset <- dataset [foto_mes %in% c(202101,202102,202103)]

table(dataset$foto_mes, dataset$clase_ternaria)

table(dataset_04$clase_ternaria)
