rm(list=ls())
library(dplyr)
library(stringr)
library(purrr)
 
#base_path <- '~/buckets/b1/exp/colectivos/'
base_path <- '/home/user/projects/dmeyf_R/exp/c4/' #local
setwd(base_path)

test_name <- 'all'

dir <- './EC31-6-results'
filename <- 'exp_colectivos_EC31-6-results_pred_539141.csv'
dirfile <- paste0(dir,'/',filename)

exp <- substr(dir,5,6)
semilla <- str_extract(filename,"(?<=pred_)(.*)(?=.csv)")
modelo <- paste0(exp,"_",semilla)

tb_prediccion <- read.csv(dirfile, sep="\t")

meses_eval <- c(201904,202002,202005,202010,202107)
for (mes in meses_eval){
  pred_mes <- tb_prediccion %>% filter(foto_mes==mes)
  write.csv(pred_mes,paste0(dir,'/MES_pred_',modelo,'_en_',as.character(mes),'_prob.csv'), row.names=FALSE)  
}