rm(list=ls())
library(dplyr)
library(stringr)
library(purrr)
 
#base_path <- '~/buckets/b1/exp/colectivos/'
base_path <- '/home/user/projects/dmeyf_R/datasets/' #local
setwd(base_path)
df_true <- read.csv("TRUE.csv")

meses_eval <- c(201904,202002,202005,202010,202107)
for (mes in meses_eval){
  true_mes <- df_true %>% filter(foto_mes==mes)
  write.csv(true_mes,paste0(as.character(mes),'.csv'), row.names=FALSE)  
}