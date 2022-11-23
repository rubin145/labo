rm(list=ls())
library(dplyr)
library(stringr)
library(purrr)
 
#base_path <- '~/buckets/b1/exp/colectivos/'
base_path <- '/home/user/projects/dmeyf_R/exp/colectivosBIS/' #local
setwd(base_path)
df_true <- read.csv("../../datasets/202107.csv")
test_name <- '202107'

eval <- function(filename,true=NULL,exp=NULL,dir=NULL,test_name=NULL){
  cortes <- seq(from=7000, to= 15000, by= 50)
  tb_prediccion <- read.csv(filename, sep="\t")
  df_all <- full_join(tb_prediccion,true,by="numero_de_cliente" ) %>% arrange(desc(prob))
  
  ganancias <- data.frame(corte=numeric(),ganancia=numeric(),proba=numeric())
  for (corte in cortes){
    
    ganancia <- (df_all %>%
                   mutate(Predicted = case_when(row_number() <= corte ~ 1, TRUE ~ 0 )) %>%
                   mutate(ganancia = case_when(Predicted == 1 & true_class == 1 ~ 78000,
                                               Predicted == 1 & true_class == 0 ~ -2000,
                                               TRUE ~ 0)) %>%
                   summarise(GANANCIA = round(sum(ganancia)/1000000,digits=5)))[1,"GANANCIA"]
    
    proba <- df_all[corte,'prob']
    ganancias <- ganancias %>% add_row(corte=corte,ganancia=ganancia,proba=proba)
  }
  modelo = paste0(exp,"_",str_extract(filename,"(?<=pred_)(.*)(?=.csv)"))
  ganancias <- ganancias %>% mutate(modelo = modelo, test= test_name)
  
  write.csv(ganancias,paste0(dir,'/ganancias_',modelo,'_en_',test_name,'.csv'), row.names=FALSE)
}

for (dir in list.dirs(full.names=TRUE,recursive=FALSE)){
  exp <- substr(dir,5,6)
  modelos <- list.files(path = dir,
                        pattern = "pred.*.csv", full.names = TRUE) %>%
    map(eval,true=df_true,exp=exp,dir=dir,test_name=test_name)
}