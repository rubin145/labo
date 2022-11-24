rm(list=ls())
library(dplyr)
library(stringr)
library(purrr)
 
#base_path <- '~/buckets/b1/exp/colectivos/'
base_path <- '/home/user/projects/dmeyf_R/exp/c4/' #local
setwd(base_path)
test_name <- '202106'
sep <- ','
df_true <- read.csv(paste0("../../datasets/",test_name,".csv"))

eval <- function(filename,true=NULL,exp=NULL,dir=NULL,test_name=test_name,sep='\t'){
  mes <- as.integer(str_extract(filename,"(?<=_en_)(.*)(?=_prob.csv)"))
  ranks <- list()
  ranks$min <- 7000
  ranks$max <- 14000
  tb_prediccion <- read.csv(filename, sep=sep)
  tb_prediccion <- tb_prediccion %>% mutate(prob = round(prob,4)) #### ojo acá
  df_all <- full_join(tb_prediccion,true,by="numero_de_cliente" ) %>% arrange(desc(prob))
  probs <- df_all[ranks$min:ranks$max,] %>% select(prob) %>% unique() %>% unlist(., use.names=FALSE)
  ganancias <- data.frame(corte=numeric(),ganancia=numeric(),proba=numeric())
  for (proba in probs){
    
    ganancia <- (df_all %>%
                   mutate(Predicted = case_when(prob >= proba ~ 1, TRUE ~ 0 )) %>%
                   mutate(ganancia = case_when(Predicted == 1 & true_class == 1 ~ 78000,
                                               Predicted == 1 & true_class == 0 ~ -2000,
                                               TRUE ~ 0)) %>%
                   summarise(GANANCIA = round(sum(ganancia)/1000000,digits=5)))[1,"GANANCIA"]
    
    corte <- (df_all %>% with(which(prob == proba)) )[1]
    ganancias <- ganancias %>% add_row(corte=corte,ganancia=ganancia,proba=proba)
  }
  modelo = paste0(exp,"_",str_extract(filename,"(?<=pred_)(.*)(?=_en)"))
  ganancias <- ganancias %>% mutate(modelo = modelo, foto_mes= mes)
  
  write.csv(ganancias,paste0(dir,'/ganancias_',modelo,'_en_',test_name , '_prob.csv'), row.names=FALSE)
  #write.csv(ganancias,paste0(dir,'/ganancias_',modelo,'_en_',test_name,'_prob.csv'), row.names=FALSE)
}

for (dir in list.dirs(full.names=TRUE,recursive=FALSE)){
  exp <- substr(dir,5,6)
  modelos <- list.files(path = dir,
                        pattern = paste0("MES_pred.*",test_name,"_prob.csv"), full.names = TRUE) %>%
    map(eval,true=df_true,exp=exp,dir=dir,test_name=test_name,sep=sep)
}
