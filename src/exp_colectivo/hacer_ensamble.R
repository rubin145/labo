rm(list=ls())
library(dplyr)
library(stringr)
library(purrr)
 
#base_path <- '~/buckets/b1/exp/colectivos/'
base_path <- '/home/user/projects/dmeyf_R/exp/colectivosBIS/' #local
setwd(base_path)

preds <- data.frame(numero_de_cliente=numeric(),foto_mes=numeric())
for (dir in list.dirs(full.names=TRUE,recursive=FALSE)){
  exp <- substr(dir,5,6)
  for (file in list.files(path = dir, pattern = "pred.*.csv", full.names = TRUE)){
    model <- str_extract(file,"(?<=pred_)(.*)(?=.csv)")
    exp_model <- paste0(exp,'_',model)
    pred <- read.csv(file,sep="\t") %>% select(numero_de_cliente,foto_mes,prob) %>%
      setNames(c('numero_de_cliente','foto_mes',exp_model))
    preds <- full_join(preds,pred,by=c('numero_de_cliente','foto_mes'))
    rm(pred,model,exp_model)
  }
  rm(exp)
}
i <- length(colnames(preds))
preds$prob <- rowMeans(preds[, 3:i])
preds <- preds %>% select(numero_de_cliente,foto_mes,prob)
nmbr <- sample(1:1000, 1, replace=FALSE)
filename <- paste0('pred_ensamble_',as.character(nmbr),'.csv')
write.csv(preds,filename,row.names=FALSE)
