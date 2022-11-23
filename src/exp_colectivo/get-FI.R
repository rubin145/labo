rm(list=ls())

library(dplyr)
library(lightgbm)
library(stringr)

base_path <- "/home/user/projects/dmeyf_R/exp/colectivos/"
setwd(base_path)

model <- lgb.load('/home/user/projects/dmeyf_R/exp/colectivos/EC12-6-results--incompleto/exp_colectivos_EC12-6-results_modelo_01_058.model')

model_string <- model$save_model_to_string(NULL) # saves best iteration
best_model <- lgb.load(model_str = model_string)


importance = lgb.importance(best_model,percentage=FALSE)

parametros <- list()
parametros$galo <- NULL


extract_params <- function(filename){
  param_names <- c('num_iterations','learning_rate','num_leaves','feature_fraction','min_data_in_leaf')
  model <- lgb.load(filename)
  parameters <- model$save_model_to_string(NULL) %>%
    str_replace_all("[\r\n]" , "") %>% 
    str_extract("(?<=parameters)(.*)(?=end of parameters)")
  rm(model)
  params = list()
  exp <- str_extract(filename,"(?<=EC)(.*?)(?=-6-results)")
  rank_it <- str_extract(filename,"(?<=modelo_)(.*)(?=.model)")
  params$modelo <- paste0(exp,'_',rank_it)
  rm(exp,rank_it)
  for (param in param_names){
    params[param] <- as.double(str_extract(parameters,paste0("(?<=",param,": )(.*?)(?=])")))
  }
  return(as.data.frame(params))
}



write.csv 

modelos_todos=data.frame(
  modelo=character(),
  num_iterations=numeric(),
  learning_rate=numeric(),
  num_leaves=numeric(),
  feature_fraction=numeric(),
  min_data_in_leaf=numeric()
)

for (dir in list.dirs(full.names=TRUE,recursive=FALSE)){
  modelos <- list.files(path = dir,
                          pattern = "*model", full.names = TRUE) %>% 
    lapply(extract_params) %>%
    bind_rows
  modelos_todos <- bind_rows(modelos_todos,modelos) %>% arrange(modelo)
}

write.csv(modelos_todos,paste0('modelos.csv'),row.names=FALSE)