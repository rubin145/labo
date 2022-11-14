rm(list=ls())

library(dplyr)

base_path <- "/home/user/projects/dmeyf_R/exp/colectivos/"
setwd(base_path)

todos=data.frame(corte=numeric(), ganancia=numeric(),modelo=character())
for (dir in list.dirs(full.names=TRUE,recursive=FALSE)){
  todos_exp <- list.files(path = dir,
                         pattern = "ganancias.*.csv", full.names = TRUE) %>% 
    lapply(read.csv) %>%
    bind_rows
  todos <- bind_rows(todos,todos_exp)
}

mejores <- todos %>%
  group_by(modelo) %>%
  filter(ganancia == max(ganancia)) %>%
  arrange(modelo) %>% distinct(modelo, .keep_all= TRUE)

write.csv(mejores,paste0('mejores.csv'),row.names=FALSE)

trecemiles <- todos %>%
  group_by(modelo) %>%
  filter(corte == 13000) %>%
  arrange(modelo)

write.csv(trecemiles,paste0('trecemiles.csv'),row.names=FALSE)