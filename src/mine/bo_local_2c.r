rm(list = ls())
gc(verbose = FALSE)

require("data.table")
require("rpart")
require("ggplot2")
require("lightgbm")
require("xgboost")

# Poner la carpeta de la materia de SU computadora local
setwd("/home/user/projects/dmeyf_R")
# Poner sus semillas
semillas = c(539141, 746773, 448883, 190207, 982343)
# 

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/competencia2_2022.csv.gz")

#saco las columnas en las que detecto concept drifting
dataset [, ccajas_otras := NULL]
#dataset [, Visa_madelantodolares := NULL]
#Master_fultimo_cierre
#Visa_fultimo_cierre

#Feature ennginiriirng.
dataset [, VisaMaster_Fvencimiento_min := pmin(Visa_Fvencimiento, Master_Fvencimiento)]
dataset [, VisaMaster_Fvencimiento_max := pmax(Visa_Fvencimiento, Master_Fvencimiento)]

dataset [, VisaMaster_Finiciomora_min := pmin(Visa_Finiciomora, Master_Finiciomora)]
dataset [, VisaMaster_Finiciomora_max := pmax(Visa_Finiciomora, Master_Finiciomora)]

dataset [, VisaMaster_mlimitecompra_min := pmin(Visa_mlimitecompra, Master_mlimitecompra)]
dataset [, VisaMaster_mlimitecompra_max := pmax(Visa_mlimitecompra, Master_mlimitecompra)]

dataset [, VisaMaster_fultimo_cierre_min := pmin(Visa_fultimo_cierre, Master_fultimo_cierre)]
dataset [, VisaMaster_fultimo_cierre_max := pmax(Visa_fultimo_cierre, Master_fultimo_cierre)]

dataset [, VisaMaster_fechaalta_min := pmin(Visa_fechaalta, Master_fechaalta)]
dataset [, VisaMaster_fechaalta_max := pmax(Visa_fechaalta, Master_fechaalta)]

dataset [, VisaMaster_delinquency := rowSums(.SD), .SDcols = c('Visa_delinquency','Master_delinquency')]
dataset [, VisaMaster_mfinanciacion_limite := rowSums(.SD), .SDcols = c('Visa_mfinanciacion_limite','Master_mfinanciacion_limite')]
dataset [, VisaMaster_msaldototal := rowSums(.SD), .SDcols = c('Visa_msaldototal','Master_msaldototal')]
dataset [, VisaMaster_msaldopesos := rowSums(.SD), .SDcols = c('Visa_msaldopesos','Master_msaldopesos')]
dataset [, VisaMaster_msaldodolares := rowSums(.SD), .SDcols = c('Visa_msaldodolares','Master_msaldodolares')]
dataset [, VisaMaster_mconsumospesos := rowSums(.SD), .SDcols = c('Visa_mconsumospesos','Master_mconsumospesos')]
dataset [, VisaMaster_mconsumosdolares := rowSums(.SD), .SDcols = c('Visa_mconsumosdolares','Master_mconsumosdolares')]
dataset [, VisaMaster_mlimitecompra := rowSums(.SD), .SDcols = c('Visa_mlimitecompra','Master_mlimitecompra')]
dataset [, VisaMaster_madelantopesos := rowSums(.SD), .SDcols = c('Visa_madelantopesos','Master_madelantopesos')]
dataset [, VisaMaster_madelantodolares := rowSums(.SD), .SDcols = c('Visa_madelantodolares','Master_madelantodolares')]
dataset [, VisaMaster_mpagado := rowSums(.SD), .SDcols = c('Visa_mpagado','Master_mpagado')]
dataset [, VisaMaster_mpagospesos := rowSums(.SD), .SDcols = c('Visa_mpagospesos','Master_mpagospesos')]
dataset [, VisaMaster_mpagosdolares := rowSums(.SD), .SDcols = c('Visa_mpagosdolares','Master_mpagosdolares')]
dataset [, VisaMaster_mconsumototal := rowSums(.SD), .SDcols = c('Visa_mconsumototal','Master_mconsumototal')]
dataset [, VisaMaster_cconsumos := rowSums(.SD), .SDcols = c('Visa_cconsumos','Master_cconsumos')]
dataset [, VisaMaster_cadelantosefectivo := rowSums(.SD), .SDcols = c('Visa_cadelantosefectivo','Master_cadelantosefectivo')]
dataset [, VisaMaster_mpagominimo := rowSums(.SD), .SDcols = c('Visa_mpagominimo','Master_mpagominimo')]

#con esto saco las columnas Visa_ y Master_ originales.
#dataset [, grep("^(Master_|Visa_).*", colnames(dataset)):=NULL]

dataset [, uso_estables_pr := rowSums(.SD), .SDcols = c('cprestamos_personales','cprestamos_prendarios','cprestamos_hipotecarios','cplazo_fijo','cinversion1','cinversion2','cseguro_vida','cseguro_auto','cseguro_vivienda','cseguro_accidentes_personales','ccaja_seguridad')]
dataset [,uso_estables_pr_bool := uso_estables_pr > 0 ]
dataset [, uso_estables_tr := rowSums(.SD), .SDcols = c('ctarjeta_debito_transacciones','ctarjeta_visa_transacciones','ctarjeta_master_transacciones','cpayroll_trx','cpayroll2_trx','ctarjeta_master_debitos_automaticos','ctarjeta_visa_debitos_automaticos')]
dataset [,uso_estables_tr_bool := uso_estables_tr > 0 ]

marzo <- dataset[foto_mes == 202103]
mayo <- dataset[foto_mes == 202105]
rm(dataset)

# Clase BAJA+1 y BAJA+2 juntas
clase_binaria <- ifelse(marzo$clase_ternaria == "CONTINUA", 0, 1)
clase_real <- marzo$clase_ternaria
marzo$clase_ternaria <- NULL
mayo$clase_ternaria <- NULL


dtrain <- xgb.DMatrix(
  data = data.matrix(marzo),
  label = clase_binaria, missing = NA)

## ---------------------------
## Step 3: XGBoost, ... para generar nuevas variables
## ---------------------------

set.seed(semillas[1])
# param_fe <- list(
#   colsample_bynode = 0.8,
#   learning_rate = 1,
#   max_depth = 7, # <--- IMPORTANTE CAMBIAR
#   num_parallel_tree = 20, # <--- IMPORTANTE CAMBIAR
#   subsample = 0.8,
#   objective = "binary:logistic"
# )

param_fe <- list(
  max_depth = 4,
  eta = 0.1,
  num_parallel_tree = 4,
  objective = "binary:logistic")
nrounds <- 5

###

xgb_model <- xgb.train(params = param_fe, data = dtrain, nrounds = 1)




marzo_mtrx <- xgb.create.features(model = xgb_model, data.matrix(marzo))
mayo <- xgb.create.features(model = xgb_model, data.matrix(mayo))


dtrain_lgb  <- lgb.Dataset(
  data = marzo,
  label = clase_binaria)

mlgb <- lgb.train(
  dtrain_lgb,
  params = list(
    objective = "binary",
    max_bin = 15,
    min_data_in_leaf = 4000,
    learning_rate = 0.05,
    num_iterations = 250 ## <-- aumento las iteraciones
  ),
  verbose = -1)