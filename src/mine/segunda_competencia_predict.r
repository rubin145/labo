rm( list=ls() )  #remove all objects

require("data.table")
require("rlist")

require("lightgbm")
require("xgboost")


if (".env" %in% list.files(path=".", pattern=NULL, all.files=TRUE,full.names=FALSE)) {
  readRenviron(".env"); local <- as.logical(Sys.getenv("LOCAL")) } else {local <- FALSE}
semillas = c(539141, 746773, 448883, 190207, 982343)

PARAM  <- list()
PARAM$experimento  <- "001"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$train_test      <- c( 202103,202105 )
PARAM$input$future        <- 202105
PARAM$hyperparametertuning$xval_folds  <- 5
PARAM$hyperparametertuning$POS_ganancia  <- 78000
PARAM$hyperparametertuning$NEG_ganancia  <- -2000
PARAM$trainingstrategy$undersampling  <-  1.0   # un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$semilla_azar   <- semillas[1]  #Aqui poner la propia semilla
PARAM$hyperparametertuning$semilla_azar  <- semillas[2]  #Aqui poner la propia semilla, PUEDE ser distinta a la de trainingstrategy


fganancia_logistic_lightgbm  <- function( probs, datos) 
{
  vpesos   <- get_field(datos, "weight")
  
  #vector de ganancias
  vgan  <- ifelse( vpesos == 1.0000002, PARAM$hyperparametertuning$POS_ganancia, 
                   ifelse( vpesos == 1.0000001, PARAM$hyperparametertuning$POS_ganancia *0.6 , # a ver qué pasa cambiando acá
                           PARAM$hyperparametertuning$NEG_ganancia / PARAM$trainingstrategy$undersampling ) )
  
  tbl  <- as.data.table( list( "vprobs" = probs, "vgan" = vgan ) )
  setorder( tbl,  -vprobs )
  ganancia <- tbl[ 1:GLOBAL_envios, sum( vgan ) ]
  
  return( list( "name"= "ganancia", 
                "value"=  ganancia,
                "higher_better"= TRUE ) )
}

#Establezco el Working Directory (según local o cloud)
if (local) {setwd("/home/user/projects/dmeyf_R")
} else { setwd("~/buckets/b1/") 
}



dataset  <- fread( PARAM$input$dataset )
dataset <- dataset [foto_mes %in% PARAM$input$train_test]




### agrego FE ###

#saco las columnas en las que detecto concept drifting
dataset [, ccajas_otras := NULL]
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

dataset[ foto_mes %in% PARAM$input$training, clase01 := ifelse( clase_ternaria=="CONTINUA", 0L, 1L) ]
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "azar", "training" ) )

marzo <- dataset[foto_mes %in% PARAM$input$training ]
clase01 <- ifelse(marzo$clase_ternaria == "CONTINUA", 0, 1)
marzo [, clase01 := NULL]
clase01_total <- dataset [,clase01]
dataset [, clase01 := NULL]

dtrain_xgb <- xgb.DMatrix(
  data = data.matrix( marzo ),
  label = clase01, missing = NA)

set.seed(semillas[1])
param_fe <- list(
  max_depth = 4,
  eta = 0.1,
  num_parallel_tree = 4,
  objective = "binary:logistic")
nrounds <- 5

xgb_model <- xgb.train(params = param_fe, data = dtrain_xgb, nrounds = 1)

dataset <- as.data.table(as.matrix( xgb.create.features(model = xgb_model, data.matrix(dataset))  ))
dataset [, clase01 := clase01_total]

rm(list = c("marzo","xgb_model","clase01","clase01_total","dtrain_xgb"))





set.seed( PARAM$trainingstrategy$semilla_azar )
dataset[  , azar := runif( nrow( dataset ) ) ]
dataset[  , training := 0L ]
dataset[ foto_mes %in% PARAM$input$training & 
           ( azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c( "BAJA+1", "BAJA+2" ) ),
         training := 1L ]

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ training == 1L, campos_buenos, with=FALSE]),
                        label= dataset[ training == 1L, clase01 ],
                        weight=  dataset[ training == 1L, ifelse( clase_ternaria=="BAJA+2", 1.0000002, ifelse( clase_ternaria=="BAJA+1",  1.0000001, 1.0) )],
                        free_raw_data= FALSE  )

###entrenar con los hiperparámetros encontrados
envios= 8000
GLOBAL_envios <<- as.integer(envios/PARAM$hyperparametertuning$xval_folds)   #asigno la variable global

param_basicos  <- list( objective= "binary",
                        metric= "custom",
                        first_metric_only= TRUE,
                        boost_from_average= TRUE,
                        feature_pre_filter= FALSE,
                        verbosity= -100,
                        max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                        min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                        lambda_l1= 0.0,         #por ahora, lo dejo fijo
                        lambda_l2= 0.0,         #por ahora, lo dejo fijo
                        max_bin= 31,            #por ahora, lo dejo fijo
                        num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                        force_row_wise= TRUE,   #para que los alumnos no se atemoricen con tantos warning
                        seed= PARAM$hyperparametertuning$semilla_azar
)

param_optimizados <- list( learning_rate= 0.1939022,
                           feature_fraction= 0.8829033,
                           min_data_in_leaf= 3558,
                           num_leaves= 149
)

#el parametro discolo, que depende de otro
param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/param_optimizados$learning_rate) )

param_completo  <- c( param_basicos, param_variable, param_optimizados )

set.seed( PARAM$hyperparametertuning$semilla_azar )
modelo  <- lgb.train( data= dtrain,
                     eval= fganancia_logistic_lightgbm,
                     param= param_completo
                     #verbose= -100
)

dapply <- data.matrix(dataset [foto_mes == PARAM$input$future ] )


preds <- predict(modelocv, newdata=dpredict, type="response")


prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := prediccion ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 8000, 13000, by=500 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]
  
  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  PARAM$experimento, "_", envios, ".csv" ),
          sep= "," )
}

#--------------------------------------

quit( save= "no" )
#



