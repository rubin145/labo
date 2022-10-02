# source( "~/labo/src/lightgbm/z723_lightgbm_binaria_BO.r" )
# Este script esta pensado para correr en Google Cloud
#   8 vCPU
#  32 GB memoria RAM
# 256 GB espacio en disco

# se entrena con POS =  { BAJA+1, BAJA+2 }
# Optimizacion Bayesiana de hiperparametros de  lightgbm, con el metodo TRADICIONAL de los hiperparametros originales de lightgbm
# 5-fold cross validation
# la probabilidad de corte es un hiperparametro

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")

require("lightgbm")
require("xgboost")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

if (".env" %in% list.files(path=".", pattern=NULL, all.files=TRUE,full.names=FALSE)) {
  readRenviron(".env"); local <- as.logical(Sys.getenv("LOCAL")) } else {local <- FALSE}
semillas = c(539141, 746773, 448883, 190207, 982343)

#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
  makeNumericParam("learning_rate",    lower=    0.0005, upper=    0.3),
  makeNumericParam("feature_fraction", lower=    0.4  , upper=    1),
  makeIntegerParam("min_data_in_leaf", lower=    150L   , upper=  8000L),
  makeIntegerParam("num_leaves",       lower=   100L   , upper=  800L),
  makeNumericParam("lambda_l1",        lower=   0.0    , upper=  10),
  makeNumericParam("lambda_l2",        lower=   0.0    , upper=  10),
  makeIntegerParam("envios",           lower= 8000L   , upper= 13000L),
  makeIntegerParam("binaria_tipo",     lower= 1L   , upper= 2L),
  mak
)

#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM  <- list()

PARAM$experimento  <- "002"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$train_test      <- c( 202103,202105 )

PARAM$trainingstrategy$undersampling  <-  1.0   # un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$semilla_azar   <- semillas[1]  #Aqui poner la propia semilla

PARAM$hyperparametertuning$iteraciones <- 500
PARAM$hyperparametertuning$xval_folds  <- 5
PARAM$hyperparametertuning$POS_ganancia  <- 78000
PARAM$hyperparametertuning$NEG_ganancia  <- -2000

PARAM$hyperparametertuning$semilla_azar  <- semillas[2]  #Aqui poner la propia semilla, PUEDE ser distinta a la de trainingstrategy

#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./exp/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )
  
  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )
    
    cat( linea, file=archivo )
  }
  
  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )
  
  cat( linea, file=archivo, append=TRUE )  #grabo al archivo
  
  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#esta funcion calcula internamente la ganancia de la prediccion probs

fganancia_logistic_lightgbm  <- function( probs, datos) 
{
  vpesos   <- get_field(datos, "weight")
  
  #vector de ganancias
  vgan  <- ifelse( vpesos == 1.0000002, PARAM$hyperparametertuning$POS_ganancia, 
                   ifelse( vpesos == 1.0000001, PARAM$hyperparametertuning$NEG_ganancia , # a ver qué pasa cambiando acá
                           PARAM$hyperparametertuning$NEG_ganancia / PARAM$trainingstrategy$undersampling ) )
  
  tbl  <- as.data.table( list( "vprobs" = probs, "vgan" = vgan ) )
  setorder( tbl,  -vprobs )
  ganancia <- tbl[ 1:GLOBAL_envios, sum( vgan ) ]
  
  return( list( "name"= "ganancia", 
                "value"=  ganancia,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  gc()  #libero memoria
  
  #llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1
  
  #para usar en fganancia_logistic_lightgbm 
  GLOBAL_envios <<- as.integer(x$envios/PARAM$hyperparametertuning$xval_folds)   #asigno la variable global
  
  kfolds  <- PARAM$hyperparametertuning$xval_folds   # cantidad de folds para cross validation
  
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
  
  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )
  
  param_completo  <- c( param_basicos, param_variable, x )
  
  set.seed( PARAM$hyperparametertuning$semilla_azar )
  modelocv  <- lgb.cv( data= dtrain,
                       eval= fganancia_logistic_lightgbm,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
  )
  
  #obtengo la ganancia
  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]
  
  ganancia_normalizada  <-  ganancia* kfolds     #normailizo la ganancia
  
  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL     #elimino de la lista el componente  "early_stopping_rounds"
  
  #Voy registrando la importancia de variables
  if( ganancia_normalizada >  GLOBAL_gananciamax )
  {
    GLOBAL_gananciamax  <<- ganancia_normalizada
    modelo  <- lgb.train( data= dtrain,
                          param= param_completo,
                          verbose= -100
    )
    
    tb_importancia  <- as.data.table( lgb.importance(modelo ) )
    archivo_importancia  <- paste0( "impo_", GLOBAL_iteracion,".txt")
    fwrite( tb_importancia,
            file= archivo_importancia,
            sep= "\t" )
  }
  
  
  #el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra
  
  #logueo 
  xx  <- param_completo
  xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear( xx, arch= klog )
  
  return( ganancia_normalizada )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

#Establezco el Working Directory (según local o cloud)
if (local) {setwd("/home/user/projects/dmeyf_R")
} else { setwd("~/buckets/b1/") 
}

#cargo el dataset donde voy a entrenar el modelo
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


#### 

#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd( paste0( "./exp/", PARAM$experimento, "/") )   #Establezco el Working Directory DEL EXPERIMENTO

#en estos archivos quedan los resultados
kbayesiana  <- paste0( PARAM$experimento, ".RDATA" )
klog        <- paste0( PARAM$experimento, ".txt" )


GLOBAL_iteracion  <- 0   #inicializo la variable global
GLOBAL_gananciamax <- -1 #inicializo la variable global

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog )
  GLOBAL_iteracion  <- nrow( tabla_log )
  GLOBAL_gananciamax  <- tabla_log[ , max( ganancia ) ]
}



#paso la clase a binaria que tome valores {0,1}  enteros
if (x$binaria_tipo == 1){
  dataset[ foto_mes %in% PARAM$input$training, clase01 := ifelse( clase_ternaria=="CONTINUA", 0L, 1L) ]
  } else {
    dataset[ foto_mes %in% PARAM$input$training, clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]
}
marzo <- dataset[foto_mes %in% PARAM$input$training ]
clase01 <- marzo$clase_01
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

campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "azar", "training" ) )



###

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

#Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
  fn=       funcion_optimizar, #la funcion que voy a maximizar
  minimize= FALSE,   #estoy Maximizando la ganancia
  noisy=    TRUE,
  par.set=  hs,     #definido al comienzo del programa
  has.simple.signature = FALSE   #paso los parametros en una lista
)

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= PARAM$hyperparametertuning$iteraciones )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( kbayesiana ) ) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}


quit( save="no" )