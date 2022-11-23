#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU

# ZZ final que necesita de UNDERSAMPLING

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

require("lightgbm")

#Parametros del script
PARAM  <- list()
PARAM$exp_col <- "31"
PARAM$experimento  <- paste0("EC",PARAM$exp_col,"-6-results")
PARAM$exp_input  <- paste0("EC",PARAM$exp_col,"-5-underBO")

PARAM$modelos  <- 1
# FIN Parametros del script
semillas = c(539141, 746773, 448883, 190207, 982343)
#ksemilla  <- 936659

#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
vprob_optima  <- c()

fganancia_lgbm_meseta  <- function( probs, datos) 
{
  vlabels  <- get_field(datos, "label")
  
  tbl  <- as.data.table( list( "prob"= probs, 
                               "gan" = ifelse( vlabels==1 , 78000, -2000 ) ) )
  
  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]
  
  gan  <-  tbl[ , max(gan_acum) ]
  
  pos  <- which.max(  tbl[ , gan_acum ] ) 
  vprob_optima  <<- c( vprob_optima, tbl[ pos, prob ] )
  
  rm( tbl )
  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}




base_dir <- "~/buckets/b1/"

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/colectivos/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/colectivos/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#leo la salida de la optimizaciob bayesiana
arch_log  <- paste0( base_dir, "exp/colectivos/", PARAM$exp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
setorder( tb_log, -ganancia )

#leo el nombre del expermento de la Training Strategy
arch_TS  <- paste0( base_dir, "exp/colectivos/", PARAM$exp_input, "/TrainingStrategy.txt" )
TS  <- readLines( arch_TS, warn=FALSE )

#leo el dataset donde voy a entrenar el modelo final
arch_dataset  <- paste0( base_dir, "exp/colectivos/", TS, "/dataset_train_final.csv.gz" )
#arch_validate  <- paste0( base_dir, "exp/colectivos/", TS, "/dataset_validation_final.csv.gz" )

dataset  <- fread( arch_dataset )
#validate  <- fread( arch_validate )


#leo el dataset donde voy a aplicar el modelo final
arch_future  <- paste0( base_dir, "exp/colectivos/", TS, "/dataset_future.csv.gz" )
dfuture <- fread( arch_future )


#defino la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]
#validate[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]


campos_buenos  <- setdiff( colnames(dataset), c( "clase_ternaria", "clase01") )

parametros  <- list( 
  boosting= "gbdt",               #puede ir  dart  , ni pruebe random_forest
  objective= "binary",
  metric= "custom",
  first_metric_only= TRUE,
  boost_from_average= TRUE,
  feature_pre_filter= FALSE,
  force_row_wise= TRUE,           #para que los alumnos no se atemoricen con tantos warning
  verbosity= -100,
  max_depth=  -1,                 # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split= 0.0,         #por ahora, lo dejo fijo
  min_sum_hessian_in_leaf= 0.001, #por ahora, lo dejo fijo
  lambda_l1= 0.0,                 #por ahora, lo dejo fijo
  lambda_l2= 0.0,                 #por ahora, lo dejo fijo
  max_bin= 31,                    #por ahora, lo dejo fijo
  num_iterations= 9999,           #un numero muy grande, lo limita early_stopping_rounds
  
  bagging_fraction= 1.0,          #por ahora, lo dejo fijo
  pos_bagging_fraction= 1.0,      #por ahora, lo dejo fijo
  neg_bagging_fraction= 1.0,      #por ahora, lo dejo fijo
  
  drop_rate=  0.1,                #solo se activa en  dart
  max_drop= 50,                   #solo se activa en  dart
  skip_drop= 0.5,                 #solo se activa en  dart
  
  extra_trees= FALSE,
  num_iterations = 1962,
  learning_rate = 0,00515472,
  num_leaves = 94,
  feature_fraction = 0,300683,
  min_data_in_leaf = 2473
  #seed=  ksemilla
)
#genero un modelo para cada uno de las modelos_qty MEJORES iteraciones de la Bayesian Optimization
for (ksemilla in semillas) {
  parametros$seed <- ksemilla
  arch_modelo  <- paste0( "modelo_" ,
                          sprintf( "%06d", ksemilla ),
                          ".model" )
  
  
  #creo CADA VEZ el dataset de lightgbm
  dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ , campos_buenos, with=FALSE] ),
                          label=   dataset[ , clase01],
                          weight=  dataset[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                          free_raw_data= FALSE
  )
  
  #dvalidate  <- lgb.Dataset( data=  data.matrix( validate[ , campos_buenos, with=FALSE] ),
   #                          label= validate[ , clase01 ],
   #                          weight= validate[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
   #                          free_raw_data= FALSE  )
  
  ganancia  <- parametros$ganancia
  
  #elimino los parametros que no son de lightgbm
  parametros$ganancia     <- NULL
  parametros$iteracion_bayesiana  <- NULL
  
  #genero el modelo entrenando en los datos finales
  set.seed( parametros$seed )
  modelo_final  <- lgb.train(data= dtrain,
                             param=  parametros,
                             #valids= list( valid= dvalidate ),
                             eval = fganancia_lgbm_meseta,
                             verbose= -100 )
  
  #grabo el modelo, achivo .model
  lgb.save( modelo_final,
            file= arch_modelo )
  
  #creo y grabo la importancia de variables
  tb_importancia  <- as.data.table( lgb.importance( modelo_final ) )
  fwrite( tb_importancia,
          file= paste0( "impo_", 
                        sprintf( "%06d", ksemilla ),
                        ".txt" ),
          sep= "\t" )
  
  
  #genero la prediccion, Scoring
  prediccion  <- predict( modelo_final,
                          data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )
  
  tb_prediccion  <- dfuture[  , list( numero_de_cliente, foto_mes ) ]
  tb_prediccion[ , prob := prediccion ]
  
  
  nom_pred  <- paste0( "pred_",
                       sprintf( "%06d", ksemilla ),
                       ".csv"  )
  
  fwrite( tb_prediccion,
          file= nom_pred,
          sep= "\t" )
  
  rm(dtrain,tb_prediccion,tb_importancia,modelo_final)
  
  gc()
}