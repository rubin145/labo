rm(list = ls())
gc(verbose = FALSE)

require("data.table")
require("rpart")
require("rpart.plot")
require("treeClust")
require("ggplot2")

#require("ROCR")
#require("lubridate")
#require("lhs")
require("DiceKriging")
require("mlrMBO")

setwd("/home/user/projects/dmeyf_R")
semillas = c(539141, 746773, 448883, 190207, 982343)

#---- funcs exper

leaves_table <- function(model, train, target, prefix = "") {
  leaves_train_table <- data.table(
    # Devuelve en que hoja cae un caso
    leaves = rpart.predict.leaves(model, train, type = "where"),
    classes = train[, clase_ternaria],
    target = train[, get(target)]
  )
  leaves <- dcast(
    leaves_train_table,
    leaves ~ classes, length,
    value.var = "target")
  leaves <- leaves[
    dcast(
      leaves_train_table,
      leaves ~ target, length,
      value.var = "target"),
    on = .(leaves)]
  leaves[, n := SI + NO]
  leaves[, p := round(SI / n,4)]
  leaves <- leaves[order(-p),]
  leaves[, gan := `BAJA+2` * 78000 - (CONTINUA + `BAJA+1`) * 2000]
  leaves[, ':='(SI = NULL, NO = NULL)]
  setnames(leaves, old = c("BAJA+1", "BAJA+2", "CONTINUA", "n", "p", "gan"),
           new = c(paste0(prefix, "b1"),
                   paste0(prefix, "b2"),
                   paste0(prefix, "cont"),
                   paste0(prefix, "n"),
                   paste0(prefix, "p"),
                   paste0(prefix, "gan")))
  leaves[]
}


modelo_rpart <- function(train, test, cp =  0, ms = 20, mb = 1, md = 10) {
  modelo <- rpart(clase_binaria ~ ., data = train,
                  xval = 0,
                  cp = cp,
                  minsplit = ms,
                  minbucket = mb,
                  maxdepth = md)
  
  test_prediccion <- predict(modelo, test, type = "prob")
  roc_pred <-  ROCR::prediction(test_prediccion[, "SI"],
                                test$clase_binaria,
                                label.ordering = c("NO", "SI"))
  auc_t <-  ROCR::performance(roc_pred, "auc")
  
  unlist(auc_t@y.values)
}


tomar_muestra <- function(datos, resto = 10000) {
  t <- datos$clase_binaria == "SI"
  r <- rep(FALSE, length(datos$clase_binaria))
  r[!t][sample.int(resto, n = (length(t) - sum(t)))] <- TRUE
  t | r
}


experimento_rpart <- function(ds, semillas, cp = 0, ms = 20, mb = 1, md = 10) {
  auc <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70,
                                              list = FALSE)
    train  <-  ds[in_training, ]
    test   <-  ds[-in_training, ]
    train_sample <- tomar_muestra(train)
    r <- modelo_rpart(train[train_sample,], test, 
                      cp = cp, ms = ms, mb = mb, md = md)
    auc <- c(auc, r)
  }
  mean(auc)
}

#---- FE


dataset  <- fread("./datasets/competencia1_2022.csv" )

#clase binaria
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

#unifico visa y master

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
dataset [, grep("^(Master_|Visa_).*", colnames(dataset)):=NULL]

dataset [, uso_estables_pr := rowSums(.SD), .SDcols = c('cprestamos_personales','cprestamos_prendarios','cprestamos_hipotecarios','cplazo_fijo','cinversion1','cinversion2','cseguro_vida','cseguro_auto','cseguro_vivienda','cseguro_accidentes_personales','ccaja_seguridad')]
dataset [,uso_estables_pr_bool := uso_estables_pr > 0 ]
dataset [, uso_estables_tr := rowSums(.SD), .SDcols = c('ctarjeta_debito_transacciones','ctarjeta_visa_transacciones','ctarjeta_master_transacciones','cpayroll_trx','cpayroll2_trx','ctarjeta_master_debitos_automaticos','ctarjeta_visa_debitos_automaticos')]
dataset [,uso_estables_tr_bool := uso_estables_tr > 0 ]

ddevelop  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

set.seed(semillas[1])
in_training <- caret::createDataPartition(dataset$clase_binaria,
                                          p = 0.70, list = FALSE)
dtrain  <-  ddevelop[in_training, ]
dtest   <-  ddevelop[-in_training, ]



## --- BO

obj_fun_md_ms <- function(x) {
  experimento_rpart(ddevelop, semillas
                    , md = x$maxdepth
                    , ms = x$minsplit)
}

obj_fun <- makeSingleObjectiveFunction(
  minimize = FALSE,
  fn = obj_fun_md_ms,
  par.set = makeParamSet(
    makeIntegerParam("maxdepth",  lower = 4L, upper = 20L),
    makeIntegerParam("minsplit",  lower = 1L, upper = 200L)
    # makeNumericParam <- para parámetros continuos
  ),
  # noisy = TRUE,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl, iters = 16L)
ctrl <- setMBOControlInfill(
  ctrl,
  crit = makeMBOInfillCritEI(),
  opt = "focussearch",
  # sacar parámetro opt.focussearch.points en próximas ejecuciones
  opt.focussearch.points = 20
)

lrn <- makeMBOLearner(ctrl, obj_fun)

surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl, )
print(run_md_ms)

#--- TR

modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=         0,
                 cp=          -0.54,#  -0.89
                 minsplit=  89,   # 621
                 minbucket=  278,   # 309
                 maxdepth=     9 )  #  12

pred_testing <- predict(modelo, dtest, type = "prob")
print(sum(
  (pred_testing[, "SI"] >= 0.025) * ifelse(
    # Usamos la clase ternaria para calcular la gan
    dtest$clase_ternaria == "BAJA+2",
    78000,
    -2000) / 0.3
)
)

train_modelo <- leaves_table(modelo, dtrain, "clase_binaria")
print(train_modelo)

# PR

dapply[ VisaMaster_fultimo_cierre_min== 1, VisaMaster_fultimo_cierre_min :=  4 ]
dapply[ VisaMaster_fultimo_cierre_min== 7, VisaMaster_fultimo_cierre_min := 11 ]
dapply[ VisaMaster_fultimo_cierre_min==21, VisaMaster_fultimo_cierre_min := 25 ]
dapply[ VisaMaster_fultimo_cierre_min==14, VisaMaster_fultimo_cierre_min := 18 ]
dapply[ VisaMaster_fultimo_cierre_min==28, VisaMaster_fultimo_cierre_min := 32 ]
dapply[ VisaMaster_fultimo_cierre_min==35, VisaMaster_fultimo_cierre_min := 39 ]
dapply[ VisaMaster_fultimo_cierre_min> 39, VisaMaster_fultimo_cierre_min := VisaMaster_fultimo_cierre_min + 4 ]

dapply[ VisaMaster_fultimo_cierre_max== 1, VisaMaster_fultimo_cierre_max :=  4 ]
dapply[ VisaMaster_fultimo_cierre_max== 7, VisaMaster_fultimo_cierre_max := 11 ]
dapply[ VisaMaster_fultimo_cierre_max==21, VisaMaster_fultimo_cierre_max := 25 ]
dapply[ VisaMaster_fultimo_cierre_max==14, VisaMaster_fultimo_cierre_max := 18 ]
dapply[ VisaMaster_fultimo_cierre_max==28, VisaMaster_fultimo_cierre_max := 32 ]
dapply[ VisaMaster_fultimo_cierre_max==35, VisaMaster_fultimo_cierre_max := 39 ]
dapply[ VisaMaster_fultimo_cierre_max> 39, VisaMaster_fultimo_cierre_max := VisaMaster_fultimo_cierre_max + 4 ]

prediccion  <- predict( object=  modelo,
                        newdata= dapply,
                        type = "prob")

dfinal  <- copy( dapply[ , list(numero_de_cliente) ] )
dfinal[ , prob_SI := prediccion[ , "SI"] ]

setorder( dfinal, -prob_SI)

set.seed(539141)  
dfinal[ , azar := runif( nrow(dapply) ) ]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder( dfinal, -prob_SI, azar )

#dir.create( "./exp/" )

# SUBMIT

submit=6
dir.create(paste0("./exp/KA",submit))

for( corte  in  c( 1703,4000, 7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000 ) )
{
  #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]
  
  
  fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
          file= paste0( "./exp/KA",submit,"/KA",submit,"_",corte, ".csv"),
          sep=  "," )
}