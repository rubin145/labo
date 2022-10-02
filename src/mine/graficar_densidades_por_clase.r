#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")


#------------------------------------------------------------------------------
setwd("/home/user/projects/dmeyf_R")

#graficar_campo  <- function( campo, campo_clase, valores_clase )
graficar_campo  <- function( campo)

{

  #quito de grafico las colas del 5% de las densidades
  qA  <- quantile(  dataset[ clase_ternaria=='BAJA+1' , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qB  <- quantile(  dataset[ clase_ternaria=='BAJA+2' , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )

  xxmin  <- pmin( qA[[1]], qB[[1]] )
  xxmax  <- pmax( qA[[2]], qB[[2]] )

  densidad_A  <- density( dataset[ clase_ternaria=='BAJA+1', get(campo) ],
                          kernel="gaussian", na.rm=TRUE )

  densidad_B  <- density( dataset[ clase_ternaria=='BAJA+2' , get(campo) ],
                          kernel="gaussian", na.rm=TRUE )

  plot( densidad_A,
        col="blue",
        xlim= c( xxmin, xxmax ),
        ylim= c( 0, pmax( max(densidad_A$y), max(densidad_B$y) ) ),
        #main= paste0( campo, ",   ", " in ",  paste(,collapse=","))
        main= paste0( campo)
  )

  lines(densidad_B, col="red", lty=2)

  legend(  "topright",
           legend=c("BAJA+1", "BAJA+2"),
           col=c("blue", "red"), lty=c(1,2))

}
#------------------------------------------------------------------------------
#Aqui comienza el programa
#setwd("/buckets/b1")

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/competencia2_2022.csv.gz")

dataset  <- dataset[  foto_mes %in% c( 202101, 202102, 202103 ) ]

montos <- grep("^c.*",colnames(dataset))


cols <- c()
for (col in montos){
  variable = colnames(dataset)[col]
  cols <- append(cols,variable)
}
#dataset[ , (cols) := lapply(.SD, function(x) x^2), .SDcols = cols]
dataset[ , (cols) := lapply(.SD, log), .SDcols = cols]

#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202103,
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]


# Entreno el modelo
# utilizo los mejores hiperparametros encontrados en una Bayesian Optimizationcon 5-fold Cross Validation
modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dataset[ foto_mes==202103 ],  #los datos donde voy a entrenar
                 xval=         0,
                 cp=           -0.69,
                 minsplit=    870,
                 minbucket=     9,
                 maxdepth=      9)


campos_modelo  <- names( modelo$variable.importance )
campos_buenos  <- c( campos_modelo,  setdiff( colnames(dataset), campos_modelo ) )
campos_buenos  <-  setdiff(  campos_buenos,  c( "foto_mes","clase_ternaria","clase_binaria" ) )


dir.create( "./exp/",  showWarnings = FALSE )
dir.create( "./exp/clase_contra_clase/", showWarnings = FALSE )
setwd("./exp/clase_contra_clase/")



pdf("densidades_clase_contra_clase_exponencial.pdf")

for( campo in  campos_buenos )
{
  cat( campo, "  " )

  #graficar_campo( campo, "clase_ternaria", c( "BAJA+1", "BAJA+2", "CONTINUA" ) )
  #graficar_campo( campo, "clase_ternaria", c( "BAJA+1", "BAJA+2" ) )
  #graficar_campo( campo, "clase_ternaria", c( "BAJA+2" ) )
  #graficar_campo( campo, "clase_ternaria", c( "BAJA+1" ) )
  #graficar_campo( campo, "clase_ternaria", c( "CONTINUA" ) )
  graficar_campo(campo)
}

dev.off()