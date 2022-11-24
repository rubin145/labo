#Necesita para correr en Google Cloud
#  32 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

#Parametros del script
PARAM  <- list()
PARAM$dataset  <- "competenciaFINAL_2022.csv.gz"

#------------------------------------------------------------------------------

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

#------------------------------------------------------------------------------
setwd( "~/buckets/b1/datasets" )
#setwd('/home/user/projects/dmeyf_R/datasets')

#cargo el dataset
dataset  <- fread( PARAM$dataset )

#sintrue <- c(202108,202109)
#dataset[ !(foto_mes %in% sintrue), ]
dataset[ , true_class := ifelse( clase_ternaria == "BAJA+2", 1, 0 )]
cols <- c("numero_de_cliente","foto_mes","true_class")
dataset[, setdiff(names(dataset), cols) := NULL][]

#------------------------------------------------------------------------------
#grabo el dataset
fwrite( dataset,
        file=  "TRUE.csv",
        logical01= TRUE,
        sep= "," )