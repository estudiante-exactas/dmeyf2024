## Correr este script con la base de datos provista para la competencia 2 para crear columna clase_ternaria en el dataset a subir a Google Cloud

# limpio la memoria
rm(list = ls()) # remove all objects
gc() 

#install.packages("data.table")
#install.packages("lightgbm")

require("data.table")
require("lightgbm")

setwd("C:/Mis cosas/Maestria Data Mining/DMEyF")

 dataset <- fread("./datasets/competencia_02_crudo.csv")
 
# ###########  Creacion clase ternaria

library(dplyr)
library(tidyr)
library(lubridate)

# convierto el periodo a tipo date y genero los 2 periodos subsiguientes para cada fila

dataset$periodo0 <- as.Date(paste0(dataset$foto_mes,"01"),"%Y%m%d")

dataset$periodo1 <- dataset$periodo0 %m+% months(1)
dataset$periodo2 <- dataset$periodo1 %m+% months(1)

# concateno el numero del cliente con los 3 periodos relevantes

dataset$cliente_periodo0 <- paste(dataset$numero_de_cliente, dataset$periodo0, sep = " - ")
dataset$cliente_periodo1 <- paste(dataset$numero_de_cliente, dataset$periodo1, sep = " - ")
dataset$cliente_periodo2 <- paste(dataset$numero_de_cliente, dataset$periodo2, sep = " - ")

# para el caso general, asigno baja+1 y baja+2 si la combinacion (id,periodo) no esta en la columna original

dataset <- dataset %>% mutate(clase_ternaria = case_when(
   !(cliente_periodo1 %in% dataset$cliente_periodo0) ~ "BAJA+1",
   !(cliente_periodo2 %in% dataset$cliente_periodo0) ~ "BAJA+2",
   .default = "CONTINUA"
 ))

# corrijo las asignaciones del paso anterior para los ultimos 2 meses

ultimo_periodo <- max(dataset$periodo0)
anteultimo_periodo <- ultimo_periodo %m-% months(1)

 dataset <- dataset %>% mutate(clase_ternaria = case_when(
   (periodo0 == anteultimo_periodo) & (clase_ternaria != "BAJA+1") ~ NA,
   periodo0 == ultimo_periodo ~ NA,
   .default = clase_ternaria
 ))

 # totales para cada periodo

 dataset %>%
    group_by(periodo0, clase_ternaria) %>%
    summarize(count = n()) %>%
    arrange(periodo0, clase_ternaria) %>%
    pivot_wider(names_from = clase_ternaria, values_from = count)

 # elimino columnas auxiliares

 dataset <- dataset %>% select(-c(periodo0:cliente_periodo2))

 cantidad_nulos <- sum(is.na(dataset$clase_ternaria))
 dataset_nulos <- dataset[is.na(dataset$clase_ternaria), ] # son los foto_mes = 202107 y 202108

 write.csv(dataset, "./datasets/prueba_competencia_02_R.csv") # creo archivo

###################################################################################

dataset <- fread("./datasets/competencia_02_R.csv")

