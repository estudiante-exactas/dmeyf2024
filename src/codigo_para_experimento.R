
# SETEO INICIAL

rm(list = ls()) 
gc()

require("data.table")
require("lightgbm")
require("rlist")
require("primes")
require("DiceKriging") # Para BO
require("mlrMBO") # Para BO

setwd("C:/Mis cosas/Maestria Data Mining/DMEyF")


###############  PRIMERA PARTE: Realizo optimización bayesiana de parámetros, incluyendo los parámetros de bagging

# 1. Defino los parámetros de la corrida en una lista, la variable global  PARAM

PARAM <- list()
PARAM$experimento <- "Experimento_optimizacion_bagging"

PARAM$semilla_primigenia<- 777787 

dataset <- "./datasets/dataset.csv" 
PARAM$input$dataset <- "./datasets/dataset.csv" 
PARAM$input$training <- c(202104) # meses donde se entrena el modelo
PARAM$finalmodel$max_bin <- 31

#### Parametros para optimizacion bayesiana

PARAM$trainingstrategy$undersampling <- 1.0

PARAM$hyperparametertuning$iteraciones <- 150
PARAM$hyperparametertuning$xval_folds <- 5
PARAM$hyperparametertuning$POS_ganancia <- 273000
PARAM$hyperparametertuning$NEG_ganancia <- -7000

# Aqui se cargan los bordes de los hiperparametros
hs <- makeParamSet(
  makeNumericParam("learning_rate", lower = 0.01, upper = 0.3),
  makeIntegerParam("num_leaves", lower = 8L, upper = 1024L),
  makeNumericParam("feature_fraction", lower = 0.1, upper = 1.0),
  makeIntegerParam("min_data_in_leaf", lower = 1L, upper = 8000L),
  makeIntegerParam("envios", lower = 5000L, upper = 15000L),
  makeIntegerParam("bagging_freq", lower = 1L, upper = 200L),
  makeNumericParam("neg_bagging_fraction", lower = 0, upper = 1),
  makeNumericParam("pos_bagging_fraction", lower = 0, upper = 1),
  makeNumericParam("bagging_fraction", lower = 0, upper = 1)
)


############ LO NECESARIO PARA LA BAYESIANA

# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

loguear <- function(
    reg, arch = NA, folder = "./exp/",
    ext = ".txt", verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)
  
  if (!file.exists(archivo)) # Escribo los titulos
  {
    linea <- paste0(
      "fecha\t",
      paste(list.names(reg), collapse = "\t"), "\n"
    )
    
    cat(linea, file = archivo)
  }
  
  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t", # la fecha y hora
    gsub(", ", "\t", toString(reg)), "\n"
  )
  
  cat(linea, file = archivo, append = TRUE) # grabo al archivo
  
  if (verbose) cat(linea) # imprimo por pantalla
}
#------------------------------------------------------------------------------
# esta funcion calcula internamente la ganancia de la prediccion probs
# es llamada por lightgbm luego de construir cada  arbolito

fganancia_logistic_lightgbm <- function(probs, datos) {
  vpesos <- get_field(datos, "weight")
  
  # vector de ganancias
  vgan <- ifelse(vpesos == 1.0000002, PARAM$hyperparametertuning$POS_ganancia,
                 ifelse(vpesos == 1.0000001, PARAM$hyperparametertuning$NEG_ganancia,
                        PARAM$hyperparametertuning$NEG_ganancia /
                          PARAM$trainingstrategy$undersampling
                 )
  )
  
  tbl <- as.data.table(list("vprobs" = probs, "vgan" = vgan))
  setorder(tbl, -vprobs)
  ganancia <- tbl[1:GLOBAL_envios, sum(vgan)]
  
  return(list(
    "name" = "ganancia",
    "value" = ganancia,
    "higher_better" = TRUE
  ))
}
#------------------------------------------------------------------------------
# esta funcion solo puede recibir los parametros que se estan optimizando
# el resto de los parametros se pasan como variables globales,


EstimarGanancia_lightgbm <- function(x) {
  gc() # libero memoria
  
  # llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1
  
  # para usar en fganancia_logistic_lightgbm
  # asigno la variable global
  GLOBAL_envios <<- as.integer(x$envios / PARAM$hyperparametertuning$xval_folds)
  
  # cantidad de folds para cross validation
  kfolds <- PARAM$hyperparametertuning$xval_folds
  
  param_basicos <- list(
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    verbosity = -100,
    max_bin = 31, # por ahora, lo dejo fijo
    num_iterations = 9999, # valor grande, lo limita early_stopping_rounds
    force_row_wise = TRUE, # para evitar warning
    seed = ksemilla_azar1
  )
  
  # el parametro discolo, que depende de otro
  param_variable <- list(
    early_stopping_rounds =
      as.integer(50 + 5 / x$learning_rate)
  )
  
  param_completo <- c(param_basicos, param_variable, x)
  
  set.seed(ksemilla_azar1)
  modelocv <- lgb.cv(
    data = dtrain,
    eval = fganancia_logistic_lightgbm,
    stratified = TRUE, # sobre el cross validation
    nfold = kfolds, # folds del cross validation
    param = param_completo,
    verbose = -100
  )
  
  # obtengo la ganancia
  ganancia <- unlist(modelocv$record_evals$valid$ganancia$eval)[modelocv$best_iter]
  
  ganancia_normalizada <- ganancia * kfolds # normalizo la ganancia
  
  # asigno el mejor num_iterations
  param_completo$num_iterations <- modelocv$best_iter
  # elimino de la lista el componente
  param_completo["early_stopping_rounds"] <- NULL
  
  
  # el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  # esta es la forma de devolver un parametro extra
  attr(ganancia_normalizada, "extras") <-
    list("num_iterations" = modelocv$best_iter)
  
  # logueo
  xx <- param_completo
  xx$ganancia <- ganancia_normalizada # le agrego la ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear(xx, arch = klog)
  
  # Voy registrando la importancia de variables
  if (ganancia_normalizada > GLOBAL_gananciamax) {
    GLOBAL_gananciamax <<- ganancia_normalizada
    modelo <- lgb.train(
      data = dtrain,
      param = param_completo,
      verbose = -100
    )
    
    tb_importancia <- as.data.table(lgb.importance(modelo))
    archivo_importancia <- paste0("impo_", GLOBAL_iteracion, ".txt")
    fwrite(tb_importancia,
           file = archivo_importancia,
           sep = "\t" )
    
    loguear(xx, arch = klog_mejor)
  }
  
  return(ganancia_normalizada)
}


primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia) # inicializo
# me quedo con PARAM$qsemillas   semillas
PARAM$semillas <- sample(primos, 2 )
ksemilla_azar1 <- PARAM$semillas[1]
ksemilla_azar2 <- PARAM$semillas[2]

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(PARAM$input$dataset)
dataset <- dataset[foto_mes == 202104, ]

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

# en estos archivos quedan los resultados
kbayesiana <- paste0(PARAM$experimento, ".RDATA")
klog <- paste0(PARAM$experimento, ".txt")
klog_mejor <- paste0(PARAM$experimento, "_mejor.txt")

GLOBAL_iteracion <- 0 # inicializo la variable global
GLOBAL_gananciamax <- -1 # inicializo la variable global

# si ya existe el archivo log, traigo hasta donde llegue
if (file.exists(klog)) {
  tabla_log <- fread(klog)
  GLOBAL_iteracion <- nrow(tabla_log)
  GLOBAL_gananciamax <- tabla_log[, max(ganancia)]
}


# paso la clase a binaria que tome valores {0,1}  enteros
dataset[
  foto_mes %in% PARAM$input$training,
  clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)
]


# los campos que se van a utilizar
campos_buenos <- setdiff(
  colnames(dataset),
  c("clase_ternaria", "clase01", "azar", "training")
)

# defino los datos que forma parte del training
# aqui se hace el undersampling de los CONTINUA
# notar que para esto utilizo la SEGUNDA semila
set.seed(ksemilla_azar2)
dataset[, azar := runif(nrow(dataset))]
dataset[, training := 0L]
dataset[
  foto_mes %in% PARAM$input$training &
    (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
  training := 1L
]

# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[training == 1L, campos_buenos, with = FALSE]),
  label = dataset[training == 1L, clase01],
  weight = dataset[training == 1L, ifelse(clase_ternaria == "BAJA+2", 1.0000002, ifelse(clase_ternaria == "BAJA+1", 1.0000001, 1.0))],
  free_raw_data = FALSE
)

library(mlr)

# Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar <- EstimarGanancia_lightgbm # la funcion que voy a maximizar

configureMlr(show.learner.output = FALSE)

library(smoof)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar, # la funcion que voy a maximizar
  minimize = FALSE, # estoy Maximizando la ganancia
  noisy = TRUE,
  par.set = hs, # definido al comienzo del programa
  has.simple.signature = FALSE # paso los parametros en una lista
)

# cada 600 segundos guardo el resultado intermedio
ctrl <- makeMBOControl(
  save.on.disk.at.time = 600, # se graba cada 600 segundos
  save.file.path = kbayesiana
) # se graba cada 600 segundos


# indico la cantidad de iteraciones que va a tener la Bayesian Optimization
ctrl <- setMBOControlTermination(
  ctrl,
  iters = PARAM$hyperparametertuning$iteraciones
) # cantidad de iteraciones

# defino el mÃ©todo estandar para la creacion de los puntos iniciales,
# los "No Inteligentes"
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

# establezco la funcion que busca el maximo
surr.km <- makeLearner(
  "regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  control = list(trace = TRUE)
)

### NO CORRER PORQUE YA LO CORRI

# # inicio la optimizacion bayesiana
# if (!file.exists(kbayesiana)) {
#   run <- mbo(obj.fun, learner = surr.km, control = ctrl)
# } else {
#   run <- mboContinue(kbayesiana) # retomo en caso que ya exista
# }
# 
# 
# cat("\n\nLa optimizacion Bayesiana ha terminado\n")


# Duración aproximada: 10 hs.


### PREDICCIONES
getwd()

setwd("C:/Mis cosas/Maestria Data Mining/DMEyF/exp/Experimento_optimizacion_bagging")

semillas <- c(319993, 919393, 112909, 777787, 155317, 573761, 750317, 379817, 999961, 333331)
cortes <- seq(3000, 9000, by = 500)
#cortes <- seq(3000, 7000, by = 500)

# Cargo los mejores hiperparámetros del archivo
mejores_parametros <- fread("Experimento_optimizacion_bagging_mejor.txt")
i = which.max(unlist(mejores_parametros[,17]))
best_params <- list(
  learning_rate = mejores_parametros$learning_rate[i],
  num_leaves = mejores_parametros$num_leaves[i],
  feature_fraction = mejores_parametros$feature_fraction[i],
  min_data_in_leaf = mejores_parametros$min_data_in_leaf[i],
  num_iterations = mejores_parametros$num_iterations[i],
  bagging_freq = mejores_parametros$bagging_freq[i], 
  bagging_fraction = mejores_parametros$bagging_fraction[i],
  neg_bagging_fraction = mejores_parametros$neg_bagging_fraction[i],
  pos_bagging_fraction = mejores_parametros$pos_bagging_fraction[i]
    )


# Crear un dataframe para guardar los resultados de todas las semillas
resultados_ganancia <- data.table(semilla = integer(), envios = integer(), ganancia = numeric())

# Iterar por cada semilla
for (semilla in semillas) {
  
  # Defino los parámetros básicos
  param_completo <- list(
    objective = "binary",
    metric = "binary_logloss",  # Cambiar si usas una métrica personalizada
    boost_from_average = TRUE,
    max_bin = 31,
    verbosity = -100,
    force_row_wise = TRUE,
    seed = semilla,
    bagging_freq = 0,
    bagging_fraction = 1, 
    neg_bagging_fraction = 1, 
    pos_bagging_fraction = 1, 
    verbose = -100
  )
  
  # Combino los parámetros básicos con los mejores hiperparámetros
  param_completo <- c(param_completo, best_params)
#  print(param_completo)
  cat("\nProcesando semilla:", semilla, "\n")
  
  # Establecer la semilla
  set.seed(semilla)
  
  print(dtrain)
  # Entrenar el modelo
  modelo <- lgb.train(
    data = dtrain,
    param = param_completo,
    )

  # Predecir para el mes futuro
  dapply <- dataset[foto_mes == "202104"]
  if (nrow(dapply) == 0) stop("No hay datos para predicción.")
  
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )
  
  # Generar tabla de entrega para la semilla actual
  tb_entrega <- dapply[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  tb_entrega[, prob := prediccion]
  
  # Calcular ganancia por cortes
  for (envios in cortes) {
    tb_entrega <- tb_entrega[order(-prob)]
    tb_entrega[, Predicted := 0L]
    tb_entrega[1:envios, Predicted := 1L]
    
    # Calcular ganancia considerando solo los envíos
    ganancia <- sum(
      (tb_entrega$clase_ternaria[tb_entrega$Predicted == 1L] == "BAJA+2") * 273000 +
        (tb_entrega$clase_ternaria[tb_entrega$Predicted == 1L] != "BAJA+2") * -7000
    )
    
    # Guardar los resultados en el dataframe
    resultados_ganancia <- rbind(resultados_ganancia, 
                                 data.table(semilla = semilla, envios = envios, ganancia = ganancia))
  }
}

# Crear gráfica de ganancias vs envíos
library(ggplot2)
library(dplyr)

# Calcular el promedio de ganancia para cada envío
promedio_ganancia <- resultados_ganancia %>%
  group_by(envios) %>%
  summarize(promedio_ganancia = mean(ganancia))

# Crear el gráfico
grafico <- ggplot(resultados_ganancia, aes(x = envios, y = ganancia, color = factor(semilla))) +
  geom_line() +
  geom_point() +
  geom_line(data = promedio_ganancia, aes(x = envios, y = promedio_ganancia), color = "black", linetype = "dashed", size = 1) +  # Línea de promedio
  labs(
    title = "Ganancias vs Envíos por Semilla",
    x = "Cantidad de Envíos",
    y = "Ganancia",
    color = "Semilla"
  ) +
  theme_minimal()

# Mostrar la gráfica
print(grafico)


###############  SEGUNDA PARTE: Realizo optimización bayesiana de parámetros, sin los parámetros de bagging

getwd()
setwd("C:/Mis cosas/Maestria Data Mining/DMEyF")

# 1. Defino los parámetros de la corrida en una lista, la variable global  PARAM

PARAM <- list()
PARAM$experimento <- "Experimento_SIN_optimizacion_bagging"
PARAM$semilla_primigenia<- 777787 
dataset <- "./datasets/dataset.csv" 
PARAM$input$dataset <- "./datasets/dataset.csv" 
PARAM$input$training <- c(202104) # meses donde se entrena el modelo
PARAM$finalmodel$max_bin <- 31

#### Parametros para optimizacion bayesiana

PARAM$trainingstrategy$undersampling <- 1.0

PARAM$hyperparametertuning$iteraciones <- 150
PARAM$hyperparametertuning$xval_folds <- 5
PARAM$hyperparametertuning$POS_ganancia <- 273000
PARAM$hyperparametertuning$NEG_ganancia <- -7000

primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia) # inicializo
# me quedo con PARAM$qsemillas   semillas
PARAM$semillas <- sample(primos, 2 )
ksemilla_azar1 <- PARAM$semillas[1]
ksemilla_azar2 <- PARAM$semillas[2]


# Aqui se cargan los bordes de los hiperparametros
hs <- makeParamSet(
  makeNumericParam("learning_rate", lower = 0.01, upper = 0.3),
  makeIntegerParam("num_leaves", lower = 8L, upper = 1024L),
  makeNumericParam("feature_fraction", lower = 0.1, upper = 1.0),
  makeIntegerParam("min_data_in_leaf", lower = 1L, upper = 8000L),
  makeIntegerParam("envios", lower = 5000L, upper = 15000L)#,
)


############ LO NECESARIO PARA LA BAYESIANA

# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

loguear <- function(
    reg, arch = NA, folder = "./exp/",
    ext = ".txt", verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)
  
  if (!file.exists(archivo)) # Escribo los titulos
  {
    linea <- paste0(
      "fecha\t",
      paste(list.names(reg), collapse = "\t"), "\n"
    )
    
    cat(linea, file = archivo)
  }
  
  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t", # la fecha y hora
    gsub(", ", "\t", toString(reg)), "\n"
  )
  
  cat(linea, file = archivo, append = TRUE) # grabo al archivo
  
  if (verbose) cat(linea) # imprimo por pantalla
}
#------------------------------------------------------------------------------
# esta funcion calcula internamente la ganancia de la prediccion probs
# es llamada por lightgbm luego de construir cada  arbolito

fganancia_logistic_lightgbm <- function(probs, datos) {
  vpesos <- get_field(datos, "weight")
  
  # vector de ganancias
  vgan <- ifelse(vpesos == 1.0000002, PARAM$hyperparametertuning$POS_ganancia,
                 ifelse(vpesos == 1.0000001, PARAM$hyperparametertuning$NEG_ganancia,
                        PARAM$hyperparametertuning$NEG_ganancia /
                          PARAM$trainingstrategy$undersampling
                 )
  )
  
  tbl <- as.data.table(list("vprobs" = probs, "vgan" = vgan))
  setorder(tbl, -vprobs)
  ganancia <- tbl[1:GLOBAL_envios, sum(vgan)]
  
  return(list(
    "name" = "ganancia",
    "value" = ganancia,
    "higher_better" = TRUE
  ))
}
#------------------------------------------------------------------------------
# esta funcion solo puede recibir los parametros que se estan optimizando
# el resto de los parametros se pasan como variables globales,


EstimarGanancia_lightgbm <- function(x) {
  gc() # libero memoria
  
  # llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1
  
  # para usar en fganancia_logistic_lightgbm
  # asigno la variable global
  GLOBAL_envios <<- as.integer(x$envios / PARAM$hyperparametertuning$xval_folds)
  
  # cantidad de folds para cross validation
  kfolds <- PARAM$hyperparametertuning$xval_folds
  
  param_basicos <- list(
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    verbosity = -100,
    max_bin = 31, # por ahora, lo dejo fijo
    num_iterations = 9999, # valor grande, lo limita early_stopping_rounds
    force_row_wise = TRUE, # para evitar warning
    seed = ksemilla_azar1
  )
  
  # el parametro discolo, que depende de otro
  param_variable <- list(
    early_stopping_rounds =
      as.integer(50 + 5 / x$learning_rate)
  )
  
  param_completo <- c(param_basicos, param_variable, x)
  
  set.seed(ksemilla_azar1)
  modelocv <- lgb.cv(
    data = dtrain,
    eval = fganancia_logistic_lightgbm,
    stratified = TRUE, # sobre el cross validation
    nfold = kfolds, # folds del cross validation
    param = param_completo,
    verbose = -100
  )
  
  # obtengo la ganancia
  ganancia <- unlist(modelocv$record_evals$valid$ganancia$eval)[modelocv$best_iter]
  
  ganancia_normalizada <- ganancia * kfolds # normalizo la ganancia
  
  # asigno el mejor num_iterations
  param_completo$num_iterations <- modelocv$best_iter
  # elimino de la lista el componente
  param_completo["early_stopping_rounds"] <- NULL
  
  
  # el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  # esta es la forma de devolver un parametro extra
  attr(ganancia_normalizada, "extras") <-
    list("num_iterations" = modelocv$best_iter)
  
  # logueo
  xx <- param_completo
  xx$ganancia <- ganancia_normalizada # le agrego la ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear(xx, arch = klog)
  
  # Voy registrando la importancia de variables
  if (ganancia_normalizada > GLOBAL_gananciamax) {
    GLOBAL_gananciamax <<- ganancia_normalizada
    modelo <- lgb.train(
      data = dtrain,
      param = param_completo,
      verbose = -100
    )
    
    tb_importancia <- as.data.table(lgb.importance(modelo))
    archivo_importancia <- paste0("impo_", GLOBAL_iteracion, ".txt")
    fwrite(tb_importancia,
           file = archivo_importancia,
           sep = "\t" )
    
    loguear(xx, arch = klog_mejor)
  }
  
  return(ganancia_normalizada)
}


primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia) # inicializo
# me quedo con PARAM$qsemillas   semillas
PARAM$semillas <- sample(primos, 2 )
ksemilla_azar1 <- PARAM$semillas[1]
ksemilla_azar2 <- PARAM$semillas[2]

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(PARAM$input$dataset)
dataset <- dataset[foto_mes == 202104, ]

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

# en estos archivos quedan los resultados
kbayesiana <- paste0(PARAM$experimento, ".RDATA")
klog <- paste0(PARAM$experimento, ".txt")
klog_mejor <- paste0(PARAM$experimento, "_mejor.txt")

GLOBAL_iteracion <- 0 # inicializo la variable global
GLOBAL_gananciamax <- -1 # inicializo la variable global

# si ya existe el archivo log, traigo hasta donde llegue
if (file.exists(klog)) {
  tabla_log <- fread(klog)
  GLOBAL_iteracion <- nrow(tabla_log)
  GLOBAL_gananciamax <- tabla_log[, max(ganancia)]
}


# paso la clase a binaria que tome valores {0,1}  enteros
dataset[
  foto_mes %in% PARAM$input$training,
  clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)
]


# los campos que se van a utilizar
campos_buenos <- setdiff(
  colnames(dataset),
  c("clase_ternaria", "clase01", "azar", "training")
)

# defino los datos que forma parte del training
# aqui se hace el undersampling de los CONTINUA
# notar que para esto utilizo la SEGUNDA semila
set.seed(ksemilla_azar2)
dataset[, azar := runif(nrow(dataset))]
dataset[, training := 0L]
dataset[
  foto_mes %in% PARAM$input$training &
    (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
  training := 1L
]


# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[training == 1L, campos_buenos, with = FALSE]),
  label = dataset[training == 1L, clase01],
  weight = dataset[training == 1L, ifelse(clase_ternaria == "BAJA+2", 1.0000002, ifelse(clase_ternaria == "BAJA+1", 1.0000001, 1.0))],
  free_raw_data = FALSE
)


library(mlr)

# Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar <- EstimarGanancia_lightgbm # la funcion que voy a maximizar

configureMlr(show.learner.output = FALSE)

library(smoof)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar, # la funcion que voy a maximizar
  minimize = FALSE, # estoy Maximizando la ganancia
  noisy = TRUE,
  par.set = hs, # definido al comienzo del programa
  has.simple.signature = FALSE # paso los parametros en una lista
)

# cada 600 segundos guardo el resultado intermedio
ctrl <- makeMBOControl(
  save.on.disk.at.time = 600, # se graba cada 600 segundos
  save.file.path = kbayesiana
) # se graba cada 600 segundos


# indico la cantidad de iteraciones que va a tener la Bayesian Optimization
ctrl <- setMBOControlTermination(
  ctrl,
  iters = PARAM$hyperparametertuning$iteraciones
) # cantidad de iteraciones

# defino el mÃ©todo estandar para la creacion de los puntos iniciales,
# los "No Inteligentes"
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

# establezco la funcion que busca el maximo
surr.km <- makeLearner(
  "regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  control = list(trace = TRUE)
)


## NO CORRER PORQUE YA LO CORRI

# inicio la optimizacion bayesiana
if (!file.exists(kbayesiana)) {
  run <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else {
  run <- mboContinue(kbayesiana) # retomo en caso que ya exista
}


cat("\n\nLa optimizacion Bayesiana ha terminado\n")

# Duración aproximada: 10 hs.


### PREDICCIONES
getwd()

# Cargo los mejores hiperparámetros del archivo
mejores_parametros2 <- fread("Experimento_SIN_optimizacion_bagging_mejor.txt")
i = which.max(unlist(mejores_parametros2[,17]))
best_params2 <- list(
  learning_rate = mejores_parametros2$learning_rate[i],
  num_leaves = mejores_parametros2$num_leaves[i],
  feature_fraction = mejores_parametros2$feature_fraction[i],
  min_data_in_leaf = mejores_parametros2$min_data_in_leaf[i],
  num_iterations = mejores_parametros2$num_iterations[i]
)

semillas <- c(319993, 919393, 112909, 777787, 155317, 573761, 750317, 379817, 999961, 333331)
#cortes <- seq(1000, 7000, by = 500)
cortes <- seq(3000, 9000, by = 500)
resultados_ganancia2 <- data.table(semilla = integer(), envios = integer(), ganancia = numeric())

# Iterar por cada semilla
for (semilla in semillas) {
  
  # Defino los parámetros básicos
  param_completo2 <- list(
    objective = "binary",
    metric = "binary_logloss",  
    boost_from_average = TRUE,
    max_bin = 31,
    verbosity = -100,
    force_row_wise = TRUE,
    seed = semilla,
    bagging_freq = 0, 
    bagging_fraction = 1, 
    neg_bagging_fraction = 1, 
    pos_bagging_fraction = 1, 
    verbose = -100
  )
  
  # Combino los parámetros básicos con los mejores hiperparámetros
  param_completo2 <- c(param_completo2, best_params2)
  
  cat("\nProcesando semilla:", semilla, "\n")
  #print(param_completo)
  # Establecer la semilla
  set.seed(semilla)
  print(semilla)
  # Entrenar el modelo
  
  print(dtrain)
  modelo <- lgb.train(
    data = dtrain,
    param = param_completo2,
  )
  
  # Predecir para el mes futuro
  dapply <- dataset[foto_mes == "202104"]
  if (nrow(dapply) == 0) stop("No hay datos para predicción.")
  
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )
  
  # Generar tabla de entrega para la semilla actual
  tb_entrega2 <- dapply[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  tb_entrega2[, prob := prediccion]
  
  # Calcular ganancia por cortes
  for (envios in cortes) {
    tb_entrega2 <- tb_entrega2[order(-prob)]
    tb_entrega2[, Predicted := 0L]
    tb_entrega2[1:envios, Predicted := 1L]
    
    # Calcular ganancia considerando solo los envíos
    ganancia <- sum(
      (tb_entrega2$clase_ternaria[tb_entrega2$Predicted == 1L] == "BAJA+2") * 273000 +
        (tb_entrega2$clase_ternaria[tb_entrega2$Predicted == 1L] != "BAJA+2") * -7000
    )
    
    # Guardar los resultados en el dataframe
    resultados_ganancia2 <- rbind(resultados_ganancia2, 
                                 data.table(semilla = semilla, envios = envios, ganancia = ganancia))
  }
}

# Crear gráfica de ganancias vs envíos

# Calcular el promedio de ganancia para cada envío
promedio_ganancia <- resultados_ganancia2 %>%
  group_by(envios) %>%
  summarize(promedio_ganancia = mean(ganancia))

# Crear el gráfico
grafico <- ggplot(resultados_ganancia2, aes(x = envios, y = ganancia, color = factor(semilla))) +
  geom_line() +
  geom_point() +
  geom_line(data = promedio_ganancia, aes(x = envios, y = promedio_ganancia), color = "black", linetype = "dashed", size = 1) +  # Línea de promedio
  labs(
    title = "Ganancias vs Envíos por Semilla",
    x = "Cantidad de Envíos",
    y = "Ganancia",
    color = "Semilla"
  ) +
  theme_minimal()

# Mostrar la gráfica
print(grafico)

