# DiegoRiveraBrainFood2:
# Fecha: 04/07/2009
# Este Script busca enfocar un planteamiento distinto a DiegoRiveraBrainFood, mas orientado a Machine Learning.
# Script desarrollado para la obtencion de proyecciones de consumo diario de GAS en la ciudad de Punta Arenas.
# En base a lo anterior se buscará obtener:
#   - predicciones diarias, de 1 a 7 días.
#   - predicciones semanales, de una a 5 semanas.
#   - predicciones mensules, de 1 a 3 meses.
# Para lo anterior se cuenta con información del consumo de gas natural entre el 20 de octubre de 1995 y 31 de 
# diciembre de 2002.

# setting de directorio de trabajo en mi equipo. Considerar nuevo setting al probar en otro equipo.
setwd("~/GoogleDrive/DataScience/BrainFoodChallenge")

# ** Es script fue desarrollado en un equipo MacBook. Por caracteristicas del sistema operativo utilizado en la
# maquina (macOSMojave v.10.14.1) existe una inconsistencia al momento de cargar data tipo texto en idioma español.
# Error experimentado anteriormente y de momento no he podido corregirlo.
# Los problemas en la lectura se presentan en las tildes y la consonante "ñ", leyendo estos como caracteres extraños y
# en caso de presentar algun caso en el header, simplemente arrojando error.
# Debido a lo anterior se trabajo con el archivo "Consumo Gas Región Magallanes copy.csv" que es una copia del archivo
# original donde se removieron todas las tildes y "ñ"

# Data Frame con la data historica del consumo de gas en Punta Arenas entre 20 de octubre de 1995 y 31 de diciembre 
# de 2002.
consumo_historico_gas <- read.csv("Consumo Gas Región Magallanes copy.csv", header = T, sep = ";")

# *********************************************** FASE EXPLORATORIA: ***********************************************
# exlorando la data de consumo diario de gas. Se asume que la unidad de cosumo es (m^3/día)
summary(consumo_historico_gas)
str(consumo_historico_gas)

# Respecto a la Data referida al Consumo tengo lo siguiente:
# Consumo: Min.: 333.0 - 1st Qu.:579.0 - Median :715.0 - Mean: 725.6 - 3rd Qu.: 876.0 - Max.: 1222.0

# boxplot para validar que hayan outliers en la data de consumo. BrainFoodChallenge/Fig1.pdf.
boxplot(consumo_historico_gas$Consumo)

# variable ValoresOutlier vacia, revalido lo anterior.
ValoresOutlier <- boxplot(consumo_historico_gas$Consumo)$out

# Visualizo el consumo historico vs los meses de ocurrencia para ver el comportamiento y dispercion del consumo y 
# tener una primera apreciacion de los datos. BrainFoodChallenge/Fig2.pdf
plot(consumo_historico_gas$Consumo, consumo_historico_gas$Mes)
# Es posible deducir un consumo estacional entre los meses de inicio de año, al alza y los meses de fin de año a 
# la baja. De todas maneras seguire analizando la data antes de tomar alguna decision.

# Repito el mismo ejercicio para el consumo historico vs los anios de ocurrencia para ver el comportamiento del 
# consumo y tener una apreciacion ddistinta de los datos. BrainFoodChallenge/Fig3.pdf
plot(consumo_historico_gas$Consumo, consumo_historico_gas$Anio)

# Luego de revisar los primeros graficos, he podido darme cuenta de las tendencias de crecimiento en el consumo,
# correlacion positiva durante la primera mitad del año, y tendencia de disminucion en el consumo con correlacion 
# negativa en la segunda mitad del año (Fig2). Ademas la Fig3, me muestra entre los años 1996 y 2002 el consumo
# es casi el mismo, con unos casos puntuales de mayor o menor consumo de gas, pero practimante iguales. Lo que me
# da a asumir que la correlacion positiva durante la primera mitad del año y la negativa para la segunda mitad se
# repite entre 1996 y 2002. Lo confirmaré a continuación. Para 1995 sólo tengo data desde 20 de octubre en adelante,
# por eso es que el consumo es tan acotado y bajo.

# Debido a lo anterior he decido analizar la data en dos partes. Lo correspondiente al primer semestre, correlacion
# del consumo de gas en el tiempo positiva y la del segundo semestre con correlacion entre el consumo de gas en el
# tiempo negativa.

# Asignando nueva variable correspondiente al semestre, segun el mes de lectura del consumo de gas.
# De Enero a Junio -> Primer Semestre.
# De Julio a Diciembre -> Segundo Semestre.
for (i in 1:nrow(consumo_historico_gas)) {
  if (consumo_historico_gas$Mes[i] == 1 || consumo_historico_gas$Mes[i] == 2 || consumo_historico_gas$Mes[i] == 3 ||
      consumo_historico_gas$Mes[i] == 4 || consumo_historico_gas$Mes[i] == 5 || consumo_historico_gas$Mes[i] == 6) {
    consumo_historico_gas$Semestre[i] <- "1er Semestre"
    consumo_historico_gas$SemestreAnio[i] <- paste("1er Semestre - ",consumo_historico_gas$Anio[i], sep = "", 
                                               collapse = NULL)
  } else {
    consumo_historico_gas$Semestre[i] <- "2do Semestre"
    consumo_historico_gas$SemestreAnio[i] <- paste("2do Semestre - ",consumo_historico_gas$Anio[i], sep = "", 
                                               collapse = NULL)
  }
  
  # Aprovecho de crear una nueva variable en el data frame con el dia y nombre del mes para facilitar la lectura
  # de la data. Con esto me limito a enfocarme solo en el mes y dia para futuros analisis, omitiendo por ejemplo
  # qué dia de la semana corresponde o si es feriado o no, como lo muestra en el data frame. Para efecto de este 
  # analisis, esto ultimo de momento no se tendrá en consideracion. 
  consumo_historico_gas$dia_mes[i] <- paste(consumo_historico_gas$Dia[i],"-",month.name[consumo_historico_gas$Mes[i]]
                                            , sep = "",collapse = NULL)
  
  # Variable con data del nombre del mes y año, siguiendo la logica anterior.
  consumo_historico_gas$mes_anio[i] <- paste(month.name[consumo_historico_gas$Mes[i]],"-",
                                             consumo_historico_gas$Anio[i], sep = "", collapse = NULL)
  
  # variable con referente de la cantidad de dias corridos de la muestra de datos.
  consumo_historico_gas$dias_corridos <- c(1:nrow(consumo_historico_gas))
}

# ******************************************************************************************************************
# ******************************************* Busqueda Tendencia por Año *******************************************
# ******************************************************************************************************************
# Con esta representación gráfico valido mi supuesto de las correlaciones positivas y negativas del consumo de gas
# para primer y segundo semestre, respectivamente, entre el 20 de Octubre de 1995 al 31 de Diciembre de 2002.
# BrainFoodChallenge/Fig4.pdf
ConsumoTotal <- consumo_historico_gas$Consumo
diasTotal <- c(1:2630)
plot(diasTotal,ConsumoTotal, type = "l", main="Consumo de Gas Diario en la Ciudad de Pta.Arenas desde 20 de Octubre 
     de 1995 al 31 de Diciembre de 2002", xlab = "20 de Octubre de 1995 al 31 de Diciembre de 2002", 
     ylab = "Consumo de Gas (m3/dia)", ylim = c(300,1250))

# ******************************************************************************************************************
# ******************************************** Enfoque Machine Learning ********************************************
# ******************************************************************************************************************

# Considerando la distribucion total como una distribucion gaussiana. Ocupare un efoque de red neuronal.
# Para ello trabajare con el library 'neuralnet'
# install.packages('neuralnet')
library('neuralnet')

attach(consumo_historico_gas)

# ********************************************* Normalizacion de Datos *********************************************
# Normalización de datos. Uno de los procedimientos más importantes al formar una red neuronal es la normalización 
# de datos. Esto implica ajustar los datos a una escala común para poder comparar con precisión los valores
# predichos y reales. Si no se normalizan los datos, el valor de predicción seguirá siendo el mismo en todas las
# observaciones, independientemente de los valores de entrada.
# Se pueden normalizar datos de dos maneras en R:
#  - Escalando el data frame automáticamente usando la función de scale() en R
#  - Transforma los datos utilizando una técnica de normalización max-min.
# Trataré de implementar las dos técnicas.
# Debidos a los errores obtenidos con los metodos de normalizacion de datos mencionados anteriormente, trabajare
# con un nuevo data frame que contenga unicamente data original numerica.
#  - scale() -> Error in colMeans(x, na.rm = TRUE) : 'x' must be numeric
#  - maxmin -> Error in Summary.factor(c(7L, 6L, 1L, 3L, 4L, 5L, 2L, 7L, 6L, 1L, 3L,:
#                 ‘min’ not meaningful for factors 

# carga de data original sin toda la data de dia, mes, anio, dias de semana y feriados.
# y con fecha formato numeros
library('lubridate')
gas_data <- read.csv("gas_data.csv", header = T)
# debido a errores en la generacion del plot de la red neural se dejo la finalmente la fecha como correlativo
# numerico desde 1 a 2630, representando el correlativo de dias. Aunque realizar una red neuronal con una sola
# variable independiente carece de sentido. Por motivos netos de mostrar una forma distinta enfocar el problema
# se continuara con el desarrollo del modelo.
gas_data$fecha <- as.numeric(gas_data$fecha)
# revisando data
summary(gas_data)
str(gas_data)

attach(gas_data)

# realizando el ajuste en la data original pude normalizar los datos para funcion scale().
scaledata <- scale(gas_data)

# Para método normalización max-min, invocamos la siguiente función para normalizar los datos:
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Luego, usando lapply para ejecutar la función a través de los datos existentes, "gas_data", data frame original y
# ajustado.
# lo mismo técnica de normalización max-min.
maxmindf <- as.data.frame(lapply(gas_data, normalize))

# Ahora estan escalados los nuevos data frames (scaledata, maxmindf)
# revisando los resultados de ambas normalizaciones
scaledata
summary(scaledata)
str(scaledata)
maxmindf
summary(maxmindf)
str(maxmindf)

# Basare los datos de entrenamiento (trainset) en el 80% de las observaciones. 
# Los datos de la prueba (testset) se basaran en el 20% restante de las observaciones.
# Training and Test Data
trainset_scale <- scaledata[1:1888, ]
testset_scale <- scaledata[1889:2630, ]

trainset_maxmin <- maxmindf[1:1888, ]
testset_maxmin <- maxmindf[1889:2630, ]

# revisando los resultados de Training and Test Data
trainset_scale
str(trainset_scale)
testset_scale
str(testset_scale)

trainset_maxmin
str(trainset_maxmin)
testset_maxmin
str(testset_maxmin)

# ****************************** Entrenando un modelo de red neuronal usando neuralnet *****************************
# Se tiene:
#  - El uso de neuralnet para "retroceder" la variable dependiente "dividendo" contra las otras variables
#    independientes.
#  - Establecer el número de capas ocultas en (2,1) según la fórmula oculta = (2,1).
#  - La variable linear.output se establece como FALSE, dado que se supone que el impacto de las variables 
#    independientes en la variable dependiente (dividendo) no es lineal.
#  - El umbral se establece en 0.01, lo que significa que si el cambio en el error durante una iteración es menor 
#    al 1%, entonces el modelo no llevará a cabo ninguna otra optimización.

# Decidir el número de capas ocultas en una red neuronal no es una ciencia exacta. De hecho, hay casos en que la 
# precisión probablemente será mayor sin ninguna capa oculta. Por lo tanto, la prueba y el error juegan un papel
# importante en este proceso.

# Una posibilidad es comparar cómo cambia la precisión de las predicciones a medida que se modifique el número de 
# capas ocultas.

# Red Neuronal para trainset_scale
nn_scale <- neuralnet(Consumo ~ fecha, data=trainset_scale, hidden=c(2,1), linear.output=FALSE, 
                      threshold=0.01)
nn_scale$result.matrix
# BrainFoodChallenge/Fig5.2.pdf
plot(nn_scale)

# Red Neuronal para trainset_maxmin
nn_maxmin <- neuralnet(Consumo ~ fecha, data=trainset_maxmin, hidden=c(2,1), linear.output=FALSE, 
                      threshold=0.01)
nn_maxmin$result.matrix
# BrainFoodChallenge/Fig6.2.pdf
plot(nn_maxmin)

# Ahora genero el error del modelo de red neuronal, junto con los pesos entre las entradas, las capas ocultas y las 
# salidas:

nn_scale$result.matrix
# error: 872.675030259

nn_maxmin$result.matrix
# error: 30.120082057

# Como se comento anteriormente, decidir el número de capas ocultas en una red neuronal no es una ciencia exacta.
# conlleva varias pruebas, pero en funcion del tiempo continuare con los resultados obtenidos desde la normalizacion
# maxmin.

# **************************************** Probando la precisión del modelo ****************************************
# Como ya se mencionó, la red neuronal se ha creado utilizando los datos de entrenamiento. Luego comparare esto con 
# los datos de prueba para medir la precisión del pronóstico de la red neuronal.

# a continuacion:
#  - La función "subset" se utiliza para eliminar la variable dependiente de los datos de prueba
#  - La función "compute" crea la variable de predicción.
#  - Una variable de "resultados" luego compara los datos predichos con los datos reales
#  - Luego se crea una matriz de confusión con la función de tabla para comparar el número de positivos y negativos 
#    verdaderos / falsos

# Test the resulting output
temp_test <- subset(testset_maxmin, select = c("fecha"))
head(temp_test)
nn.results <- compute(nn_maxmin, temp_test)
results <- data.frame(actual = testset_maxmin$Consumo, prediction = nn.results$net.result)

# Los resultados previstos se comparan con los resultados reales
results

# ********************************************** Matriz de Confusion ***********************************************
# Luego, redondeamos nuestros resultados utilizando sapply y creamos una matriz de confusión para comparar el número
# de positivos y negativos verdaderos/falsos:
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)

# Se utiliza una matriz de confusión para determinar el número de positivos verdaderos y falsos generados por 
# nuestras predicciones. El modelo genera 399 verdaderos negativos (0's), (-) verdaderos positivos (1's -> solo 
# considera una variable independiente. Lo que distorsinoona aun mas el ejercicio). 
# mientras que hay 343 falsos negativos.

# Finalmente, obtenemos una tasa de precisión de 53.77% (399/742) para determinar el consumo de gas futuro.

# ******************************************************************************************************************
# ************************************************** Conclusiones **************************************************
# ******************************************************************************************************************

# En este script se intento tratar con un approach distinto el problema planteado acerca del consumo de gas en Punta
# Arenas, enfocado al uso de redes neuronales para darle una perspectiva oriantada al machine learning.
# Si bien es una metodologia bastante interesante y con potenciales cada vez mas versatiles, no es necesario
# aplicarlos para solucionar todos los problemas que se presentan.
# De todas maneras se vieron las etapas inicialespara el uso de una red neuronal para resolver problemas estimacion
# de datos. Tales como:
#  - Normalizar los datos para un análisis significativo
#  - Clasificar datos utilizando una red neuronal
#  - Exactitud de la prueba utilizando una matriz de confusión.


