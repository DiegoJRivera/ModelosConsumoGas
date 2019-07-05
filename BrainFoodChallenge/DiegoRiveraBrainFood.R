# DiegoRiveraBrainFood:
# Fecha: 04/07/2009
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
# *************************************************** Regresiones **************************************************
# ******************************************************************************************************************
# Es pequeño ejercico me ayuda a revalidar mi intención de tratar el consumo de gas de manera separa entre primer y
# segundo semestre.
# Si bien intento hacer una regresion lineal con todas la data de consumo entre 20 de Octubre de 1995 al 31 de Diciembre
# de 2002. Este modelo es bastante poco representativo ya que la variabilidad presente en la variable de consumo 
# de gas mediante la variable independiente correspondientes a los dias del periodo es bajísima, R^2 es apenas
# representa un 4,792%.
reg1 <- lm(consumo_historico_gas$Consumo~consumo_historico_gas$dias_corridos)
summary(reg1) # revisando el resumen correspondiente

# Representaciógrafica de lo mencionado anteriormente. BrainFoodChallenge/Fig5.pdf
plot(diasTotal,ConsumoTotal, type = "p", main="Consumo de Gas Diario en la Ciudad de Pta.Arenas desde 20 de Octubre 
     de 1995 al 31 de Diciembre de 2002", xlab = "20 de Octubre de 1995 al 31 de Diciembre de 2002", 
     ylab = "Consumo de Gas (m3/dia)", ylim = c(300,1250))
# representacion grafica del modelo reg1.
abline(reg1, col = "red")

# ******************************************************************************************************************
# ***************************************** Busqueda Tendencia por Semestre ****************************************
# ******************************************************************************************************************
# Dado a los resultados obtenidos anteriormente y basados en la grafica de consumo anual (año a año y de 1995 a 2002)
# se dividio el consumo en dos periodos. El primero con una tendencia al aumento y pendiente positiva, 1er Semestre
# que corresponde a los meses de Enero, Febrero, Marzo, Abril, Mayo y Junio.
# El 2do Semestre, meses de Julio, Agosto, Septiembre, Octubre, Noviembre y Diciembre con un comportamiento
# decreciente en el consumo de gas, pendiente negativa.

library('dplyr')
# Consumo de todo el periodo de data agrupado por primer y segundo semestre
ConsumoSemestre <- consumo_historico_gas %>%
  select(Consumo, Semestre) %>%
  group_by(Semestre)

library('sqldf')
# Consumo de gas diario correspondiente al primer semestre entre 1995 y 2002
Consumo1Semestre <- sqldf("select Consumo, Semestre, mes_anio from consumo_historico_gas where 
                          Semestre = '1er Semestre' ")

# Consumo de gas diario correspondiente al segundo semestre entre 1995 y 2002
Consumo2Semestre <- sqldf("select Consumo, Semestre, mes_anio from consumo_historico_gas where
                          Semestre = '2do Semestre' ")
# revisando el resumen de la data obtenida.
summary(Consumo1Semestre)
summary(Consumo2Semestre)

DiasSemestre1 <- c(1:1269) # numero de dias en el periodo correspondiente
# Representacion grafica del consumo de gas diario correspondiente al primer semestre entre 1996 y 2002.
# BrainFoodChallenge/Fig6.pdf
plot(DiasSemestre1,Consumo1Semestre$Consumo, type = "p", main="Consumo de Gas Diario en la Ciudad de Pta.Arenas
     durante el Primer Semestre entre los años 1996 a 2002", xlab = "1er Semestre entre Años 1996 - 2002", 
     ylab = "Consumo de Gas (m3/dia)", ylim = c(300,1250))

# Un modelo de regresion lineal aplicado a este grupo de datos no seria representativo, ya que se tienen 7 grupos
# de consumos en distintos periodos de tiempo, cada uno con su propia pendiente, por lo cual un modelo que aplique
# a los 7 deberia considerar la media de consumo entre todos los dias del mismo periodo y no un modelo para todos 
# los grupos por separado.

# Como se acaba de comentar, este modelo cuenta con un R^2 de 8,799%.
reg2 <- lm(Consumo1Semestre$Consumo~DiasSemestre1)
summary(reg2) # revisando el resumen correspondiente
# Revalidando que el modelo considera muy pocas observaciones.
abline(reg2, col = "red")

# Solo para visualizarlo se hara el mismo ejercicio con el consumo de gas para el segundo semestre. Pero los
# resultados son los mismo.
DiasSemestre2 <- c(1:1361) # numero de dias en el periodo correspondiente
# Representacion grafica del consumo de gas diario correspondiente al segundo semestre entre 1995 y 2002.
# BrainFoodChallenge/Fig7.pdf
plot(DiasSemestre2,Consumo2Semestre$Consumo, type = "p", main="Consumo de Gas Diario en la Ciudad de Pta.Arenas 
     durante el Segundo Semestre entre los años 1995 a 2002", xlab = "1er Semestre entre Años 1995 - 2002", 
     ylab = "Consumo de Gas (m3/dia)", ylim = c(300,1200))

# R^2 del modelo en cuestion, 1,68%
reg3 <- lm(Consumo2Semestre$Consumo~DiasSemestre2)
summary(reg3) # revisando el resumen correspondiente
# Revalidando que el modelo considera muy pocas observaciones.
abline(reg3, col = "blue")

# Aplicando mismo criterio pero para un año en particular, ya que las tendencias tanto de primer semestre como de 
# segundo semestre son similares. Usando de manera aleatoria el año 2000
# Consumo de gas diario correspondiente al primer semestre del año 2000
Consumo1Semestre2000 <- sqldf("select Consumo, Semestre, Anio from consumo_historico_gas where Anio = 2000 and
                          Semestre = '1er Semestre' ")
# Consumo de gas diario correspondiente al segundo semestre del año 2000
Consumo2Semestre2000 <- sqldf("select Consumo, Semestre, Anio from consumo_historico_gas where Anio = 2000 and
                          Semestre = '2do Semestre' ")

# revisando resumen de la data
summary(Consumo1Semestre2000)
summary(Consumo2Semestre2000)

DiasSemestre1_2000 <- c(1:182) # numero de dias en el periodo correspondiente
# Representacion grafica del consumo de gas diario correspondiente al primer semestre del año 2000.
# BrainFoodChallenge/Fig8.pdf
plot(DiasSemestre1_2000,Consumo1Semestre2000$Consumo, type = "p", main="Consumo de Gas Diario en la Ciudad de 
     Pta.Arenas durante el Primer Semestre del año 2000", xlab = "1er Semestre Año 2000", 
     ylab = "Consumo de Gas (m3/dia)", ylim = c(350,1100))

# reg4 modelo del Consumo de diario correspondiente al primer semestre año 2000. Con un R^2 de 84%.
reg4 <- lm(Consumo1Semestre2000$Consumo~DiasSemestre1_2000)
summary(reg4) # revisando el resumen correspondiente
# Como se esperaba el modelo se ajusta bastante bien.
abline(reg4, col = "red")


# Repeticion idem para consumo de gas de segundo semestre del año 2000.
DiasSemestre2_2000 <- c(1:184) # numero de dias en el periodo correspondiente
# Representacion grafica del consumo de gas diario correspondiente al segundo semestre del año 2000.
# BrainFoodChallenge/Fig9.pdf
plot(DiasSemestre2_2000,Consumo2Semestre2000$Consumo, type = "p", main="Consumo de Gas Diario en la Ciudad de 
     Pta.Arenas durante el Segundo Semestre del año 2000", xlab = "2do Semestre Año 2000", 
     ylab = "Consumo de Gas (m3/dia)", ylim = c(400,1150))

# reg5 modelo del Consumo de diario correspondiente al segundo semestre año 2000. Con un R^2 de 79,87%.
reg5 <- lm(Consumo2Semestre2000$Consumo~DiasSemestre2_2000)
summary(reg5) # revisando el resumen correspondiente
# Como se esperaba, nuevamente el modelo se ajusta bastante bien.
abline(reg5, col = "blue")

# Por simple y sana curiosidad, una prueba mas al azar, antes de proceceder al criterio final de "Promdio Diario 
#Anual por Semestre". Eligiendo al azar año 1997.

# Consumo de gas diario correspondiente al primer semestre del año 1997.
Consumo1Semestre1997 <- sqldf("select Consumo, Semestre, Anio from consumo_historico_gas where Anio = 1997 and
                          Semestre = '1er Semestre' ")
# Consumo de gas diario correspondiente al segundo semestre del año 1997.
Consumo2Semestre1997 <- sqldf("select Consumo, Semestre, Anio from consumo_historico_gas where Anio = 1997 and
                          Semestre = '2do Semestre' ")

# Revisando el resumen de los nuevos datos procesados.
summary(Consumo1Semestre1997)
summary(Consumo2Semestre1997)

DiasSemestre1_1997 <- c(1:181) # numero de dias en el periodo correspondiente
# Representacion grafica del consumo de gas diario correspondiente al primer semestre del año 1997.
# BrainFoodChallenge/Fig10.pdf
plot(DiasSemestre1_1997,Consumo1Semestre1997$Consumo, type = "p", main="Consumo de Gas Diario en la Ciudad de 
     Pta.Arenas durante el Primer Semestre del año 1997", xlab = "1er Semestre Año 1997", 
     ylab = "Consumo de Gas (m3/dia)", ylim = c(300,1100))

# reg6 modelo del Consumo de diario correspondiente al primer semestre año 1997. Con un R^2 de 82,91%.
reg6 <- lm(Consumo1Semestre1997$Consumo~DiasSemestre1_1997)
summary(reg6) # revisando el resumen correspondiente
# Como se esperaba, nuevamente el modelo se ajusta bastante bien.
abline(reg6, col = "red")

DiasSemestre2_1997 <- c(1:184) # numero de dias en el periodo correspondiente
# Representacion grafica del consumo de gas diario correspondiente al segundo semestre del año 1997.
# BrainFoodChallenge/Fig11.pdf
plot(DiasSemestre2_1997,Consumo2Semestre1997$Consumo, type = "p", main="Consumo de Gas Diario en la Ciudad de 
     Pta.Arenas durante el Segundo Semestre del año 1997", xlab = "2do Semestre Año 1997", 
     ylab = "Consumo de Gas (m3/dia)", ylim = c(400,1150))

# reg7 modelo del Consumo de diario correspondiente al segundo semestre año 1997. Con un R^2 de 85,88%.
reg7 <- lm(Consumo2Semestre1997$Consumo~DiasSemestre2_1997)
summary(reg7) # revisando el resumen correspondiente
# Como se esperaba, nuevamente el modelo se ajusta bastante bien.
abline(reg7, col = "blue")

# ******************************************************************************************************************
# ********************** Busqueda Tendencias Promedios Diarios entre 1995 y 2002 por Semestre **********************
# ******************************************************************************************************************
# Consumo Promedio Por cada dia equivalente entre la data de 1995 y 2002. Ej. Consumo Promedio de 1-April de los
# años 1996, 1997, 1998, 1999, 2000, 2001 y 2002.
ConsumoPromedioDiario <- sqldf("select AVG(Consumo) as 'ConsumoPromedio', Semestre, dia_mes, Dia, Mes from 
                               consumo_historico_gas group by dia_mes order by Mes, Dia")

# Valores de Consumo de Primer Semestre -> Para Modelo 1
ConsumoPromDiario1Semestre <- sqldf("select ConsumoPromedio, Semestre from ConsumoPromedioDiario where Semestre = 
                                    '1er Semestre' ")

# Valores de Consumo de Segundo Semestre -> Para Modelo 2
ConsumoPromDiario2Semestre <- sqldf("select ConsumoPromedio, Semestre from ConsumoPromedioDiario where Semestre = 
                                    '2do Semestre' ")
# Chequeando resultados
summary(ConsumoPromDiario1Semestre)
summary(ConsumoPromDiario2Semestre)

# **************************************************** Modelo 1 ****************************************************
# Representacion grafica para Primer Modelo: consumo promedio diario de gas para el primer semestre
DiasSemestre1ConsumoProm <- c(1:182) # numero de dias en el periodo correspondiente
# Representacion grafica del Modelo de consumo promedio diario de gas para el primer semestre.
# BrainFoodChallenge/Fig12.pdf
plot(DiasSemestre1ConsumoProm,ConsumoPromDiario1Semestre$ConsumoPromedio, type = "p", main="Promedio de Consumo de 
     Gas Diario en la Ciudad de Pta.Arenas durante el Primer Semestre entre los años 1996 y 2002", xlab = "1er 
     Semestre entre los años 1996 y 2002", ylab = "Consumo de Gas (m3/dia)", ylim = c(400,1000))

# Para revalidar la correlacion entre el consumo de gas promedio y los dias, realizo el test de correlacion
cor.test(x = DiasSemestre1ConsumoProm, y = ConsumoPromDiario1Semestre$ConsumoPromedio, method = "pearson" )
# revalidando la intensidad de la relacion (r=0.9738013) y significativa (p-value< 2.2e-16)

# reg8 Promedio de Consumo de Gas Diario correspondiente al primer semestre entre los años 1996 y 2002
# lm() devuelve el valor de la variable y para x = 0 (interseccion) junto con la pendiente de la recta.
reg8 <- lm(ConsumoPromDiario1Semestre$ConsumoPromedio~DiasSemestre1ConsumoProm)
# revisando la informacion del modelo
summary(reg8)
# La primera columna (Estimate) devuelve el valor estimado para los dos parámetros de la ecuación del modelo lineal 
# (426.06103 y 2.96589) que equivalen a la ordenada en el origen y la pendiente.
# Se muestran los errores estándar, el valor del estadístico t y el p-value (dos colas) de cada uno de los dos 
# parámetros. Esto permite determinar si los parámetros son significativamente distintos de 0 (<2e-16), es decir, 
# que tienen importancia en el modelo. En estos modelos, el parámetro más informativo suele ser la pendiente.
# Para este modelo, tanto la ordenada en el origen como la pendiente son significativas (p-values < 0.05).
# Multiple R-squared:  0.9483. 94,83% de la variabilidad presente en la variable de consumo promedio de gas mediante
# la variable independiente, dias del primer semestre.
# El p-value obtenido en el test F (< 2.2e-16) determina que sí es significativamente superior la varianza explicada
# por el modelo en comparación a la varianza total. Es el parámetro que determina si el modelo es significativo y
# por lo tanto se puede aceptar.
# El modelo lineal generado sigue la ecuación:
# consumo promedio diario de gas para el primer semestre (m^3/dia) = 426.06103 + 2.96589 dias. Lo que se entiende 
# como que por cada dia corrido que transcurra durante el primer semestre el consumo de gas promedio diario se
# incrementa en promedio 2.96589 m^3. 

# Viendo los intervalos de confianza para los parametros del modelo.
confint(reg8)
#                                 2.5 %       97.5 %
# (Intercept)                415.313374   436.808694
# DiasSemestre1ConsumoProm     2.864022     3.067749

# Representacion grafica del modelo para Consumo del Primer Semestre
abline(reg8, col = "red")

# Además de la línea de mínimos cuadrados es recomendable incluir los límites superior e inferior del intervalo de 
# confianza. Esto permite identificar la región en la que, según el modelo generado y para un determinado nivel de
# confianza, se encuentra el valor promedio de la variable dependiente.Se añaden al gráfico líneas formadas por los 
# límites superiores e inferiores calculados para cada predicción.
# Representando el intervalo de confianza a lo largo de todo el modelo.
# Se genera una secuencia de valores x_i que abarquen todo el rango de las observaciones de la variable X
puntos1erSemestre <- seq(from = min(DiasSemestre1ConsumoProm), to = max(DiasSemestre1ConsumoProm), length.out = 100)

# Se predice el valor de la variable "Y" junto con su intervalo de confianza para cada uno de los puntos generados.
# En la función predict() hay que nombrar a los nuevos puntos con el mismo nombre que la variable X del modelo.
# Devuelve una matriz.
limites_intervalo1erSemestre <- predict(object = reg8,
                                        newdata = data.frame(DiasSemestre1ConsumoProm = puntos1erSemestre),
                                        interval = "confidence", level = 0.95)

head(limites_intervalo1erSemestre, 3) 
#         fit      lwr      upr
# 1  429.0269 418.3675 439.6864
# 2  434.4494 423.9506 444.9482
# 3  439.8719 429.5329 450.2109

# Finalmente se añaden al gráfico las líneas formadas por los límites superior e inferior.
lines(x = puntos1erSemestre, y = limites_intervalo1erSemestre[,2],type = "l", col = 2, lty = 3)
lines(x = puntos1erSemestre, y = limites_intervalo1erSemestre[,3],type = "l", col = 3, lty = 3)

# Verificando condiciones para poder aceptar el modelo lineal Promedio de Consumo de Gas Diario correspondiente al 
# primer semestre entre los años 1996 y 2002

# La función lm() calcula y almacena los valores predichos por el modelo y los residuos.
ConsumoPromDiario1Semestre$prediccion <- reg8$fitted.values
ConsumoPromDiario1Semestre$residuos   <- reg8$residuals
head(ConsumoPromDiario1Semestre)

# Los residuos se distribuyen de forma aleatoria entorno al 0 por lo que se acepta la linealidad. 
# BrainFoodChallenge/Fig13.pdf
plot(ConsumoPromDiario1Semestre$prediccion,ConsumoPromDiario1Semestre$residuos, type = "p")

# Se puede apreciar como los residuos se distribuyen de forma normal con media 0.
# BrainFoodChallenge/Fig14.pdf
hist(ConsumoPromDiario1Semestre$residuos)

# Considerare la representación gráfica como evidencia de presencia de homocedasticidad en el modelo.
# Aunque es recomendable realizar el contraste de hipótesis.

# **************************************************** Modelo 2 ****************************************************
# Representacion grafica para Segundo Modelo: consumo promedio diario de gas para el segundo semestre
DiasSemestre2ConsumoProm <- c(1:184) # numero de dias en el periodo correspondiente
# Representacion grafica del Modelo de consumo promedio diario de gas para el segundo semestre.
# BrainFoodChallenge/Fig15.pdf
plot(DiasSemestre2ConsumoProm,ConsumoPromDiario2Semestre$ConsumoPromedio, type = "p", main="Promedio de Consumo de 
     Gas Diario en la Ciudad de Pta.Arenas durante el Segundo Semestre entre los años 1995 y 2002", xlab = "2do 
     Semestre entre los años 1995 y 2002", ylab = "Consumo de Gas (m3/dia)", ylim = c(400,1000))

# Para revalidar la correlacion entre el consumo de gas promedio y los dias, realizo el test de correlacion
cor.test(x = DiasSemestre2ConsumoProm, y = ConsumoPromDiario2Semestre$ConsumoPromedio, method = "pearson" )
# revalidando la intensidad de la relacion negativa (r=-0.9705867) y significativa (p-value< 2.2e-16)

# reg9 Promedio de Consumo de Gas Diario correspondiente al segundo semestre entre los años 1995 y 2002
# lm() devuelve el valor de la variable y para x = 0 (interseccion) junto con la pendiente de la recta.
reg9 <- lm(ConsumoPromDiario2Semestre$ConsumoPromedio~DiasSemestre2ConsumoProm)
# revisando la informacion del modelo
summary(reg9)
# La primera columna (Estimate) devuelve el valor estimado para los dos parámetros de la ecuación del modelo lineal 
# (1013.09428 y  -2.73773) que equivalen a la ordenada en el origen y la pendiente.
# Comparando esto ultimo con el primer modelo, se aprecia la diferencia de la ordenada en el origen considerablemente
# mayor (Modelo 1 = 426.06103, Modelo 2 = 1013.09428). Esto se explica por el consumo del Modelo 1 inicia en el
# primer semestre cuando es verano y se espera que el consumo de gas sea menor que en invierno que es cuando inicia
# el periodo del modelo 2. Por lo mismo el consumo de gas se espera que sea creciente en el Modelo 1 
# (pendiente = 2.96589) y decreciente en el Modelo 2 (pendiente = -2.73773). El resto de los resultados son bastante
# similares al primer modelo.
# Se muestran los errores estándar, el valor del estadístico t y el p-value (dos colas) de cada uno de los dos 
# parámetros. Esto permite determinar si los parámetros son significativamente distintos de 0 (<2e-16), es decir, 
# que tienen importancia en el modelo. En estos modelos, el parámetro más informativo suele ser la pendiente.
# Para este modelo, tanto la ordenada en el origen como la pendiente son significativas (p-values < 0.05).
# Multiple R-squared: 0.942. 94,2% de la variabilidad presente en la variable de consumo promedio de gas mediante
# la variable independiente, dias del segundo semestre.
# El p-value obtenido en el test F (< 2.2e-16) determina que sí es significativamente superior la varianza explicada
# por el modelo en comparación a la varianza total. Es el parámetro que determina si el modelo es significativo y
# por lo tanto se puede aceptar.
# El modelo lineal generado sigue la ecuación:
# consumo promedio diario de gas para el segundo semestre (m^3/dia) = 1013.09428 - 2.73773 dias. Lo que se entiende 
# como que por cada dia corrido que transcurra durante el segundo semestre el consumo de gas promedio diario se
# reduce en promedio 2.73773 m^3. 

# Viendo los intervalos de confianza para los parametros del modelo.
confint(reg9)
#                                  2.5 %        97.5 %
# (Intercept)                1002.500315   1023.688254
# DiasSemestre2ConsumoProm     -2.837046     -2.638406
 
# Representacion grafica del modelo para Consumo del Segundo Semestre
abline(reg9, col = "blue")

# Al igual que en el primer modelo, se representara el intervalo de confianza a lo largo de todo el modelo.
# Se genera una secuencia de valores x_i que abarquen todo el rango de las observaciones de la variable X
puntos2doSemestre <- seq(from = min(DiasSemestre2ConsumoProm), to = max(DiasSemestre2ConsumoProm))

# Se predice el valor de la variable "Y" junto con su intervalo de confianza para cada uno de los puntos generados.
# En la función predict() hay que nombrar a los nuevos puntos con el mismo nombre que la variable X del modelo.
# Devuelve una matriz.
limites_intervalo2doSemestre <- predict(object = reg9,
                                        newdata2 = data.frame(DiasSemestre2ConsumoProm = puntos2doSemestre),
                                        interval = "confidence", level = 0.95)

head(limites_intervalo2doSemestre, 3) 
#         fit      lwr      upr
# 1  1010.357 999.8486 1020.865
# 2  1007.619 997.1967 1018.041
# 3  1004.881 994.5445 1015.218

# Finalmente se añaden al gráfico las líneas formadas por los límites superior e inferior.
lines(puntos2doSemestre,limites_intervalo2doSemestre[,2],type = "l", col = 2, lty = 3)
lines(puntos2doSemestre,limites_intervalo2doSemestre[,3],type = "l", col = 3, lty = 3)

# Verificando condiciones para poder aceptar el modelo lineal Promedio de Consumo de Gas Diario correspondiente al 
# segundo semestre entre los años 1995 y 2002

# La función lm() calcula y almacena los valores predichos por el modelo y los residuos.
ConsumoPromDiario2Semestre$prediccion <- reg9$fitted.values
ConsumoPromDiario2Semestre$residuos   <- reg9$residuals
head(ConsumoPromDiario2Semestre)

# Los residuos se distribuyen de forma aleatoria entorno al 0 por lo que se acepta la linealidad.
# BrainFoodChallenge/Fig16.pdf
plot(ConsumoPromDiario2Semestre$prediccion,ConsumoPromDiario2Semestre$residuos, type = "p")

# Se puede apreciar como los residuos se distribuyen de forma normal con media 0, menor que en el modelo 1, pero
# para este caso aceptable. BrainFoodChallenge/Fig17.pdf
hist(ConsumoPromDiario2Semestre$residuos)

# Considerare al igual que en el modelo1, la representación gráfica como evidencia de presencia de homocedasticidad 
# en el modelo. Aunque es recomendable realizar el contraste de hipótesis.

# ******************************************************************************************************************
# ******************************************** Conclusiones y Resultados *******************************************
# ******************************************************************************************************************

# **************************************************** Modelo 1 ****************************************************
# consumo promedio diario de gas para el primer semestre (m^3/dia) = 426.06103 + 2.96589 dias.
# Conclusión:
# Dado que se satisfacen las condiciones para considerar válido el modelo 1 de regresión lineal por mínimos cuadrados
# y que el p-value (< 2.2e-16) indica que el ajuste es significativo, y el buen numero de R^2 (0.9483). Se considera
# finalmente valido el modelo, por lo cual en el transcurso de dias es ideal para predecir el consumo promedio de
# gas. 

# *Modelo de regresión lineal Promedio de Consumo de Gas Diario correspondiente al primer semestre entre los años 
# 1996 y 2002

# **************************************************** Modelo 2 ****************************************************
# consumo promedio diario de gas para el segundo semestre (m^3/dia) = 1013.09428 - 2.73773 dias.
# Conclusión:
# AL igual que en el primer modelo, se aceptan las condiciones para considerar válido el modelo 2 de regresión lineal
# por mínimos cuadrados. Con un p-value (< 2.2e-16) indica que el ajuste es significativo, y nuevamente un buen valor
# de R^2 ( 0.942). Se considera finalmente valido el modelo, por lo cual en el transcurso de dias es ideal para 
# predecir el consumo promedio de gas. 

# *Modelo de regresión lineal Promedio de Consumo de Gas Diario correspondiente al segundo semestre entre los años 
# 1995 y 2002

# *************************************************** Resultados ***************************************************

# Para contestar a las interrogantes planteadas donde el interés primordial es obtener predicciones diarias de 1 a 7
# días, además, semanales de 1-5 semanas, y mensuales de 1 a 3 meses; se pueden considerar los siguientes criterios.
# Dado a la metodologia utilizada para obtener los modelos de prediccion de consumo de gas en la ciudad de Punta 
# Arenas, se deberá tener en consideracion para qué periodo se desea conocer la estamacion. Ya que de acuerdo a lo
# comentado a lo largo del desarrollo de los modelos, dependerá si se desea estimar para un periodo correspondiente
# al primer o segundo semestre de un año, debido a las tendencias del consumo.

# De acuerdo a lo anterior se satisfaceran las predicciones solicitadas para el primer y segundo semestre.
# consumo promedio diario de gas para el primer semestre (m^3/dia) = 426.06103 + 2.96589 dias.
# consumo promedio diario de gas para el segundo semestre (m^3/dia) = 1013.09428 - 2.73773 dias.

# prediccion de consumo de gas de 1 a 7 días para primer y segundo semestre:
dias <- c(1:7)
PrediccionDiaria <- as.data.frame(dias)

for (j in 1:nrow(PrediccionDiaria)) {
  PrediccionDiaria$ConsumoEstimado1Semestre[j] <- (PrediccionDiaria$dias[j]*2.96589 + 426.06103)
  PrediccionDiaria$ConsumoEstimado2Semestre[j] <- (PrediccionDiaria$dias[j]*-2.73773 + 1013.09428)
}

PrediccionDiaria
summary(PrediccionDiaria)
# PromedioConsumoEstimado1Semestre es de 437.9 m^3. PromedioConsumoEstimado2Semestre es de 1002.1 m^3.
# La prediccion del consumo por dia se encuentra en el DataFrame PrediccionDiaria

# prediccion de consumo de gas de 1 a 5 semanas para primer semestre. 5 semanas (35 dias):
dias2 <- c(1:35)
PrediccionSemanal <- as.data.frame(dias2)

for (k in 1:nrow(PrediccionSemanal)) {
  PrediccionSemanal$ConsumoEstimado1Semestre[k] <- (PrediccionSemanal$dias[k]*2.96589 + 426.06103)
  PrediccionSemanal$ConsumoEstimado2Semestre[k] <- (PrediccionSemanal$dias[k]*-2.73773 + 1013.09428)
}

PrediccionSemanal
summary(PrediccionSemanal)
# PromedioConsumoEstimado1Semestre de 479.4 m^3. PromedioConsumoEstimado2Semestre es de 963.8 m^3.
# La prediccion del consumo por dia se encuentra en el DataFrame PrediccionSemanal

# prediccion de consumo de gas de 1 a 3 meses para primer semestre. 3 semanas (90 dias):
dias3 <- c(1:90)
PrediccionMensual <- as.data.frame(dias3)

for (l in 1:nrow(PrediccionMensual)) {
  PrediccionMensual$ConsumoEstimado1Semestre[l] <- (PrediccionMensual$dias[l]*2.96589 + 426.06103)
  PrediccionMensual$ConsumoEstimado2Semestre[l] <- (PrediccionMensual$dias[l]*-2.73773 + 1013.09428)
}

PrediccionMensual
summary(PrediccionMensual)
# PromedioConsumoEstimado de 561 m^3. PromedioConsumoEstimado2Semestre es de 888.5 m^3.
# La prediccion del consumo por dia se encuentra en el DataFrame PrediccionMensual


