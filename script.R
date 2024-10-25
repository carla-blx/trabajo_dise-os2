# Cargar librerias
library(astsa)
library(forecast)
library(dplyr)
library(zoo)

# Leer data
quet <- read.csv("quet.csv")
head(quet)  #visualizar 5 primeras observaciones

# Convertir los datos de dispensaciones en un objeto de serie temporal
quet.ts <- ts(quet[,2], frequency=12, start=c(2011,1)) # Visualizar la serie temporal completa quet.ts

# Graficar la serie temporal de dispensaciones
plot(quet.ts, ylim=c(0,40000), type='l', col="blue", xlab="Mes", ylab="Dispensaciones") 

# Agregar una línea vertical para indicar la fecha de intervención (1 de enero de 2014)
abline(v=2014, col="gray", lty="dashed", lwd=2)

# Visualizar las funciones de autocorrelación (ACF) y autocorrelación parcial (PACF) de los datos sin diferenciar
acf2(quet.ts, max.lag=24)

# Visualizar las funciones ACF y PACF de los datos diferenciados estacionalmente
acf2(diff(diff(quet.ts,12)), max.lag=24)

# Crear una variable que representa un cambio abrupto (escalón) a partir de enero de 2014
step <- as.numeric(time(quet.ts) >= 2014) # Toma el valor 1 desde 2014 en adelante

# Mostrar la variable 'step'
step

# Crear una variable que representa un cambio gradual (rampa) a partir de enero de 2014
ramp <- append(rep(0,36), seq(1,12,1)) # Valores de 1 a 12 para los meses de 2014

# Mostrar la variable 'ramp'
ramp

# Utilizar un algoritmo automatizado para identificar los mejores parámetros del modelo ARIMA
# Especificando diferencias de primer orden y diferencias estacionales de primer orden
model1 <- auto.arima(quet.ts, seasonal=TRUE, xreg=cbind(step, ramp), max.d=1, max.D=1, stepwise=FALSE, trace=TRUE)

# Verificar los residuales del modelo para asegurarse de que sean ruido blanco
checkresiduals(model1)

# Realizar la prueba de Ljung-Box con 24 rezagos
Box.test(model1$residuals, lag = 24, type = "Ljung-Box")

# Resumen del modelo ajustado y estimación de intervalos de confianza para los parámetros
summary(model1)
confint(model1)

# Ajustar el modelo ARIMA a los datos hasta diciembre de 2013 para pronosticar el contrafactual
model2 <- Arima(window(quet.ts, end=c(2013,12)), order=c(2,1,0), seasonal=list(order=c(0,1,1), period=12))

# Realizar pronósticos para los 12 meses posteriores a la intervención
fc <- forecast(model2, h=12)

# Convertir los pronósticos en una serie temporal
fc.ts <- ts(as.numeric(fc$mean), start=c(2014,1), frequency=12)

# Combinar los datos observados con los pronósticos contrafactuales
quet.ts.2 <- ts.union(quet.ts, fc.ts)

# Graficar los datos observados y los pronósticos contrafactuales
plot(quet.ts.2, type="l", plot.type="s", col=c('blue','red'), xlab="Month", ylab="Dispensings", linetype=c("solid","dashed"), ylim=c(0,40000))

# Corregir las advertencias eliminando el parámetro incorrecto
# Agregar una línea vertical para indicar la intervención
abline(v=2014, lty="dashed", col="gray")

# Agregar puntos a las series para mejorar la visualización
points(time(quet.ts.2), quet.ts.2[,1], pch=19, col='blue')
points(time(quet.ts.2), quet.ts.2[,2], pch=19, col='red')





