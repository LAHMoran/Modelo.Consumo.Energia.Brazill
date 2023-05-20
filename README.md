# Modelo.Consumo.Energia.Brazill
Modelo de consumo de energía domestica de 1980 a 2019 

attach(DB_Brazil)
# Paso 1: Declarar las variables
Yt <- ts(y, frequency = 1, start = c(1980))
ELE <- ts(ele, frequency = 1, start = c(1980))
PRELt <- ts(Prel, frequency = 1, start = c(1980))
TMt <- ts(tm, frequency = 1, start = c(1980))

# Paso 2: Generar diferencias
dly <- diff(Yt)
dlele <- diff(ELE)
dlpr <- diff(PRELt)
dltm <- diff(TMt)
dltm2 <- diff((TMt)^2)

# Paso 3: Instalar y cargar el paquete "dyn"
install.packages("dyn")
library(dyn)

# Paso 4: Crear el objeto ltm2
ltm2 <- I(log(TMt)^2)

# Paso 5: Creación de objetos de diferencia de variables
modlp <- lm(log(ELE) ~ log(Yt) + log(PRELt) + log(TMt) + ltm2)
summary(modlp)

modlps <- lm(log(ELE) ~ log(Yt) + log(PRELt) + log(TMt) + ltm2 + 0)
summary(modlps)

# Paso 6: Criterio para seleccionar el modelo

# Paso 7: Modelo dinámico con corrección de errores
mcei <- ts(modlp$residuals, start = 1980, frequency = 1)
mcen <- ts(modlps$residuals, start = 1980, frequency = 1)

modcp1 <- dyn$lm(dlele ~ dly + dlpr + dltm + lag(mcei, -1) + 0)
summary(modcp1)

modcp2 <- dyn$lm(dlele ~ dly + dlpr + dltm + lag(mcen, -1) + 0)
summary(modcp2)

# Paso 8: Metodología de Hendy (de lo general a lo particular)
modcp <- dyn$lm(dlele ~ dly + lag(dly, -1) + lag(dly, -2) + lag(dly, -3) +
                  lag(dly, -4) + dlpr + lag(dlpr, -1) + lag(dlpr, -2) +
                  lag(dlpr, -3) + lag(dlpr, -4) + dltm + lag(dltm, -1) +
                  lag(dltm, -2) + lag(dltm, -3) + lag(dltm, -4) + dltm2 +
                  lag(dltm2, -1) + lag(dltm2, -2) + lag(dltm2, -3) +
                  lag(dltm2, -4) + lag(mcei, -1) + 0)
summary(modcp)

# Paso 1: Instalar el paquete lmtest si no está instalado
install.packages("lmtest")

# Paso 2: Cargar el paquete lmtest
library(lmtest)

# Paso 3: Generar los residuos del modelo y almacenarlos en una variable
residuos <- residuals(modcp2)

# Paso 4: Realizar el correlograma de los residuos
acf(residuos)

# Paso 5: Realizar la prueba Breusch-Godfrey para autocorrelación
bgtest(modcp2, order = 2)  # Prueba de orden 2
bgtest(modcp2, order = 3)  # Prueba de orden 3

#########################################################################333
Clase 19 Mayo
#primero attach con la base

attach(DB_Brazil)
# Paso 1: Declarar las variables
Yt <- ts(y, frequency = 1, start = c(1980))
ELE <- ts(ele, frequency = 1, start = c(1980))
PRELt <- ts(Prel, frequency = 1, start = c(1980))
TMt <- ts(tm, frequency = 1, start = c(1980))
#nombrar de nuevo objeto

#crear logaritmo

ly <- log(Yt)
lele <- log(ELE)
lpr <- log(PRELt)
ltm <- log(TMt)
ltm2 <- log((TMt)^2)

#convertir nuestras series a series de tiempo

Yt <- ts(y,frequency=1,start=c(1980))
ELE=ts(ele,frequency=1,start=c(1980))
PRELt = ts(Prel,frequency=1,start=c(1980))
TMt = ts(tm,frequency= 1,start=c(1980))
#falta declarar este
TMt2 = ts(tm2,frequency= 1,start=c(1980))
#doble logaritmo

modeledl <- lm(lele~ly+lpr+ltm);summary(modeledl)
modelenv <- lm(ELE~Yt+PRELt+TMt);summary(modelenv)

#Extraer Residuales
reseledl<-modeledl$residuals
reselenv<-modelenv$residuals

#histogramas
hist(reseledl)
hist(reselenv)
#paquetes a intalar
install.packages("ggplot")
install.packages("car")
install.packages("stats")
install.packages("moments")
#llamar a librerias
library("car")
library(ggplot)
library(stats)
library(moments)
#grafica para ver si hay datos atipicos
qqPlot(reseledl)
qqPlot(reselenv)
#pruebas necesarias para instalar
#prueba jaque bera 5.99
#JB de modelo doble - log
29*(((skewness(reseledl)^2)/6)+(((kurtosis(reseledl)-3)^2)/24))
29*(((skewness(reselenv)^2)/6)+(((kurtosis(reselenv)-3)^2)/24))
#pruebas Jaque Bera
jarque.test(reseledl)
jarque.test(reselenv)
#hacer grafico de estimados y observados
lele_hat <- modeledl$fitted.values
lele_hat <- ts(lele_hat, start = c(1990), frequency = 1)
plot(lele)
lines(lele_hat, col = "green")

ELE_hat<-modelenv4fitted.values
#############33
#prueba de normalidad a modelo de corto plazo, el que no lleva intercepto, que esta en diferencias de logaritmos
# las pruebas son qqploy y pruebas Jaque Bera
########################################################################################################
