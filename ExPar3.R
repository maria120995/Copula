# ExPar3.R
#
# Nombre alumno: Garcia Rodriguez Maria Guadalupe

#----------------------------------------------------------

install.packages("rriskDistributions")
install.packages("goftest")
install.packages("VineCopula")
install.packages("copula")
install.packages("subcopem2D")
install.packages("ADGofTest")
install.packages("fitdistrplus")
install.packages("rriskDistributions")

## Paquetes requeridos
library(VineCopula)
library(goftest)
library(copula)
library(subcopem2D)
library(ADGofTest)
library(fitdistrplus)
library(rriskDistributions)

# y cualquier otro necesario

set.seed(50)   #Para que nos salga lo mismo jiji 
## Cargar datos


datos <- read.csv(file = file.choose()) # leer archivo: datos.csv
str(datos) # verificar estructura de los datos


## Breve explicación sobre los datos:
#Los conjuntos de datos constan de varias variables predictoras médicas (independientes) y
#una variable objetivo (dependiente), Resultado. Las variables independientes incluyen la 
#cantidad de embarazos que ha tenido el paciente, su IMC, nivel de insulina, edad, etc.
#No cuento con información sobre el país o el año donde se realizo esta encuesta.

# Proporcionar la fuente de descarga de los datos, qué miden las
# variables (X,Y) y por qué es de esperarse que exista dependencia
# significativa entre ellas: https://www.kaggle.com/uciml/pima-indians-diabetes-database
##La variable x  corresponde al "Espesor de la pielTriceps grosor del pliegue de la piel (mm)"
#y la variable "BMI" corresponde al indice de masa corporal (peso en kg / (altura en m) ^ 2)

## Estadística descriptiva marginal

# Generar en una sola ventana de gráficos dos histogramas, uno 
# por cada variable, y resumen numérico con la función summary()

Datos1 <- datos[,1]
Datos2<- datos[,2]


par(mfrow=c(1,2))

hist(Datos1,probability = T, main ="SkinThickness", col = "aquamarine")
hist(Datos2,probability = T, main="BMI", col="mediumorchid1")

summary(Datos1)
summary(Datos2)


## Estadística descriptiva conjunta

# Generar en una sola ventana de gráficos un diagrama de dispersión
# con los datos observados de (X,Y), y en otro las pseudo-observaciones.



MatXY<-data.matrix(datos)


SC<- subcopem2D::subcopemc(MatXY,50, display = T)

dev.new()
par(mfrow =c(1,2))
plot(MatXY, main = "Observaciones (X,Y)")
plot(SC$std.sample, main = "Pseudo-observaciones", xlab = "u", ylab = "v")



# Calcular correlación de Pearson, las medidas de concordancia de
# Spearman y Kendall, la medida de dependencia monótona de Erdely
# y la medida de dependencia supremo. 

rho_2<-cor(MatXY,method = "spearman")[1,2]
tau_2<-cor(MatXY,method = "kendall")[1,2]
pearson_2<-cor(MatXY,method = "pearson")[1,2]
Erdely <- SC$DEdepMonNonSTD
Supremo <- SC$depSupNonSTD
rho_2
tau_2
pearson_2
Erdely
Supremo


#Todas mis medidas resultan positivas, por lo que descarto una dependencia decreciente, además de que
#todas están alrededor de 0.5, 0.6 por lo que son bastante dependientes, con dependencia positiva.

## Ajuste de marginales

# Determinar si hay modelos paramétricos univariados que se ajusten
# adecuadamente a las marginales, o si es necesario una estimación
# no paramétrica, y en dado caso hacerla.


candidatos1<- fit.cont(Datos1)
candidatos1$chosenDistr  #normal
candidatos1$fittedParams  # mean= 29.16543  sd=10.48670 
candidatos2<- fit.cont(Datos2)
candidatos2$chosenDistr   # normal
candidatos2$fittedParams  #   mean=32.894424     sd=6.871784 
  


#Para ver que tal se ajustan las teoricas a mis histogramas
par(mfrow=c(1,2))
hist(Datos1,probability = T, main ="SkinThickness", col = "aquamarine")
curve(dnorm(x,candidatos1$fittedParams[1],candidatos1$fittedParams[2]),add=TRUE, col="palevioletred1", lwd=2)


hist(Datos2,probability = T, main="BMI", col="mediumorchid1")
curve(dnorm(x,candidatos2$fittedParams[1],candidatos2$fittedParams[2]),add=TRUE, col="lightcyan3", lwd=2)


#Prueba de bondad y ajuste de AD
#Volvi a correr todo mi código, y me marco error por ponerle distr.fun = pnorm, 
#si se lo quito corre bien, pero la primera vez usted me lo regreso porque
#no le corrio por no poner tal cual así, no sé porque pasa esto, me corria con el y luego sin
#el, no se que pasa ahi :(, al final lo deje como lo usted me lo acepto, espero su lap
#no este loca como la mia, agrego la forma en la que mi lap corrio y en la que usted
#me acepto la primera correción


Ad<-ad.test(Datos1, distr.fun=pnorm,mean=candidatos1$fittedParams[1], sd=candidatos1$fittedParams[2])
Ad   # p-value = 0.3261

Ad_otro<-ad.test(Datos1, pnorm,mean=candidatos1$fittedParams[1], sd=candidatos1$fittedParams[2])
Ad_otro  #p-value = 0.3261

Ad1<-ad.test(Datos2,distr.fun = pnorm,mean=candidatos2$fittedParams[1], sd=candidatos2$fittedParams[2])
Ad1  # p-value = 0.2476

Ad1_otro<-ad.test(Datos2,pnorm,mean=candidatos2$fittedParams[1], sd=candidatos2$fittedParams[2])
Ad1_otro   # p-value = 0.2476

## Ajuste paramétrico de cópula bivariada

# Con base en las pseudo-observaciones, analizar si alguna cópula
# paramétrica los puede representar adecuadamente, o si es necesario
# una estimación no paramétrica


#Estas son las Cópulas que a mi parecer podrían ajustarse más otras para ver que tan lejos andaba.
 Prueba.Frank <-gofCopula(frankCopula(),SC$std.sample) #statistic = 0.020984, parameter = 5.4997, p-value = 0.1863
# prueba2 <-gofCopula(normalCopula(),SC$std.sample)  # p-value = 0.06144
# prueba3 <-gofCopula(gumbelCopula(),SC$std.sample)  #  p-value = 0.0004995
# prueba5 <-gofCopula(claytonCopula(),SC$std.sample) #p-value = 0.0004995
# prueba6 <- gofCopula(joeCopula(),SC$std.sample) 




## Estimación de distribución conjunta bivariada

# Por medio del paquete cópula construir la distribución conjunta
# estimada, simular con ella una muestra del mismo tamaño que los
# datos originales.


                                                                                                                             
distrXY <-mvdc(frankCopula(iRho(frankCopula(),rho_2)),margins=c("norm","norm"), list(list(mean=candidatos1$fittedParams[1],sd=candidatos1$fittedParams[2]), list(mean=candidatos2$fittedParams[1], sd=candidatos2$fittedParams[2]))) 
n<-length(Datos1)


matXY_simulada <- rMvdc(n, distrXY)
# Comparar gráficamente lo simulado versus lo observado (gráficos
# de dispersión, uno junto al otro).

par(mfrow=c(1,2))
plot(MatXY, main = "Observaciones (X,Y)", xlim=c(0,60), ylim=c(0,60))
plot(matXY_simulada, main = "Observaciones simuladas (X.Y)", xlim=c(0,60), ylim=c(0,60))

# Comparar numéricamente lo simulado versus lo observado por medio
# de la función summary(), medidas de concordancia y dependencia.




resumen1 <-summary(datos)
resumen2 <-summary(matXY_simulada)

#Comparando los valores de la variable X
c(resumen1[1,1], resumen2[1,1])
c(resumen1[2,1], resumen2[2,1])
c(resumen1[3,1], resumen2[3,1])
c(resumen1[4,1], resumen2[4,1])
c(resumen1[5,1], resumen2[5,1])

#Comparando los valores de la variable y

c(resumen1[1,2], resumen2[1,2])
c(resumen1[2,2], resumen2[2,2])
c(resumen1[3,2], resumen2[3,2])
c(resumen1[4,2], resumen2[4,2])
c(resumen1[5,2], resumen2[5,2])

#Comparando las medidas de dependencia y de concordancia
SC_simulada<- subcopem2D::subcopemc(matXY_simulada,50, display = F)

rho_2_simulada<-cor(matXY_simulada ,method = "spearman")[1,2]
tau_2_simulalda<-cor(matXY_simulada ,method = "kendall")[1,2]
pearson_2_simulada<-cor(matXY_simulada ,method = "pearson")[1,2]
Erdely_simulada <- SC_simulada$DEdepMonNonSTD
Supremo_simulada <- SC_simulada$depSupNonSTD

#Comparando Rho con spearman
c(rho_2,rho_2_simulada)  #0.6855248 0.7023137
c(tau_2,tau_2_simulalda) #0.4979297 0.5034371
c(pearson_2,pearson_2_simulada) # 0.6483004 0.6782690
c(Erdely,Erdely_simulada)  
c(Supremo,Supremo_simulada) #0.5288208 0.5836431

#Las medidas de concordancia se dejan "engañar" un poquito, pero la medida de dependencia Supremo 
#no se deja engañar tan fácil y castiga :) 


#Regresión

u <-rnorm(length(Datos1),mean=candidatos1$fittedParams[1],sd=candidatos1$fittedParams[2])
Fx <- pnorm(Datos1,mean=candidatos1$fittedParams[1],sd=candidatos1$fittedParams[2])
YdadoX <-function(x,y, fam, parametro){BiCopHfunc1(x,y, family = fam, par=parametro)}
raiz <- function(x){uniroot(function(y) YdadoX(x,y,5,5.4997) - (0.5), c(0,1))$root}
mediana<-mapply(raiz,Fx)
regresion <- qnorm(mediana, candidatos2$fittedParams[1], candidatos2$fittedParams[2])

dev.new()
plot(MatXY, xlab="X", ylab="Y", main = "Curva de regresión")
lines(sort(u),sort(regresion), type = "l", col="palevioletred", lwd=4)



