#PRIMER PARCIAL INTRO. AL ANALISIS MULTIVARIADO

-----------------------------------------------------------------------------------------------------

uno <- read.table("parcial1.txt", header = T) #Leer la base de datos

#Funcion que genera una muestra aleatoria de tamaño 70
genera <- function(cedula) {
  set.seed(cedula)
  data <- uno[sample(1:2100,70),]
  data
}

datos <- genera(1000752735) #Generar la muestra

-----------------------------------------------------------------------------------------------------

#1. Calcule el vector de medias y la  matriz  de covarianzas muestrales
#para las variables Masa Per_abdo Long_pie Long_man Estatura. 
#Comente sus resultados. Repita  el proceso discriminando por SEXO. 
#¿Observa diferencias entre ambos cálculos? Comente. 

#Vector de medias
vectMedias <- as.numeric(apply(datos[,2:6], 2, mean)) 
vectMedias

#Matriz de varianzas y covarianzas
matVar <- round(var(datos[,2:6]),2)
matVar

#Para ilustrar las varianzas y covarianzas, se realiza el siguiente gráfico
library(plotrix)
library(GGally)
ggpairs(datos[,2:6], diag=list(continuous = wrap("densityDiag",color="red",
                                           fill="lightgoldenrod1",alpha=0.3)),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1,method = "lm")),
        upper = list(continuous = wrap("cor", stars = F)))

hombre <- datos[datos[,1]=="Hom",] 
mujer <- datos[datos[,1]=="Muj",]

#Vectores de medias discriminado por Sexo
mHombre <- as.numeric(apply(hombre[,2:6], 2, mean))
mMujer <- as.numeric(apply(mujer[,2:6], 2, mean))
mHombre
mMujer

#Matrices de varianzas y covarianzas discriminado por sexo
varHombre <- round(var(hombre[,2:6]),2)
varMujer <- round(var(mujer[,2:6]),2)
varHombre
varMujer

#Para ilustrar mejor si existe una diferencia entre los hombres y las 
#mujeres, se realizan las siguientes gráficas.
#Lo punto rojos denotan sus medias.

library(ggplot2)
library(cowplot)

#Box plots
plotMasa <- ggplot(datos, aes(x=Sexo, y=Masa, fill=Sexo))+
  geom_boxplot(alpha=0.7)+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red",
               fill="red")+
  theme(legend.position="none")+
  labs(title="Masa vs Sexo")
  
plotAbdo <- ggplot(datos, aes(x=Sexo, y=Per_abdo, fill=Sexo))+
  geom_boxplot(alpha=0.7)+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red",
               fill="red")+
  theme(legend.position="none")+
  labs(title="Per. Abdominal vs Sexo",y="Perímetro Abdominal")

plotPie <- ggplot(datos, aes(x=Sexo, y=Long_pie, fill=Sexo))+
  geom_boxplot(alpha=0.7)+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red",
               fill="red")+
  theme(legend.position="none")+
  labs(title="Long. promedio de pies vs Sexo",y="Long. promedio de Pies")

plotMano <- ggplot(datos, aes(x=Sexo, y=Long_man, fill=Sexo))+
  geom_boxplot(alpha=0.7)+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red",
               fill="red")+
  theme(legend.position="none")+
  labs(title="Long. promedio de manos vs Sexo",y="Long. promedio de manos")

plotEst <- ggplot(datos, aes(x=Sexo, y=Estatura, fill=Sexo))+
  geom_boxplot(alpha=0.7)+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red",
               fill="red")+
  theme(legend.position="none")+
  labs(title="Estatura vs Sexo")

plot_grid(plotMasa, plotAbdo, plotPie, plotMano, plotEst)

-----------------------------------------------------------------------------------------------------

#2. Elabore un histograma para la variable Per_abdo. ¿Puede deducirse del
#mismo que la distribución de probabilidad de esta variable puede ser una
#normal? Verifique este supuesto, planteando las respectivas hipótesis.
#Si el resultado no es favorable, usando la metodologia Box y Cox, 
#encuentre una transformacion que le garantice normalidad y muestre si en
#efecto esto se logra.
#Debe aportar la evidencia de los resultados obtenidos en R.

#Histograma base
ggplot(datos, aes(x=Per_abdo))+ 
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  labs(title="Histograma: Perimetro Abdominal", 
       x="Perimetro Abdominal", y="Frecuencia")

#Histograma con número de intervalos optimos (Según Sturges)
k <- round(1+3.322*log10(nrow(datos))) #Num. intervalos
a <- (max(datos[,3])-min(datos[,3]))/k #Amplitud intervalos

ggplot(datos, aes(x=Per_abdo))+
  geom_histogram(binwidth=a, fill="#69b3a2", color="#e9ecef", 
                 alpha=0.9)+
  labs(title="Histograma: Perimetro Abdominal", 
       x="Perimetro Abdominal", y="Frecuencia")

#Histograma de Perimetro Abdominal discriminado por Sexo

kh <- round(1+3.322*log10(nrow(hombre))) #Num. intervalos
ah <- (max(hombre[,3])-min(hombre[,3]))/kh #Amplitud intervalos

km <- round(1+3.322*log10(nrow(mujer))) #Num. intervalos
am <- (max(mujer[,3])-min(mujer[,3]))/km #Amplitud intervalos

histHombre <- ggplot(hombre, aes(x=Per_abdo))+
  geom_histogram(binwidth=ah, fill="#ed5509", color="#e9ecef", 
                 alpha=0.9)+
  labs(title="Histograma: Perimetro Abdominal de Hombres", 
       x="Perimetro Abdominal", y="Frecuencia")

histMujer <- ggplot(mujer, aes(x=Per_abdo))+
  geom_histogram(binwidth=am, fill="#07a4e5", color="#e9ecef", 
                 alpha=0.9)+
  labs(title="Histograma: Perimetro Abdominal de Mujeres", 
       x="Perimetro Abdominal", y="Frecuencia")

plot_grid(histHombre, histMujer)

#Para probar el supuesto de Normalidad se plantean las siguientes 
#hipotesis:
#H0: Los datos provienen de una distribucion normal.
#Ha: Los datos no provienen de una distribucion nomal.

library(car)
test = shapiro.test(datos[,3]) 
qqPlot(datos[,3], dist="norm", xlab="Cuantiles Teoricos",
       ylab="Cuantiles Muestrales", main="Normal QQ Plot")
legend("topleft", 
       legend=rbind(c("Statistic W","P.Value"),
       round(c(test$statistic,test$p.value),digits=4)),cex=0.8)

#Se probara si existe normalidad, para los datos discriminados por 
#Sexo.

par(mfrow=c(1,2))

testh = shapiro.test(hombre[,3]) 
qqPlot(hombre[,3], dist="norm", xlab="Cuantiles Teoricos",
       ylab="Cuantiles Muestrales", main="Normal QQ Plot Hombres")
legend("topleft", 
       legend=rbind(c("Statistic W","P.Value"),
       round(c(testh$statistic,testh$p.value),digits=4)),cex=0.8)

testm = shapiro.test(mujer[,3]) 
qqPlot(mujer[,3], dist="norm", xlab="Cuantiles Teoricos",
       ylab="Cuantiles Muestrales", main="Normal QQ Plot Mujeres")
legend("topleft", 
       legend=rbind(c("Statistic W","P.Value"),
       round(c(testm$statistic,testm$p.value),digits=4)),cex=0.8)

#Por las pruebas anteriores, hemos encontrado que el Perimetro
#Abdominal de las mujeres no se distribuye como una Normal.
#Se usará la metodologia Box y Cox para encontrar una transformación
#que garantice normalidad.

library(MASS)

perAbdoM <- as.matrix(mujer[,3])
lam <- boxcox(perAbdoM~1) #Lamdas para transformaciones
lam_max <- lam$x[which.max(lam$y)] #Lamda que maximiza la función

newMujer <- (perAbdoM^(lam_max)-1)/lam_max #Transformación de datos

testNewMujer = shapiro.test(newMujer) 
qqPlot(hombre[,3], dist="norm", xlab="Cuantiles Teoricos",
       ylab="Cuantiles Muestrales", 
       main="Normal QQ Plot Mujeres con Trans. Box-Cox")
legend("topleft", 
       legend=rbind(c("Statistic W","P.Value"),
       round(c(testNewMujer$statistic,testNewMujer$p.value),
             digits=4)),cex=0.8)

-----------------------------------------------------------------------------------------------------

#3.
mu <- matrix(c(-2,3,1,4), nrow=4, byrow = TRUE)

v1 <- c(137,8,2,1)
v2 <- c(8,144,10,6)
v3 <- c(2,10,7,2)
v4 <- c(1,6,2,2)
sigma <- rbind(v1,v2,v3,v4)

a1 <- c(2,0,-3,4)
a2 <- c(3,-2,1,-1)
A <- rbind(a1,a2)

#a. Nos piden hallar la distribución de Z.
Amu <- A %*% mu
AsigmaAt <- A %*% sigma %*% t(A)

#b.
mu1 <- matrix(c(-2,1), nrow=2, byrow = TRUE)
mu2 <- matrix(c(3,4), nrow=2, byrow = TRUE)

sigma11 <- matrix(c(137,2,2,7), nrow=2, byrow = TRUE)
sigma22 <- matrix(c(144,6,6,2), nrow=2, byrow = TRUE)
sigma12 <- matrix(c(8,10,1,2), nrow=2, byrow = TRUE)
sigma21 <- matrix(c(8,1,10,2), nrow=2, byrow = TRUE)

x2 <- matrix(c(-1,2), nrow=2, byrow = TRUE)

mux1x2 <- mu1 + sigma12 %*% solve(sigma22) %*% (x2-mu2)
sigmax1x2 <- sigma11 - sigma12 %*% solve(sigma22) %*% sigma21

-----------------------------------------------------------------------------------------------------

#4. 
#a. Calcule la distancia de Mahalanobis entre los vectores de medias para
#Hombres y Mujeres.

meanHombre <- matrix(mHombre, nrow=5, byrow = TRUE)
meanMujer <- matrix(mMujer, nrow=5, byrow = TRUE)

#Distancia de Mahalanobis
distMahala <- t(meanHombre-meanMujer) %*% solve(matVar) %*% (meanHombre-meanMujer)

#b. Nos piden clasificar cada sujeto como Hombre o Mujer si se tiene a disposicion
#los siguientes registros.

sujeto1 <- c(78.3,87.2,27,19.8,179.2)
sujeto2 <- c(71.4,82.7,23.5,16.9,165.8)
sujeto3 <- c(66.2,85.4,23.7,17.4,161.2)
sujeto4 <- c(69.3,81.4,25.1,17.3,164.3)

punto4 <- rbind(sujeto1, sujeto2, sujeto3, sujeto4)

#Para poder realizar la distancia de Mahalanobis, recodemos que la matriz de 
#varianzas y covarianzas debe ser invertible.
#Para que una matriz sea invertible, no puede tener columnas linealmente
#dependientes, es decir variables altamente correlacionadas.

cor(punto4)

#Como se puede observar, P1 y P38 tienen una correlación muy alta.
#Se escoge la variable P38 y se elimina P1, pues se considera que la altura 
#puede describir de una mejor forma un Hombre o una Mujer.
#Tambien las variables P29 y P38 tienen correlación alta.
#Se escoge la variable P38 y se elimina la variable P29.
#Se hicieron varias pruebas: la primera con las variables P16, P27 y P38. No se
#obtuvo un resultado favorable, pues, todas las distancias son iguales.
#Finalmente se optó por las variable P27 y P38.

punto4 <- punto4[,c(3,5)]
varpunto4 <- var(punto4) #Vector de medias
meanpunto4 <-  as.numeric(apply(punto4, 2, mean)) #Matriz de covarianzas

#Función distacia de Mahalanobis para cada sujeto
fdm <- function(sujeto){
  mahala <- t(sujeto-meanpunto4) %*% solve(varpunto4) %*% (sujeto-meanpunto4)
  mahala
}

#Distancia de Mahalanobis para cada sujeto
distsujeto <- apply(punto4, 1, fdm)

#Ahora establezcamos un criterio para clasificar los sujetos.
#En principio, se supone que el sujeto 1 es Hombre pues es quien tiene la mayor 
#estatura y longitud de Pie.
#En base a lo anterior y teniendo en cuenta las distancias obtenidas en el 
#paso anterior, se establecerá lo siguiente:
#Si la distancia de mahalanobis del sujeto_i es mayor a 1.5, este se clasificara
#como un hombre. Si su distancia es menor o igual a 1.5, este se clasificara
#como mujer.

