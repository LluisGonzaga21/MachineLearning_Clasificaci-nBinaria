
# COMENZAR POR PONER EL DIRECTORIO DE TRABAJO, CADA CUAL EL SUYO
# setwd("c:/")

load("compress.Rda")

dput(names(compress))
# Algunos gráficos básicos

library(ggplot2)
library(plotly)

ggplot(compress, aes(age, cstrength))+geom_point(color="red") 
ggplot(compress, aes(water, cstrength))+geom_point(color="red") 

hist(compress$age)
hist(compress$water)
 
plot_ly(compress, x = ~water, y = ~age, z = ~cstrength, type ='mesh3d')

# Es bueno crear listas de variables continuas, categóricas 
# y la dependiente en un vector

listconti<-c("cement", "blast", "ash", "water", "plasti", "aggreg", 
"fineagg", "age")
vardep<-c("cstrength")

cstrength<-compress[,vardep]

# ESTANDARIZACIÓN DE TODAS LAS VARIABLES CONTINUAS

means <-apply(compress[,listconti],2,mean,na.rm=TRUE)
sds<-sapply(compress[,listconti],sd,na.rm=TRUE)

compress<-scale(compress[,listconti], center = means, scale = sds)
compress<-data.frame(cbind(compress,cstrength))
compressbien<-compress
save(compressbien,file="compressbien.Rda")

# CREACIÓN DEL MODELO DE RED CON nnet

library(nnet)

# Controlar la semilla de aleatorización es importante
# pues interviene en el proceso de optimización
set.seed(22342)

# En la red se pone la fórmula del modelo, linou=TRUE para indicar 
# que la variable dependiente es continua, size=3 para 
# indicar 4 nodos en la capa oculta, 
# y maxit=100 para indicar solo 100 iteraciones 
# del proceso de estimación-optimización

red1<-nnet(data=compress,
 cstrength~age+water,linout = TRUE,size=3,maxit=100)

summary(red1)

# Se puede obtener la estimación de un vector de observaciones
# con la función predict, habitual en los modelos de R

predi<-predict(red1,newdata=compress,type="raw")

# Unimos la predicción al archivo original para verlo

compress2<-data.frame(cbind(compress,predi))

# Esto es solo para reordenar las columnas

compress2<- compress2[, c(3,4,1,2)]

# El objeto red1 contiene el SSE para los datos originales
# en el elemento "value" de la lista
# A partir de SSE se pueden calcular el MSE y su raíz cuadrada

SSE<-red1[["value"]]
MSE<-SSE/nrow(compress)
RMSE<-sqrt(MSE)

SSE
MSE
RMSE

# Para hacer una regresión sobre el mismo modelo

reg1<-lm(data=compress,cstrength~age+water)

summary(reg1)
 
# Para obtener el MSE y el RMSE

MSE=mean(reg1$residuals^2)
RMSE<-sqrt(MSE)

MSE
RMSE


# EJEMPLO: SEPARO EN TRAIN Y TEST USANDO CARET
# Importante controlar la semilla de aleatorización

library(caret)

set.seed(12346)
# Training test una sola vez
control<-trainControl(method = "LGOCV",p=0.8,number=1,savePredictions = "all") 

nnetgrid <-expand.grid(size=c(3),decay=c(0.1))

rednnet<- train(cstrength~age+water,
 data=compressbien, method="nnet",linout = TRUE,
 trControl=control,tuneGrid=nnetgrid)

# EL ERROR PRESENTADO EL ERROR SOBRE DATOS TEST 
rednnet

# Comparo con regresión lineal, method=lm. Aquí no hay grid.

reg1<- train(cstrength~age+water,
 data=compressbien, method="lm",trControl=control)

reg1

# EJEMPLO TRAINING TEST REPETIDO  5 veces
# CÓDIGO IGUAL QUE EL ANTERIOR, PERO EN trainControl se pone number=5
# EL ERROR PRESENTADO ES LA MEDIA DE ERROR SOBRE DATOS TEST EN TODAS LAS REPETICIONES

set.seed(12346)
# Training test una sola vez
control<-trainControl(method = "LGOCV",p=0.8,number=5,savePredictions = "all") 

nnetgrid <-expand.grid(size=c(3),decay=c(0.1))

rednnet<- train(cstrength~age+water,
 data=compressbien, method="nnet",linout = TRUE,
 trControl=control,tuneGrid=nnetgrid)

rednnet

# Comparo con regresión lineal, method=lm. Aquí no hay grid.

reg1<- train(cstrength~age+water,
 data=compressbien, method="lm",trControl=control)

reg1


# EJEMPLO VARIANDO LA SEMILLA, TRAINING TEST UNA SOLA VEZ
# Ponemos trace=FALSE en train para evitar listados innecesarios

for (semilla in 120:125)
{
set.seed(semilla) 
control<-trainControl(method = "LGOCV",p=0.8,number=5,savePredictions = "all") 

nnetgrid <-expand.grid(size=c(3),decay=c(0.1))

rednnet<- train(cstrength~age+water,
 data=compressbien, method="nnet",linout = TRUE,
 trControl=control,tuneGrid=nnetgrid,trace=FALSE)

cat("\n")
print(semilla)
cat("\n")
print(rednnet$results$RMSE)

# Comparo con regresión lineal, method=lm. Aquí no hay grid.

reg1<- train(cstrength~age+water,
 data=compressbien, method="lm",trControl=control)

print(reg1$results$RMSE)
 
 }

