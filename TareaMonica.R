rm(list=ls())
getwd()
setwd("C:/Users/Usuario/OneDrive/Escritorio/Máster Big Data/Machine Learning/")

library(caret)
library(plyr)
library(readr)
library(questionr)
library(car)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(tidyr)

datos <- read.csv("mifem.csv")
head(datos)
freq(datos$outcome)
# Hay 2000 observaciones
# dead: 321, 24,8% == Yes
# live: 974, 75,2% == NO
# Hay un 41.5% de Yes

head(datos)

########################################################
#DATOS
#https://vincentarelbundock.github.io/Rdatasets/doc/ISLR/Caravan.html
########################################################

#1) PRESENTACIÓN DE DATOS. DEPURACIÓN
str(datos)
summary(datos)
#Convertimos a factor todas las variables categóricas
datos[,c(2,5,6,7,8,9,10,11)]<-lapply(datos[,c(2,5,6,7,8,9,10,11)], as.factor)
#Comprobamos si todas las variables numéricas toman más de 10 valores diferentes
#es decir, no están infrarrepresentadas.
sapply(Filter(is.numeric, datos),function(x) length(unique(x)))

datos$X<-NULL

#Evaluamos el número de missing de cada variable
library(naniar)
gg_miss_var(datos)

#¿Existen valores incorrectos? Parece que no
hist(datos$age,xlab="age",main="")
hist(datos$yronset,xlab="yronset",main="")

#Creamos listas de variables continuas, categóricas y variable dependiente
# de forma automática

listclass<-c()
listconti<-c()
nombrecol<-colnames(datos)
vardep<-c("outcome.dead")
a<-length(nombrecol)
for (i in 1:a)
{
  j = 0
  if(is.factor(datos[,i]))
  {
    j = j + 1
    listclass<-cbind(listclass,nombrecol[i])
  }
}
for (i in 1:a)
{
  j = 0
  if(is.numeric(datos[,i]))
  {
    j = j + 1
    listconti<-cbind(listconti,nombrecol[i])
  }
}

for (i in listclass)
{
  result<-noquote(paste("datos", sep="$",i))
  result
  sali<-eval(parse(text=result))
  mostrar<-freq(sali)
  print(i)
  print(mostrar)
}
#Ya tenemos todos los datos correctamente representados
summary(datos)
str(datos)

# a)Eliminar las observaciones con missing en alguna variable (sabemos que no hay)
datos2<-na.omit(datos,(!is.na(datos)))

# b)pasar las categóricas a dummies
library(dummies)
datos3<- dummy.data.frame(datos2, listclass, sep = ".")

# c)estandarizar las variables continuas
# Cálculo medias y dtipica de datos y estandarizo (solo las continuas)
means <-apply(datos3[,listconti],2,mean) 
sds<-sapply(datos3[,listconti],sd) 

# Estandarizo solo las continuas y uno con las categoricas
datosbis<-scale(datos3[,listconti], center = means, scale = sds)
numerocont<-which(colnames(datos3)%in%listconti)
datosbis<-cbind(datosbis,datos3[,-numerocont])
str(datosbis)

#Análisis de relaciones entre variables

#2) SELECCIÓN DE VARIABLES
#2.1) Análisis de las relaciones entre variables
str(datosbis)
datosbis$outcome.live<-NULL
outcome.dead <- datosbis[, vardep]

str(datosbis)
input <- datosbis[, -3]

#Volvemos a rellenar las listas de varialbes continuas y categóricas 
#con el código de arriba
listconti<-c()
nombrecol<-colnames(datosbis)
a<-length(datosbis)
for (i in 1:a)
{
  j = 0
  if(is.numeric(datosbis[,i]))
  {
    j = j + 1
    listconti<-cbind(listconti,nombrecol[i])
  }
}
listconti<-listconti[-3]

freq(datosbis$outcome.dead)
# PARA EVITAR PROBLEMAS, MEJOR DEFINIR LA VARIABLE OUTPUT
# con valores alfanuméricos Yes, No
datosbis$outcome.dead<-ifelse(datosbis$outcome.dead,"Yes","No")
freq(datosbis$outcome.dead)

#Selección de variables
full<-glm(factor(outcome.dead)~.,data=datosbis,family=binomial(link='logit'))
null<-glm(factor(outcome.dead)~1,data=datosbis,family=binomial(link='logit'))
library(MASS)
selec1 <- stepAIC(null, scope = list(upper = full), direction = "both",
                  trace = FALSE)
summary(selec1)
dput(names(selec1$coefficients))
formula(selec1)

#c("(Intercept)", "angina.nk", "highbp.nk", "age", "stroke.n", 
#"smstat.nk", "hichol.n", "angina.n", "smstat.n", "yronset", "premi.n"
#factor(outcome.dead) ~ angina.nk + highbp.nk + age + stroke.n + 
#smstat.nk + hichol.n + angina.n + smstat.n + yronset + premi.n

#Lo probamos con la funcion estep repetido
source("C:/Users/Usuario/OneDrive/Escritorio/Máster Big Data/Machine Learning/Todos los programas y datasets R/funcion steprepetido binaria.R")
lista<-steprepetidobinaria(data=datosbis,
                           vardep=vardep,listconti=listconti,sinicio=12345,
                           sfinal=12355,porcen=0.8,criterio="AIC")
tabla1 <- lista[[1]]
tabla1
dput(lista[[2]][[1]])
#c("angina.nk", "age", "highbp.nk", "yronset", "smstat.nk", "stroke.n", "hichol.n", "premi.n", "smstat.n")
dput(lista[[2]][[2]])

#BIC
lista <- steprepetidobinaria(data = datosbis, vardep = vardep,
                             listconti = listconti,
                             sinicio=12345, sfinal = 12385, porcen = 0.8, criterio = "BIC")
tabla2 <- lista[[1]]
tabla2
dput(lista[[2]][[1]])
#c("angina.nk", "stroke.n", "highbp.nk", "age", "smstat.nk", "yronset")
dput(lista[[2]][[2]])
#c("angina.nk", "stroke.n", "smstat.nk", "highbp.nk", "age")

#Comprobamos estos modelos con validación cruzada
source("C:/Users/Usuario/OneDrive/Escritorio/Máster Big Data/Machine Learning/Todos los programas y datasets R/cruzadas avnnet y log binaria.R")
library(pROC)

#c("angina.nk", "age", "highbp.nk", "yronset", "smstat.nk", "stroke.n", "hichol.n", "premi.n", "smstat.n")
medias1<-cruzadalogistica(data = datosbis,
                          vardep = "outcome.dead", listconti = c("angina.nk", "age", "highbp.nk", "yronset", "smstat.nk", "stroke.n", "hichol.n", "premi.n", "smstat.n"),
                          listclass = c(""), grupos = 4, sinicio = 1234, repe = 5)

medias1$modelo="Logística1"

#c("angina.nk", "stroke.n", "highbp.nk", "age", "smstat.nk", "yronset")
medias2<-cruzadalogistica(data = datosbis,
                          vardep = "outcome.dead",listconti = c("angina.nk", "stroke.n", "highbp.nk", "age", "smstat.nk", "yronset"),
                          listclass = c(""), grupos = 4, sinicio = 1234, repe = 5)

medias2$modelo="Logística2"

#c("angina.nk", "stroke.n", "smstat.nk", "highbp.nk", "age")
medias3<-cruzadalogistica(data=datosbis,
                          vardep="outcome.dead",listconti=c("angina.nk", "stroke.n", "smstat.nk", "highbp.nk", "age"),
                          listclass=c(""), grupos=4,sinicio=1234,repe=5)

medias3$modelo="Logística3"


union1<-rbind(medias1,medias2,medias3)

par(cex.axis=0.5)
boxplot(data=union1,tasa~modelo,col="pink",main="TASA FALLOS")
boxplot(data=union1,auc~modelo,col="pink",main="AUC")

####################################################
############# LOGÍSTICA 2 ES LA MEJOR ##############
# Pero no hay tanta diferencia en realidad. La precisión sólo varía una centésima

listconti <- c("angina.nk", "stroke.n", "highbp.nk", "age", "smstat.nk", "yronset")
noquote(paste(dput(listconti),collapse="+"))

library(caret)
#3) RED
#Tuneo de las variables del grid
control<-trainControl(method = "repeatedcv",number=4,repeats=5,
                      savePredictions = "all",classProbs=TRUE) 
# ***************************************************************
# avNNet: parámetros
# Number of Hidden Units (size, numeric)
# Weight Decay (decay, numeric)
# Bagging (bag, logical)
# ***************************************************************
avnnetgrid <-expand.grid(size=c(5,10,15,20),
                         decay=c(0.01,0.1,0.001),bag=FALSE)

redavnnet<- train(factor(outcome.dead)~angina.nk+stroke.n+highbp.nk+age+smstat.nk+yronset,
                  data=datosbis,
                  method="avNNet", linout = FALSE,maxit=100,
                  trControl=control,tuneGrid=avnnetgrid,
                  repeats=5)
redavnnet
freq(redavnnet$pred$pred)
#The final values used for the model were size = 5, decay = 0.1 and bag = FALSE.

# ************************************
# COMPARANDO LOS MODELOS FINALES
# ***********************************

#c("angina.nk", "age", "highbp.nk", "yronset", "smstat.nk", "stroke.n", "hichol.n", "premi.n", "smstat.n")
medias4<-cruzadaavnnetbin(data=datosbis,
                          vardep="outcome.dead",listconti=c("angina.nk", "age", "highbp.nk", "yronset", "smstat.nk", "stroke.n", "hichol.n", "premi.n", "smstat.n"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=5,
                          size=c(5),decay=c(0.1),repeticiones=5,itera=200)

medias4$modelo="avnnet1"

#c("angina.nk", "stroke.n", "highbp.nk", "age", "smstat.nk", "yronset")
medias5<-cruzadaavnnetbin(data=datosbis,
                          vardep="outcome.dead", listconti=c("angina.nk", "stroke.n", "highbp.nk", "age", "smstat.nk", "yronset"),
                          listclass=c(""), grupos = 4,sinicio = 1234, repe = 5,
                          size = c(5), decay = c(0.1), repeticiones = 5, itera = 200)

medias5$modelo="avnnet2"



union1<-rbind(medias1,medias2,medias3,medias4,medias5)

par(cex.axis=0.5)
boxplot(data=union1,tasa~modelo,col="pink",main="TASA FALLOS")
boxplot(data=union1,auc~modelo,col="pink",main="AUC")

#Tuneo de todas las variables, de dentro y fuera del grid
listconti <- c("angina.nk", "stroke.n", "highbp.nk", "age", "smstat.nk", "yronset")
data <- datosbis[, c(listconti, vardep)]

control <- trainControl(method = "cv",
                      number = 4, savePredictions = "all") 

set.seed(1234)
nnetgrid <- expand.grid(size = c(3, 4, 5, 7, 9), decay = c(0.001, 0.01, 0.1, 0.2), bag = FALSE)

completo <- data.frame()
listaiter <- c(50, 80, 100, 120, 200, 500, 1000, 2000)

for (iter in listaiter)
{
  rednnet<- train(factor(outcome.dead) ~ .,
                  data = data,
                  method = "avNNet", linout = FALSE, maxit = iter,
                  trControl = control, repeats = 5, tuneGrid = nnetgrid, trace = F)
  # Añado la columna del parametro de iteraciones
  rednnet$results$itera <- iter
  # Voy incorporando los resultados a completo
  completo <- rbind(completo,rednnet$results)
}

completo<-completo[order(completo$Accuracy),]
ggplot(completo, aes(x=factor(itera), y=Accuracy, 
                     color=factor(decay),pch=factor(size))) +
       geom_point(position=position_dodge(width=0.5),size=3)


#El modelo vencedor es: 
#decay=0.15
#size=3
#itera=300

#Unimos y comparamos precisión con el resto de modelos
medias6<-cruzadaavnnetbin(data=data,
                          vardep="outcome.dead",listconti=listconti,
                          listclass=c(""),grupos=4,sinicio=1234,repe=5,repeticiones=5,itera=200,
                          size=c(7),decay=c(0.1))

medias6$modelo="Red con maxit"


union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6)

par(cex.axis=0.8)
boxplot(data=union1,col="pink",tasa~modelo,main="TASA DE FALLOS")

par(cex.axis=0.7)
boxplot(data=union1,col="pink",auc~modelo,main="AUC")

#Hasta el momento nos quedamos con logística2

#Diseñamos un árbol
###############ÁRBOL############################################

library(rpart)
library(rpart.plot)
library(rattle)

arbol1<-rpart(factor(outcome.dead)~angina.nk,data=data,minbucket=10,
              method="class",parms=list(split="gini"))
summary(arbol1)
rpart.plot(arbol1, extra=1, tweak=1.2)

#IMPORTANCIA DE VARIABLES
arbol2<-rpart(factor(outcome.dead)~., data=data, minbucket=10, method="class", maxsurrogate=0)
par(cex=0.7)
arbol2$variable.importance
barplot(arbol2$variable.importance)
par(cex=1)
rpart.plot(arbol2, extra=105, tweak=1.2, type=1, nn=TRUE)
asRules(arbol2)

#Tuneado de las variables del árbol, concretamente minbu
library(pROC)
for (minbu in seq(from=10, to=60, by=5))
{
  print(minbu)
  cat("/n")
  
  arbolgrid <-  expand.grid(cp=c(0))
  
  arbolcaret<- train(factor(outcome.dead)~angina.nk+stroke.n+highbp.nk+age+smstat.nk+yronset,data=datosbis,
                     method="rpart",minbucket=minbu,trControl=control,tuneGrid=arbolgrid)
  
  # arbolcaret
  
  sal<-arbolcaret$pred
  
  salconfu<-confusionMatrix(sal$pred,sal$obs)
  print(salconfu)
  
  #curvaroc<-roc(response = sal$obs, predictor = sal$Yes)
  #auc<-curvaroc$auc
#  print(auc)
}

#Nos quedamos con cp = 0 y minbucket = x
source("C:/Users/Usuario/OneDrive/Escritorio/Máster Big Data/Machine Learning/Todos los programas y datasets R/cruzada arbolbin.R")
medias7<-cruzadaarbolbin(data=datosbis, vardep="outcome.dead", listconti=listconti,
                         listclass=c(""), grupos=4, sinicio=1234, repe=5,
                         cp=c(0), minbucket=20)
medias7$modelo="arbol"

#Comparamos de nuevo los modelos
union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7)
par(cex.axis=0.5)
boxplot(data=union1,tasa~modelo,main="Tasa fallos")
boxplot(data=union1,auc~modelo,main="AUC")



########################### BAGGING ####################################


#Usamos nodesize = 20 como en el árbol del apartado anterior

library(randomForest)
source("C:/Users/Usuario/OneDrive/Escritorio/Máster Big Data/Machine Learning/Todos los programas y datasets R/cruzada rf binaria.R")
listconti <- c("angina.nk", "stroke.n", "highbp.nk", "age", "smstat.nk", "yronset")
paste(listconti,collapse="+")

# Control de la semilla para reproducibilidad en caret si se usa validación cruzada simple 

#  Una semilla general (set.seed) 

medias8<-cruzadarfbin(data=datosbis, vardep="outcome.dead",
                      listconti=c("angina.nk", "stroke.n", "highbp.nk", "age", "smstat.nk", "yronset"),
                      listclass=c(""),
                      grupos=4,sinicio=1234,repe=20,nodesize=20,
                      mtry=7,ntree=3000,replace=TRUE,sampsize=50)

medias8$modelo="bagging50"


medias9<-cruzadarfbin(data=datosbis, vardep="outcome.dead",
                      listconti=c("angina.nk", "stroke.n", "highbp.nk", "age", "smstat.nk", "yronset"),
                      listclass=c(""),
                      grupos=4,sinicio=1234,repe=20,nodesize=20,
                      mtry=7,ntree=3000,replace=TRUE,sampsize=100)

medias9$modelo="bagging100"

medias10<-cruzadarfbin(data=datosbis, vardep="outcome.dead",
                      listconti=c("angina.nk", "stroke.n", "highbp.nk", "age", "smstat.nk", "yronset"),
                      listclass=c(""),
                      grupos=4,sinicio=1234,repe=20,nodesize=20,
                      mtry=7,ntree=3000,replace=TRUE,sampsize=150)


medias10$modelo="bagging150"

medias11<-cruzadarfbin(data=datosbis, vardep="outcome.dead",
                      listconti=c("angina.nk", "stroke.n", "highbp.nk", "age", "smstat.nk", "yronset"),
                      listclass=c(""),
                      grupos=4,sinicio=1234,repe=20,nodesize=20,
                      mtry=7,ntree=3000,replace=TRUE,sampsize=200)

medias11$modelo="bagging200"

medias12<-cruzadarfbin(data=datosbis, vardep="outcome.dead",
                      listconti=c("angina.nk", "stroke.n", "highbp.nk", "age", "smstat.nk", "yronset"),
                      listclass=c(""),
                      grupos=4,sinicio=1234,repe=20,nodesize=20,
                      mtry=7,ntree=3000,replace=TRUE,sampsize=300)


medias12$modelo="bagging300"

medias13<-cruzadarfbin(data=datosbis, vardep="outcome.dead",
                      listconti=c("angina.nk", "stroke.n", "highbp.nk", "age", "smstat.nk", "yronset"),
                      listclass=c(""),
                      grupos=4,sinicio=1234,repe=20,nodesize=20,
                      mtry=7,ntree=3000,replace=TRUE)
# En este modelo al no usar sampsize usa el que tiene por defecto 
# caret, que son todas las observaciones con reemplazamiento (1295) 

medias13$modelo="baggingBASE"
union1<-rbind(union1,medias9,medias10,medias11,medias12,medias13)
par(cex.axis=0.5)
boxplot(data=union1,tasa~modelo,main="Tasa fallos")
boxplot(data=union1,auc~modelo,main="AUC")

#baggin 100 sale muy bien
########################## RANDOM FOREST ###############################

paste(listconti,collapse="+")

set.seed(12345)
rfgrid<-expand.grid(mtry=c(3,4,5,6,7))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

rf<- train(factor(outcome.dead)~angina.nk+stroke.n+highbp.nk+age+smstat.nk+yronset,data=datosbis,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=300,nodesize=20,replace=TRUE,
           importance=TRUE)

rf

#Recomienda mtry = 3
#Por tanto, realizamos validación cruzada con mtry = 3 (90% de accuracy wooow)
medias14<-cruzadarfbin(data=datosbis, 
                      vardep="outcome.dead",listconti=listconti,
                      listclass=c(""),
                      grupos=4,sinicio=1234,repe=10,nodesize=20,
                      mtry=3,ntree=3000,replace=TRUE,sampsize=100)

medias14$modelo="rf"

union1<-rbind(union1,medias14)
par(cex.axis=0.5)
boxplot(data=union1,col="pink",tasa~modelo,main="Tasa fallos")
boxplot(data=union1,col="pink", auc~modelo,main="AUC")
union2<-c()
union2<-rbind(medias8,medias9,medias10,medias11,medias12,medias13,medias14)
boxplot(data=union2,col="pink",tasa~modelo,main="Tasa fallos")
boxplot(data=union2,col="pink", auc~modelo,main="AUC")

########################################################################
######################### GRADIENT BOOSTING ############################
########################################################################
library(caret)

set.seed(12345)

gbmgrid<-expand.grid(shrinkage=c(0.2,0.1,0.05,0.03,0.01,0.001,0.0001),
                     n.minobsinnode=c(5,10,20,30),
                     n.trees=c(100,500,1000,5000),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

gbm<- train(factor(outcome.dead)~angina.nk+stroke.n+highbp.nk+age+smstat.nk+yronset,
            data=datosbis,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="bernoulli", bag.fraction=1,verbose=FALSE)

gbm

plot(gbm)

#•Tuning parameter 'interaction.depth' was held constant at a value of 2
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were n.trees = 100, interaction.depth = 2, shrinkage = 0.1
#and n.minobsinnode = 20.
# ESTUDIO DE EARLY STOPPING
# Probamos a fijar algunos parámetros para ver como evoluciona
# en función de las iteraciones

gbmgrid<-expand.grid(shrinkage=c(0.1),
                     n.minobsinnode=c(20),
                     n.trees=c(50,100,300,500,800,1000,1200),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

gbm<- train(factor(outcome.dead)~angina.nk+stroke.n+highbp.nk+age+smstat.nk+yronset,
            data=datosbis,method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="bernoulli", bag.fraction=1,verbose=FALSE)
#Tuning parameter 'interaction.depth' was held constant at a value of 2
#Tuning parameter 'shrinkage' was held constant at a value of 0.01
#Tuning parameter 'n.minobsinnode' was held constant at a value of 10
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were n.trees = 300, interaction.depth = 2, shrinkage = 0.01 and n.minobsinnode = 10.
gbm
plot(gbm)

# IMPORTANCIA DE VARIABLES
par(cex=1.3)
summary(gbm)

tabla<-summary(gbm)
par(cex=1.5,las=2)
barplot(tabla$rel.inf,names.arg=row.names(tabla))

#Validación cruzada
#Fijamos 50 árboles
source ("C:/Users/Usuario/OneDrive/Escritorio/Máster Big Data/Machine Learning/Todos los programas y datasets R/cruzada gbm binaria.R")
medias15<-cruzadagbmbin(data=datosbis, vardep="outcome.dead",
                       listconti=listconti,
                       listclass=c(""),
                       grupos=4,sinicio=1234,repe=20,
                       n.minobsinnode=20,shrinkage=0.1,n.trees=50,interaction.depth=2)

medias15$modelo="gbm"


union1<-rbind(union1,medias15)
union2<-rbind(union2,medias15)

par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="pink")
boxplot(data=union1,auc~modelo,main="AUC",col="pink")
boxplot(data=union2,tasa~modelo,main="TASA FALLOS",col="pink")
boxplot(data=union2,auc~modelo,main="AUC",col="pink")

########################### XGBOOST ####################################
#Validación cruzada

library(caret)

set.seed(12345)

xgbmgrid<-expand.grid(
  min_child_weight=c(5,10,20),
  eta=c(0.1,0.05,0.03,0.01,0.001),
  nrounds=c(100,500,1000,5000),
  max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm<- train(factor(outcome.dead)~.,data=datosbis,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)

xgbm

plot(xgbm)

#Tuning parameter 'max_depth' was held constant at a value of 6
#Tuning parameter 'gamma' was held constant at a value of 0
#Tuning parameter 'colsample_bytree' was held constant at a value of 1
#Tuning parameter 'subsample' was held constant at a value of 1
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were nrounds = 500, max_depth = 6, eta = 0.01, 
#gamma = 0, colsample_bytree = 1, min_child_weight = 10 and subsample = 1.

# ESTUDIO DE EARLY STOPPING
# Probamos a fijar algunos parámetros para ver como evoluciona
# en función de las iteraciones

xgbmgrid<-expand.grid(eta=c(0.1),
                      min_child_weight=c(20),
                      nrounds=c(50,100,150,200,250,300,400,500,600,700),
                      max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

set.seed(12345)
control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm<- train(factor(outcome.dead)~.,data=datosbis,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)

plot(xgbm)


# Probamos con otras semillas para la validación cruzada

set.seed(12367)
control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm<- train(factor(outcome.dead)~.,data=datosbis,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)

plot(xgbm)


# IMPORTANCIA DE VARIABLES

varImp(xgbm)
plot(varImp(xgbm))

# PRUEBO PARÁMETROS CON VARIABLES SELECCIONADAS

xgbmgrid<-expand.grid(
  min_child_weight=c(5,10,20),
  eta=c(0.1,0.05,0.03,0.01,0.001),
  nrounds=c(100,500,1000,5000),
  max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

  xgbm<- train(factor(outcome.dead)~angina.nk+stroke.n+highbp.nk+age+smstat.nk+yronset,
             data=datosbis,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)

xgbm

plot(xgbm)

#Tuning parameter 'max_depth' was held constant at a value of 6
#Tuning parameter 'colsample_bytree' was held constant at a value of 1
#Tuning parameter 'subsample' was held constant at a value of 1
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were nrounds = 100, max_depth = 6,
#eta = 0.01, gamma = 0, colsample_bytree = 1, min_child_weight = 5
#and subsample = 1.

source ("C:/Users/Usuario/OneDrive/Escritorio/Máster Big Data/Machine Learning/Todos los programas y datasets R/cruzada xgboost binaria.R")

medias16<-cruzadaxgbmbin(data=datosbis, vardep=vardep,
                        listconti=listconti,
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,
                        min_child_weight=10,eta=0.03,nrounds=100,max_depth=6,
                        gamma=0,colsample_bytree=1,subsample=1,
                        alpha=0,lambda=0)

medias16$modelo="xgbm"

union1<-rbind(union1, medias16)
union2<-c()
#comparamos sólo los modelos relacionados con árboles
union2<-rbind(union2,medias8,medias9,medias14,medias15,medias16)
par(cex.axis=0.8,cex=1)
par(mar = c(7, 4, 4, 2) + 0.1)
boxplot(data=union1,tasa~modelo,xlab = "",las = 2,main="TASA FALLOS",col="pink")
par(mar = c(7, 4, 4, 2) + 0.1)
boxplot(data=union1,auc~modelo,xlab = "",las = 2,main="AUC",col="pink")
boxplot(data=union2,tasa~modelo,main="TASA FALLOS",col="pink")
boxplot(data=union2,auc~modelo,main="AUC",col="pink")

########################################################################
###################### SUPORT VECTOR MACHINES ##########################
########################################################################

#TUNEADO SVM LINEAL

set.seed(12345)
SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,7,10,13,20,50))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

paste(listconti,collapse="+")
SVM<- train(data=datosbis,factor(outcome.dead)~angina.nk+stroke.n+highbp.nk+age+smstat.nk+yronset,
            method="svmLinear",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM$results
plot(SVM$results$C,SVM$results$Accuracy)

#c = 3 está bien
# Rehago el grid para observar mejor el intervalo de C entre 0 y 0.6
SVMgrid<-expand.grid(C=c(0.0001,0.001,0.01,0.1,0.2,0.3,0.4,0.5))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

SVM<- train(data=datosbis,factor(outcome.dead)~angina.nk+stroke.n+highbp.nk+age+smstat.nk+yronset,
            method="svmLinear",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM$results
plot(SVM$results$C,SVM$results$Accuracy)
#Concreto a c=0.1

#TUNEADO SVM POLINOMIAL

#  SVM Polinomial: PARÁMETROS C, degree, scale

SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5),
                     degree=c(2,3),scale=c(0.1,0.5,1,2,5))

control<-trainControl(method = "cv",
                      number=4,savePredictions = "all") 


SVM<- train(data=datosbis,factor(outcome.dead)~angina.nk+stroke.n+highbp.nk+age+smstat.nk+yronset,
            method="svmPoly",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM

SVM$results
plot(SVM$results$C,SVM$results$Accuracy)


# LOS GRÁFICOS DOS A DOS NO SIRVEN
# plot(SVM$results$C,SVM$results$Accuracy)
# plot(SVM$results$degree,SVM$results$Accuracy)
# plot(SVM$results$scale,SVM$results$Accuracy)


dat<-as.data.frame(SVM$results)
library(ggplot2)

# PLOT DE DOS VARIABLES CATEGÓRICAS, UNA CONTINUA
ggplot(dat, aes(x=factor(C), y=Accuracy, 
                color=factor(degree),pch=factor(scale))) +
  geom_point(position=position_dodge(width=0.5),size=3)

# SOLO DEGREE=2
dat2<-dat[dat$degree==3,]  

ggplot(dat2, aes(x=factor(C), y=Accuracy, 
                 colour=factor(scale))) +
  geom_point(position=position_dodge(width=0.5),size=3)



#  SVM RBF: PARÁMETROS C, sigma

SVMgrid<-expand.grid(C=c(0.05,0.1,0.2,0.5,1,2),
                     sigma=c(0.005,0.01,0.05,0.1,0.5,1))

control<-trainControl(method = "cv",
                      number=4,savePredictions = "all") 


SVM<- train(data=datosbis,factor(outcome.dead)~angina.nk+stroke.n+highbp.nk+age+smstat.nk+yronset,
            method="svmRadial",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM

dat<-as.data.frame(SVM$results)

ggplot(dat, aes(x=factor(C), y=Accuracy, 
                color=factor(sigma)))+ 
  geom_point(position=position_dodge(width=0.5),size=3)

#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were sigma = 0.01 and C = 0.1.

source ("C:/Users/Usuario/OneDrive/Escritorio/Máster Big Data/Machine Learning/Todos los programas y datasets R/cruzada SVM binaria lineal.R")
source ("C:/Users/Usuario/OneDrive/Escritorio/Máster Big Data/Machine Learning/Todos los programas y datasets R/cruzada SVM binaria polinomial.R")
source ("C:/Users/Usuario/OneDrive/Escritorio/Máster Big Data/Machine Learning/Todos los programas y datasets R/cruzada SVM binaria RBF.R")

medias20<-cruzadaSVMbin(data=datosbis, vardep="outcome.dead",
                       listconti=listconti,
                       listclass=c(""),
                       grupos=4,sinicio=1234,repe=5,C=0.1)
medias20$modelo="SVM"


medias21<-cruzadaSVMbinPoly(data=datosbis, vardep="outcome.dead",
                           listconti=listconti,
                           listclass=c(""),
                           grupos=4,sinicio=1234,repe=5,
                           C=0.01,degree=2,scale=0.1)

medias21$modelo="SVMPoly"


medias22<-cruzadaSVMbinRBF(data=datosbis, vardep="outcome.dead",
                           listconti=listconti,
                           listclass=c(""),
                           grupos=4,sinicio=1234,repe=5,
                           C=0.5,sigma=0.1)

medias22$modelo="SVMRBF"


union1<-rbind(union1,medias20,medias21,medias22)
union3<-c()
union3<-rbind(union3, medias20, medias21, medias22)
par(cex.axis=0.8)
par(mar = c(7, 4, 4, 2) + 0.1)
boxplot(data=union1,tasa~modelo,las=2,xlab = "",main="TASA FALLOS",col="pink")
par(mar = c(7, 4, 4, 2) + 0.1)
boxplot(data=union1,auc~modelo,xlab = "",las=2,main="AUC",col="pink")
boxplot(data=union3,tasa~modelo,main="TASA FALLOS",col="pink")
boxplot(data=union3,auc~modelo,main="AUC",col="pink")

uni<-union1
uni$modelo <- with(uni,
                   reorder(modelo,tasa, median))
par(cex.axis=0.8,las=2)
boxplot(data=uni,tasa~modelo,col="pink",main="TASA FALLOS")

uni<-union1
uni$modelo <- with(uni,
                   reorder(modelo,auc, median))
par(cex.axis=1.2,las=2)
boxplot(data=uni,auc~modelo,col="pink",main="AUC")


################### Ensamblado ###############################

library(caretEnsemble)

# ************************************************************
# Estas tres líneas son importantes, sustituir por el nombre
# de variable y archivo
# ************************************************************

formula1<-as.formula(paste("factor(","outcome.dead",")","~."))
datosbis$outcome.dead<-as.factor(datosbis$outcome.dead)
levels(datosbis$outcome.dead) <- make.names(levels(factor(datosbis$outcome.dead)))

# Aquí se fijan el número de repeticiones de validación cruzada 
# y la semilla
set.seed(3005)
repeticiones=10


# Manera de evaluar los modelos
stackControl <- trainControl(method="repeatedcv", 
                             number=4, repeats=repeticiones, savePredictions=TRUE, classProbs=TRUE)

# Parámetros para caret, tunear antes

# Muy importante considerar esto:

# Cada method (algoritmo) tiene parámetros a tunear con Grid
# y parámetros específicos que no se pueden tunear.
# --Los que se pueden tunear hay que ponerlos en un Grid aunque 
# solo se les de un valor (ver por ejemplo gbmGrid).
# --Los que no se pueden tunear hay que nombrarlos directamente
# en train (ver por ejemplo rf)
library(caretEnsemble)

gbmGrid <- expand.grid(n.trees = c(50),
                       interaction.depth = c(2), shrinkage =c(0.1), n.minobsinnode = c(20))

rfGrid <- expand.grid(mtry=c(3))

svmRadialGrid <- expand.grid(sigma=c(0.05),C=c(0.5))

svmlinGrid <- expand.grid(C=c(0.1))

svmPolyGrid <- expand.grid(C=c(0.1),degree=c(2),scale=c(0.1))

set.seed(3005)

models <- caretList(outcome.dead~., data=datosbis, trControl=stackControl,
                    tuneList=list(
                      parrf=caretModelSpec(method="rf",maxnodes=30,
                                           n.trees = 200,nodesize=10,sampsize=150,tuneGrid=rfGrid), 
                      glm=caretModelSpec(method="glm"),
                      gbm=caretModelSpec(method="gbm",tuneGrid=gbmGrid),
                      svmlinear=caretModelSpec(method="svmLinear",tuneGrid=svmlinGrid), 
                      svmPoly=caretModelSpec(method="svmPoly",tuneGrid=svmPolyGrid),
                      svmradial=caretModelSpec(method="svmRadial",tuneGrid=svmRadialGrid)
                    ))

results <- resamples(models)
summary(results)
dotplot(results)

modelCor(results)
splom(results)
results[[2]]

ense <- caretEnsemble(models)

# Aquí se recomiendan los pesos para el ensamblado 
# de todos los modelos y se ve la tasa de aciertos
# de cada modelo y ensamblado
summary(ense)

#Validación cruzada
source ("C:/Users/Usuario/OneDrive/Escritorio/Máster Big Data/Machine Learning/Todos los programas y datasets R/cruzadas ensamblado binaria fuente.R")
#Preparar el archivo

set.seed(12345)

archivo<-datosbis

grupos<-4
sinicio<-1234
repe<-50


# APLICACIÓN CRUZADAS PARA ENSAMBLAR
# AQUÍ ME FALTA METER LOS PARÁMETROS QUE HEMOS TUNEADO EN TODOS LOS PASOS ANTERIORES
listclass <- c("")
vardep
medias1<-cruzadalogistica(data=archivo,
                          vardep=vardep,listconti=listconti,
                          listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe)

medias1bis<-as.data.frame(medias1[1])
medias1bis$modelo<-"Logistica"
predi1<-as.data.frame(medias1[2])
predi1$logi<-predi1$Yes

medias2<-cruzadaavnnetbin(data=archivo,
                          vardep=vardep,listconti=listconti,
                          listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                          size=c(3),decay=c(0.15),repeticiones=5,itera=300)

medias2bis<-as.data.frame(medias2[1])
medias2bis$modelo<-"avnnet"
predi2<-as.data.frame(medias2[2])
predi2$avnnet<-predi2$Yes


medias3<-cruzadarfbin(data=archivo,
                      vardep=vardep,listconti=listconti,
                      listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                      mtry=3,ntree=300,nodesize=20,replace=TRUE)


medias3bis<-as.data.frame(medias3[1])
medias3bis$modelo<-"rf"
predi3<-as.data.frame(medias3[2])
predi3$rf<-predi3$Yes

medias4<-cruzadagbmbin(data=archivo,
                       vardep=vardep,listconti=listconti,
                       listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                       n.minobsinnode=20,shrinkage=0.1,n.trees=50,interaction.depth=2)

medias4bis<-as.data.frame(medias4[1])
medias4bis$modelo<-"gbm"
predi4<-as.data.frame(medias4[2])
predi4$gbm<-predi4$Yes

medias5<-cruzadaxgbmbin(data=archivo,
                        vardep=vardep,listconti=listconti,
                        listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                        min_child_weight=10,eta=0.03,nrounds=100,max_depth=6,
                        gamma=0,colsample_bytree=1,subsample=1,
                        alpha=0,lambda=0,lambda_bias=0)


medias5bis<-as.data.frame(medias5[1])
medias5bis$modelo<-"xgbm"
predi5<-as.data.frame(medias5[2])
predi5$xgbm<-predi5$Yes


medias6<-cruzadaSVMbin(data=archivo,
                       vardep=vardep,listconti=listconti,
                       listclass=listclass,grupos=grupos,
                       sinicio=sinicio,repe=repe,C=0.1)

medias6bis<-as.data.frame(medias6[1])
medias6bis$modelo<-"svmLinear"
predi6<-as.data.frame(medias6[2])
predi6$svmLinear<-predi6$Yes


medias7<-cruzadaSVMbinPoly(data=archivo,
                           vardep=vardep,listconti=listconti,
                           listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                           C=0.01,degree=2,scale=0.1)

medias7bis<-as.data.frame(medias7[1])
medias7bis$modelo<-"svmPoly"
predi7<-as.data.frame(medias7[2])
predi7$svmPoly<-predi7$Yes

medias8<-cruzadaSVMbinRBF(data=archivo,
                          vardep=vardep,listconti=listconti,
                          listclass=listclass,grupos=grupos,
                          sinicio=sinicio,repe=repe,
                          C=0.5,sigma=0.05)

medias8bis<-as.data.frame(medias8[1])
medias8bis$modelo<-"svmRadial"
predi8<-as.data.frame(medias8[2])
predi8$svmRadial<-predi8$Yes

union1bis<-rbind(medias1bis,medias2bis,
              medias3bis,medias4bis,medias5bis,medias6bis,
              medias7bis,medias8bis)

par(cex.axis=0.8)
boxplot(data=union1bis,tasa~modelo,col="pink",main='TASA FALLOS')
boxplot(data=union1bis,auc~modelo,col="pink",main='AUC')




# CONSTRUCCIÓN DE TODOS LOS ENSAMBLADOS
# SE UTILIZARÁN LOS ARCHIVOS SURGIDOS DE LAS FUNCIONES LLAMADOS predi1,...

unipredi<-cbind(predi1,predi2,predi3,predi4,predi5,predi6,predi7,predi8)

# Esto es para eliminar columnas duplicadas
unipredi<- unipredi[, !duplicated(colnames(unipredi))]

# Construccion de ensamblados, cambiar al gusto

unipredi$predi9<-(unipredi$logi+unipredi$avnnet)/2
unipredi$predi10<-(unipredi$logi+unipredi$rf)/2
unipredi$predi11<-(unipredi$logi+unipredi$gbm)/2
unipredi$predi12<-(unipredi$logi+unipredi$xgbm)/2
unipredi$predi13<-(unipredi$logi+unipredi$svmLinear)/2
unipredi$predi14<-(unipredi$logi+unipredi$svmPoly)/2
unipredi$predi15<-(unipredi$logi+unipredi$svmRadial)/2
unipredi$predi16<-(unipredi$avnnet+unipredi$rf)/2
unipredi$predi17<-(unipredi$avnnet+unipredi$gbm)/2
unipredi$predi18<-(unipredi$avnnet+unipredi$xgbm)/2
unipredi$predi19<-(unipredi$avnnet+unipredi$svmLinear)/2

unipredi$predi20<-(unipredi$avnnet+unipredi$svmPoly)/2
unipredi$predi21<-(unipredi$avnnet+unipredi$svmRadial)/2
unipredi$predi22<-(unipredi$rf+unipredi$gbm)/2
unipredi$predi23<-(unipredi$rf+unipredi$xgbm)/2
unipredi$predi24<-(unipredi$rf+unipredi$svmLinear)/2
unipredi$predi25<-(unipredi$rf+unipredi$svmPoly)/2
unipredi$predi26<-(unipredi$rf+unipredi$svmRadial)/2
unipredi$predi27<-(unipredi$gbm+unipredi$xgbm)/2
unipredi$predi28<-(unipredi$gbm+unipredi$svmLinear)/2
unipredi$predi29<-(unipredi$gbm+unipredi$svmPoly)/2
unipredi$predi30<-(unipredi$gbm+unipredi$svmRadial)/2
unipredi$predi31<-(unipredi$logi+unipredi$avnnet+unipredi$rf)/3
unipredi$predi32<-(unipredi$logi+unipredi$avnnet+unipredi$gbm)/3
unipredi$predi33<-(unipredi$logi+unipredi$avnnet+unipredi$xgbm)/3
unipredi$predi34<-(unipredi$logi+unipredi$avnnet+unipredi$svmLinear)/3
unipredi$predi35<-(unipredi$logi+unipredi$avnnet+unipredi$svmPoly)/3
unipredi$predi36<-(unipredi$logi+unipredi$avnnet+unipredi$svmRadial)/3
unipredi$predi37<-(unipredi$logi+unipredi$rf+unipredi$gbm)/3
unipredi$predi38<-(unipredi$logi+unipredi$rf+unipredi$xgbm)/3
unipredi$predi39<-(unipredi$logi+unipredi$rf+unipredi$svmLinear)/3

unipredi$predi40<-(unipredi$logi+unipredi$rf+unipredi$svmPoly)/3
unipredi$predi41<-(unipredi$logi+unipredi$rf+unipredi$svmRadial)/3
unipredi$predi42<-(unipredi$logi+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi43<-(unipredi$logi+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi44<-(unipredi$logi+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi45<-(unipredi$logi+unipredi$gbm+unipredi$svmPoly)/3
unipredi$predi46<-(unipredi$logi+unipredi$gbm+unipredi$svmRadial)/3
unipredi$predi47<-(unipredi$logi+unipredi$xgbm+unipredi$svmLinear)/3
unipredi$predi48<-(unipredi$logi+unipredi$xgbm+unipredi$svmPoly)/3
unipredi$predi49<-(unipredi$logi+unipredi$xgbm+unipredi$svmRadial)/3
unipredi$predi50<-(unipredi$rf+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi51<-(unipredi$rf+unipredi$gbm+unipredi$svmPoly)/3
unipredi$predi52<-(unipredi$rf+unipredi$gbm+unipredi$svmRadial)/3
unipredi$predi53<-(unipredi$rf+unipredi$xgbm+unipredi$svmLinear)/3
unipredi$predi54<-(unipredi$rf+unipredi$xgbm+unipredi$svmPoly)/3
unipredi$predi55<-(unipredi$rf+unipredi$xgbm+unipredi$svmRadial)/3
unipredi$predi56<-(unipredi$rf+unipredi$avnnet+unipredi$gbm)/3
unipredi$predi57<-(unipredi$rf+unipredi$avnnet+unipredi$xgbm)/3
unipredi$predi58<-(unipredi$rf+unipredi$avnnet+unipredi$svmLinear)/3
unipredi$predi59<-(unipredi$rf+unipredi$avnnet+unipredi$svmPoly)/3

unipredi$predi60<-(unipredi$rf+unipredi$avnnet+unipredi$svmRadial)/3
unipredi$predi61<-(unipredi$avnnet+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi62<-(unipredi$avnnet+unipredi$gbm+unipredi$svmPoly)/3
unipredi$predi63<-(unipredi$avnnet+unipredi$gbm+unipredi$svmRadial)/3
unipredi$predi64<-(unipredi$logi+unipredi$rf+unipredi$gbm+unipredi$avnnet)/4
unipredi$predi65<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet)/4
unipredi$predi66<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet)/4
unipredi$predi67<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet+unipredi$svmLinear)/5
unipredi$predi68<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet+unipredi$svmPoly)/5
unipredi$predi69<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet+unipredi$svmRadial)/5


# Listado de modelos a considerar, cambiar al gusto

dput(names(unipredi))

listado<-c("logi", "avnnet", 
           "rf","gbm",  "xgbm", "svmLinear",  "svmPoly", 
           "svmRadial","predi9", "predi10", "predi11", "predi12", 
           "predi13", "predi14", "predi15", "predi16", "predi17", "predi18", 
           "predi19", "predi20", "predi21", "predi22", "predi23", "predi24", 
           "predi25", "predi26", "predi27", "predi28", "predi29", "predi30", 
           "predi31", "predi32", "predi33", "predi34", "predi35", "predi36", 
           "predi37", "predi38", "predi39", "predi40", "predi41", "predi42", 
           "predi43", "predi44", "predi45", "predi46", "predi47", "predi48", 
           "predi49", "predi50", "predi51", "predi52", "predi53", "predi54", 
           "predi55", "predi56", "predi57", "predi58", "predi59", "predi60", 
           "predi61", "predi62", "predi63", "predi64", "predi65", "predi66", 
           "predi67", "predi68", "predi69")

# Cambio a Yes, No, todas las predicciones

# Defino funcion tasafallos

tasafallos<-function(x,y) {
  confu<-confusionMatrix(x,y)
  tasa<-confu[[3]][1]
  return(tasa)
}

auc<-function(x,y) {
  curvaroc<-roc(response=x,predictor=y)
  auc<-curvaroc$auc
  return(auc)
}

# Se obtiene el numero de repeticiones CV y se calculan las medias por repe en
# el data frame medias0

repeticiones<-nlevels(factor(unipredi$Rep))
unipredi$Rep<-as.factor(unipredi$Rep)
unipredi$Rep<-as.numeric(unipredi$Rep)


medias0<-data.frame(c())
for (prediccion in listado)
{
  unipredi$proba<-unipredi[,prediccion]
  unipredi[,prediccion]<-ifelse(unipredi[,prediccion]>0.5,"Yes","No")
  for (repe in 1:repeticiones)
  {
    paso <- unipredi[(unipredi$Rep==repe),]
    pre<-factor(paso[,prediccion])
    archi<-paso[,c("proba","obs")]
    archi<-archi[order(archi$proba),]
    obs<-paso[,c("obs")]
    tasa=1-tasafallos(pre,obs)
    t<-as.data.frame(tasa)
    t$modelo<-prediccion
    auc<-suppressMessages(auc(archi$obs,archi$proba))
    t$auc<-auc
    medias0<-rbind(medias0,t)
  }
}

# Finalmente boxplot

par(cex.axis=0.5,las=2)
boxplot(data=medias0,tasa~modelo,col="pink",main="TASA FALLOS")

# Para AUC se utiliza la variable auc del archivo medias0

boxplot(data=medias0,auc~modelo,col="pink",main="AUC")

# PRESENTACION TABLA MEDIAS

library(dplyr)
tablamedias<-medias0 %>%
  group_by(modelo) %>%
  summarize(tasa=mean(tasa))     

tablamedias<-as.data.frame(tablamedias[order(tablamedias$tasa),])


# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN TASA
# PARA EL GRAFICO

medias0$modelo <- with(medias0,
                       reorder(modelo,tasa, mean))
par(cex.axis=0.7,las=2)
boxplot(data=medias0,tasa~modelo,col="pink", main='TASA FALLOS')

# ************************************
# PARA AUC
# ************************************

# PRESENTACION TABLA MEDIAS

tablamedias2<-medias0 %>%
  group_by(modelo) %>%
  summarize(auc=mean(auc))     

tablamedias2<-tablamedias2[order(-tablamedias2$auc),]


# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN AUC
# PARA EL GRAFICO

medias0$modelo <- with(medias0,
                       reorder(modelo,auc, mean))
par(cex.axis=0.7,las=2)
boxplot(data=medias0,auc~modelo,col="pink", main='AUC', horizontal=FALSE,
        las=2, main = "Perpendicular")

#logi es la mejor en AUC.
#predi27 es la mejor en Tasa de errores.

# Se pueden escoger listas pero el factor hay que pasarlo a character
# para que no salgan en el boxplot todos los niveles del factor

listadobis<-c("logi", "avnnet", 
              "rf","gbm",  "xgbm", "svmLinear",  "svmPoly", 
              "svmRadial","predi27", "predi42", "predi13", "predi43","predi14","predi15") 

medias0$modelo<-as.character(medias0$modelo)

mediasver<-medias0[medias0$modelo %in% listadobis,]


mediasver$modelo <- with(mediasver,
                         reorder(modelo,auc, median))

par(cex.axis=0.9,las=2)
boxplot(data=mediasver,auc~modelo,xlab="",col="pink",main='AUC')



# GRÁFICOS DE APOYO PARA OBSERVAR COMPORTAMIENTO DE LOS MODELOS

unipredi<-cbind(predi1,predi2,predi3,predi4,predi5,predi6,predi7,predi8)
# Esto es para eliminar columnas duplicadas
unipredi<- unipredi[, !duplicated(colnames(unipredi))]
# Añadir ensamblados
unipredi$predi27<-(unipredi$gbm+unipredi$xgbm)/2
unipredi$predi42<-(unipredi$logi+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi43<-(unipredi$logi+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi14<-(unipredi$logi+unipredi$svmPoly)/2
unipredi$predi15<-(unipredi$logi+unipredi$svmRadial)/2
unipredi$predi13<-(unipredi$logi+unipredi$svmLinear)/2

# Me quedo con la primera repetición de validación cruzada para los análisis
#CUIDADO. Antes ponía Rep1 REP01
unigraf<-unipredi[unipredi$Rep=="Rep01",]
# Correlaciones entre predicciones de cada algoritmo individual
solos<-c("logi", "avnnet",
         "rf","gbm",  "xgbm", "svmLinear",  "svmPoly",
         "svmRadial")
mat<-unigraf[,solos]
matrizcorr<-cor(mat)
matrizcorr
library(corrplot)
corrplot(matrizcorr, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45,cl.lim=c(0.7,1),is.corr=FALSE)

library(ggplot2)

qplot(svmRadial,logi,data=unigraf,colour=obs)+
  geom_hline(yintercept=0.5, color="black", size=1)+
  geom_vline(xintercept=0.5, color="black", size=1)

qplot(predi27,logi,data=unigraf,colour=obs)+
  geom_hline(yintercept=0.5, color="black", size=1)+
  geom_vline(xintercept=0.5, color="black", size=1)

qplot(gbm,svmPoly,data=unigraf,colour=obs)+
  geom_hline(yintercept=0.5, color="black", size=1)+
  geom_vline(xintercept=0.5, color="black", size=1)



