
#### ANALISIS DISCRIMINANTE ####

library(openxlsx)
library(tidyverse)
library(datasets)
library(MASS)

data(iris)

str(iris)

iris$Species <- as.factor(iris$Species)

str(iris)
summary(iris)

#Vamos a ver su distribución
par(mfrow= c(2,2))
hist(iris$Sepal.Length, main = 'Longitud del sépalo', xlab =  'Sepal.Length')
hist(iris$Sepal.Width, main= 'Grosor del sépalo', xlab = 'Sepal.Width')
hist(iris$Petal.Length, main= 'Longitud del pétalo', xlab= 'Petal.Length')
hist(iris$Petal.Width, main = 'Grosor del pétalo', xlab = 'pegtalo.grosor')
#No es distribución normal, tiene más sentido  hacerlo por especie

#boxplot por especie:

irisVer <- subset(iris, iris$Species == "versicolor")
irisSet <- subset(iris, iris$Species == "setosa")
irisVir <- subset(iris, iris$Species == "virginica")
summary(irisSet)
summary(irisVer)
summary(irisVir)

par(mfrow=c(1,3),mar=c(7,3,2,1))
boxplot(irisVer[,1:4], main="Versicolor",ylim = c(0,8),las=2)
boxplot(irisSet[,1:4], main="Setosa",ylim = c(0,8),las=2)
boxplot(irisVir[,1:4], main="Virginica",ylim = c(0,8),las=2)

par(mfrow=c(1,3))
hist(irisVer$Petal.Length,xlim=c(0,8),ylim=c(0,20), 
     main = 'Longitud del pétalo versicolor', xlab = 'Petal.Length')
hist(irisSet$Petal.Length,xlim=c(0,8),ylim=c(0,20), 
     main = 'Longitud del pétalo setosa', xlab = 'Petal.Length')
hist(irisVir$Petal.Length,xlim=c(0,8),ylim=c(0,20),
     main = 'Longitud del pétalo virginica', xlab = 'Petal.Length')

library(car)
#Se ve las funciones de densidad en la diagonal principal y la correlación
scatterplotMatrix(iris[1:50, 1:4])+
  title(main = 'SETOSA')  
scatterplotMatrix(iris[51:100, 1:4])+
  title(main = 'VERSICOLOR')
scatterplotMatrix(iris[101:150, 1:4])+
  title(main = 'VIRGINICA')


corSetosa <- cor(iris[1:50, 1:4])  #Se observa la matriz de correlaciones
corVersicolor <- cor(irisVer[,1:4])
corVirginica <- cor(iris[101:150, 1:4])
corIris <- cor(iris[1:4])

par(mfrow = c(1,1))
parcoord(iris[,1:4], col = iris[,5],var.label = TRUE)
par(xpd = TRUE)
legend(0.85, 0.6, as.vector(unique(iris$Species)), fill = c(1,2,3))


library(reshape2)
library(knitr)

#Se realiza Saphiro para contrastar la hipótesis de que las variables cumplen normalidad multivariante
datos_tidy <- melt(iris, value.name = "valor")
kable(datos_tidy %>% group_by(Species, variable) %>% 
        summarise(p_value_Shapiro.test = round(shapiro.test(valor)$p.value,5)))
#Vemos que Petal.Width en setosa y versicolor no cumple una distribución normal


#Box's M-test para comprobar si la matriz de varianzas es igual en todas las Species:

library(biotools)
boxM(data = iris[, -5], grouping = iris[, 5])


library(gmodels)
library(MASS)

ldaIris<- lda(data =  iris, iris$Species~.)

pred.prob<- predict(ldaIris)  #class es por defecto #Obtenemos la probabilidad de pertenecer a cada especie

pred.class<- predict(ldaIris)$class  #class es por defecto #Obtenemos la predicción de las Species (la que tiene probabilidad mayor)

table(pred.class, iris$Species)  #En la tabla observamos cuál es el error de nuestra predicción, acierta el 100% en las setosa, 48/50 en las versicolor y 49/50 en las virginica


irisPredic<- data.frame(iris, pred.class)  #El dataset original añadiéndole una columna con las predicciones de especie

#Si queremos predecir un iris que nos encontremos por el campo:

predict(ldaIris, newdata= data.frame(Sepal.Length=5,Sepal.Width=2, Petal.Length= 6, Petal.Width= 3))
predict(ldaIris, newdata= data.frame(Sepal.Length=5,Sepal.Width=2, Petal.Length= 6, Petal.Width= 3))$class

#Representación gráfica de las clasificaciones:

library(klaR)
partimat(Species ~ .,
         data = iris, method = "lda", prec = 200,
         image.colors = c("darkgoldenrod1", "snow2", "skyblue2"),
         col.mean = "firebrick", main = 'LDA')


qdaIris <- qda(Species~., iris)
p.qda<- predict(qdaIris)
c.qda<- predict(qdaIris)$class
table(c.qda, iris$Species)

irisPredic2<- data.frame(irisPredic, c.qda)  #Observamos que los resultados son los mismos en ambos análisis discriminantes

        