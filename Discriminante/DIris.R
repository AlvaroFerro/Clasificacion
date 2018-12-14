#Como siempre al principio realizamos la carga de librerías
library(MASS)
library(tidyverse)
library(klaR)


require(scales)
require(gridExtra)
library(graphics)

##Realizamos el text m de box 
library(biotools)
boxM(iris[,1:4], iris[,5])

##Tambi


##Establecemos la semilla para que los resultados no cambien
set.seed(123)
data <- iris
##Primero comenzamos con una exploración de los datos.
View(iris)
##Ahora su estructura
str(data)
head(iris) 
dim(iris)

cor(iris[,-5])
set.seed(123)
##Ahora establecemos la muestra de entrenamiento
training_sample <- sample(c(T,F),nrow(iris),replace = T,prob = c(0.6,0.4)) ##Funcion sample saca una muestra del data frame
train <- iris[training_sample,]
test <- iris[!training_sample,]

##Y ahora realizariamos el análisis discriminante.
set.seed(123)
lda.iris <- lda(Species ~., train)

lda.iris
##Ahora realizamos un plot en el cual se observan las posiciones de las diferentes especies
plot(lda.iris,col = as.integer(train$Species))
plot(lda.iris, dimen=1,type = 'b')
##Realizamos otro plot, que indica esto?
par()
partimat(Species ~.,data = train , method = 'lda')


##Ahora realizamos la predicción.
lda.train <- predict(lda.iris)
train$lda <- lda.train$class
table(train$lda,train$Species)##Y los resultados que podemos observar, la predicción sale perfecta en función de cada una de las especies de plantas

grid.table(tableGrob(train$lda,train$Species), tableGrob(test$lda,train$Species))
##Ahora analizamos la parte del test.
lda.test <- predict(lda.iris,test)
test$lda <- lda.test$class
##Como podemos observar en nuestro test se han producido 2 fallos, 1 virginica en versicolor y otra versicolor en virgilica.
table(test$lda,test$Species)

qda.iris <- qda(Species ~Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,data = train)



qda.iris
##Ahora realizamos la representación gráfica.
partimat(Species ~Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, method = 'qda',data = train, main = "Partimat QDA")


qda.train <- predict(qda.iris)
train$qda <- qda.train$class
table(train$qda,train$Species)



qda.test <- predict(qda.iris,test)
test$qda <- qda.test$class
table(test$qda,test$Species)

library(ggplot2)
library(gridExtra)

dat <- data.frame(iris$Sepal.Length, iris$Species)
SL <-ggplot(dat,aes(x=iris.Sepal.Length)) + 
  geom_histogram(data=subset(dat,iris.Species == "setosa"),fill = "red", alpha = 0.4, bins = 50) +
  geom_histogram(data=subset(dat,iris.Species == "versicolor"),fill = "blue", alpha = 0.4, bins = 50) +
  geom_histogram(data=subset(dat,iris.Species == "virginica"),fill = "green", alpha = 0.4, bins = 50)
dat <- data.frame(iris$Sepal.Width, iris$Species)
SW <-ggplot(dat,aes(x=iris.Sepal.Width)) + 
  geom_histogram(data=subset(dat,iris.Species == "setosa"),fill = "red", alpha = 0.2, bins = 50) +
  geom_histogram(data=subset(dat,iris.Species == "versicolor"),fill = "blue", alpha = 0.2, bins = 50) +
  geom_histogram(data=subset(dat,iris.Species == "virginica"),fill = "green", alpha = 0.2, bins = 50)
dat <- data.frame(iris$Petal.Length, iris$Species)
PL <-ggplot(dat,aes(x=iris.Petal.Length)) + 
  geom_histogram(data=subset(dat,iris.Species == "setosa"),fill = "red", alpha = 0.4, bins = 50) +
  geom_histogram(data=subset(dat,iris.Species == "versicolor"),fill = "blue", alpha = 0.4, bins = 50) +
  geom_histogram(data=subset(dat,iris.Species == "virginica"),fill = "green", alpha = 0.4, bins = 50)
dat <- data.frame(iris$Petal.Width, iris$Species)
PW <-ggplot(dat,aes(x=iris.Petal.Width)) + 
  geom_histogram(data=subset(dat,iris.Species == "setosa"),fill = "red", alpha = 0.4, bins = 50) +
  geom_histogram(data=subset(dat,iris.Species == "versicolor"),fill = "blue", alpha = 0.4, bins = 50) +
  geom_histogram(data=subset(dat,iris.Species == "virginica"),fill = "green", alpha = 0.4, bins = 50)

# Setosa -> Red
# Versicolor -> Blue
# Virginica -> Green
grid.arrange(SL, SW,PL, PW, ncol = 2)
