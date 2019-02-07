# Se cargan las librerías necesarias

library(ggplot2)
library(dplyr)
library(mvnormtest)
library(biotools)
library(klaR)

# Se carga el dataset ya limpio, hallado en el anterior script

read.csv("DatasetLimpio.csv", row.names = 1) -> data

data_discriminante <- data[, -10] # Para el discriminante se usará las columna de las 3 variables

summary(data_discriminante) 
## Como existen muchas variables de tipo factor, sólo se podrá comprobar la normalidad y heterocedasticidad en las numéricas
## De esta manera también se representará gráficamente estas variables, y algunas de los factores

ggplot(data = data_discriminante, mapping = aes(x = Edad)) +
  geom_bar(mapping = aes(fill = Categoria3))
## Esta claramente desviada hacia la derecha esta distribución. No parece que haya un reparto heterogéneo según categoría

ggplot(data = data_discriminante, mapping = aes(x = Superficie)) +
  geom_bar(mapping = aes(fill = Categoria3))
## Podría ser normal, pero tiene algún atípico en la parte derecha

ggplot(data = data_discriminante, mapping = aes(x = Ingresos)) +
  geom_bar()
## Parece normal

ggplot(data = data_discriminante, mapping = aes(x = TamanoMunicipio)) +
  geom_bar(mapping = aes(fill = Categoria3)) # No hay diferencia

ggplot(data = data_discriminante, mapping = aes(x = DensidadZona)) +
  geom_bar(mapping = aes(fill = Categoria3)) # No se detecta gran diferencia

ggplot(data = data_discriminante, mapping = aes(x = Sexo)) +
  geom_bar(mapping = aes(fill = Categoria3)) # Tampoco

ggplot(data = data_discriminante, mapping = aes(x = Estudios)) +
  geom_bar(mapping = aes(fill = Categoria3)) # Mucho bajo-medio por debajo de ESO

ggplot(data = data_discriminante, mapping = aes(x = SituacionLaboral)) +
  geom_bar(mapping = aes(fill = Categoria3)) # Como es lógico parados y ocupados TP mucho consumo muy bajo. En los otros dos no hay grandes diferencias

ggplot(data = data_discriminante, mapping = aes(x = Vivienda)) +
  geom_bar(mapping = aes(fill = Categoria3)) # Alquiler mucho muy bajo, la otra 40-40 consumo no muy bajo


# Por si acaso, se contrastará con el test de Saphiro la normalidad de estas
data_disc_num <- data_discriminante[, c(3, 8:10)]

cons_muybajo <- data_disc_num %>%
  filter(Categoria3 == "Consumo muy bajo")

cons_bajomedio <- data_disc_num %>%
  filter(Categoria3 == "Consumo bajo-medio")

cons_medioalto <- data_disc_num %>%
  filter(Categoria3 == "Consumo medio-alto")

cons_muybajo <- t(cons_muybajo[, -4])
mshapiro.test(cons_muybajo)
cons_bajomedio <- t(cons_bajomedio[, -4])
mshapiro.test(cons_bajomedio)
cons_medioalto <- t(cons_medioalto[, -4])
mshapiro.test(cons_medioalto)

## Queda demostrada la no normalidad de las variables de cada categoría numérica

# HETEROCEDASTICIDAD

boxM(data = data_disc_num[, 1:3], data_disc_num[,4])
## Claramente se rechaza la hipótesis nula, dando evidencias de que la varianza no es constante en los grupos numéricos

# ESCALAR VARIABLES PARA CORREGIR LA NORMALIDAD

scale_var <- scale(data_disc_num[,1:3])
data_discriminante_fact <- data_discriminante[, c(1, 2, 4:7, 10)]
data_discriminante_sca <- cbind(scale_var, data_discriminante_fact)

# CREACIÓN MUESTRAS TRAIN Y TEST

set.seed(888)
muestra <- sample(4220)
muestra <- muestra[1:2954] # 70% train
data_train <- data_discriminante_sca[muestra, ]
data_test <- data_discriminante_sca[-muestra,]

# Más o menos las muestras guardan la proporción del dataset original
summary(data_train$Categoria3)
summary(data_test$Categoria3)
summary(data_discriminante$Categoria3)

# MODELO LINEAL DISCRIMINANTE

mod_lineal <- lda(Categoria3 ~ ., data = data_train) # Se crea el modelo
pred_lineal <- predict(object = mod_lineal, newdata = data_test) 

table(data_test$Categoria3, pred_lineal$class, dnn = c("Real", "Predicho")) # Se predice con el test 72,35% de acierto (algo bajo)

partimat(Categoria3 ~., data = data, method = "lda") # No me funciona
plot(pred_lineal$x, col = pred_lineal$class) # Claramente se distinguen los distintos grupos. Rojo y verde se identifican bien, pero negros puede estar difusos

# MODELO CUADRÁTICO DISCRIMINANTE

mod_quad <- qda(Categoria3 ~ ., data = data_train)
pred_quad <- predict(object = mod_quad, newdata = data_test)

table(data_test$Categoria3, pred_quad$class, dnn = c("Real", "Predicho")) # Acierto del 73,46% (algo superior pero insuficiente)
partimat(Categoria3 ~., data = data, method = "qda") # Tampoco funciona