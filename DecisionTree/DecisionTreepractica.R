library(readxl)
library(tibble)
library(caTools)

ruta <- 'datos.xlsx'

datos <- read_xlsx(ruta)


colnames(datos) <- c('Hogar','AyudaFamilias','RentaMenos16','VacacionesOutdoor',
                     'CapacidadAfrontar', 'TVColor', 'Ordenador','LlegarFinMes', 'RegimenTenencia',
                     'Miembros','RentaTotalAnterior',
                     'HogarPobreza', 'Region', 'EdadMayor', 'HorasSemanales', 'Mayores16','SexoMayor','ActMayor')


#### Tratamiento de variables ######

#Quitamos la columna de Renta pues incurririamos en un problema circular como usted bien ha explicado
datos <- datos[,c(-1, -6, -11, -13, -3, -17)]
str(datos)

datos$HogarPobreza <- as.factor(as.character(datos$HogarPobreza))


datos$VacacionesOutdoor <- as.factor(datos$VacacionesOutdoor)
datos$CapacidadAfrontar <- as.factor(datos$CapacidadAfrontar)
datos$Ordenador <- as.factor(datos$Ordenador)
datos$LlegarFinMes <- as.factor(datos$LlegarFinMes)
datos$RegimenTenencia <- as.factor(datos$RegimenTenencia)
datos$Miembros <- as.numeric(datos$Miembros)
datos$SexoMayor <- as.factor(datos$SexoMayor)
datos$ActMayor <- as.factor(datos$ActMayor)

str(datos)
datos <- as.data.frame(datos)
#### Modelo de Regresion Linar #####
str(datos)
size <- floor(nrow(datos) * 0.6)

set.seed(123)
train_ind <- sample(seq_len(nrow(datos)), size = size)
training.set <- datos[train_ind, ]
test.set <- datos[-train_ind, ]

modelo01 <- glm(HogarPobreza ~ ., family = 'binomial'(link = 'logit'), data = training.set)
summary(modelo01)
exp(coef(modelo01))

anova(modelo01, test = "Chisq")

#Now the results are consistent, and no longer dependent 
#on their level of aggregation (tabulation). I have notified the maintainer of the pscl package. Maybe he has some interest.
library(pscl)
pR2(modelo01, 4)

str(test.set)
str(training.set)

fitted.results <- predict(modelo01, newdata = test.set, type = 'response')
fitted.results <- ifelse(fitted.results > 0.68, 1,0)

logit.perf <- table(test.set$HogarPobreza, fitted.results, dnn = c("Actual", "Predicted"))
logit.perf

misClasificError <- mean(fitted.results != test.set$HogarPobreza)
print(paste('Accuracy',1-misClasificError))

#ARBOLES DE CLASIFICACIÓN
#datos recoge todoe l data set que comprende 477 observaciones y 12 variables
set.seed(123)
train <- sample(nrow(datos), 0.6*nrow(datos)) #Para el train nos quedaremos con el 70% de la sobservaciones
datos.train<- datos[train,]# 286 observaciones y 12 variables

datos.test <- datos[-train,]# 191 observaciones y 12 variables
datos$HogarPobreza<- factor(datos$HogarPobreza, levels=c(0,1), labels=c("No en riesgo de pobreza", "Sí en riesgo de pobreza"))
table(datos.train$HogarPobreza)# 210 no están en riesgo de pobreza y 123 que sí lo están
table(datos.test$HogarPobreza)# 81 no están en riesgo de pobreza y 63 sí que lo están

library(rpart)
set.seed(123)
arbol <- rpart(HogarPobreza ~ ., data=datos.train, method="class",parms=list(split="information"))
plot(arbol, uniform = TRUE, branch=0.4, compress=FALSE)
text(arbol, use.n = TRUE, cex = 0.75, all=TRUE)
print(arbol)
summary(arbol)
arbol$cptable #esta es la tabla de complejidad paramétrica en donde tendremos los erroresde validación cruzada, tendremos que ver cuál de ellos es el que 
#minimiza el error de cross-validation
plotcp(arbol)
printcp(arbol)
plot(arbol)
text(arbol, use.n = TRUE, cex = 0.75, all=TRUE)
arbol$cptable[which.min(arbol$cptable[,"xerror"]),"CP"]# el error que minimiza la validación cruzada es cp=0.01
arbol.podado = prune(arbol, cp = 0.02884615)
arbol.podado

plot(arbol.podado, uniform = TRUE, branch=0.4, compress=FALSE)
text(arbol.podado, use.n = TRUE, cex = 0.75, all=TRUE)


library(rpart.plot)

prp(arbol.podado, type = 2, extra = 104,fallen.leaves = TRUE, main="Decision Tree")
arbol.pred <- predict(arbol.podado, datos.test, type="class")

arbol.perf <- table(datos.test$HogarPobreza, arbol.pred,dnn=c("Actual", "Predicted"))
arbol.perf
rpart.plot(arbol.podado)
rpart.plot(arbol.podado, box.palette="GnBu",branch.lty=3, shadow.col="gray", nn=TRUE,main="Árbol de clasificación usando rpart.plot")


install.packages("partykit")
library(partykit)

plot(as.party(arbol.podado))

#El paequete party proporciona árboles de regresión no paramétrixa para respuestas nominales, ordinales, numéricas, censuradas o multivariantes
#El crecmimiento del árbol se basa en reglas estadísticas de parada, de forma que no se hace necesaria la poda
library(party)

arbol.party1 = ctree(HogarPobreza ~ ., datos.train)
plot(arbol.party1, main="Árbol de inferencia condicional para los Hogares en riesgo de pobreza")
ctree.pred <- predict(arbol.party1, datos.test, type="response")
ctree.perf <- table(datos.test$HogarPobreza, ctree.pred, dnn=c("Actual", "Predicted"))
ctree.perf












