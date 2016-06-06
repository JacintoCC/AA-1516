
###
#   PRÁCTICA 3 - JACINTO CARRASCO CASTILLO
###

# Realiza una pausa durante la ejecución del código
pause <- function(){
  cat("Pulse cualquier tecla")
  s <- scan()
}

# Obtención de una función que recibe un vector como entrada
# a partir de un hiperplano y devuelve el producto escalar con los
# coeficientes del hiperplano.
# @param vec Vector de coeficientes del hiperplano
# @param change_signs Valor lógico que determina si debemos cambiar
#                     el signo de los coeficientes del vector
# @return función que realiza el producto escalar de un vector con 
#         los coeficientes del hiperplano
hypplane_to_func <- function( vec, change_signs = FALSE ){
  if(change_signs){
    vec[2:length(vec)] <- -vec[2:length(vec)]
  }
  f <- function(x){
    # @return( vec[1]*x[1]+vec[2]*x[2]+vec[3]*x[3] )
    return( x %*% vec )
  }
  return(f)
}


# Establecimiento de la semilla
set.seed(3141592)



# EJERCICIO 1

library(ISLR)
data("Auto")

### Apartado a)

pairs(Auto)
pause()

boxplot(Auto$mpg ~ Auto$cylinders, main = "Boxplot MPG ~ cylinders",
        xlab = "cylinders", ylab = "MPG")
pause()

### Apartado c)

num.datos <- length(Auto)
train.index.Auto <- sample(1:num.datos, size = num.datos*0.8, 
                           replace = F)

train.Auto <- Auto[train.index.Auto,]
test.Auto <- Auto[-train.index.Auto,]

### Apartado d)

mpg01 <- sign(Auto$mpg - median(Auto$mpg))
Auto <- data.frame(cbind(mpg01,Auto[-1]))

par(mfrow=c(2,2))
boxplot(Auto$mpg01 ~ Auto$cylinders, main = "Boxplot MPG ~ cylinders",
        xlab = "cylinders", ylab = "MPG01")
boxplot(Auto$displacement ~ Auto$mpg01, 
        main = "Boxplot displacement ~  MPG01",
        ylab = "displacement", xlab = "MPG01")
boxplot(Auto$weight ~ Auto$mpg01, 
        main = "Boxplot weight ~  MPG01",
        ylab = "weight", xlab = "MPG01")
boxplot(Auto$horsepower ~ Auto$mpg01, 
        main = "Boxplot horsepower ~  MPG01",
        ylab = "horsepower", xlab = "MPG01")
par(mfrow=c(1,1))
pause()

boxplot( Auto$acceleration ~ Auto$mpg01, 
        main = "Boxplot Aceleración ~  MPG01",
        ylab = "Aceleración", xlab = "MPG01")
pause()

high_mpg <- Auto[Auto$mpg01==1,
                 c("mpg01","displacement", "cylinders", 
                   "horsepower", "weight")]
low_mpg <- Auto[Auto$mpg01==-1,
                c("mpg01","displacement", "cylinders", 
                  "horsepower", "weight")]

train.index.h <- sample(1:nrow(high_mpg), size = 0.8*nrow(high_mpg),
                        replace = F)
train.index.l <- sample(1:nrow(low_mpg), size = 0.8*nrow(low_mpg),
                        replace = F)

train.Auto <- rbind(high_mpg[train.index.h,],
                    low_mpg[train.index.l,])
test.Auto <- rbind(high_mpg[-train.index.h,],
                    low_mpg[-train.index.l,])

train.Auto[,1] <- sign(train.Auto[,1]-median(train.Auto[,1]))
test.Auto[,1] <- sign(test.Auto[,1]-median(test.Auto[,1]))

library(e1071)
library(ROCR)


glm.model <- glm(as.factor(mpg01) ~ horsepower + weight + 
                   displacement + cylinders,
                 data = Auto, family = "binomial")
coefs <- coef(glm.model)
print(coefs)
pause()


prob.1.glm <- predict.glm(glm.model, newdata = test.Auto, 
                          type = "response")
pred.label.glm <- ifelse(prob.1.glm > 0.5, 1, -1)
prediction.glm <- prediction(prob.1.glm,test.Auto[,1])
E.test <- sum(pred.label.glm != test.Auto["mpg01"])/nrow(test.Auto)*100
cat("Error de test para regresión logística: ", E.test)
pause()


library(class)

scale.factor <- scale(train.Auto[-1])
test.Auto[-1] <- scale(test.Auto[-1], 
                       center = attr(scale.factor,"scaled:center"),
                       scale = attr(scale.factor,"scaled:scale"))


knn.model <- knn(train.Auto[,-1], test.Auto[,-1], train.Auto[,1],
                 k=1, prob = TRUE)
pred.label.knn <- sign(as.numeric(knn.model)-1.5)
prob.1.knn <- sapply(1:length(knn.model), function(i)
  if(pred.label.knn[i]==1)  return(attr(knn.model,"prob")[i])
  else return(1- attr(knn.model,"prob")[i]))
E.test <-sum(test.Auto[,"mpg01"]!=pred.label.knn)/nrow(test.Auto)*100
cat("Error de test para 1-NN: ", E.test)
pause()


tuned.knn <- tune.knn(as.matrix(train.Auto[,-1]),
                      as.factor(train.Auto[,1]), 
                      k=1:10,
                      tunecontrol = tune.control(sampling = "cross"))
best.k <- tuned.knn$best.parameters[1]
best.model.knn <- knn(train.Auto[,-1], test.Auto[,-1], 
                      train.Auto[,1], k=best.k, prob = TRUE)

pred.label.knn <- sign(as.numeric(best.model.knn)-1.5)
prob.1.knn <- sapply(1:nrow(test.Auto), function(i)
  if(pred.label.knn[i]==1)  return(attr(best.model.knn,"prob")[i])
  else return(1- attr(best.model.knn,"prob")[i]))

prediction.knn <- prediction(prob.1.knn,test.Auto[,1])
E.test <- sum(test.Auto[,1] != pred.label.knn)/nrow(test.Auto)*100
cat("Error en test", paste(best.k,"-NN: ",sep=""), E.test)
pause()

perf.glm <- performance(prediction.glm,"tpr","fpr")
auc.glm <- performance(prediction.glm,"auc")@y.values[[1]]
plot(perf.glm@x.values[[1]],perf.glm@y.values[[1]],type='l', 
     main = paste('Área Bajo la Curva GLM:', auc.glm), 
     xlab='Tasa falsos positivos',
     ylab = 'Tasa verdaderos positivos')
segments(0,0,1,1, col=3)
pause()


perf.knn <- performance(prediction.knn,"tpr","fpr")
auc.knn <- performance(prediction.knn,"auc")@y.values[[1]]
plot(perf.knn@x.values[[1]],perf.knn@y.values[[1]],type='l', 
     main = paste('Área Bajo la Curva KNN:', auc.knn),
     xlab='Tasa falsos positivos',
     ylab = 'Tasa verdaderos positivos')
segments(0,0,1,1, col=3)
pause()


plot(perf.glm@x.values[[1]],perf.glm@y.values[[1]],type='l', 
     main = 'Curvas ROC GLM y KNN', xlab='Tasa falsos positivos',
     ylab = 'Tasa verdaderos positivos')
points(perf.knn@x.values[[1]],perf.knn@y.values[[1]],type='l', col=4)
segments(0,0,1,1, col=3)
pause()

### e) Bonus 1. Estimar el error de test de ambos modelos (RL, K-NN) pero usando validación cruzada de 5-particiones. Comparar con los resultados obtenidos en el punto anterior. 

### f) Bonus 2. Ajustar el mejor modelo de regresión posible considerando la variable *mpg* como salida y el resto como predictoras. Justificar el modelo ajustado en base al patrón de los residuos. Estimar su error de entrenamiento y test.

detach("package:ROCR")
detach("package:e1071")
detach("package:ISLR")
detach("package:class")

# EJERCICIO 2

set.seed(3141592)
library(MASS)
library(glmnet)

data("Boston")

#### Apartado a)
x <- model.matrix(crim~., Boston)[,-1]
y <- Boston$crim

cv.lasso <- cv.glmnet(x, y, family = "gaussian")

lambda.min <- cv.lasso$lambda.min
cat("Lambda con menor error: ", lambda.min)
pause()

train.index <- sample(1:nrow(x), size = nrow(x)*4/5)

x.train.boston <- x[train.index,]
y.train.boston <- y[train.index]

x.test.boston <- x[-train.index,]
y.test.boston <- y[-train.index]

threshold <- 0.5
lasso.model <- glmnet(x.train.boston, y.train.boston, 
                      lambda = lambda.min)
lasso.coef <- predict(lasso.model, type = "coeff", s=lambda.min)[1:14,]
selected.var <- names(lasso.coef[abs(lasso.coef) > threshold])[-1]
print(selected.var)
pause()

#### Apartado b)
x <- model.matrix(crim~., Boston[c("crim",selected.var)])[,-1]

x.train.boston <- x[train.index,]
x.test.boston <- x[-train.index,]

cv.wdecay <- cv.glmnet(x.train.boston, y.train.boston, 
                       family = "gaussian", alpha=0) 

lambda.min <- cv.wdecay$lambda.min 
cat("Lambda con menor error: ", lambda.min)
pause()

ridge.mod = glmnet(x, y, alpha=0, lambda=lambda.min)
ridge.pred = predict (ridge.mod, s=lambda.min, newx=x.test.boston)
cat("MSE óptimo", mean((ridge.pred - y.test.boston)^2))
pause()

ridge.mod = glmnet(x, y, alpha=0, lambda=0)
ridge.pred = predict (ridge.mod, s=lambda.min, newx=x.test.boston)
cat("MSE (lambda=0)", mean((ridge.pred - y.test.boston)^2))
pause()


x <- model.matrix(crim~., Boston)[,-1]
train.index <- sample(1:nrow(x), size = nrow(x)*4/5)
x.train.boston <- x[train.index,]
x.test.boston <- x[-train.index,]

cv.wdecay <- cv.glmnet(x.train.boston, y.train.boston, 
                       family = "gaussian", alpha=0) 

lambda.min <- cv.wdecay$lambda.min 
cat("Lambda con menor error: ", lambda.min)
pause()


ridge.mod = glmnet(x, y, alpha=0, lambda=lambda.min)
ridge.pred = predict (ridge.mod, s=lambda.min, newx=x.test.boston)
cat("MSE todos los param:", mean((ridge.pred - y.test.boston)^2))
pause()

#### Apartado c)
library(e1071)

label.crim <- sign(Boston$crim - median(Boston$crim))
Boston.class <- data.frame(x=Boston[-1], y=as.factor(label.crim))
svm.model  <- svm(y~., data = Boston.class, kernel="linear")

y.svm.pred = predict(svm.model, Boston.class)
confusion.matrix <- table(predict= y.svm.pred, truth= Boston.class$y)
E_in <- (confusion.matrix[2,1]+confusion.matrix[1,2])/sum(confusion.matrix)
print(confusion.matrix)
cat("Ein", E_in*100)
pause()

x <- model.matrix(y~., Boston.class)[,-1]
y <- label.crim

train.index <- sample(1:nrow(x), size = nrow(x)*4/5)

train.boston <- Boston.class[train.index,]
test.boston <- Boston.class[-train.index,]

tune.out <- tune(svm, y~., data = train.boston, kernel="linear",
                 ranges=list(cost= c(0.001, 0.01, 0.1, 1, 5, 10, 100))
                 )
best.cost.svm <- tune.out$best.parameters[,1]
cat("Mejor valor del parámetro cost:", best.cost.svm)
pause()

svm.model  <- svm(y~., data = train.boston, kernel="linear")
y.svm.pred = predict(svm.model , test.boston)

confusion.matrix <- table(predict=y.svm.pred, truth=test.boston$y)
E_test.linear <- (confusion.matrix[2,1]+confusion.matrix[1,2])/
  sum(confusion.matrix)
print(confusion.matrix)
cat("E_test para núcleo lineal", E_test.linear*100)
pause()

tune.out <- tune(svm, y~., data = train.boston, kernel="polynomial",
                 ranges=list(cost= c(0.001, 0.01, 0.1, 1, 5, 10, 100))
                 )
best.cost.svm <- tune.out$best.parameters[,1]
svm.model  <- svm(y~., data = train.boston, kernel="polynomial")
y.svm.pred = predict(svm.model , test.boston)

confusion.matrix <- table(predict=y.svm.pred, truth=test.boston$y)
E_test.polynomial <- (confusion.matrix[2,1]+confusion.matrix[1,2])/
  sum(confusion.matrix)
print(confusion.matrix)
cat("E_test para núcleo polinómico", E_test.polynomial*100)
pause()

tune.out <- tune(svm, y~., data = train.boston, kernel="radial",
                 ranges=list(cost= c(0.001, 0.01, 0.1, 1, 5, 10, 100))
                 )
best.cost.svm <- tune.out$best.parameters[,1]
svm.model  <- svm(y~., data = train.boston, kernel="radial")
y.svm.pred = predict(svm.model , test.boston)

confusion.matrix <- table(predict=y.svm.pred, truth=test.boston$y)
E_test.radial <- (confusion.matrix[2,1]+confusion.matrix[1,2])/
  sum(confusion.matrix)
print(confusion.matrix)
cat("E_test para núcleo de base radial", E_test.radial*100)
pause()

tune.out <- tune(svm, y~., data = train.boston, kernel="sigmoid",
                 ranges=list(cost= c(0.001, 0.01, 0.1, 1, 5, 10, 100))
                 )
best.cost.svm <- tune.out$best.parameters[,1]
svm.model  <- svm(y~., data = train.boston, kernel="sigmoid")
y.svm.pred = predict(svm.model , test.boston)

confusion.matrix <- table(predict=y.svm.pred, truth=test.boston$y)
E_test.sigmoid <- (confusion.matrix[2,1]+confusion.matrix[1,2])/
  sum(confusion.matrix)
print(confusion.matrix)
cat("E_test para núcleo sigmoidal", E_test.sigmoid*100)
pause()

#### Bonus. Estimar el error de entrenamiento y test por validación cruzada de 5 particiones.

detach("package:MASS")
detach("package:glmnet")
detach("package:e1071")

# EJERCICIO 3

library(MASS)
library(randomForest)

#### Apartado 1
set.seed(31415926)
data("Boston")
train.boston <- sample(1:nrow(Boston), size = nrow(Boston)*0.8)

#### Apartado 2
bag.boston = randomForest(medv~., data=Boston, subset=train.boston,
                          importance = TRUE )
y.pred.bag = predict( bag.boston, newdata=Boston[-train.boston,])
plot(y.pred.bag, Boston[-train.boston,"medv"],
     main="Pred. medv vs real", xlab = "Predicted", ylab = "Medv")
abline(0 ,1)
cat("MSE", mean ((y.pred.bag-Boston[-train.boston,"medv"])^2))
pause()

#### 3) Ajustar un modelo de regresión usando ``Random Forest''. Obtener una estimación del número de árboles necesario. Justificar el resto de parámetros usados en el ajuste. Calcular el error de test y compararlo con el obtenido con bagging.

error.stable <- FALSE
best.ntree <- 10
nTrees <- 10
error.prev <- Inf
error.best <- Inf
  
while( !error.stable && nTrees < 5000){
  mtry <- tuneRF(Boston[train.boston, -14], 
                 Boston[train.boston, "medv"],
                 ntreeTry = nTrees, trace = FALSE, plot = FALSE)
  mtry <- mtry[which.min(mtry[,2]),1]
  bag.boston <- randomForest(medv~., data=Boston, subset=train.boston,
                            mtry=mtry, importance = T, nTrees=nTrees)
  y.pred.rf <- predict( bag.boston, newdata=Boston[-train.boston,])
  error.current <- mean((y.pred.rf - Boston[-train.boston,"medv"])^2)
  
  if(error.best > error.current){
    best.ntree <- nTrees
    error.best <- error.current
  }
  
  if( abs(error.prev - error.current) < 0.01){
    error.stable <- TRUE
  }
  else{
    error.prev <- error.current
    nTrees <- nTrees +10
  }
}
cat("Best nTree", best.ntree)
pause()

mtry <- tuneRF(Boston[train.boston, -which(names(Boston) %in% "medv")],
               Boston[train.boston, "medv"], ntreeTry = best.ntree,
               trace = FALSE, plot=FALSE)
mtry <- mtry[which.min(mtry[,2]),1]
bag.boston = randomForest(medv~., data=Boston, subset=train.boston,
                          mtry=mtry, importance = T, nTrees=best.ntree)
y.pred.rf = predict( bag.boston, newdata=Boston[-train.boston,])
cat("MSE:", mean((y.pred.rf - Boston[-train.boston,"medv"])^2))
pause()

#### Apartado 4

detach("package:randomForest")
library(gbm)
library(caret)

boost.boston = gbm(medv~., data=Boston[train.boston,],
                    distribution="gaussian", n.trees=5000,
                    interaction.depth=4)
print(summary(boost.boston))
pause()

y.pred.boost = predict(boost.boston, newdata=Boston[-train.boston,],
                       n.trees = 5000)
cat("MSE:",mean((y.pred.boost - Boston[-train.boston,"medv"])^2))
pause()

boost.boston = gbm(medv~., data=Boston[train.boston,],
                    distribution="gaussian", n.trees=4450,
                    interaction.depth =5, shrinkage=0.01
                   ) 
y.pred.boost = predict(boost.boston, newdata=Boston[-train.boston,],
                       n.trees = 4450) 
cat("MSE:",mean((y.pred.boost - Boston[-train.boston,"medv"])^2))
pause()

detach("package:gbm")
detach("package:caret")

# EJERCICIO 4

library(ISLR)
data("OJ")
set.seed(3241592)


#### Apartado 1
library(tree)
train.OJ <- sample(1:nrow(OJ), size = 800, replace = FALSE)

tree.OJ <- tree(Purchase~., data=OJ, subset=train.OJ)

#### Apartado 2
summary(tree.OJ)
pause()

#### Apartado 3
plot(tree.OJ)
text(tree.OJ, pretty =0)
pause()

#### Apartado 4

tree.OJ.pred = predict(tree.OJ, OJ[-train.OJ,-1],type ="class")
confusion.matrix.OJ <-table(tree.OJ.pred, OJ[-train.OJ,1] )
print(confusion.matrix.OJ)
cat("Precisión", 
    100*sum(diag(confusion.matrix.OJ))/sum(confusion.matrix.OJ))
pause()

#### Apartado 5
tree.cv.OJ <- cv.tree(tree.OJ, K=5)
size.min.OJ <- tree.cv.OJ$size[which.min(tree.cv.OJ$dev)]
cat("Tamaño mínimo", size.min.OJ)
pause()

prune.OJ <- prune.tree(tree.OJ, best=size.min.OJ)
plot(prune.OJ)
text(prune.OJ)
pause()


tree.OJ.pred = predict(prune.OJ, OJ[-train.OJ,-1],type ="class")
confusion.matrix.OJ <-table(tree.OJ.pred, OJ[-train.OJ,1] )
print(confusion.matrix.OJ)
cat("Precisión", 
    100*sum(diag(confusion.matrix.OJ))/sum(confusion.matrix.OJ))
pause()

#### Bonus 4. Generar un gráfico con el tamaño del árbol en el eje x (número de nodos) y la tasa de error de validación cruzada en el eje y. ¿Qué tamaño de árbol corresponde a la tasa más pequeña de error de clasificación por validación cruzada?


plot(tree.cv.OJ$size, tree.cv.OJ$dev, type="b", ylab = "Error",
     main = "Error según nodos terminales", xlab = "Número de nodos",
     col=(7:1==which.min(tree.cv.OJ$dev))[7:1]+1)
pause()

detach("package:tree")
detach("package:ISLR")
