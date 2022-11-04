##Tarea 9
##COA-501 Herramientas de cómputo para investigadores (R y Python)
## Alumno: Ivan Fermin Quiroz Ibañez


#1.- Con el conjunto de datos de Boston, ejecute los tres modelos lm, rf y nn y rf con un conjunto de datos
#de entrenamiento (train) con 70 % de las observaciones y 30 % para el conjunto de datos de prueba
#(test). Defina cinco funciones para calcular los cinco criterios de desempeño del modelo con base en los
#valores observado y predichos (mencionados en clase).
#Contraste sus resultados de desempeño en un Cuadro con los resultados con los mismos modelos lm, nn
#y rf con 300 y 206. Grafique los valores predichos de los tres modelos versus los valores observados, en
#los conjuntos de prueba. Comente sus resultados.

#lectura de base de detos
Boston <- read.csv("Boston_dataset.csv")
Boston <- Boston[,-1]
head(Boston, 10)

#Regresion lineal

# Split the data into training and testing set
set.seed(123)
index<- sample(1:nrow(Boston), round(0.70 * nrow(Boston)))
train_lm <- Boston[index,]
test_lm <- Boston[-index,]

#otra forma
#split <- sample.split(data, SplitRatio = 0.8)#muestra del 80% 
#train <- subset(data, split == "TRUE") #80%
#test <- subset(data, split == "FALSE") #20%

ml <- lm(medv~.,data = train_lm)
summary(ml)


#R^2
y_pred_lm <- predict(ml, test_lm)
SSE = sum((y_pred_lm-test_lm$medv)^2)
SST = sum((y_pred_lm-mean(test_lm$medv))^2)
r2_test = 1 - SSE/SST

#CME y RCME
MSE.lm <- sum((test_lm$medv-y_pred_lm)^2)/nrow(test_lm)
MSE.lm
RMSE.ml <- sqrt(MSE.lm)
RMSE.ml

#funciones mse,rms,rae,mae,mape
cme<- function(yobs, ypre)
{
  n <- length(yobs)
  suma.error <- sum ((yobs - ypre) ^ 2 ) / n
  return(suma.error)
}
cme.lm.test <- cme(test_lm$medv, y_pred_lm)
cme.lm.test


rms <- function(yobs, ypre)
{
  n <- length(yobs)
  error_ <- yobs - ypre
  suma.error <- sum (error_ ^ 2 )
  return(sqrt(suma.error / n))
}
rms.lm.test <- rms(test_lm$medv, y_pred_lm)
rms.lm.test

rae<- function(yobs, ypre)
{
  error_ <- yobs - ypre
  suma.error <- sum (error_ ^ 2 )
  suma.yi <- sum(yobs ^ 2)
  return(sqrt(suma.error / suma.yi ))
}
rae.lm.test <- rae(test_lm$medv, y_pred_lm)
rae.lm.test

mae<- function(yobs, ypre)
{
  n <- length(yobs)
  error_ <- yobs - ypre
  suma.error <- sum (abs(error_))
  return(suma.error / n )
}
mae.lm.test <- mae(test_lm$medv, y_pred_lm)
mae.lm.test

mape<- function(yobs, ypre)
{
  n <- length(yobs)
  error_ <- (yobs - ypre) / yobs
  suma.error <- sum (abs(error_))
  return((suma.error / n ) * 100 )
}
mape.lm.test <- mape(test_lm$medv, y_pred_lm)
mape.lm.test



# Plot observados vs predichos
plot(test_lm$medv, y_pred_lm, col = "red", 
     main = 'ML: Observados vs Predichos')
abline(lm(y_pred~test_$medv), lwd = 2)
text(10, 30, label=expression("R"^2* "="))
text(10.5,29, paste(round(cor(y_pred_lm,test_lm$medv)^2, 3)), pos=4)

#Random Forest
require(randomForest)
require(MASS)  #Package which contains the Boston housing dataset

# data(Boston)
attach(Boston)
set.seed(123)
dim(Boston)

class(Boston)


# Separeting Training and Test Sets

#training Sample with 354 observations (70%)

train = sample(1:nrow(Boston),354)
?Boston  #to search on the dataset

# We are going to use variable ′medv′ as the Response variable, 
# which is the Median Housing Value. We will fit 500 Trees.

#  Fitting the Random Forest

Boston.rf = randomForest(medv ~ . , data = Boston , subset = train)
Boston.rf

# Plotting the Error vs Number of Trees Graph.
plot(Boston.rf)

# Now we can compare the Out of Bag Sample Errors and Error on Test set

oob.err = double(13)
test.err = double(13)

#mtry is no of Variables randomly chosen at each split

prueba.set <- Boston[-train,]

for(mtry in 1:13) 
{
  rf=randomForest(medv ~ . , data = Boston , subset = train,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,Boston[-train,]) #Predictions on Test Set for each Tree
  
  test.err[mtry]= with(Boston[-train,], mean( (medv - pred)^2)) 
  
  # Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}


# error de prueba
test.err

#out of bag error estimation
oob.err

#
# Plotting both Test Error and Out of Bag Error
#

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))


#Metricas de random forest
y_pred_rf <- predict(Boston.rf, prueba.set)
cme.rf.test <- cme(prueba.set$medv,y_pred_rf)
cme.rf.test
rms.rf.test <- rms(prueba.set$medv,y_pred_rf)
rms.rf.test
rae.rf.test <- rae(prueba.set$medv,y_pred_rf)
rae.rf.test
mae.rf.test <- mae(prueba.set$medv,y_pred_rf)
mae.rf.test
mape.rf.test <- mape(prueba.set$medv,y_pred_rf)
mape.rf.test


# Plot observados vs predichos
plot(prueba.set$medv, y_pred_rf, col = "red", 
     main = 'RF: Observados vs Predichos')
abline(lm(y_pred_rf~prueba.set$medv), lwd = 2)
text(10, 40, label=expression("R"^2* "="))
text(11,39, paste(round(cor(y_pred_rf,prueba.set$medv)^2, 3)), pos=4)




#Redes neuronales

# Import Required packages
set.seed(123)
library(neuralnet)
library(MASS)

# Boston dataset from MASS
data_boston <- Boston


# Normalize the data
maxs <- apply(data_boston, 2, max) 
mins <- apply(data_boston, 2, min)
scaled <- as.data.frame(scale(data_boston, center = mins, 
                              scale = maxs - mins))

# Split the data into training and testing set
index_tr <- sample(1:nrow(data_boston), round(0.70 * nrow(data_boston)))
train_nn <- scaled[index,]
test_nn<- scaled[-index,]


n <- names(train_nn)
n
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
f

# Build Neural Network
nn <- neuralnet(f, 
                data = train_nn, hidden = c(5, 3), 
                linear.output = TRUE)

# Predict on test data
pr.nn <- compute(nn, test_nn[,1:13])

# Compute mean squared error
pr.nn_ <- pr.nn$net.result * (max(data_boston$medv) - min(data_boston$medv))+ min(data_boston$medv)
test.r <- (test_nn$medv)*(max(data_boston$medv) - min(data_boston$medv)) + min(data_boston$medv)
MSE.nn <- sum((test.r - pr.nn_)^2) / nrow(test_nn)
MSE.nn

# Plot the neural network
plot(nn)



#Metricas de redes neuronales
cme.rn.test <- cme(test.r,pr.nn_)
cme.rn.test
rms.rn.test <- rms(test.r,pr.nn_)
rms.rn.test
rae.rn.test <- rae(test.r,pr.nn_)
rae.rn.test
mae.rn.test <- mae(test.r,pr.nn_)
mae.rn.test
mape.rn.test <- mape(test.r,pr.nn_)
mape.rn.test
r2.lm <- round(cor(y_pred_lm,test_lm$medv)^2, 3)
r2.rf <- round(cor(y_pred_rf,prueba.set$medv)^2, 3)
r2.rn <- round(cor(y_pred_nn,test_nn$medv)^2, 3)


# Plot observados vs predichos
y_pred_nn <- pr.nn_
plot(test.r, y_pred_nn, col = "red", 
     main = 'RN: Observados vs Predichos')
abline(lm(y_pred_nn~test.r), lwd = 2)
text(10, 40, label=expression("R"^2* "="))
text(10.5,39, paste(round(cor(y_pred_nn,test_nn$medv)^2, 3)), pos=4)

#metricas
cme.model<-cbind(cme.lm.test,cme.rf.test,cme.rn.test)
rms.model<-cbind(rms.lm.test,rms.rf.test,rms.rn.test)
rae.model<-cbind(rae.lm.test,rae.rf.test,rae.rn.test)
mae.model<-cbind(mae.lm.test,mae.rf.test,mae.rn.test)
mape.model<-cbind(mape.lm.test,mape.rf.test,mape.rn.test)
r2.model <- cbind(r2.lm,r2.rf,r2.rn)
metricas <- rbind(cme.model,rms.model,rae.model,mae.model,mape.model,r2.model)
colnames(metricas) <- c("lm","rf","rn")
metricas <- cbind("metrica"=c("cme","rms","rae","mae","mape","R^2"),metricas)
metricas <- as.data.frame(metricas)


#instalar gt
#devtools::install_github("rstudio/gt")
library(gt)
metricas %>%gt()

#Conclusiones: De acuerdo con las métricas cme, rms, rae, mae, mape y R^2 
#el mejor modelo es el de bosques aleatorios para predecir medv de la base Boston.

#2.- Ejecute el modelo NN para resolver un problema de clasificación, copie las instrucciones a un script
#R, del enlace siguiente: En este ejemplo se clasifican las variedades de vino en función de las
#características enunciadas.
#https://www.r-bloggers.com/2017/02/multilabel-classification-with-neuralnet-package/


# load libs
require(neuralnet)
require(nnet)
require(ggplot2)
set.seed(123)

# Load data and set variables names
wines <- read.csv("wines.csv")
names(wines) <- c("label",
                  "Alcohol",
                  "Malic_acid",
                  "Ash",
                  "Alcalinity_of_ash",
                  "Magnesium",
                  "Total_phenols",
                  "Flavanoids",
                  "Nonflavanoid_phenols",
                  "Proanthocyanins",
                  "Color_intensity",
                  "Hue",
                  "OD280_OD315_of_diluted_wines",
                  "Proline")
head(wines)


plt1 <- ggplot(wines, aes(x = Alcohol, y = Magnesium, colour = as.factor(label))) +
  geom_point(size=3) +
  ggtitle("Wines")
plt2 <- ggplot(wines, aes(x = Alcohol, y = Proline, colour = as.factor(label))) +
  geom_point(size=3) +
  ggtitle("Wines")

#usando PCA
pca <- prcomp(wines, scale=T, center = T)
plt3 <- ggplot(wines, aes(x = pca$x[,1], y = pca$x[,2], colour = as.factor(label))) +
  geom_point(size=3) +
  ggtitle("Wines")


plt1;plt2;plt3

# Encode as a one hot vector multilabel data
train <- cbind(wines[, 2:14], class.ind(as.factor(wines$label)))
# Set labels name
names(train) <- c(names(wines)[2:14],"l1","l2","l3")

# Scale data
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
train[, 1:13] <- data.frame(lapply(train[, 1:13], scl))
head(train)

# Set up formula
n <- names(train)
f <- as.formula(paste("l1 + l2 + l3 ~", paste(n[!n %in% c("l1","l2","l3")], collapse = " + ")))
f


nn <- neuralnet(f,
                data = train,
                hidden = c(13, 10, 3),
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign = "minimal")

plot(nn)

# Compute predictions
pr.nn <- compute(nn, train[, 1:13])

# Extract results
pr.nn_ <- pr.nn$net.result
head(pr.nn_)

# Accuracy (training set)
original_values <- max.col(train[, 14:16])
pr.nn_2 <- max.col(pr.nn_)
mean(pr.nn_2 == original_values)

# Set seed for reproducibility purposes
set.seed(500)
# 10 fold cross validation
k <- 10
# Results from cv
outs <- NULL
# Train test split proportions
proportion <- 0.95 # Set to 0.995 for LOOCV

# Crossvalidate, go!
for(i in 1:k)
{
  index <- sample(1:nrow(train), round(proportion*nrow(train)))
  train_cv <- train[index, ]
  test_cv <- train[-index, ]
  nn_cv <- neuralnet(f,
                     data = train_cv,
                     hidden = c(13, 10, 3),
                     act.fct = "logistic",
                     linear.output = FALSE)
  
  # Compute predictions
  pr.nn <- compute(nn_cv, test_cv[, 1:13])
  # Extract results
  pr.nn_ <- pr.nn$net.result
  # Accuracy (test set)
  original_values <- max.col(test_cv[, 14:16])
  pr.nn_2 <- max.col(pr.nn_)
  outs[i] <- mean(pr.nn_2 == original_values)
}

mean(outs)


#3.- Copie las instrucciones en un script R y ejecute el modelo RF para resolver un problema de
#clasificación descrito en el siguiente enlace: En este ejemplo se utiliza el conjunto de datos (iris dataset)
#para clasificar tres variedades de iris.
#https://www.r-bloggers.com/2021/04/random-forest-in-r/

library(randomForest)
library(datasets)
library(caret)

#Getting Data
data<-iris
str(data)

data$Species <- as.factor(data$Species)
table(data$Species)

#Data Partition
set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

#Random Forest in R
rf <- randomForest(Species~., data=train, proximity=TRUE) 
print(rf)

#Confusion Matrix and Statistics
p1 <- predict(rf, train)
confusionMatrix(p1, train$Species)

#Prediction & Confusion Matrix – test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$ Species)

#Error rate of Random Forest
plot(rf)

#Tune mtry
t <- tuneRF(train[,-5], train[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)

#No. of nodes for the trees
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")
#Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)

#Partial Dependence Plot
partialPlot(rf, train, Petal.Width, "setosa")

#Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf, train$Species)

#4.- Utilice sus scripts de los problemas 2 (clasificador NN) y 3 (clasificador RF) utilice los datos de iris
#(iris dataset) con el clasificador NN; y los datos de vino (wine dataset) con el clasificador RF. Utilice la
#mismas particiones de ambos ejemplos, y compare el desempeño de los modelos NN y RF. Reporte en
#Cuadro la comparación de sus resultados comente sus resultados.

#Random forest con base wine

library(randomForest)
library(datasets)
library(caret)

#Getting Data
data<-wines
str(data)

data$label <- as.factor(data$label)
table(data$label)

#Data Partition
set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train.wine <- data[ind==1,]
test.wine <- data[ind==2,]

#Random Forest in R
rf <- randomForest(label~., data=train.wine, proximity=TRUE) 
print(rf)

#Confusion Matrix and Statistics
p1 <- predict(rf, train.wine)
confusionMatrix(p1, train.wine$label)

#Prediction & Confusion Matrix – test data
p2 <- predict(rf, test.wine)
confusionMatrix(p2, test.wine$label)

#Error rate of Random Forest
plot(rf)

#Tune mtry
t <- tuneRF(train.wine[,-1], train.wine[,1],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 178,
            trace = TRUE,
            improve = 0.05)
t
#No. of nodes for the trees
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")
#Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)


#Partial Dependence Plot
partialPlot(rf, train.wine, Proline, "3")

#Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf, train.wine$label)

#árbol de decisión
library(tree)
decision_tree <- tree(label ~ ., data = data) # Interpretation
# 1. use tree function  
# 2. sort label
# 3. based on all(.) variables
# 4. data is wine dataset
summary(decision_tree)
plot(decision_tree)
text(decision_tree)




#Redes neuronales con base iris

# load libs
require(neuralnet)
require(nnet)
require(ggplot2)
set.seed(123)

# Load data and set variables names
iris <- iris
head(iris)
str(iris)
plot(iris)

plt1 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point(size=3) +
  ggtitle("Iris")
plt2 <- ggplot(iris, aes(x = Petal.Length, y = Sepal.Length, colour = Species)) +
  geom_point(size=3) +
  ggtitle("Iris")

#usando PCA
pca <- prcomp(iris[,-5], scale=T, center = T)
plt3 <- ggplot(iris, aes(x = pca$x[,1], y = pca$x[,2], colour = Species)) +
  geom_point(size=3) +
  ggtitle("Iris")


plt1;plt2;plt3

# Encode as a one hot vector multilabel data
train.iris <- cbind(iris[, 1:4], class.ind(as.factor(iris$Species)))

# Set labels name
names(train.iris) <- c(names(iris)[1:4],"I1","I2","I3")

# Scale data
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
train.iris[, 1:3] <- data.frame(lapply(train.iris[, 1:3], scl))
head(train.iris)

# Set up formula
n <- names(train.iris)
f <- as.formula(paste("I1 + I2 + I3 ~", paste(n[!n %in% c("I1","I2","I3")], collapse = " + ")))
f


nn <- neuralnet(f,
                data = train.iris,
                hidden = c(4, 2, 3),
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign = "minimal")

plot(nn)

# Compute predictions
pr.nn <- compute(nn, train.iris[, 1:4])

# Extract results
pr.nn_ <- pr.nn$net.result
head(pr.nn_)

# Accuracy (training set)
original_values <- max.col(train.iris[, 5:7])
pr.nn_2 <- max.col(pr.nn_)
mean(pr.nn_2 == original_values)

# Set seed for reproducibility purposes
set.seed(500)
# 10 fold cross validation
k <- 10
# Results from cv
outs <- NULL
# Train test split proportions
proportion <- 0.95 # Set to 0.995 for LOOCV

# Crossvalidate, go!
for(i in 1:k)
{
  index <- sample(1:nrow(train.iris), round(proportion*nrow(train.iris)))
  train_cv <- train.iris[index, ]
  test_cv <- train.iris[-index, ]
  nn_cv <- neuralnet(f,
                     data = train_cv,
                     hidden = c(4, 3, 2),
                     act.fct = "logistic",
                     linear.output = FALSE)
  
  # Compute predictions
  pr.nn <- compute(nn_cv, test_cv[, 1:4])
  # Extract results
  pr.nn_ <- pr.nn$net.result
  # Accuracy (test set)
  original_values <- max.col(test_cv[,5:7])
  pr.nn_2 <- max.col(pr.nn_)
  outs[i] <- mean(pr.nn_2 == original_values)
}

mean(outs)


#Métricas usadas en Machine Learning en clasificación
#Matriz de confusión o error
#Precisión
#Recall o sensibilidad o TPR (Tasa positiva real)
#Precisión
#Especificidad o TNR (Tasa negativa real)
#F1-Score
#Área bajo la curva de funcionamiento del receptor (ROC) (AUC)
#Pérdida logarítmica
#Cohen’s Kappa

#Conclusiones: tomando en cuenta el valor de la precisión (accuracy), se considera
#que en general ambos modelos para clasificar son buenos, ligeramente mejor el
#random forest a redes neuronales.

#cuadro con valores de precision
iris.p <-rbind(c(0.94,0.9)) 
wine.p <- rbind(c(1,0.98))
pres <- rbind(iris.p,wine.p)
pres <- as.data.frame(pres)
colnames(pres) <- c("RF","RN")
pres <- cbind(dataset=c("iris","wine"),pres)

library(gt)
pres %>% gt()




#espacio de trabajo
save.image("T9COA501IFQI.RData")

