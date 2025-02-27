
#GRADIENT BOOSTING MACHINE REGRESSION DATA#
# reproduction of Diego Fernandez Garcia's class, with any random database
# https://www.youtube.com/watch?v=pSZNFdjJbB0

#### Loading packages ####
library(quantmod)
library(gbm)
library(readxl)
library(tseries)
library(xts)
library(zoo)

#### Reading data ####

preciopm <- read_excel("C:/Users/celia/Downloads/preciopm.xls")

View(preciopm)

datas <- seq(from = as.Date("1990-01-01"), by = "month", length.out = nrow(preciopm))

preciopm <- data.frame(daTa = datas, precio = preciopm$precio)

serie_temporal <- xts(preciopm[,2], order.by = preciopm[,1])
View(serie_temporal)

#### Target and predict features ####

precio_y <- monthlyReturn(serie_temporal) #target feature = variação atual
variation_precio_x <- lag(precio_y, k=1) #predictor feature = variação passada, com lag=1
precio_all <- cbind(precio_y, variation_precio_x)
colnames(precio_all) <- c("precio_y", "variation_precio_x")
precio_all <- na.exclude(precio_all)


head(serie_temporal)
head(precio_y)


#### Training and Testing Ranges ####

precio_Trainig <- window(precio_all, end = "2012-04-01") #valores escolhidos aleatoriamente
precio_Testing <- window(precio_all, start = "2012-04-01") #valores escolhidos aleatoriamente

####Gradient Boosting Machine Regression ####

gbmTraining <- gbm(precio_y ~ variation_precio_x, distribution = "gaussian", data = precio_Trainig, n.trees = 1000, shrinkage = 0.1, bag.fraction = 1)
gbmTraining$train.error
# o $train.error é o MSE de todos os resultados das observações do training data rodados sobre as arvores do modelo
# como bag.fraction = 1, nao existe OOB error rate


####Predição####
predicted_values <- predict(gbmTraining, newdata = precio_Testing, n.trees = 1000)

plot(as.numeric(precio_Testing$precio_y), predicted_values)

#### avaliação do modelo por MSE #### 
#comparar o MSE dos valores preditos com a variancia dos dados observados

MSE <- mean((as.numeric(precio_Testing$precio_y) - predicted_values)^2 )
var_real <- var(precio_Testing$precio_y)

FINAL_RATIO <- MSE / var_real
print(FINAL_RATIO)

# resultado: 0.9016758, modelo ruim