install.packages("randomForest")

library(ggplot2)
library(cowplot)
library(randomForest)

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

data <- read.csv(url, header = FALSE)

View(data)

colnames(data) <- c( "age",
                     "sex",# 0 = female, 1 = male
                     "cp", # chest pain 
                           # 1 = typical angina, 
                           # 2 = atypical angina, 
                          # 3 = non-anginal pain, 
                          # 4 = asymptomatic
                     "trestbps", # resting blood pressure (in mm Hg)
                     "chol", # serum cholestoral in mg/dl
                     "fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
                     "restecg", # resting electrocardiographic results
                                # 1 = normal
                                # 2 = having ST-T wave abnormality
                                # 3 = showing probable or definite left ventricular hypertrophy
                     "thalach", # maximum heart rate achieved
                     "exang",   # exercise induced angina, 1 = yes, 0 = no
                     "oldpeak", # ST depression induced by exercise relative to rest
                     "slope", # the slope of the peak exercise ST segment 
                              # 1 = upsloping 
                              # 2 = flat 
                              # 3 = downsloping 
                     "ca", # number of major vessels (0-3) colored by fluoroscopy
                     "thal", # this is short of thalium heart scan
                              # 3 = normal (no cold spots)
                              # 6 = fixed defect (cold spots during rest and exercise)
                              # 7 = reversible defect (when cold spots only appear during exercise)
                     "hd" # (the predicted attribute) - diagnosis of heart disease 
                          # 0 if less than or equal to 50% diameter narrowing
                          # 1 if greater than 50% diameter narrowing
)


head(data)

str(data) # this shows that we need to tell R which columns contain factors
          # it also shows us that there are some missing values. There are "?"s
          # in the dataset.

####LIMPANDO OS DADOS####

## First, replace "?"s with NAs.
data[data == "?"] <- NA

data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex) #avisar o R que M e F sao categorias#

#avisa o R que as outras dummies categoricas tb sao categorias#
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

data$ca <- as.integer(data$ca) #assim tira os valores decimais#

#e tranforma em factors (cateorias)"
data$ca <- as.factor(data$ca)

data$thal <- as.integer(data$thal) # "thal" also had "?"s in it.
data$thal <- as.factor(data$thal)

## This next line replaces 0 and 1 with "Healthy" and "Unhealthy"
data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd) # Now convert to a factor
data$hd <- as.factor(data$hd) # Now convert to a factor

str(data) ## this shows that the correct columns are factors and we've replaced
## "?"s with NAs because "?" no longer appears in the list of factors
## for "ca" and "thal"


####BUILDING A RANDOM FOREST####

set.seed(42)

#aqui ele vai imputar valores para os NA do data#
data.imputed <- rfImpute( hd ~ ., data = data, iter=6) 

View(data.imputed)

#o random forest automaticamente separa o out e insample datas, OOB é out-of-bagging error rate#

#o comando do random foreste de fato, com o data.imputed#
model <- randomForest(hd ~ ., data=data.imputed, proximity=TRUE)


#### COM MIL ARVORES MELHORARIA O MODELO? ####
##rodar e avaliar do modelo com 500 arvores##
oob.error.data <- data.frame(Trees=rep(1:nrow(model$err.rate), times=3), #err.rate é uma matriz vinculada ao model#
                             Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
                             Error=c(model$err.rate[,"OOB"], 
                                     model$err.rate[,"Healthy"], 
                                     model$err.rate[,"Unhealthy"]))

model$err.rate

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

##rodar e avaliar do modelo com 1000 arvores##
model <- randomForest(hd ~ ., data=data.imputed, ntree=1000, proximity=TRUE)


oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Healthy"], 
          model$err.rate[,"Unhealthy"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

##resultado da comparação: OOB estimate of  error rate: 17.16% é maior com mil arvores##



#### SABER O NUMERO IDEAL DE VARIAVEIS PARA APLICAR NOS NÓS####

oob.values <- vector(length=10) ##fazemos um vetor de 10 numeros q vao rodar de 1 a 10 o numero de variaveis#
for(i in 1:10) {
  temp.model <- randomForest(hd ~ ., data=data.imputed, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1] #para salvar os valores de ooberrrate#
}

oob.values

#the optimal number of variables is the one with the lowest OOB eror rate value# 
