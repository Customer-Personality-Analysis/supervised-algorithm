library(readr)
dir <- getwd()
df <- read.csv(paste(dir, "/data/marketing_campaign.csv", sep =""), header = TRUE)

#Carga de paquetes a utilizar
library(caret)
library(rsample)
library(ggplot2)
 

#scale the dataset (normalize data)
df <- df[, 2:20]


#Crear training 70%) y test (30%) sets para la data utilizando rsample package
set.seed(123)
data_split <- initial_split(df, prop = .7, strata = "MntWines")
data_train <- training(data_split)
data_test <- testing(data_split)

#Definicion de strategia de rsampling - Utilizando 10-fold cv repetido 5 veces
cv <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5
)

#Crear grid de valores de hiperparametros
hyper_grid <- expand.grid(k = seq(2, 25, by = 1))

#Utilizando grid search, estimar modelo knn
knn_fit <- train(
  MntWines ~ .,
  data = data_train,
  method = "knn",
  trControl = cv,
  tuneGrid = hyper_grid,
  metric = "RMSE"
)

# Imprimir y Graficar resultados CV
knn_fit
ggplot(knn_fit)

#RMSE was used to select the optimal model using the smallest value.
#Multiple Linear Regression


model1 <- lm(MntWines ~ Income + Recency + Year_Birth + MntMeatProducts + NumStorePurchases, 
          data = data_train)

model2 <- lm(MntWines ~ Income + Recency + Year_Birth + MntFishProducts + NumStorePurchases, 
             data = data_train)

model3 <- lm (MntWines ~ Income + Year_Birth + MntFishProducts + NumStorePurchases, 
             data = data_train)

model4 <- lm (MntWines ~ Income + Year_Birth + MntMeatProducts + NumStorePurchases, 
              data = data_train)


summary(model1)
summary(model2)
summary(model3)
summary(model4)


#Evaluaci�n de la precisi�n del modelo 
# Entrenamiento del modelo mediante una validaci�n cruzada 10-fold 

# Model 1 CV
set.seed(123) #para reproducibilidad 

(cv_model1 <-train(
  form = MntWines ~ Income + Recency + Year_Birth + MntMeatProducts + NumStorePurchases,
  data = data_train,
  method = "lm", 
  trControl = trainControl(method = "cv", number = 10)
))

# Model 2 CV
set.seed(123) #para reproducibilidad

(cv_model2 <- train(
  form = MntWines ~ Income + Recency + Year_Birth + MntFishProducts + NumStorePurchases,
  data = data_train,
  method = "lm", 
  trControl = trainControl(method = "cv", number = 10)
))

# Model 3 CV
set.seed(123) #para reproducibilidad

(cv_model3 <- train(
  form = MntWines ~ Income + Year_Birth + MntMeatProducts + NumStorePurchases,
  data = data_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))


# Model 4 CV
set.seed(123) #para reproducibilidad

(cv_model4 <- train(
  form = MntWines ~ Income + Year_Birth + MntFishProducts + NumStorePurchases,
  data = data_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))


summary(resamples(list(
  model1 = cv_model1,
  model2 = cv_model2,
  model3 = cv_model3,
  model4 = cv_model4
)))
