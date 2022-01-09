# Carga de paquetes a utilizar
library(caret)
library(rsample)
library(ggplot2)
library(readr)

# Carga de datos
dir <- getwd()
df <- read.csv(paste(dir, "/data/marketing_campaign.csv", sep = ""),
 header = TRUE)

# Scale the dataset (normalize data)
df <- df[, 2:20]

# Crear training 70%) y test (30%) sets para la data utilizando rsample package
set.seed(123)
data_split <- initial_split(df, prop = .7, strata = "MntWines")
data_train <- training(data_split)
data_test <- testing(data_split)

# Definicion de strategia de rsampling - Utilizando 10-fold cv repetido 5 veces
cv <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5
)

# Crear grid de valores de hiperparametros
hyper_grid <- expand.grid(k = seq(2, 25, by = 1))

# Utilizando grid search, estimar modelo knn
knn_fit <- train(
  MntWines ~ .,
  data = data_train,
  method = "knn",
  trControl = cv,
  tuneGrid = hyper_grid,
  metric = "RMSE"
)

# Imprimir y Graficar resultados CV
knn_fit # Este codigo puede durar mucho tiempo ejecutandose
ggplot(knn_fit)

#RMSE was used to select the optimal model using the smallest value.

#Multiple Linear Regression
model1 <- lm(MntWines ~ Income + Recency + Year_Birth +
 MntMeatProducts + NumStorePurchases,
          data = data_train)

model2 <- lm(MntWines ~ Income + Recency + Year_Birth +
 MntFishProducts + NumStorePurchases,
             data = data_train)

model3 <- lm(MntWines ~ Income + Year_Birth +
 MntFishProducts + NumStorePurchases,
             data = data_train)

model4 <- lm(MntWines ~ Income + Year_Birth +
MntMeatProducts + NumStorePurchases,
              data = data_train)

summary(model1)
summary(model2)
summary(model3)
summary(model4)

# Evaluacion de la precision del modelo
# Entrenamiento del modelo mediante una validacin cruzada 10-fold


# Model 1 CV
set.seed(123) #para reproducibilidad
(cv_model1 <- train(
  form = MntWines ~ Income + Recency + Year_Birth + MntMeatProducts
   + NumStorePurchases,
  data = data_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))

# Model 2 CV
set.seed(123) #para reproducibilidad
(cv_model2 <- train(
  form = MntWines ~ Income + Recency + Year_Birth + MntFishProducts
  + NumStorePurchases,
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

# 1 Graficos Linear regression:
  
p1 <- ggplot(data_train, aes(data_train$Income + data_train$Recency
 + data_train$Year_Birth + data_train$MntMeatProducts
 + data_train$NumStorePurchases, data_train$MntWines)) +
geom_point(size = 1, alpha = .4) +
geom_smooth(se = FALSE) +
scale_y_continuous("Wine purchase", labels = scales::dollar) +
xlab("Predictors") +
ggtitle(paste("Non transformed data with a non-linear relationship"))

p2 <- ggplot(data_train, aes(data_train$Income + data_train$Recency +
data_train$Year_Birth + data_train$MntMeatProducts +
data_train$NumStorePurchases, data_train$MntWines)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10("Wine purchase", labels = scales::dollar,
   breaks = seq(0, 400000, by = 100000)) +
   xlab("Predictors") +
   ggtitle(paste("Transforming variables
   to provide a near-linear relationship "))

gridExtra::grid.arrange(p1, p2, nrow = 1)

# Aunque el método de regresión lineal presume una
# relación lineal entre predictor (s) y
# la variable de respuesta, una regresión no lineal como
# se presenta a continuación con relación a la data utilizada,
# puede ser trasformada en una relación lineal o cercana a lo lineal
# al transformar las variables predictoras y la variable respuesta.

# 2. Constant variance among residuals:
df1 <- broom::augment(model1, data = data_train)
p1 <- ggplot(df1, aes(.fitted, .resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab("Predicted values") +
  ylab("Residuals") +
  ggtitle("Model 1", subtitle =
  "data_train$MntWines ~ data_train$Income+data_train$Recency+
  data_train$Year_Birth+data_train$MntMeatProducts+
  data_train$NumStorePurchases")

df2 <- broom::augment(model3, data = data_train)
p2 <- ggplot(df2, aes(.fitted, .resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab("Predicted values") +
  ylab("Residuals") +
  ggtitle("Model3", subtitle = "data_train$MntWines ~ ")
gridExtra::grid.arrange(p1, p2, nrow = 1)


# El método de regresión lineal presume la existencia de variación
# constante entre residuales. La primera figura muestra como
# los datos muestran heteroskedasticidad (variación no constante
# entre residuales) mientras que la segunda figura muestra signos
# de homoscedasticidad (variación constante entre residuales).
# Esta última lográndose a través de la transformación de
# las variables tanto de predicción como de respuesta.


#3.	No autocorrelation:
df1 <- mutate(df1, id=row_number())
df2 <- mutate(df2, id=row_number())

p1 <- ggplot(df1, aes(id, .resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab("Row 10") +
  ylab("Residuals") +
  ggtitle("Model 1", subtitle = "Correlated residuals.")

p2 <- ggplot(df2, aes(id, .resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab("Row ID") +
  ylab("Residuals") +
  ggtitle("Model 3", subtitle = "Uncorrelated residuals.")

gridExtra::grid.arrange(p1, p2, nrow = 1)


# La regresión lineal asume que no hay correlación en los errores residuales.
# En este caso podemos observar que la primera figura muestra que los
# residuales reaccionan a un factor de correlación, mientras
# que la segunda figura muestra menos signos de
#correlación entre residuales por ende la autocorrelacion se reduce.