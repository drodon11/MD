# Cargar librerias
list.of.packages <- c(
  "dplyr", "fpc", "reshape2", "tidyr", "ggplot2", "stats", 
  "cluster", "factoextra", "colorspace", "patchwork", 
  "tidyverse", "ggpubr", "NbClust", "HDclassif", "clustMixType", 
  "clusterSim", "pracma", "DataVisualizations", "entropy",
  "clevr", "dendextend", "ggdendro", "gridExtra", "idealista18",
  "GGally", "ggplot2", "caret", "ggfortify")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages) > 0) {
  install.packages(new.packages)
}

invisible(lapply(list.of.packages, require, character.only = TRUE))
rm(list.of.packages, new.packages)

# Cargar la base de datos
input_path <- file.path(getwd(), "data", "interim", "flightprices_preprocessed.rds")
dd <- readRDS(input_path)

# Estructura del dataset
str(dd)
summary(dd)

# Conversión de variables categóricas a factores
tipos <- sapply(dd, class)
varCat <- names(tipos)[which(tipos %in% c("character", "factor"))]
varNum <- names(tipos)[which(tipos %in% c("integer", "numeric"))]

for(var in varCat) {
  dd[, var] <- as.factor(as.character(dd[,var]))
}

# 3. ANALISIS EXPLORATORIO

# Matriz de dispersión para ver relaciones lineales iniciales
GGally::ggpairs(dd, columns = varNum)

# Correlaciones numéricas (detección de multicolinealidad previa)
cor(dd[,varNum])

# Visualización de la relación principal: Precio vs Distancia
ggplot(dd, aes(x = travelDistance, y = totalPrice)) + 
  geom_point(alpha=0.3) + 
  geom_smooth(method="lm", color="red") +
  labs(title="Relación Precio vs Distancia de vuelo", x="Distancia", y="Precio total")

# 4. REGRESIÓN LINEAL SIMPLE

# Modelización del precio en función de la distancia recorrida
modelo_simple <- lm(totalPrice ~ travelDistance, data=dd)
summary(modelo_simple)

# Coeficientes: cuánto aumenta el precio por cada unidad de distancia
coef(modelo_simple)

# 5. DIAGNOSTICOS DE RESIDUOS

# Visualizacion de supuestos (linealidad, homocedasticidad, normalidad) ---------------------> NO FUNCIONA
library("ggfortify")
autoplot(modelo_simple) + theme_minimal()

# Test de homocedasticidad (Breusch-Pagan)
lmtest::bgtest(modelo_simple)

# Test de normalidad (Shapiro-Wilk)
shapiro.test(sample(residuals(modelo_simple), 5000))

summary(modelo_simple)$r.squared
summary(modelo_simple)$adj.r.squared
confint(modelo_simple)

nuevo_vuelo <- data.frame(totalPrice = 188)
predict(modelo_simple, newdata = nuevo_vuelo)

# 6. REGRESIÓN LINEAL MÚLTIPLE

# Se añaden más predictores: tasas, escalas, y asientos libres
modelo_multiple <- lm(totalPrice ~ travelDistance + taxAmount + layoverNumber + seatsLeft, data=dd)
summary(modelo_multiple)

# Factor de Inflación de Varianza (VIF) para detectar redundancia
# VIF > 5-10 indica problemas graves
car::vif(modelo_multiple)

# 7. VARIABLES CUALITATIVAS E INTERACCIONES

# Influye el tipo de cabina (economy) en la relación Distancia-Precio?
modelo_interaccion <- lm(totalPrice ~ travelDistance * economy + taxAmount, data=dd)
summary(modelo_interaccion)

# Visualización de la interacción
ggplot(dd, aes(x = travelDistance, y = totalPrice, color = economy)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  labs(title = "Interacción: Efecto de la distancia según clase de vuelo")

# 8. SELECCIÓN AUTOMATICA DE VARIABLES

# Buscar el mejor modelo basado en el criterio de información de Akaike (AIC)
modelo_completo <- lm(totalPrice ~ ., data = dd %>% select(-flightDate, -departure, -arrival))
modelo_step <- step(modelo_completo, direction = "both", trace = 0)
summary(modelo_step)

# Comparación formal entre el modelo simple y el mejor modelo encontrado (ANOVA)
anova(modelo_simple, modelo_step)

# 9. EVALUACION PREDICTIVA (TRAIN / TEST)

set.seed(123)
trainIndex <- createDataPartition(dd$totalPrice, p = .8, list = FALSE)
train_data <- dd[trainIndex, ]
test_data  <- dd[-trainIndex, ]

# Ajustamos modelo en entrenamiento y predecimos en prueba
final_model <- lm(formula(modelo_step), data = train_data)
predictions <- predict(final_model, newdata = test_data)

# Métricas de error (RMSE y MAE)
rmse <- sqrt(mean((test_data$totalPrice - predictions)^2))
mae <- mean(abs(test_data$totalPrice - predictions))
cat("RMSE:", rmse, "| MAE:", mae)


# 10. TRANSFORMACIONES (LOG-NORMAL)

# Si los residuos no son normales, probamos a transformar la variable respuesta
modelo_log <- lm(log(totalPrice) ~ travelDistance + taxAmount + economy, data = dd)
summary(modelo_log)
autoplot(modelo_log, which = 2) # Revisamos mejora en gráfico Q-Q

# 11. INFLUENCIA Y OUTLIERS (DISTANCIA DE COOK)

cooks <- cooks.distance(modelo_step)
threshold <- 4 / nrow(dd)

# Identificar observaciones que "mueven" demasiado el modelo
df_cooks <- data.frame(obs = 1:length(cooks), dist = cooks)
ggplot(df_cooks, aes(x = obs, y = dist)) +
  geom_hline(yintercept = threshold, color = "red", linetype = "dashed") +
  geom_point(aes(color = dist > threshold)) +
  labs(title = "Detección de Observaciones Influyentes", y = "Distancia de Cook")