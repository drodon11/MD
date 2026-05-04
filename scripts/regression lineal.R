# ==============================================================================
#        MODELOS LINEALES - FLIGHT PRICES
# ==============================================================================

rm(list = ls())

# ==============================================================================
# 1. PAQUETES
# ==============================================================================

list.of.packages <- c(
  "dplyr",
  "ggplot2",
  "tidyr",
  "tibble",
  "GGally",
  "caret",
  "car",
  "lmtest",
  "ggfortify"
)

new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[, "Package"])
]

if (length(new.packages) > 0) {
  install.packages(new.packages)
}

invisible(lapply(list.of.packages, require, character.only = TRUE))

rm(list.of.packages, new.packages)

# ==============================================================================
# 2. CARGA DEL DATASET
# ==============================================================================

input_path <- file.path(getwd(), "data", "interim", "flightprices_preprocessed.rds")

dd <- readRDS(input_path)

cat("\nDimensiones del dataset original:\n")
print(dim(dd))

cat("\nNombres de variables:\n")
print(names(dd))

cat("\nEstructura del dataset:\n")
str(dd)

cat("\nResumen del dataset:\n")
print(summary(dd))

# ==============================================================================
# 3. PREPARACION INICIAL
# ==============================================================================

dd <- na.omit(dd)

dd <- dd |>
  mutate(across(where(is.character), as.factor))

if (!("price" %in% names(dd))) {
  stop("No existe una variable llamada 'price'. Revisa el nombre exacto de la variable objetivo.")
}

dd$price <- as.numeric(dd$price)

tipos <- sapply(dd, class)

varCat <- names(tipos)[tipos %in% c("character", "factor")]
varNum <- names(tipos)[tipos %in% c("integer", "numeric")]
varNumPred <- setdiff(varNum, "price")

cat("\nDimensiones después de eliminar missing values:\n")
print(dim(dd))

cat("\nVariables categóricas:\n")
print(varCat)

cat("\nVariables numéricas:\n")
print(varNum)

cat("\nVariables numéricas predictoras:\n")
print(varNumPred)

tabla_variables <- data.frame(
  Variable = names(dd),
  Tipo = sapply(dd, class)
)

cat("\nTabla de variables:\n")
print(tabla_variables)

# ==============================================================================
# 4. ANALISIS EXPLORATORIO
# ==============================================================================

ggplot(dd, aes(x = price)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(
    title = "Distribución del precio de los vuelos",
    x = "Precio",
    y = "Frecuencia"
  ) +
  theme_minimal()

ggplot(dd, aes(y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Boxplot del precio de los vuelos",
    y = "Precio"
  ) +
  theme_minimal()

if (length(varNum) > 1) {
  cor_matrix <- cor(dd[, varNum], use = "complete.obs")
  cat("\nMatriz de correlaciones:\n")
  print(round(cor_matrix, 3))
}

vars_ggpairs <- head(varNum, 6)

if (length(vars_ggpairs) >= 2) {
  GGally::ggpairs(dd, columns = vars_ggpairs)
}

# ==============================================================================
# 5. SELECCION DE VARIABLE PARA MODELO SIMPLE
# ==============================================================================

if ("duration" %in% varNumPred) {
  x_simple <- "duration"
} else if ("days_left" %in% varNumPred) {
  x_simple <- "days_left"
} else {
  x_simple <- varNumPred[1]
}

cat("\nVariable usada para el modelo simple:\n")
print(x_simple)

grafico_simple <- ggplot(dd, aes(x = .data[[x_simple]], y = price)) +
  geom_point(alpha = 0.4) +
  labs(
    title = paste("Precio frente a", x_simple),
    x = x_simple,
    y = "Precio"
  ) +
  theme_minimal()

print(grafico_simple)

# ==============================================================================
# 6. REGRESION LINEAL SIMPLE
# ==============================================================================

formula_simple <- as.formula(
  paste("price ~", x_simple)
)

modelo_simple <- lm(formula_simple, data = dd)

cat("\nResumen del modelo lineal simple:\n")
print(summary(modelo_simple))

cat("\nCoeficientes del modelo simple:\n")
print(coef(modelo_simple))

grafico_simple +
  geom_smooth(method = "lm", se = TRUE, color = "red")

cat("\nPrimeros residuos del modelo simple:\n")
print(head(data.frame(
  observado = dd$price,
  ajustado = fitted(modelo_simple),
  residuo = residuals(modelo_simple)
)))

ggfortify::autoplot(modelo_simple) +
  theme_minimal()

cat("\nTest de Breusch-Pagan para homocedasticidad:\n")
print(lmtest::bptest(modelo_simple))

residuos_simple <- residuals(modelo_simple)

set.seed(2108)

cat("\nTest de normalidad de Shapiro-Wilk:\n")
if (length(residuos_simple) > 5000) {
  print(shapiro.test(sample(residuos_simple, 5000)))
} else {
  print(shapiro.test(residuos_simple))
}

cat("\nR2 modelo simple:\n")
print(summary(modelo_simple)$r.squared)

cat("\nR2 ajustado modelo simple:\n")
print(summary(modelo_simple)$adj.r.squared)

cat("\nIntervalos de confianza modelo simple:\n")
print(confint(modelo_simple))

nuevo_vuelo_simple <- data.frame(
  valor = median(dd[[x_simple]], na.rm = TRUE)
)

names(nuevo_vuelo_simple) <- x_simple

cat("\nPredicción puntual para nuevo vuelo:\n")
print(predict(modelo_simple, newdata = nuevo_vuelo_simple))

cat("\nIntervalo de confianza para nuevo vuelo:\n")
print(predict(
  modelo_simple,
  newdata = nuevo_vuelo_simple,
  interval = "confidence"
))

cat("\nIntervalo de predicción para nuevo vuelo:\n")
print(predict(
  modelo_simple,
  newdata = nuevo_vuelo_simple,
  interval = "prediction"
))

# ==============================================================================
# 7. REGRESION LINEAL MULTIPLE
# ==============================================================================

preferidas <- c(
  "duration",
  "days_left",
  "airline",
  "source_city",
  "destination_city",
  "stops",
  "class",
  "departure_time",
  "arrival_time"
)

vars_modelo_multiple <- preferidas[preferidas %in% names(dd)]

if (length(vars_modelo_multiple) < 3) {
  otras_vars <- setdiff(names(dd), c("price", vars_modelo_multiple))
  vars_modelo_multiple <- unique(c(vars_modelo_multiple, head(otras_vars, 5)))
}

cat("\nVariables usadas en el modelo múltiple:\n")
print(vars_modelo_multiple)

formula_multiple <- as.formula(
  paste("price ~", paste(vars_modelo_multiple, collapse = " + "))
)

modelo_multiple <- lm(formula_multiple, data = dd)

cat("\nResumen del modelo lineal múltiple:\n")
print(summary(modelo_multiple))

cat("\nR2 modelo simple:\n")
print(summary(modelo_simple)$r.squared)

cat("\nR2 modelo múltiple:\n")
print(summary(modelo_multiple)$r.squared)

cat("\nR2 ajustado modelo múltiple:\n")
print(summary(modelo_multiple)$adj.r.squared)

# ==============================================================================
# 8. VARIABLES CATEGORICAS
# ==============================================================================

if (length(varCat) > 0) {
  
  cat_var <- varCat[1]
  
  formula_factor <- as.formula(
    paste("price ~", x_simple, "+", cat_var)
  )
  
  modelo_factor <- lm(formula_factor, data = dd)
  
  cat("\nVariable categórica usada:\n")
  print(cat_var)
  
  cat("\nResumen del modelo con variable categórica:\n")
  print(summary(modelo_factor))
  
} else {
  cat("\nNo hay variables categóricas disponibles.\n")
}

# ==============================================================================
# 9. INTERACCIONES
# ==============================================================================

if (length(varCat) > 0) {
  
  formula_interaccion <- as.formula(
    paste("price ~", x_simple, "*", cat_var)
  )
  
  modelo_interaccion <- lm(formula_interaccion, data = dd)
  
  cat("\nResumen del modelo con interacción:\n")
  print(summary(modelo_interaccion))
  
  top_cats <- names(sort(table(dd[[cat_var]]), decreasing = TRUE))[
    1:min(4, length(unique(dd[[cat_var]])))
  ]
  
  dd_inter <- dd |>
    filter(.data[[cat_var]] %in% top_cats)
  
  ggplot(
    dd_inter,
    aes(x = .data[[x_simple]], y = price, color = .data[[cat_var]])
  ) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = paste("Interacción entre", x_simple, "y", cat_var),
      x = x_simple,
      y = "Precio",
      color = cat_var
    ) +
    theme_minimal()
}

# ==============================================================================
# 10. MULTICOLINEALIDAD
# ==============================================================================

cat("\nVIF del modelo múltiple:\n")
print(try(car::vif(modelo_multiple)))

# ==============================================================================
# 11. MODELO COMPLETO
# ==============================================================================

modelo_completo <- lm(price ~ ., data = dd)

cat("\nResumen del modelo completo:\n")
print(summary(modelo_completo))

# ==============================================================================
# 12. SELECCION DE VARIABLES POR AIC
# ==============================================================================

modelo_step <- step(modelo_completo, trace = 1)

cat("\nResumen del modelo seleccionado por AIC:\n")
print(summary(modelo_step))

# ==============================================================================
# 13. COMPARACION FORMAL ENTRE MODELOS
# ==============================================================================

cat("\nANOVA entre modelo simple y múltiple:\n")
print(anova(modelo_simple, modelo_multiple))

# ==============================================================================
# 14. EVALUACION PREDICTIVA TRAIN / TEST
# ==============================================================================

set.seed(2108)

trainIndex <- caret::createDataPartition(
  dd$price,
  p = 0.8,
  list = FALSE,
  times = 1
)

train <- dd[trainIndex, ]
test  <- dd[-trainIndex, ]

modelo_train <- lm(formula(modelo_step), data = train)

pred_test <- predict(modelo_train, newdata = test)

rmse <- sqrt(mean((test$price - pred_test)^2))
mae  <- mean(abs(test$price - pred_test))
mse  <- mean((test$price - pred_test)^2)

r2_test <- 1 - sum((test$price - pred_test)^2) /
  sum((test$price - mean(test$price))^2)

metricas_test <- data.frame(
  MAE = mae,
  MSE = mse,
  RMSE = rmse,
  R2_test = r2_test
)

cat("\nMétricas en test:\n")
print(metricas_test)

resultados_test <- data.frame(
  Real = test$price,
  Predicho = pred_test,
  Error = test$price - pred_test
)

ggplot(resultados_test, aes(x = Real, y = Predicho)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Valores reales frente a valores predichos",
    subtitle = "La línea roja representa la predicción perfecta",
    x = "Precio real",
    y = "Precio predicho"
  ) +
  theme_minimal()

ggplot(resultados_test, aes(x = Predicho, y = Error)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_hline(
    yintercept = 0,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Residuos en el conjunto de test",
    x = "Precio predicho",
    y = "Error"
  ) +
  theme_minimal()

# ==============================================================================
# 15. VALIDACION CRUZADA
# ==============================================================================

set.seed(2108)

control <- trainControl(method = "cv", number = 5)

modelo_cv <- train(
  formula(modelo_step),
  data = dd,
  method = "lm",
  trControl = control
)

cat("\nValidación cruzada:\n")
print(modelo_cv)

# ==============================================================================
# 16. MODELO LOG-LINEAL
# ==============================================================================

modelo_log <- lm(
  update(formula(modelo_step), log(price) ~ .),
  data = dd
)

cat("\nResumen del modelo log-lineal:\n")
print(summary(modelo_log))

ggfortify::autoplot(modelo_log) +
  theme_minimal()

# ==============================================================================
# 17. MODELO CUADRATICO
# ==============================================================================

formula_cuadratica <- as.formula(
  paste("price ~", x_simple, "+ I(", x_simple, "^2)")
)

modelo_cuadratico <- lm(formula_cuadratica, data = dd)

cat("\nResumen del modelo cuadrático:\n")
print(summary(modelo_cuadratico))

cat("\nANOVA entre modelo simple y cuadrático:\n")
print(anova(modelo_simple, modelo_cuadratico))

# ==============================================================================
# 18. OUTLIERS, LEVERAGE E INFLUENCIA
# ==============================================================================

cooks <- cooks.distance(modelo_multiple)

df_cooks <- data.frame(
  index = 1:length(cooks),
  cooks = cooks
)

threshold <- 4 / nrow(dd)

cat("\nTop 20 observaciones más influyentes según Cook:\n")
print(head(df_cooks[order(-df_cooks$cooks), ], 20))

ggplot(df_cooks, aes(x = index, y = cooks)) +
  geom_segment(aes(xend = index, yend = 0), alpha = 0.6) +
  geom_hline(
    yintercept = threshold,
    color = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Distancia de Cook",
    subtitle = "La línea roja representa el umbral 4/n",
    x = "Observación",
    y = "Cook's Distance"
  ) +
  theme_minimal()

ggplot(df_cooks, aes(x = index, y = cooks)) +
  geom_segment(aes(xend = index, yend = 0), alpha = 0.6) +
  geom_point(
    data = subset(df_cooks, cooks > threshold),
    aes(x = index, y = cooks),
    color = "red",
    size = 2
  ) +
  geom_hline(
    yintercept = threshold,
    color = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Detección de observaciones influyentes",
    subtitle = "Los puntos rojos superan el umbral 4/n",
    x = "Observación",
    y = "Cook's Distance"
  ) +
  theme_minimal()

df_cooks_sorted <- df_cooks[order(-df_cooks$cooks), ]

top_cooks <- df_cooks_sorted[1:min(50, nrow(df_cooks_sorted)), ]

ggplot(top_cooks, aes(x = reorder(index, -cooks), y = cooks)) +
  geom_col(fill = "darkorange") +
  geom_hline(
    yintercept = threshold,
    color = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Top observaciones más influyentes",
    x = "Observación",
    y = "Cook's Distance"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ==============================================================================
# 19. COMPARACION GLOBAL DE MODELOS
# ==============================================================================

resumen_modelos <- data.frame(
  Modelo = c(
    "Simple",
    "Multiple",
    "Completo",
    "Stepwise",
    "Log-lineal",
    "Cuadratico"
  ),
  R2 = c(
    summary(modelo_simple)$r.squared,
    summary(modelo_multiple)$r.squared,
    summary(modelo_completo)$r.squared,
    summary(modelo_step)$r.squared,
    summary(modelo_log)$r.squared,
    summary(modelo_cuadratico)$r.squared
  ),
  R2_ajustado = c(
    summary(modelo_simple)$adj.r.squared,
    summary(modelo_multiple)$adj.r.squared,
    summary(modelo_completo)$adj.r.squared,
    summary(modelo_step)$adj.r.squared,
    summary(modelo_log)$adj.r.squared,
    summary(modelo_cuadratico)$adj.r.squared
  ),
  AIC = c(
    AIC(modelo_simple),
    AIC(modelo_multiple),
    AIC(modelo_completo),
    AIC(modelo_step),
    AIC(modelo_log),
    AIC(modelo_cuadratico)
  )
)

cat("\nResumen comparativo de modelos:\n")
print(resumen_modelos)

# ==============================================================================
# 20. RESUMEN FINAL
# ==============================================================================

cat("\n============================================================\n")
cat("RESUMEN FINAL\n")
cat("============================================================\n")

cat("\nVariable respuesta: price\n")

cat("\nModelo simple usado:\n")
print(formula_simple)

cat("\nModelo múltiple usado:\n")
print(formula_multiple)

cat("\nModelo stepwise final:\n")
print(formula(modelo_step))

cat("\nMétricas test modelo stepwise:\n")
print(metricas_test)

cat("\nComparación global de modelos:\n")
print(resumen_modelos)

cat("\n============================================================\n")
cat("FIN DEL SCRIPT\n")
cat("============================================================\n")