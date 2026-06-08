# ==============================================================================
#                             NAIVE BAYES & kNN
# ==============================================================================

# --- SETUP Y CARGA DE DATOS ---
rm(list = ls())

# --- Añadidas librerías necesarias para gráficos y manipulación ---
list.of.packages <- c("naivebayes", "VIM", "caret", "ggplot2", "reshape2", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages) > 0) install.packages(new.packages, dependencies = TRUE)
invisible(lapply(list.of.packages, require, character.only = TRUE))

# Cargamos las particiones
load("data/interim/model_data.RData")

train_df$economy_f <- as.factor(make.names(train_df$economy_f))
test_df$economy_f  <- as.factor(make.names(test_df$economy_f))

# Seleccionamos las columnas numéricas que venimos usando
vars_num <- c("elapsedDays", "taxAmount", "totalPrice", 
              "travelDistance", "segmentDistance", "layoverNumber")

formula_class <- as.formula(paste("economy_f ~", paste(vars_num, collapse = " + ")))


# ==============================================================================
#                               NAIVE BAYES
# ==============================================================================

# -- Naive Bayes Estándar (Original) --
nb <- naive_bayes(formula_class, data = train_df, laplace = 1)


# --- Caret & Hyperparameter Tuning ---
cat("\nEntrenando Naive Bayes optimizado con tuning grid...\n")
set.seed(1994)

# Definición de la rejilla de parámetros
nb_grid <- expand.grid(usekernel = c(TRUE, FALSE),
                       laplace = c(0, 0.5, 1), 
                       adjust = c(0.75, 1, 1.25, 1.5))

naive_bayes_ramia <- train(economy_f ~ ., 
                           data = train_df[, c("economy_f", vars_num)], 
                           method = "naive_bayes",
                           tuneGrid = nb_grid,
                           trControl = trainControl(method = "cv", number = 10, classProbs = TRUE))

# Mostrar los valores óptimos seleccionados
print(naive_bayes_ramia$finalModel$tuneValue)

# Gráfico del proceso de tuning
plot(naive_bayes_ramia)

ggplot(melt(naive_bayes_ramia$resample[, -4]), aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Estabilidad de Naive Bayes (Resamples)", x = NULL, y = NULL) +
  theme_minimal()

# --- Predicción y Evaluación (Matriz de Confusión) para Naive Bayes ---
cat("\n Evaluando rendimiento de Naive Bayes con Matrices Avanzadas...\n")

# 1. Predicción sobre el conjunto de test
nb_preds <- predict(naive_bayes_ramia, newdata = test_df[, vars_num])

# 2. Matriz de confusión formal con Caret (Accuracy, Kappa, Sensitivity...)
matrix_stats_nb <- confusionMatrix(nb_preds, test_df$economy_f)
print(matrix_stats_nb)

# 3. Visualización gráfica de la matriz de confusión con ggplot2
conf_tbl_nb <- table(Predicted = nb_preds, Actual = test_df$economy_f)
conf_df_nb  <- as.data.frame(conf_tbl_nb)
colnames(conf_df_nb) <- c("Predicted", "Actual", "Freq")

ggplot(conf_df_nb, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  # Usamos un tono verde para diferenciarlo visualmente de la matriz azul del kNN
  scale_fill_gradient(low = "#f7fcf0", high = "#006d2c") + 
  labs(title = "Matriz de Confusión Naive Bayes", x = "Valor Real", y = "Predicción") +
  theme_minimal()


# ---  Frontera de Decisión Naive Bayes (PCA Plano) ---
cat("\nGenerando frontera de decisión para Naive Bayes...\n")
X_train_num <- train_df[, vars_num]
X_train_num[] <- lapply(X_train_num, as.numeric)

preproc_nb_pca <- preProcess(X_train_num, method = c("center", "scale", "pca"), pcaComp = 2)
Z_train_nb <- predict(preproc_nb_pca, X_train_num)
Z_test_nb <- predict(preproc_nb_pca, test_df[, vars_num])

modelo_nb_pca <- train(x = Z_train_nb, y = train_df$economy_f, method = "nb",
                       trControl = trainControl(method = "cv", number = 5, classProbs = TRUE))

# Crear malla para el fondo del gráfico
h <- 0.1
x_min <- min(Z_train_nb$PC1) - 1; x_max <- max(Z_train_nb$PC1) + 1
y_min <- min(Z_train_nb$PC2) - 1; y_max <- max(Z_train_nb$PC2) + 1
grid_nb <- expand.grid(PC1 = seq(x_min, x_max, by = h), PC2 = seq(y_min, y_max, by = h))
grid_nb$pred <- predict(modelo_nb_pca, newdata = grid_nb)

df_train_nb <- data.frame(Z_train_nb, clase = train_df$economy_f)
df_test_nb  <- data.frame(Z_test_nb,  clase = test_df$economy_f)

ggplot() +
  geom_raster(data = grid_nb, aes(PC1, PC2, fill = pred), alpha = 0.6) +
  geom_point(data = df_train_nb, aes(PC1, PC2, color = clase), size = 1.5) +
  geom_point(data = df_test_nb,  aes(PC1, PC2, color = clase), size = 2, shape = 21, stroke = 1) +
  labs(title = "Frontera Naive Bayes en plano PCA", x = "PC1", y = "PC2") +
  theme_minimal()


# ==============================================================================
#                                  kNN
# ==============================================================================

# -- 2A. kNN Imputación/Clasificación Base (Original) --
train_idx <- 1:nrow(train_df)
test_idx <- (nrow(train_df) + 1):(nrow(train_df) + nrow(test_df))

df_class <- rbind(train_df[, c("economy_f", vars_num)], 
                  test_df[, c("economy_f", vars_num)])
df_class$economy_f[test_idx] <- NA

result_knn1 <- kNN(df_class, variable = "economy_f", k = 1)
table(Prediccion = result_knn1$economy_f[test_idx], Real = test_df$economy_f)


# --- kNN Clasificador Formal (Escalado + Tuning + CV) ---
cat("\nEntrenando kNN formal con escalado y validación cruzada...\n")

X_trainC <- train_df[, vars_num]
X_testC  <- test_df[, vars_num]
y_trainC <- train_df$economy_f
y_testC  <- test_df$economy_f

# 1. Escalamos los datos usando caret
preproc_knn <- preProcess(X_trainC, method = c("center", "scale"))
X_trainC_scaled <- predict(preproc_knn, X_trainC)
X_testC_scaled  <- predict(preproc_knn, X_testC)

# 2. Entrenamiento con optimización automatizada de K y 5-Fold CV
set.seed(1994)
knn_ramia <- train(x = X_trainC_scaled, y = y_trainC,
                   method = "knn",
                   trControl = trainControl(method = "cv", number = 5),
                   tuneGrid = expand.grid(k = seq(1, 31, by = 2)))

print(knn_ramia)
plot(knn_ramia) # Gráfico de Accuracy vs K

# 3. Gráfico personalizado de Tasa de Error vs K
tablaResultados <- knn_ramia$results
tablaResultados$error <- 1 - tablaResultados$Accuracy
plot(tablaResultados$k, tablaResultados$error, type = "b", col = "dodgerblue", 
     xlab = "k, number of neighbors", ylab = "classification error",
     main = "(Test) Error Rate vs Neighbors")
abline(h = min(tablaResultados$error), col = "darkorange", lty = 3)


# --- Validación Avanzada (Matriz de Confusión Completa) ---
cat("\n Evaluando rendimiento de kNN con Matrices Avanzadas...\n")
knn_preds <- predict(knn_ramia, newdata = X_testC_scaled)

# Matriz de confusión con todas las métricas (Sensitivity, Specificity, Kappa...)
matrix_stats <- confusionMatrix(knn_preds, y_testC)
print(matrix_stats)

# Visualización gráfica de la matriz de confusión con ggplot2
conf_tbl <- table(Predicted = knn_preds, Actual = y_testC)
conf_df  <- as.data.frame(conf_tbl)
colnames(conf_df) <- c("Predicted", "Actual", "Freq")

ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "#f7fcf0", high = "#F2F255") +
  labs(title = "Matriz de Confusión kNN", x = "Valor Real", y = "Predicción") +
  theme_minimal()


# --- Frontera de Decisión kNN (PCA Plano) ---
cat("\n Generando frontera de decisión para kNN...\n")
preproc_knn_pca <- preProcess(X_trainC, method = c("center", "scale", "pca"), pcaComp = 2)
Z_train_knn <- predict(preproc_knn_pca, X_trainC)
Z_test_knn  <- predict(preproc_knn_pca, X_testC)

modelo_knn_pca <- train(x = Z_train_knn, y = y_trainC, method = "knn",
                        tuneGrid = data.frame(k = knn_ramia$bestTune$k),
                        trControl = trainControl(method = "cv", number = 5))

grid_knn <- expand.grid(PC1 = seq(x_min, x_max, by = h), PC2 = seq(y_min, y_max, by = h))
grid_knn$pred <- predict(modelo_knn_pca, newdata = grid_knn)

df_train_knn <- data.frame(Z_train_knn, clase = y_trainC)
df_test_knn  <- data.frame(Z_test_knn,  clase = y_testC)

ggplot() +
  geom_raster(data = grid_knn, aes(PC1, PC2, fill = pred), alpha = 0.6) +
  geom_point(data = df_train_knn, aes(PC1, PC2, color = clase), size = 1.5) +
  geom_point(data = df_test_knn,  aes(PC1, PC2, color = clase), size = 2, shape = 21, stroke = 1) +
  labs(title = sprintf("Frontera kNN en plano PCA (k=%d)", knn_ramia$bestTune$k), 
       x = "PC1", y = "PC2") +
  theme_minimal()


# ==============================================================================
#          3. kNN REGRESIÓN (Predicción de totalPrice): TUNING Y EVALUACIÓN
# ==============================================================================

# --- Búsqueda del K óptimo para Regresión (RMSE) ---
cat("\n--- Búsqueda del K óptimo mediante Cross-Validation ---\n")

# Configuramos Cross-Validation (5 folds)
ctrl_reg <- trainControl(method = "cv", number = 5)

# Definimos la cuadrícula de Ks impares a probar (del 3 al 31)
grid_knn <- expand.grid(k = seq(3, 31, by = 2))

# Entrenamos el modelo kNN buscando minimizar el RMSE. 
# NOTA: Solo usamos variables operativas, excluyendo taxAmount y log_price para evitar fugas (leakage).
set.seed(1994)
knn_reg_cv <- train(
  totalPrice ~ elapsedDays + travelDistance + segmentDistance + layoverNumber, 
  data = train_df, 
  method = "knn",
  trControl = ctrl_reg,
  tuneGrid = grid_knn,
  metric = "RMSE",
  preProcess = c("center", "scale") # ¡Escalado crucial para kNN!
)

# Imprimir los resultados del tuning
print(knn_reg_cv)

# Gráfico para ver cómo cae el error (RMSE) a medida que aumentamos K
# Puedes exportar este gráfico para tu informe
par(mfrow=c(1,1))
plot(knn_reg_cv, main = "kNN Regresión: RMSE vs. Número de Vecinos (K)")


# --- Evaluar el mejor modelo en el Test Set ---
cat("\n--- Evaluación en Test Set del Mejor kNN ---\n")
y_pred <- predict(knn_reg_cv, newdata = test_df)
y_real <- test_df$totalPrice

# Calcular métricas de regresión
rmse_knn <- RMSE(y_pred, y_real)
mae_knn  <- MAE(y_pred, y_real)
r2_knn   <- R2(y_pred, y_real)

# Mostrar resultados finales
cat(sprintf("Mejor K seleccionado: %d\n", knn_reg_cv$bestTune$k))
cat(sprintf("RMSE en Test: $%.2f\n", rmse_knn))
cat(sprintf("MAE en Test:  $%.2f\n", mae_knn))
cat(sprintf("R-Cuadrado:   %.4f\n", r2_knn))


# --- Gráficos de evaluación del mejor modelo ---
# Vuelve a configurar el panel de gráficos a 1 fila y 2 columnas
par(mfrow=c(1,2))

# Gráfico 1: Predicho vs Real
plot(y_pred, y_real, 
     main = sprintf("Predicho vs Real (K=%d)", knn_reg_cv$bestTune$k), 
     xlab = "Predicción de Precio", ylab = "Precio Real", 
     col = "dodgerblue", pch = 20, alpha = 0.5)
abline(0, 1, col = "red", lwd = 2) # Línea ideal

# Gráfico 2: Errores Residuales
plot(y_pred - y_real, 
     main = "Errores Residuales", 
     ylab = "Diferencia (Predicho - Real)", xlab = "Índice de Vuelo",
     col = "darkgreen", pch = 20, alpha = 0.5)
abline(h = 0, col = "red", lwd = 2)
