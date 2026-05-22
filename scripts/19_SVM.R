# =========================================================================
# 15. SUPPORT VECTOR MACHINES (SVM): Clasificación y Regresión — Flight Prices
# =========================================================================

# --- 0. SETUP Y CARGA DE DATOS ---
rm(list = ls())

list.of.packages <- c(
  "kernlab", "e1071", "caret", "ggplot2", "dplyr", 
  "reshape2", "pROC", "tibble", "tidyr", "yardstick"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages) > 0) install.packages(new.packages, dependencies = TRUE)
invisible(lapply(list.of.packages, require, character.only = TRUE))

rm(list.of.packages, new.packages)

set.seed(1994)

# Carga de la partición de datos única del proyecto
load("data/interim/model_data.RData")

cat("\n--- Dimensiones iniciales de los datasets ---\n")
cat("Train original:", nrow(train_df), "x", ncol(train_df), "\n")
cat("Test original :", nrow(test_df),  "x", ncol(test_df),  "\n")


# =========================================================================
# 1. PREPARACIÓN GENERAL Y CONTROL DE TIPOS
# =========================================================================

if (!all(c("economy_f", "log_price") %in% names(train_df))) {
  stop("Faltan variables objetivo esenciales (economy_f o log_price) en los datos cargados.")
}

# Definición de la clase de interés (Economy como clase positiva)
train_df$economy_f <- factor(train_df$economy_f, levels = c("Economy", "Premium"))
test_df$economy_f  <- factor(test_df$economy_f,  levels = c("Economy", "Premium"))

train_df$economy_f <- droplevels(train_df$economy_f)
test_df$economy_f  <- droplevels(test_df$economy_f)

# Conversión automática de tipos de datos para evitar fallos de ejecución
train_df <- train_df %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.logical), as.factor))

test_df <- test_df %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.logical), as.factor))


# =========================================================================
# 2. DEFINICIÓN DE PREDICTORES BASE
# =========================================================================

pred_class <- c("log_price", "travelDistance", "layoverNumber", "airline", "nonStop", "elapsedDays", "seatsLeft")
pred_class <- pred_class[pred_class %in% names(train_df)]

# Para regresión (SVR) log_price es el target, por lo que no puede ser predictor
pred_reg   <- setdiff(pred_class, "log_price")


# =========================================================================
# 3. AISLAMIENTO DE DATASETS ESPECÍFICOS (Previene Data Leakage)
# =========================================================================

train_class <- train_df %>% select(all_of(c("economy_f", pred_class))) %>% na.omit()
test_class  <- test_df  %>% select(all_of(c("economy_f", pred_class))) %>% na.omit()

train_reg   <- train_df %>% select(all_of(c("log_price", pred_reg))) %>% na.omit()
test_reg    <- test_df  %>% select(all_of(c("log_price", pred_reg))) %>% na.omit()


# =========================================================================
# 4. ELIMINACIÓN DE VARIABLES CON VARIANZA CASI CERO (nearZeroVar)
# =========================================================================

# Clasificación
x_class <- train_class %>% select(-economy_f)
nzv_class <- nearZeroVar(x_class)
if (length(nzv_class) > 0) {
  vars_nzv_class <- names(x_class)[nzv_class]
  train_class <- train_class %>% select(-all_of(vars_nzv_class))
  test_class  <- test_class  %>% select(-all_of(vars_nzv_class))
}
test_class <- test_class[, names(train_class), drop = FALSE]

# Regresión
x_reg <- train_reg %>% select(-log_price)
nzv_reg <- nearZeroVar(x_reg)
if (length(nzv_reg) > 0) {
  vars_nzv_reg <- names(x_reg)[nzv_reg]
  train_reg <- train_reg %>% select(-all_of(vars_nzv_reg))
  test_reg  <- test_reg  %>% select(-all_of(vars_nzv_reg))
}
test_reg <- test_reg[, names(train_reg), drop = FALSE]

pred_class_final <- setdiff(names(train_class), "economy_f")
pred_reg_final   <- setdiff(names(train_reg), "log_price")


# =========================================================================
# 5. ANÁLISIS DESCRIPTIVO DE LOS TARGETS
# =========================================================================
cat("\n======================================================\n")
cat("      ANÁLISIS DESCRIPTIVO DE LOS TARGETS\n")
cat("======================================================\n")

cat("\nDistribución de economy_f en train_class:\n")
print(prop.table(table(train_class$economy_f)))

# Histograma de log_price para Regresión
p_dist_reg <- ggplot(train_reg, aes(x = log_price)) +
  geom_histogram(bins = 50, fill = "#1a7a4a", color = "white", alpha = 0.8) +
  labs(title = "Distribución de log(Precio) para Regresión", x = "log(Precio)", y = "Frecuencia") +
  theme_minimal(base_size = 13)
print(p_dist_reg)


# =========================================================================
# 6. SVM CLASIFICACIÓN — Target: economy_f
# =========================================================================
cat("\n======================================================\n")
cat("      SVM CLASIFICACIÓN (economy_f)\n")
cat("======================================================\n")

formula_class <- as.formula(paste("economy_f ~", paste(pred_class_final, collapse = " + ")))
cat("Fórmula de Clasificación:\n"); print(formula_class)

# Configuración del control con Validación Cruzada (5-Fold) para optimizar tiempos
ctrl_class <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# --- 6A. SVM Lineal (Clasificación) ---
cat("\n--- Entrenando SVM Lineal clasificación... ---\n")
set.seed(1994)
grid_lineal_class <- expand.grid(C = c(0.1, 1, 10))
svm_lineal_class <- train(
  formula_class, data = train_class, method = "svmLinear",
  metric = "ROC", trControl = ctrl_class, preProcess = c("medianImpute", "center", "scale"),
  tuneGrid = grid_lineal_class
)
print(svm_lineal_class)

# --- 6B. SVM Radial (Clasificación) ---
cat("\n--- Entrenando SVM Radial clasificación... ---\n")
set.seed(1994)
grid_radial_class <- expand.grid(sigma = c(0.001, 0.01), C = c(0.1, 1, 10))
svm_radial_class <- train(
  formula_class, data = train_class, method = "svmRadial",
  metric = "ROC", trControl = ctrl_class, preProcess = c("medianImpute", "center", "scale"),
  tuneGrid = grid_radial_class
)
print(svm_radial_class)

# --- 6C. SVM Polinomial (Clasificación) ---
cat("\n--- Entrenando SVM Polinomial clasificación... ---\n")
set.seed(1994)
grid_poly_class <- expand.grid(degree = c(2, 3), scale = c(0.01, 0.1), C = c(0.5, 2))
svm_poly_class <- train(
  formula_class, data = train_class, method = "svmPoly",
  metric = "ROC", trControl = ctrl_class, preProcess = c("medianImpute", "center", "scale"),
  tuneGrid = grid_poly_class
)
print(svm_poly_class)


# =========================================================================
# 7. COMPARACIÓN DE MODELOS DE CLASIFICACIÓN
# =========================================================================
cat("\n======================================================\n")
cat("      COMPARACIÓN DE MODELOS CLASIFICACIÓN (CV)\n")
cat("======================================================\n")

resumen_modelos_class <- resamples(list(
  SVM_Lineal     = svm_lineal_class,
  SVM_Radial     = svm_radial_class,
  SVM_Polinomial = svm_poly_class
))

print(summary(resumen_modelos_class))

rocs_class <- c(
  Lineal     = max(svm_lineal_class$results$ROC, na.rm = TRUE),
  Radial     = max(svm_radial_class$results$ROC, na.rm = TRUE),
  Polinomial = max(svm_poly_class$results$ROC, na.rm = TRUE)
)

mejor_modelo_nombre_class <- names(which.max(rocs_class))
cat("\nMejor modelo de clasificación seleccionado según ROC en CV:", mejor_modelo_nombre_class, "\n")

mejor_modelo_class <- switch(
  mejor_modelo_nombre_class,
  "Lineal"     = svm_lineal_class,
  "Radial"     = svm_radial_class,
  "Polinomial" = svm_poly_class
)


# =========================================================================
# 8. EVALUACIÓN DEL MEJOR MODELO DE CLASIFICACIÓN EN TEST
# =========================================================================
cat("\n======================================================\n")
cat("      EVALUACIÓN EN TEST (MEJOR SVM CLASIFICACIÓN)\n")
cat("======================================================\n")

pred_test_class <- predict(mejor_modelo_class, newdata = test_class)
pred_test_prob  <- predict(mejor_modelo_class, newdata = test_class, type = "prob")

cm_test <- caret::confusionMatrix(pred_test_class, test_class$economy_f, positive = "Economy")
print(cm_test)

# Plot Confusion Matrix (Heatmap)
CM_df <- as.data.frame(cm_test$table)
p_cm <- ggplot(CM_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() + geom_text(aes(label = Freq), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "#009194") +
  labs(title = "Confusion Matrix SVM Test Set", x = "Predicción", y = "Clase Real") +
  theme_minimal(base_size = 13)
print(p_cm)

# Métricas detalladas con yardstick
metricas_class_df <- tibble(
  truth = test_class$economy_f,
  estimate = pred_test_class,
  prob_economy = pred_test_prob$Economy
)

metricas_class <- bind_rows(
  yardstick::accuracy(metricas_class_df, truth = truth, estimate = estimate),
  yardstick::precision(metricas_class_df, truth = truth, estimate = estimate, event_level = "first"),
  yardstick::recall(metricas_class_df, truth = truth, estimate = estimate, event_level = "first"),
  yardstick::f_meas(metricas_class_df, truth = truth, estimate = estimate, event_level = "first"),
  yardstick::roc_auc(metricas_class_df, truth = truth, prob_economy, event_level = "first")
)
print(metricas_class)


# =========================================================================
# 9. FRONTERA DE DECISIÓN DEL SVM EN ESPACIO PCA (2D)
# =========================================================================
cat("\n--- DECISION BOUNDARY (ESPACIO PCA 2D) ---\n")

vars_num <- c("log_price", "travelDistance", "layoverNumber", "elapsedDays", "seatsLeft")
vars_num <- vars_num[vars_num %in% names(train_class)]

if (length(vars_num) >= 2) {
  pca_res <- prcomp(train_class[, vars_num], scale. = TRUE)
  var_exp <- round(summary(pca_res)$importance[2, 1:2] * 100, 1)
  
  df_pca_train <- data.frame(
    PC1       = pca_res$x[, 1],
    PC2       = pca_res$x[, 2],
    economy_f = train_class$economy_f
  )
  
  set.seed(1994)
  svm_pca <- ksvm(economy_f ~ PC1 + PC2, data = df_pca_train, kernel = "rbfdot", C = 1, prob.model = TRUE)
  
  grid_pc1  <- seq(min(df_pca_train$PC1), max(df_pca_train$PC1), length.out = 200)
  grid_pc2  <- seq(min(df_pca_train$PC2), max(df_pca_train$PC2), length.out = 200)
  mesh_pca  <- expand.grid(PC1 = grid_pc1, PC2 = grid_pc2)
  mesh_pca$pred_class <- predict(svm_pca, newdata = mesh_pca)
  
  p_boundary <- ggplot() +
    geom_tile(data = mesh_pca, aes(x = PC1, y = PC2, fill = pred_class), alpha = 0.3) +
    geom_point(data = df_pca_train, aes(x = PC1, y = PC2, color = economy_f), size = 0.8, alpha = 0.6) +
    scale_fill_manual(values = c("Economy" = "#A8C8F9", "Premium" = "#F9A8A8"), name = "Región Predicha") +
    scale_color_manual(values = c("Economy" = "#1a5fa8", "Premium" = "#c0392b"), name = "Clase Real") +
    labs(
      title = "Decision Boundary (SVM Radial) en Espacio PCA 2D",
      subtitle = sprintf("PC1 (%.1f%% var) vs PC2 (%.1f%% var)", var_exp[1], var_exp[2]),
      x = "PC1", y = "PC2"
    ) +
    theme_minimal(base_size = 13)
  print(p_boundary)
}


# =========================================================================
# 10. SVM REGRESIÓN (SVR) — Target: log_price
# =========================================================================
cat("\n======================================================\n")
cat("      SUPPORT VECTOR REGRESSION (SVR)\n")
cat("======================================================\n")

formula_reg <- as.formula(paste("log_price ~", paste(pred_reg_final, collapse = " + ")))
cat("Fórmula de Regresión (SVR):\n"); print(formula_reg)

ctrl_reg <- trainControl(
  method = "cv",
  number = 5
)

# --- 10A. SVR Lineal ---
cat("\n--- Entrenando SVR Lineal... ---\n")
set.seed(1994)
grid_svr_lineal <- expand.grid(C = c(0.1, 1, 10))
svr_lineal <- train(
  formula_reg, data = train_reg, method = "svmLinear",
  metric = "RMSE", trControl = ctrl_reg, preProcess = c("medianImpute", "center", "scale"),
  tuneGrid = grid_svr_lineal
)
print(svr_lineal)

# --- 10B. SVR Radial ---
cat("\n--- Entrenando SVR Radial... ---\n")
set.seed(1994)
grid_svr_radial <- expand.grid(sigma = c(0.001, 0.01), C = c(0.1, 1, 10))
svr_radial <- train(
  formula_reg, data = train_reg, method = "svmRadial",
  metric = "RMSE", trControl = ctrl_reg, preProcess = c("medianImpute", "center", "scale"),
  tuneGrid = grid_svr_radial
)
print(svr_radial)

# --- 10C. SVR Polinomial ---
cat("\n--- Entrenando SVR Polinomial... ---\n")
set.seed(1994)
grid_svr_poly <- expand.grid(degree = c(2, 3), scale = c(0.01, 0.1), C = c(0.5, 2))
svr_poly <- train(
  formula_reg, data = train_reg, method = "svmPoly",
  metric = "RMSE", trControl = ctrl_reg, preProcess = c("medianImpute", "center", "scale"),
  tuneGrid = grid_svr_poly
)
print(svr_poly)


# =========================================================================
# 11. COMPARACIÓN DE MODELOS DE REGRESIÓN (SVR)
# =========================================================================
cat("\n======================================================\n")
cat("      COMPARACIÓN DE MODELOS REGRESIÓN SVR (CV)\n")
cat("======================================================\n")

rmses_reg <- c(
  Lineal     = min(svr_lineal$results$RMSE, na.rm = TRUE),
  Radial     = min(svr_radial$results$RMSE, na.rm = TRUE),
  Polinomial = min(svr_poly$results$RMSE, na.rm = TRUE)
)

comparacion_reg <- tibble(
  Modelo  = names(rmses_reg),
  RMSE_CV = as.numeric(rmses_reg)
) %>% arrange(RMSE_CV)

print(comparacion_reg)

mejor_modelo_nombre_reg <- comparacion_reg$Modelo[1]
cat("\nMejor regresor seleccionado según RMSE en CV:", mejor_modelo_nombre_reg, "\n")

mejor_modelo_reg <- switch(
  mejor_modelo_nombre_reg,
  "Lineal"     = svr_lineal,
  "Radial"     = svr_radial,
  "Polinomial" = svr_poly
)


# =========================================================================
# 12. EVALUACIÓN Y MÉTRICAS DE REGRESIÓN (SVR) EN TEST
# =========================================================================
cat("\n======================================================\n")
cat("      EVALUACIÓN EN TEST (MEJOR SVR)\n")
cat("======================================================\n")

pred_test_reg <- predict(mejor_modelo_reg, newdata = test_reg)

# Métricas de evaluación en el espacio logarítmico (log_price)
rmse_log   <- sqrt(mean((test_reg$log_price - pred_test_reg)^2))
mae_log    <- mean(abs(test_reg$log_price - pred_test_reg))
ss_res_log <- sum((test_reg$log_price - pred_test_reg)^2)
ss_tot_log <- sum((test_reg$log_price - mean(test_reg$log_price))^2)
r2_log     <- 1 - (ss_res_log / ss_tot_log)

cat("\n--- MÉTRICAS SVR EN ESPACIO LOG-PRICE (TEST) ---\n")
cat(sprintf("  RMSE : %.4f\n", rmse_log))
cat(sprintf("  MAE  : %.4f\n", mae_log))
cat(sprintf("  R²   : %.4f\n", r2_log))

# Transformación de vuelta a Euros reales para la interpretación del negocio
precios_reales    <- exp(test_reg$log_price)
precios_predichos <- exp(pred_test_reg)

rmse_euros <- sqrt(mean((precios_reales - precios_predichos)^2))
mae_euros  <- mean(abs(precios_reales - precios_predichos))

cat("\n--- MÉTRICAS SVR EN VALORES REALES (TEST) ---\n")
cat(sprintf("  RMSE Real : %.2f €\n", rmse_euros))
cat(sprintf("  MAE Real  : %.2f €\n", mae_euros))

# Métricas detalladas usando yardstick para consistencia del proyecto
metricas_reg_df <- tibble(
  truth = test_reg$log_price,
  estimate = pred_test_reg
)

metricas_reg_ys <- bind_rows(
  yardstick::rmse(metricas_reg_df, truth = truth, estimate = estimate),
  yardstick::mae(metricas_reg_df, truth = truth, estimate = estimate),
  yardstick::rsq(metricas_reg_df, truth = truth, estimate = estimate)
)

cat("\nMétricas yardstick regresión (SVR):\n")
print(metricas_reg_ys)


# =========================================================================
# 13. PLOT DE EVALUACIÓN: PREDICHO VS REAL (SVR)
# =========================================================================
cat("\n======================================================\n")
cat("      PLOT DE EVALUACIÓN: PREDICHO VS REAL\n")
cat("======================================================\n")

df_eval_reg <- data.frame(
  real     = precios_reales,
  predicho = precios_predichos
)

p_reg_eval <- ggplot(df_eval_reg, aes(x = real, y = predicho)) +
  geom_point(alpha = 0.3, color = "#1a7a4a", size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title    = paste("SVR", mejor_modelo_nombre_reg, "Regresión: Predicho vs Real"),
    subtitle = sprintf("R² (Log) = %.4f  |  MAE Real = %.2f €", r2_log, mae_euros),
    x        = "Precio Real (€)",
    y        = "Precio Predicho (€)"
  ) +
  theme_minimal(base_size = 13)
print(p_reg_eval)


# =========================================================================
# 14. GUARDADO DE MODELOS Y PREDICCIONES
# =========================================================================
cat("\n======================================================\n")
cat("      GUARDADO DE RESULTADOS\n")
cat("======================================================\n")

out_dir <- file.path(getwd(), "data", "results")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Guardar modelos campeones de SVM/SVR para el ensamble o reporte final
save(mejor_modelo_class, mejor_modelo_reg, 
     file = file.path(out_dir, "svm_best_models.RData"))

cat("\nModelos guardados con éxito en:", out_dir, "\n")
cat("  - svm_best_models.RData\n")


# =========================================================================
cat("\n======================================================\n")
cat("      SVM & SVR COMPLETADO CON ÉXITO\n")
cat("======================================================\n")