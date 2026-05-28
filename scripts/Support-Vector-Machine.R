# ==============================================================================
#        SUPPORT VECTOR MACHINE - FLIGHT PRICES
#        Adaptado a tus datos reales
#        Usa: data/interim/model_data.RData
#        Target: economy_f
# ==============================================================================

rm(list = ls())

# ==============================================================================
# 1. PAQUETES NECESARIOS
# ==============================================================================

paquetes <- c(
  "caret",
  "kernlab",
  "e1071",
  "dplyr",
  "ggplot2",
  "pROC",
  "yardstick",
  "tibble",
  "tidyr",
  "purrr"
)

instalar_si_falta <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

invisible(lapply(paquetes, instalar_si_falta))

library(caret)
library(kernlab)
library(e1071)
library(dplyr)
library(ggplot2)
library(pROC)
library(yardstick)
library(tibble)
library(tidyr)
library(purrr)

set.seed(42)

# ==============================================================================
# 2. CARGA DE DATOS YA PARTICIONADOS
# ==============================================================================

load("data/interim/model_data.RData")

cat("\nObjetos cargados:\n")
print(ls())

cat("\nDimensiones train_df:\n")
print(dim(train_df))

cat("\nDimensiones test_df:\n")
print(dim(test_df))

cat("\nPredictores disponibles:\n")
print(predictores)

# ==============================================================================
# 3. PREPARACIÓN DE DATOS
# ==============================================================================

objetivo <- "economy_f"

if (!objetivo %in% names(train_df)) {
  stop("No existe economy_f en train_df.")
}

if (!objetivo %in% names(test_df)) {
  stop("No existe economy_f en test_df.")
}

vars_svm <- unique(c(objetivo, predictores))

train <- train_df |>
  select(all_of(vars_svm))

test <- test_df |>
  select(all_of(vars_svm))

train <- train |>
  mutate(
    across(where(is.character), as.factor),
    across(where(is.logical), as.factor),
    economy_f = factor(economy_f, levels = c("Premium", "Economy"))
  )

test <- test |>
  mutate(
    across(where(is.character), as.factor),
    across(where(is.logical), as.factor),
    economy_f = factor(economy_f, levels = c("Premium", "Economy"))
  )

train <- na.omit(train)
test <- na.omit(test)

clase_positiva <- "Premium"
clase_negativa <- "Economy"

cat("\nDimensiones train antes de nearZeroVar:\n")
print(dim(train))

cat("\nDimensiones test antes de nearZeroVar:\n")
print(dim(test))

cat("\nDistribución train:\n")
print(table(train[[objetivo]]))
print(round(prop.table(table(train[[objetivo]])), 3))

cat("\nDistribución test:\n")
print(table(test[[objetivo]]))
print(round(prop.table(table(test[[objetivo]])), 3))

# ==============================================================================
# 4. ELIMINAR VARIABLES PROBLEMÁTICAS
# ==============================================================================

# nearZeroVar no debe aplicarse al target
x_train <- train |>
  select(-all_of(objetivo))

nzv <- nearZeroVar(x_train)

if (length(nzv) > 0) {
  vars_nzv <- names(x_train)[nzv]
  
  cat("\nVariables eliminadas por varianza casi cero:\n")
  print(vars_nzv)
  
  train <- train |>
    select(-all_of(vars_nzv))
  
  test <- test |>
    select(-all_of(vars_nzv))
} else {
  cat("\nNo se detectan variables de varianza casi cero.\n")
}

# Asegurar que test tiene las mismas columnas que train
test <- test[, names(train), drop = FALSE]

cat("\nDimensiones train después de nearZeroVar:\n")
print(dim(train))

cat("\nDimensiones test después de nearZeroVar:\n")
print(dim(test))

# ==============================================================================
# 5. EXPLORACIÓN INICIAL
# ==============================================================================

train |>
  count(economy_f) |>
  ggplot(aes(x = economy_f, y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Distribución de la variable objetivo",
    x = "Clase",
    y = "Número de observaciones"
  ) +
  theme_minimal()

# ==============================================================================
# 6. CONTROL DE ENTRENAMIENTO
# ==============================================================================

formula_modelo <- as.formula(paste(objetivo, "~ ."))

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  verboseIter = TRUE
)

cat("\nFórmula del modelo:\n")
print(formula_modelo)

cat("\nClase positiva:\n")
print(clase_positiva)

# ==============================================================================
# 7. SVM LINEAL
# ==============================================================================

set.seed(42)

# Quitamos C = 100 porque suele provocar más problemas de optimización.
grid_svm_lineal <- expand.grid(
  C = c(0.01, 0.1, 1, 10)
)

modelo_svm_lineal <- train(
  formula_modelo,
  data = train,
  method = "svmLinear",
  metric = "ROC",
  trControl = ctrl,
  preProcess = c("medianImpute", "center", "scale"),
  tuneGrid = grid_svm_lineal
)

cat("\n============================================================\n")
cat("SVM LINEAL\n")
cat("============================================================\n")

print(modelo_svm_lineal)

if (nrow(modelo_svm_lineal$results) > 1) {
  plot(modelo_svm_lineal)
} else {
  print(modelo_svm_lineal$results)
}

cat("\nMejor C del SVM lineal:\n")
print(modelo_svm_lineal$bestTune)

cat("\nMejor ROC medio CV lineal:\n")
print(max(modelo_svm_lineal$results$ROC, na.rm = TRUE))

# ==============================================================================
# 8. SVM RADIAL
# ==============================================================================

set.seed(42)

# Grid más estable que el original para tus 20.000 filas.
grid_svm_radial <- expand.grid(
  sigma = c(0.001, 0.005, 0.01),
  C = c(0.25, 0.5, 1, 2)
)

modelo_svm_radial <- train(
  formula_modelo,
  data = train,
  method = "svmRadial",
  metric = "ROC",
  trControl = ctrl,
  preProcess = c("medianImpute", "center", "scale"),
  tuneGrid = grid_svm_radial
)

cat("\n============================================================\n")
cat("SVM RADIAL\n")
cat("============================================================\n")

print(modelo_svm_radial)

if (nrow(modelo_svm_radial$results) > 1) {
  plot(modelo_svm_radial)
} else {
  print(modelo_svm_radial$results)
}

cat("\nMejor sigma:\n")
print(modelo_svm_radial$bestTune$sigma)

cat("\nMejor C:\n")
print(modelo_svm_radial$bestTune$C)

cat("\nMejor ROC medio CV radial:\n")
print(max(modelo_svm_radial$results$ROC, na.rm = TRUE))

# ==============================================================================
# 9. SVM POLINOMIAL
# ==============================================================================

set.seed(42)

# Grid conservador para evitar problemas de convergencia.
grid_svm_poly <- expand.grid(
  degree = c(2),
  scale = c(0.001, 0.01),
  C = c(0.25, 1)
)

modelo_svm_poly <- train(
  formula_modelo,
  data = train,
  method = "svmPoly",
  metric = "ROC",
  trControl = ctrl,
  preProcess = c("medianImpute", "center", "scale"),
  tuneGrid = grid_svm_poly
)

cat("\n============================================================\n")
cat("SVM POLINOMIAL\n")
cat("============================================================\n")

print(modelo_svm_poly)

if (nrow(modelo_svm_poly$results) > 1) {
  plot(modelo_svm_poly)
} else {
  print(modelo_svm_poly$results)
}

cat("\nMejores parámetros polinomial:\n")
print(modelo_svm_poly$bestTune)

cat("\nMejor ROC medio CV polinomial:\n")
print(max(modelo_svm_poly$results$ROC, na.rm = TRUE))

# ==============================================================================
# 10. COMPARACIÓN DE MODELOS
# ==============================================================================

roc_lineal <- max(modelo_svm_lineal$results$ROC, na.rm = TRUE)
roc_radial <- max(modelo_svm_radial$results$ROC, na.rm = TRUE)
roc_poly <- max(modelo_svm_poly$results$ROC, na.rm = TRUE)

comparacion_r <- tibble(
  modelo = c("SVM lineal", "SVM radial", "SVM polinomial"),
  ROC_CV = c(roc_lineal, roc_radial, roc_poly)
) |>
  arrange(desc(ROC_CV))

cat("\n============================================================\n")
cat("COMPARACIÓN DE MODELOS\n")
cat("============================================================\n")

print(comparacion_r)

ggplot(comparacion_r, aes(x = reorder(modelo, ROC_CV), y = ROC_CV)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Comparación de modelos SVM",
    x = "Modelo",
    y = "ROC AUC medio en CV"
  ) +
  theme_minimal()

mejor_nombre <- comparacion_r$modelo[1]

if (mejor_nombre == "SVM radial") {
  mejor_modelo <- modelo_svm_radial
} else if (mejor_nombre == "SVM polinomial") {
  mejor_modelo <- modelo_svm_poly
} else {
  mejor_modelo <- modelo_svm_lineal
}

cat("\nMejor modelo en R:\n")
print(mejor_nombre)

# ==============================================================================
# 11. EVALUACIÓN EN TEST
# ==============================================================================

pred_clase <- predict(mejor_modelo, newdata = test)
pred_prob <- predict(mejor_modelo, newdata = test, type = "prob")

cat("\nPrimeras probabilidades:\n")
print(head(pred_prob))

cm_r <- confusionMatrix(
  data = pred_clase,
  reference = test[[objetivo]],
  positive = clase_positiva
)

cat("\n============================================================\n")
cat("MATRIZ DE CONFUSIÓN\n")
cat("============================================================\n")

print(cm_r)

metricas_df <- tibble(
  truth = test[[objetivo]],
  estimate = pred_clase,
  prob_premium = pred_prob[[clase_positiva]]
)

metricas_r <- bind_rows(
  accuracy(metricas_df, truth = truth, estimate = estimate),
  precision(metricas_df, truth = truth, estimate = estimate, event_level = "first"),
  recall(metricas_df, truth = truth, estimate = estimate, event_level = "first"),
  f_meas(metricas_df, truth = truth, estimate = estimate, event_level = "first"),
  roc_auc(metricas_df, truth = truth, prob_premium, event_level = "first")
)

cat("\n============================================================\n")
cat("MÉTRICAS EN TEST\n")
cat("============================================================\n")

print(metricas_r)

# ==============================================================================
# 12. CURVA ROC
# ==============================================================================

roc_obj <- roc(
  response = test[[objetivo]],
  predictor = pred_prob[[clase_positiva]],
  levels = c(clase_negativa, clase_positiva)
)

plot(
  roc_obj,
  main = paste("Curva ROC -", mejor_nombre)
)

cat("\nAUC en test:\n")
print(auc(roc_obj))

# ==============================================================================
# 13. PREDICCIÓN DE UN NUEVO REGISTRO
# ==============================================================================

nuevo_registro <- test[1, , drop = FALSE]

clase_real <- nuevo_registro[[objetivo]]
nuevo_registro[[objetivo]] <- NULL

pred_nuevo_clase <- predict(mejor_modelo, newdata = nuevo_registro)
pred_nuevo_prob <- predict(mejor_modelo, newdata = nuevo_registro, type = "prob")

cat("\n============================================================\n")
cat("PREDICCIÓN DE UN NUEVO REGISTRO\n")
cat("============================================================\n")

cat("Clase real:", as.character(clase_real), "\n")
cat("Clase predicha:", as.character(pred_nuevo_clase), "\n")

print(pred_nuevo_prob)

# ==============================================================================
# 14. GUARDAR RESULTADOS
# ==============================================================================

dir.create("models", showWarnings = FALSE, recursive = TRUE)

saveRDS(mejor_modelo, "models/mejor_svm_economy_f.rds")
saveRDS(comparacion_r, "models/comparacion_svm_economy_f.rds")
saveRDS(metricas_r, "models/metricas_svm_economy_f.rds")
saveRDS(cm_r, "models/confusion_matrix_svm_economy_f.rds")

cat("\n============================================================\n")
cat("FIN DEL SCRIPT SVM\n")
cat("============================================================\n")