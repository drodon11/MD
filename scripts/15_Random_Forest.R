# =========================================================================
# RANDOM FOREST: Clasificación y Regresión — Flight Prices
# =========================================================================

# --- 0. SETUP Y CARGA DE DATOS ---
rm(list = ls())

list.of.packages <- c(
  "randomForest", "ranger", "caret", "ggplot2", "dplyr",
  "reshape2", "vip", "pdp", "iml", "rpart", "rpart.plot", "future"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages) > 0) install.packages(new.packages, dependencies = TRUE)
invisible(lapply(list.of.packages, require, character.only = TRUE))

load("data/interim/model_data.RData")


# =========================================================================
# 1. ANÁLISIS DESCRIPTIVO
# =========================================================================
cat("\n======================================================\n")
cat("      ANÁLISIS DESCRIPTIVO\n")
cat("======================================================\n")

cat("Dimensiones train:", nrow(train_df), "x", ncol(train_df), "\n")
cat("Dimensiones test :", nrow(test_df),  "x", ncol(test_df),  "\n\n")
cat("Predictores disponibles:\n")
print(predictores)

cat("\nDistribución de economy_f (train):\n")
print(prop.table(table(train_df$economy_f)))
cat("\nDistribución de economy_f (test):\n")
print(prop.table(table(test_df$economy_f)))

# Distribución variable objetivo — Clasificación
p_dist_class <- ggplot(train_df, aes(x = economy_f, fill = economy_f)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = c("Economy" = "#1a5fa8", "Premium" = "#c0392b")) +
  labs(
    title = "Distribución de la variable objetivo — Clasificación",
    x     = "Tipo de billete",
    y     = "Número de observaciones"
  ) +
  theme_minimal(base_size = 13)
print(p_dist_class)

# Distribución variable objetivo — Regresión
p_dist_reg <- ggplot(train_df, aes(x = log_price)) +
  geom_histogram(bins = 60, fill = "#1a7a4a", color = "white", alpha = 0.85) +
  labs(
    title = "Distribución de log(Precio) — Regresión",
    x     = "log(Precio total)",
    y     = "Frecuencia"
  ) +
  theme_minimal(base_size = 13)
print(p_dist_reg)

cat("\nResumen variables numéricas (train):\n")
train_df %>%
  select(log_price, travelDistance, layoverNumber, elapsedDays, seatsLeft) %>%
  summary() %>%
  print()


# =========================================================================
# 2. PARTICIÓN ENTRENAMIENTO / TEST
# =========================================================================
# La partición ya está realizada en 10_Partition_Data.R:
#
# dd$economy_f <- factor(ifelse(dd$economy %in% c("Basic Economy", "Economy"),
#                               "Economy", "Premium"))
# dd$log_price <- log(dd$totalPrice)
# predictores  <- c("log_price", "travelDistance", "layoverNumber",
#                   "airline", "nonStop", "elapsedDays", "seatsLeft")
# set.seed(42)
# train_idx <- createDataPartition(dd$economy_f, p = 0.8, list = FALSE)
# train_df  <- dd[train_idx, ]
# test_df   <- dd[-train_idx, ]

cat("\nProporción train:\n"); print(round(prop.table(table(train_df$economy_f)), 4))
cat("\nProporción test:\n");  print(round(prop.table(table(test_df$economy_f)),  4))


# =========================================================================
# 3. RANDOM FOREST — CLASIFICACIÓN (economy_f)
# =========================================================================
cat("\n======================================================\n")
cat("      RANDOM FOREST: CLASIFICACIÓN\n")
cat("======================================================\n")

# Excluimos log_price: es transformación del target de regresión
pred_class    <- predictores[predictores != "log_price"]
formula_class <- as.formula(paste("economy_f ~", paste(pred_class, collapse = " + ")))
cat("Fórmula clasificación:\n"); print(formula_class)


# ------------------------------------------------------------------
# 3A. Entrenamiento básico
# ------------------------------------------------------------------
set.seed(1994)

rf_class <- randomForest(
  formula_class,
  data       = train_df,
  ntree      = 500,
  importance = TRUE
)

cat("\n--- MODELO BÁSICO ---\n")
print(rf_class)


# ------------------------------------------------------------------
# 3B. Predicciones y Confusion Matrix
# ------------------------------------------------------------------
pred_train_class <- predict(rf_class, train_df)
pred_test_class  <- predict(rf_class, test_df)

cat("\n--- CONFUSION MATRIX (TRAIN) ---\n")
cm_train_class <- caret::confusionMatrix(pred_train_class, as.factor(train_df$economy_f))
print(cm_train_class)

cat("\n--- CONFUSION MATRIX (TEST) ---\n")
cm_class <- caret::confusionMatrix(pred_test_class, as.factor(test_df$economy_f))
print(cm_class)

# Plot Confusion Matrix
CM_df <- as.data.frame(cm_class$table)

p_cm_class <- ggplot(CM_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "#1a5fa8") +
  labs(
    title = "Matriz de confusión — Random Forest Clasificación (Test)",
    x     = "Predicción",
    y     = "Clase real"
  ) +
  theme_minimal(base_size = 13)
print(p_cm_class)


# ------------------------------------------------------------------
# 3C. Indicadores de clasificación
# ------------------------------------------------------------------
cat("\n--- INDICADORES DE CLASIFICACIÓN (TEST) ---\n")
cat(sprintf("  Accuracy   : %.4f\n", cm_class$overall["Accuracy"]))
cat(sprintf("  Recall     : %.4f\n", cm_class$byClass["Recall"]))
cat(sprintf("  Precision  : %.4f\n", cm_class$byClass["Precision"]))
cat(sprintf("  F1 Score   : %.4f\n", cm_class$byClass["F1"]))
cat(sprintf("  Specificity: %.4f\n", cm_class$byClass["Specificity"]))


# =========================================================================
# 4. VALIDACIÓN CRUZADA — CLASIFICACIÓN
# =========================================================================
cat("\n======================================================\n")
cat("      VALIDACIÓN CRUZADA — CLASIFICACIÓN\n")
cat("======================================================\n")

trControl_class <- trainControl(
  method          = "cv",
  number          = 10,
  classProbs      = TRUE,
  summaryFunction = multiClassSummary,
  savePredictions = "final"
)

set.seed(1994)
rf_cv_class <- train(
  formula_class,
  data      = train_df,
  method    = "rf",
  metric    = "Accuracy",
  trControl = trControl_class,
  ntree     = 300
)

cat("\n--- RESULTADOS CV ---\n")
print(rf_cv_class)

ggplot(rf_cv_class) +
  labs(title = "Validación cruzada — Random Forest Clasificación") +
  theme_minimal(base_size = 13) |> print()

# Variabilidad por fold
cols_metricas <- intersect(
  c("Accuracy", "Kappa", "Recall", "Precision", "F1"),
  names(rf_cv_class$resample)
)

p_vars_class <- ggplot(
  melt(rf_cv_class$resample[, cols_metricas]),
  aes(x = variable, y = value, fill = variable)
) +
  geom_boxplot(show.legend = FALSE, alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Variabilidad en Cross-Validation (10-Folds) — Clasificación",
    x     = "Métrica",
    y     = "Valor"
  ) +
  theme_minimal(base_size = 13)
print(p_vars_class)


# =========================================================================
# 5. AJUSTE DE HIPERPARÁMETROS — CLASIFICACIÓN
# =========================================================================
cat("\n======================================================\n")
cat("      TUNING HIPERPARÁMETROS — CLASIFICACIÓN\n")
cat("======================================================\n")

set.seed(1994)

rf_grid_class <- expand.grid(mtry = c(2, 3, 4, 5))

rf_tuned_class <- train(
  formula_class,
  data      = train_df,
  method    = "rf",
  metric    = "Accuracy",
  trControl = trControl_class,
  tuneGrid  = rf_grid_class,
  ntree     = 500
)

cat("\n--- MEJOR HIPERPARÁMETRO (mtry) ---\n")
print(rf_tuned_class$bestTune)
print(rf_tuned_class)

ggplot(rf_tuned_class) +
  labs(title = "Tuning del parámetro mtry — Clasificación") +
  theme_minimal(base_size = 13) |> print()

# Evaluación del modelo tuneado
pred_tuned_class <- predict(rf_tuned_class, test_df)
cm_tuned_class   <- caret::confusionMatrix(pred_tuned_class, as.factor(test_df$economy_f))

cat("\n--- CONFUSION MATRIX MODELO TUNEADO (TEST) ---\n")
print(cm_tuned_class)
cat(sprintf("\n  Accuracy (tuneado) : %.4f\n", cm_tuned_class$overall["Accuracy"]))
cat(sprintf("  F1 Score (tuneado) : %.4f\n",  cm_tuned_class$byClass["F1"]))


# =========================================================================
# 6. IMPORTANCIA DE VARIABLES — CLASIFICACIÓN
# =========================================================================
cat("\n======================================================\n")
cat("      IMPORTANCIA DE VARIABLES — CLASIFICACIÓN\n")
cat("======================================================\n")

# --- Mean Decrease Gini ---
imp_class <- as.data.frame(rf_class$importance)
imp_class$Variable <- rownames(imp_class)

imp_class_top <- imp_class %>%
  arrange(desc(MeanDecreaseGini)) %>%
  mutate(Variable = reorder(Variable, MeanDecreaseGini))

cat("\n--- TOP VARIABLES (MeanDecreaseGini) ---\n")
print(head(imp_class[order(-imp_class$MeanDecreaseGini), ], 10))

p_imp_class <- ggplot(imp_class_top, aes(x = Variable, y = MeanDecreaseGini, fill = MeanDecreaseGini)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "#A8C8F9", high = "#1a5fa8") +
  coord_flip() +
  labs(
    title = "Importancia de variables — Mean Decrease Gini (Clasificación)",
    x     = NULL,
    y     = "MeanDecreaseGini"
  ) +
  theme_minimal(base_size = 13)
print(p_imp_class)

varImpPlot(
  rf_class,
  n.var = min(10, nrow(imp_class)),
  main  = "Importancia de variables — Random Forest Clasificación"
)

# --- Importancia por permutación (ranger) ---
cat("\n--- IMPORTANCIA POR PERMUTACIÓN (ranger) ---\n")

set.seed(1994)

rf_ranger_class <- ranger(
  formula_class,
  data        = train_df,
  num.trees   = 500,
  importance  = "permutation",
  probability = TRUE,
  seed        = 1994
)

vip(rf_ranger_class, num_features = length(pred_class)) +
  labs(title = "Importancia por permutación — Clasificación (ranger)") +
  theme_minimal(base_size = 13) |> print()


# =========================================================================
# 7. EXTRACCIÓN DE UN ÁRBOL — CLASIFICACIÓN
# =========================================================================
cat("\n======================================================\n")
cat("      EXTRACCIÓN DE ÁRBOL — CLASIFICACIÓN\n")
cat("======================================================\n")

arbol_1_class <- getTree(rf_class, k = 1, labelVar = TRUE)
cat("\nPrimeras filas del árbol nº 1:\n")
print(head(arbol_1_class, 20))

set.seed(1994)
arbol_aux_class <- rpart(
  formula_class,
  data    = train_df,
  method  = "class",
  control = rpart.control(maxdepth = 3, minbucket = 100, cp = 0.001)
)

rpart.plot(
  arbol_aux_class,
  main          = "Árbol auxiliar para interpretación — Clasificación",
  type          = 3,
  extra         = 104,
  fallen.leaves = TRUE,
  shadow.col    = "gray"
)


# =========================================================================
# 8. PARTIAL DEPENDENCE PLOTS — CLASIFICACIÓN
# =========================================================================
cat("\n======================================================\n")
cat("      PARTIAL DEPENDENCE PLOTS — CLASIFICACIÓN\n")
cat("======================================================\n")

partial_td <- partial(
  rf_class,
  pred.var    = "travelDistance",
  train       = train_df,
  which.class = "Economy",
  prob        = TRUE
)

autoplot(partial_td) +
  labs(
    title    = "Partial Dependence Plot — Clasificación",
    subtitle = "Efecto medio de travelDistance sobre P(Economy)",
    x        = "Distancia del vuelo",
    y        = "Probabilidad media predicha"
  ) +
  theme_minimal(base_size = 13) |> print()

partial_ed <- partial(
  rf_class,
  pred.var    = "elapsedDays",
  train       = train_df,
  which.class = "Economy",
  prob        = TRUE
)

autoplot(partial_ed) +
  labs(
    title    = "Partial Dependence Plot — Clasificación",
    subtitle = "Efecto medio de elapsedDays sobre P(Economy)",
    x        = "Días hasta el vuelo",
    y        = "Probabilidad media predicha"
  ) +
  theme_minimal(base_size = 13) |> print()


# =========================================================================
# 9. SHAP VALUES — CLASIFICACIÓN
# =========================================================================
cat("\n======================================================\n")
cat("      SHAP VALUES — CLASIFICACIÓN\n")
cat("======================================================\n")

future::plan(future::sequential)
options(future.globals.maxSize = 2 * 1024^3)

set.seed(1994)

X_train_class <- train_df %>% select(all_of(pred_class))
y_train_class <- train_df$economy_f

idx_shap <- sample(seq_len(nrow(X_train_class)), size = min(300, nrow(X_train_class)))
X_shap   <- X_train_class[idx_shap, , drop = FALSE]
y_shap   <- y_train_class[idx_shap]

predict_fun_economy <- function(model, newdata) {
  predict(model, newdata, type = "prob")[, "Economy"]
}

predictor_economy <- Predictor$new(
  model            = rf_class,
  data             = X_shap,
  y                = as.numeric(y_shap == "Economy"),
  predict.function = predict_fun_economy,
  type             = "prob"
)

# SHAP global
set.seed(1994)
effect_global_class <- FeatureImp$new(
  predictor_economy,
  loss          = "ce",
  n.repetitions = 3
)

plot(effect_global_class) +
  labs(title = "Importancia global por permutación — Clase Economy") |> print()

# SHAP local (primera observación del test)
set.seed(1994)
shap_obs_class <- Shapley$new(
  predictor_economy,
  x.interest  = test_df[1, pred_class, drop = FALSE],
  sample.size = 100
)

plot(shap_obs_class) +
  labs(title = "SHAP local para una observación — Clase Economy") |> print()


# =========================================================================
# 10. RANDOM FOREST — REGRESIÓN (log_price)
# =========================================================================
cat("\n======================================================\n")
cat("      RANDOM FOREST: REGRESIÓN (log_price)\n")
cat("======================================================\n")

pred_reg    <- predictores[predictores != "log_price"]
formula_reg <- as.formula(paste("log_price ~", paste(pred_reg, collapse = " + ")))
cat("Fórmula regresión:\n"); print(formula_reg)


# ------------------------------------------------------------------
# 10A. Entrenamiento básico
# ------------------------------------------------------------------
set.seed(1994)

rf_reg <- randomForest(
  formula_reg,
  data       = train_df,
  ntree      = 500,
  importance = TRUE
)

cat("\n--- MODELO BÁSICO ---\n")
print(rf_reg)


# ------------------------------------------------------------------
# 10B. Predicción y métricas
# ------------------------------------------------------------------
pred_test_reg <- predict(rf_reg, test_df)

rmse   <- sqrt(mean((test_df$log_price - pred_test_reg)^2))
mae    <- mean(abs(test_df$log_price - pred_test_reg))
ss_res <- sum((test_df$log_price - pred_test_reg)^2)
ss_tot <- sum((test_df$log_price - mean(test_df$log_price))^2)
r2     <- 1 - (ss_res / ss_tot)

cat("\n--- MÉTRICAS DE REGRESIÓN (TEST) ---\n")
cat(sprintf("  RMSE : %.4f\n", rmse))
cat(sprintf("  MAE  : %.4f\n", mae))
cat(sprintf("  R²   : %.4f\n", r2))

# Plot predicho vs real
df_reg_eval <- data.frame(
  real     = test_df$log_price,
  predicho = pred_test_reg
)

p_reg <- ggplot(df_reg_eval, aes(x = real, y = predicho)) +
  geom_point(alpha = 0.3, color = "#1a7a4a", size = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title    = "Random Forest Regresión: Predicho vs Real",
    subtitle = sprintf("R² = %.4f  |  RMSE = %.4f", r2, rmse),
    x        = "log(Precio Real)",
    y        = "log(Precio Predicho)"
  ) +
  theme_minimal(base_size = 13)
print(p_reg)


# =========================================================================
# 11. VALIDACIÓN CRUZADA — REGRESIÓN
# =========================================================================
cat("\n======================================================\n")
cat("      VALIDACIÓN CRUZADA — REGRESIÓN\n")
cat("======================================================\n")

trControl_reg <- trainControl(
  method = "cv",
  number = 10
)

set.seed(1994)
rf_cv_reg <- train(
  formula_reg,
  data      = train_df,
  method    = "rf",
  metric    = "RMSE",
  trControl = trControl_reg,
  ntree     = 300
)

cat("\n--- RESULTADOS CV ---\n")
print(rf_cv_reg)

ggplot(rf_cv_reg) +
  labs(title = "Validación cruzada — Random Forest Regresión") +
  theme_minimal(base_size = 13) |> print()


# =========================================================================
# 12. AJUSTE DE HIPERPARÁMETROS — REGRESIÓN
# =========================================================================
cat("\n======================================================\n")
cat("      TUNING HIPERPARÁMETROS — REGRESIÓN\n")
cat("======================================================\n")

set.seed(1994)

rf_grid_reg <- expand.grid(mtry = c(2, 3, 4, 5))

rf_tuned_reg <- train(
  formula_reg,
  data      = train_df,
  method    = "rf",
  metric    = "RMSE",
  trControl = trControl_reg,
  tuneGrid  = rf_grid_reg,
  ntree     = 500
)

cat("\n--- MEJOR HIPERPARÁMETRO (mtry) ---\n")
print(rf_tuned_reg$bestTune)
print(rf_tuned_reg)

ggplot(rf_tuned_reg) +
  labs(title = "Tuning del parámetro mtry — Regresión") +
  theme_minimal(base_size = 13) |> print()

# Métricas del modelo tuneado
pred_tuned_reg <- predict(rf_tuned_reg, test_df)
rmse_t <- sqrt(mean((test_df$log_price - pred_tuned_reg)^2))
r2_t   <- 1 - sum((test_df$log_price - pred_tuned_reg)^2) /
  sum((test_df$log_price - mean(test_df$log_price))^2)

cat(sprintf("\n  RMSE (tuneado) : %.4f\n", rmse_t))
cat(sprintf("  R²   (tuneado) : %.4f\n", r2_t))


# =========================================================================
# 13. IMPORTANCIA DE VARIABLES — REGRESIÓN
# =========================================================================
cat("\n======================================================\n")
cat("      IMPORTANCIA DE VARIABLES — REGRESIÓN\n")
cat("======================================================\n")

imp_reg <- as.data.frame(rf_reg$importance)
imp_reg$Variable <- rownames(imp_reg)

imp_reg_top <- imp_reg %>%
  arrange(desc(IncNodePurity)) %>%
  mutate(Variable = reorder(Variable, IncNodePurity))

cat("\n--- TOP VARIABLES (IncNodePurity) ---\n")
print(head(imp_reg[order(-imp_reg$IncNodePurity), ], 10))

p_imp_reg <- ggplot(imp_reg_top, aes(x = Variable, y = IncNodePurity, fill = IncNodePurity)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "#b8e0c8", high = "#1a7a4a") +
  coord_flip() +
  labs(
    title = "Importancia de variables — IncNodePurity (Regresión)",
    x     = NULL,
    y     = "IncNodePurity"
  ) +
  theme_minimal(base_size = 13)
print(p_imp_reg)

varImpPlot(
  rf_reg,
  n.var = min(10, nrow(imp_reg)),
  main  = "Importancia de variables — Random Forest Regresión"
)

# --- Importancia por permutación (ranger) ---
cat("\n--- IMPORTANCIA POR PERMUTACIÓN (ranger) ---\n")

set.seed(1994)

rf_ranger_reg <- ranger(
  formula_reg,
  data       = train_df,
  num.trees  = 500,
  importance = "permutation",
  seed       = 1994
)

vip(rf_ranger_reg, num_features = length(pred_reg)) +
  labs(title = "Importancia por permutación — Regresión (ranger)") +
  theme_minimal(base_size = 13) |> print()


# =========================================================================
# 14. EXTRACCIÓN DE UN ÁRBOL — REGRESIÓN
# =========================================================================
cat("\n======================================================\n")
cat("      EXTRACCIÓN DE ÁRBOL — REGRESIÓN\n")
cat("======================================================\n")

set.seed(1994)
arbol_aux_reg <- rpart(
  formula_reg,
  data    = train_df,
  method  = "anova",
  control = rpart.control(maxdepth = 3, minbucket = 100, cp = 0.001)
)

rpart.plot(
  arbol_aux_reg,
  main          = "Árbol auxiliar para interpretación — Regresión",
  type          = 3,
  fallen.leaves = TRUE,
  shadow.col    = "gray"
)


# =========================================================================
# 15. PARTIAL DEPENDENCE PLOTS — REGRESIÓN
# =========================================================================
cat("\n======================================================\n")
cat("      PARTIAL DEPENDENCE PLOTS — REGRESIÓN\n")
cat("======================================================\n")

partial_td_reg <- partial(rf_reg, pred.var = "travelDistance", train = train_df)

autoplot(partial_td_reg) +
  labs(
    title    = "Partial Dependence Plot — Regresión",
    subtitle = "Efecto medio de travelDistance sobre log(Precio)",
    x        = "Distancia del vuelo",
    y        = "log(Precio) medio predicho"
  ) +
  theme_minimal(base_size = 13) |> print()

partial_ed_reg <- partial(rf_reg, pred.var = "elapsedDays", train = train_df)

autoplot(partial_ed_reg) +
  labs(
    title    = "Partial Dependence Plot — Regresión",
    subtitle = "Efecto medio de elapsedDays sobre log(Precio)",
    x        = "Días hasta el vuelo",
    y        = "log(Precio) medio predicho"
  ) +
  theme_minimal(base_size = 13) |> print()


# =========================================================================
# 16. SHAP VALUES — REGRESIÓN
# =========================================================================
cat("\n======================================================\n")
cat("      SHAP VALUES — REGRESIÓN\n")
cat("======================================================\n")

set.seed(1994)

X_train_reg  <- train_df %>% select(all_of(pred_reg))
y_train_reg  <- train_df$log_price

idx_shap_reg <- sample(seq_len(nrow(X_train_reg)), size = min(300, nrow(X_train_reg)))
X_shap_reg   <- X_train_reg[idx_shap_reg, , drop = FALSE]
y_shap_reg   <- y_train_reg[idx_shap_reg]

predictor_reg <- Predictor$new(
  model = rf_reg,
  data  = X_shap_reg,
  y     = y_shap_reg
)

# SHAP global
set.seed(1994)
effect_global_reg <- FeatureImp$new(
  predictor_reg,
  loss          = "mse",
  n.repetitions = 3
)

plot(effect_global_reg) +
  labs(title = "Importancia global por permutación — Regresión (MSE)") |> print()

# SHAP local (primera observación del test)
set.seed(1994)
shap_obs_reg <- Shapley$new(
  predictor_reg,
  x.interest  = test_df[1, pred_reg, drop = FALSE],
  sample.size = 100
)

plot(shap_obs_reg) +
  labs(title = "SHAP local para una observación — Regresión") |> print()


# =========================================================================
cat("\n======================================================\n")
cat("      Random Forest COMPLETADO\n")
cat("======================================================\n")