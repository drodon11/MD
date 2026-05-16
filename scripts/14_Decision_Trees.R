# =========================================================================
# DECISION TREES (CART): Clasificación y Regresión — Flight Prices
# =========================================================================

# --- 0. SETUP Y CARGA DE DATOS ---
rm(list = ls())

list.of.packages <- c("rpart", "rpart.plot", "caret", "ggplot2", "dplyr", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages) > 0) install.packages(new.packages, dependencies = TRUE)
invisible(lapply(list.of.packages, require, character.only = TRUE))

load("data/interim/model_data.RData")


# =========================================================================
# 1. ÁRBOL DE CLASIFICACIÓN
#    Target: economy_f (Economy vs Premium)
# =========================================================================
cat("\n======================================================\n")
cat("      DECISION TREE: CLASIFICACIÓN\n")
cat("======================================================\n")

# Eliminamos log_price de los predictores para clasificación
# (es una transformación del target de regresión, no un predictor real)
pred_class <- predictores[predictores != "log_price"]
formula_class <- as.formula(paste("economy_f ~", paste(pred_class, collapse = " + ")))


# ------------------------------------------------------------------
# 1A. Árbol base (sin tuning) — equivalente al arbol del profe
# ------------------------------------------------------------------
set.seed(1994)
arbol <- rpart(formula_class, data = train_df, method = "class")

cat("\n--- RESUMEN DEL ÁRBOL BASE ---\n")
summary(arbol)

# Plot del árbol
rpart.plot(arbol,
           main = "Árbol de Clasificación — Flight Prices",
           type = 3, extra = 104, fallen.leaves = TRUE, shadow.col = "gray"
)


# ------------------------------------------------------------------
# 1B. Predicciones y Confusion Matrix (train y test)
# ------------------------------------------------------------------
cat("\n--- CONFUSION MATRIX (TRAIN) ---\n")
pred_train <- predict(arbol, train_df, type = "class")
cm_train <- caret::confusionMatrix(pred_train, as.factor(train_df$economy_f))
print(cm_train)

cat("\n--- CONFUSION MATRIX (TEST) ---\n")
pred_test <- predict(arbol, test_df, type = "class")
cm_test <- caret::confusionMatrix(pred_test, as.factor(test_df$economy_f))
print(cm_test)


# ------------------------------------------------------------------
# 1C. Plot de la Confusion Matrix (estilo profesor)
# ------------------------------------------------------------------
CM_df <- data.frame(cm_test$table)

p_cm <- ggplot(CM_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "#009194") +
  labs(
    title = "Confusion Matrix — Test Set",
    x = "Predicción", y = "Clase Real"
  ) +
  theme_minimal(base_size = 13)
print(p_cm)


# ------------------------------------------------------------------
# 1D. Indicadores: Accuracy, Recall, F1
# ------------------------------------------------------------------
cat("\n--- INDICADORES DE CLASIFICACIÓN (TEST) ---\n")

accuracy  <- cm_test$overall["Accuracy"]
recall    <- cm_test$byClass["Recall"]       # Sensibilidad / True Positive Rate
precision <- cm_test$byClass["Precision"]
f1        <- cm_test$byClass["F1"]
specificity <- cm_test$byClass["Specificity"]

cat(sprintf("  Accuracy  : %.4f\n", accuracy))
cat(sprintf("  Recall    : %.4f\n", recall))
cat(sprintf("  Precision : %.4f\n", precision))
cat(sprintf("  F1 Score  : %.4f\n", f1))
cat(sprintf("  Specificity: %.4f\n", specificity))


# ------------------------------------------------------------------
# 1E. Importancia de variables
# ------------------------------------------------------------------
cat("\n--- IMPORTANCIA DE VARIABLES ---\n")
imp <- data.frame(
  Variable   = names(arbol$variable.importance),
  Importancia = arbol$variable.importance
) %>% arrange(desc(Importancia))

print(imp)

p_imp <- ggplot(imp, aes(x = reorder(Variable, Importancia), y = Importancia, fill = Importancia)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "#A8C8F9", high = "#1a5fa8") +
  coord_flip() +
  labs(
    title = "Importancia de Variables — Árbol de Clasificación",
    x = NULL, y = "Importancia"
  ) +
  theme_minimal(base_size = 13)
print(p_imp)


# =========================================================================
# 2. CLASIFICACIÓN CON CROSS-VALIDATION Y TUNING (método del profesor)
# =========================================================================
cat("\n======================================================\n")
cat("      CLASIFICACIÓN CON CV Y TUNING\n")
cat("======================================================\n")

# trControl con multiClassSummary (como el profesor) pero adaptado a binario
trControl <- trainControl(
  method = "cv", number = 10,
  classProbs = TRUE,
  summaryFunction = multiClassSummary
)

tuneGrid <- expand.grid(cp = seq(0.01, 0.05, 0.01))

set.seed(1994)
model_cv <- train(
  formula_class,
  data      = train_df,
  method    = "rpart",
  metric    = "Accuracy",
  trControl = trControl,
  tuneGrid  = tuneGrid
)

cat("\n--- MEJOR HIPERPARÁMETRO (CP) ---\n")
print(model_cv$bestTune)

cat("\n--- MODELO FINAL ---\n")
print(model_cv$finalModel)

# Árbol del modelo con CV
rpart.plot(model_cv$finalModel,
           main = "Árbol Tuneado con CV (mejor CP)",
           type = 3, extra = 104, fallen.leaves = TRUE, shadow.col = "gray"
)

# Gráfico de variabilidad de los K-Folds (igual que el profesor)
cols_metricas <- intersect(
  c("Accuracy", "Kappa", "Recall", "Precision", "F1"),
  names(model_cv$resample)
)

p_vars <- ggplot(
  melt(model_cv$resample[, cols_metricas]),
  aes(x = variable, y = value, fill = variable)
) +
  geom_boxplot(show.legend = FALSE, alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Variabilidad en Cross-Validation (10-Folds)",
    x = "Métrica", y = "Valor"
  ) +
  theme_minimal(base_size = 13)
print(p_vars)

# Evaluación final con el modelo tuneado
cat("\n--- CONFUSION MATRIX MODELO TUNEADO (TEST) ---\n")
pred_cv_test <- predict(model_cv, test_df)
cm_cv <- caret::confusionMatrix(pred_cv_test, as.factor(test_df$economy_f))
print(cm_cv)

cat("\n--- INDICADORES MODELO TUNEADO (TEST) ---\n")
cat(sprintf("  Accuracy  : %.4f\n", cm_cv$overall["Accuracy"]))
cat(sprintf("  Recall    : %.4f\n", cm_cv$byClass["Recall"]))
cat(sprintf("  Precision : %.4f\n", cm_cv$byClass["Precision"]))
cat(sprintf("  F1 Score  : %.4f\n", cm_cv$byClass["F1"]))


# =========================================================================
# 3. PODA DEL ÁRBOL (PRUNING CON MINBUCKET)
# =========================================================================
cat("\n--- PODA: ÁRBOL CON MINBUCKET = 100 ---\n")

set.seed(1994)
prunedtree <- rpart(formula_class, data = train_df,
                    cp = 0.01, control = rpart.control(minbucket = 100))

rpart.plot(prunedtree,
           main = "Árbol Podado (minbucket = 100)",
           type = 3, extra = 104
)


# =========================================================================
# 4. DECISION BOUNDARY — MAPA 2D EN ESPACIO PCA
#    Proyección sobre PC1 y PC2 con fronteras de clasificación
# =========================================================================
cat("\n--- DECISION BOUNDARY (ESPACIO PCA) ---\n")

vars_num <- c("travelDistance", "layoverNumber", "elapsedDays", "seatsLeft")
# log_price incluido solo si está disponible en test_df como predictor
if ("log_price" %in% names(train_df)) vars_num <- c("log_price", vars_num)

# PCA sobre variables numéricas del train
pca_res <- prcomp(train_df[, vars_num], scale. = TRUE)

# Varianza explicada por PC1 y PC2
var_exp <- round(summary(pca_res)$importance[2, 1:2] * 100, 1)
cat(sprintf("  Varianza explicada — PC1: %.1f%%  PC2: %.1f%%\n", var_exp[1], var_exp[2]))

# Proyectar train al espacio PCA
df_pca_train <- data.frame(
  PC1       = pca_res$x[, 1],
  PC2       = pca_res$x[, 2],
  economy_f = train_df$economy_f
)

# Árbol entrenado en el espacio PCA
dt_pca <- rpart(economy_f ~ PC1 + PC2, data = df_pca_train,
                method = "class", control = rpart.control(cp = 0.005))

# Grid de predicción (malla 2D)
grid_pc1  <- seq(min(df_pca_train$PC1), max(df_pca_train$PC1), length.out = 200)
grid_pc2  <- seq(min(df_pca_train$PC2), max(df_pca_train$PC2), length.out = 200)
mesh_pca  <- expand.grid(PC1 = grid_pc1, PC2 = grid_pc2)
mesh_pca$pred_class <- predict(dt_pca, newdata = mesh_pca, type = "class")

# Plot
p_boundary <- ggplot() +
  geom_tile(data = mesh_pca,
            aes(x = PC1, y = PC2, fill = pred_class), alpha = 0.35) +
  geom_point(data = df_pca_train,
             aes(x = PC1, y = PC2, color = economy_f),
             size = 0.8, alpha = 0.6) +
  scale_fill_manual(
    values = c("Economy" = "#A8C8F9", "Premium" = "#F9A8A8"),
    name = "Frontera"
  ) +
  scale_color_manual(
    values = c("Economy" = "#1a5fa8", "Premium" = "#c0392b"),
    name = "Clase real"
  ) +
  labs(
    title = "Decision Boundary en Espacio PCA",
    subtitle = sprintf("PC1 (%.1f%% var) vs PC2 (%.1f%% var)", var_exp[1], var_exp[2]),
    x = "PC1", y = "PC2"
  ) +
  theme_minimal(base_size = 13) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.6)))
print(p_boundary)


# =========================================================================
# 5. ÁRBOL DE REGRESIÓN
#    Target: log_price
# =========================================================================
cat("\n======================================================\n")
cat("      DECISION TREE: REGRESIÓN (log_price)\n")
cat("======================================================\n")

pred_reg    <- predictores[predictores != "log_price"]
formula_reg <- as.formula(paste("log_price ~", paste(pred_reg, collapse = " + ")))

set.seed(1994)
dt_reg <- rpart(formula_reg, data = train_df,
                method  = "anova",
                control = rpart.control(cp = 0.005, maxdepth = 5))

rpart.plot(dt_reg,
           main = "Árbol de Regresión — log(Precio Vuelo)",
           type = 3, fallen.leaves = TRUE, shadow.col = "gray"
)

# Importancia de variables en regresión
cat("\n--- IMPORTANCIA DE VARIABLES (REGRESIÓN) ---\n")
imp_reg <- data.frame(
  Variable    = names(dt_reg$variable.importance),
  Importancia = dt_reg$variable.importance
) %>% arrange(desc(Importancia))
print(imp_reg)

p_imp_reg <- ggplot(imp_reg, aes(x = reorder(Variable, Importancia), y = Importancia, fill = Importancia)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "#b8e0c8", high = "#1a7a4a") +
  coord_flip() +
  labs(
    title = "Importancia de Variables — Árbol de Regresión",
    x = NULL, y = "Importancia"
  ) +
  theme_minimal(base_size = 13)
print(p_imp_reg)

# Predicción y métricas en test
pred_reg_test <- predict(dt_reg, test_df)

rmse <- sqrt(mean((test_df$log_price - pred_reg_test)^2))
ss_res <- sum((test_df$log_price - pred_reg_test)^2)
ss_tot <- sum((test_df$log_price - mean(test_df$log_price))^2)
r2     <- 1 - (ss_res / ss_tot)
mae    <- mean(abs(test_df$log_price - pred_reg_test))

cat("\n--- MÉTRICAS DE REGRESIÓN (TEST) ---\n")
cat(sprintf("  RMSE     : %.4f\n", rmse))
cat(sprintf("  MAE      : %.4f\n", mae))
cat(sprintf("  R²       : %.4f\n", r2))

# Plot predicho vs real
df_reg_eval <- data.frame(
  real     = test_df$log_price,
  predicho = pred_reg_test
)

p_reg <- ggplot(df_reg_eval, aes(x = real, y = predicho)) +
  geom_point(alpha = 0.3, color = "#1a7a4a", size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Árbol de Regresión: Predicho vs Real",
    subtitle = sprintf("R² = %.4f  |  RMSE = %.4f", r2, rmse),
    x = "log(Precio Real)", y = "log(Precio Predicho)"
  ) +
  theme_minimal(base_size = 13)
print(p_reg)

cat("\n======================================================\n")
cat("      Decision Tree COMPLETADO\n")
cat("======================================================\n")
