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

# Eliminar niveles vacíos del factor target para evitar errores en rpart
train_df$economy_f <- droplevels(train_df$economy_f)
test_df$economy_f  <- droplevels(test_df$economy_f)


# =========================================================================
# 1. ÁRBOL DE CLASIFICACIÓN — Target: economy_f (Economy vs Premium)
# =========================================================================
cat("\n======================================================\n")
cat("      DECISION TREE: CLASIFICACIÓN\n")
cat("======================================================\n")

# taxAmount excluida por data leakage; log_price excluida por ser target de regresión
pred_class    <- predictores[!predictores %in% c("log_price", "taxAmount")]
formula_class <- as.formula(paste("economy_f ~ totalPrice +", paste(pred_base, collapse = " + ")))

# --- Árbol base ---
set.seed(1994)
tree <- rpart(formula_class, data = train_df, method = "class", 
               control = rpart.control(cp = 0.001, minsplit = 20))

cat("\n--- RESUMEN DEL ÁRBOL BASE ---\n")
summary(tree)

rpart.plot(tree,
           main = "Árbol de Clasificación — Flight Prices",
           type = 3, extra = 104, fallen.leaves = TRUE, shadow.col = "gray"
)


# --- Confusion Matrix: Train y Test ---
cat("\n--- CONFUSION MATRIX (TRAIN) ---\n")
pred_train <- predict(tree, train_df, type = "class")
cm_train   <- caret::confusionMatrix(pred_train, train_df$economy_f)
print(cm_train)

cat("\n--- CONFUSION MATRIX (TEST) ---\n")
pred_test <- predict(tree, test_df, type = "class")
cm_test   <- caret::confusionMatrix(pred_test, test_df$economy_f)
print(cm_test)


# --- Plot Confusion Matrix (heatmap) ---
CM_df <- data.frame(cm_test$table)

p_cm <- ggplot(CM_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "#009194") +
  labs(title = "Confusion Matrix — Test Set", x = "Predicción", y = "Clase Real") +
  theme_minimal(base_size = 13)
print(p_cm)


# --- Indicadores: Accuracy, Recall, Precision, F1, Specificity ---
cat("\n--- INDICADORES DE CLASIFICACIÓN (TEST) ---\n")
cat(sprintf("  Accuracy   : %.4f\n", cm_test$overall["Accuracy"]))
cat(sprintf("  Recall     : %.4f\n", cm_test$byClass["Recall"]))
cat(sprintf("  Precision  : %.4f\n", cm_test$byClass["Precision"]))
cat(sprintf("  F1 Score   : %.4f\n", cm_test$byClass["F1"]))
cat(sprintf("  Specificity: %.4f\n", cm_test$byClass["Specificity"]))


# --- Importancia de Variables ---
cat("\n--- IMPORTANCIA DE VARIABLES ---\n")
imp <- data.frame(
  Variable    = names(tree$variable.importance),
  Importancia = tree$variable.importance
) %>% arrange(desc(Importancia))
print(imp)

p_imp <- ggplot(imp, aes(x = reorder(Variable, Importancia), y = Importancia, fill = Importancia)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "#A8C8F9", high = "#1a5fa8") +
  coord_flip() +
  labs(title = "Importancia de Variables — Árbol de Clasificación", x = NULL, y = "Importancia") +
  theme_minimal(base_size = 13)
print(p_imp)


# =========================================================================
# 2. CLASIFICACIÓN CON CROSS-VALIDATION (10-Fold) Y TUNING (Grid Search cp)
# =========================================================================
cat("\n======================================================\n")
cat("      CLASIFICACIÓN CON CV Y TUNING\n")
cat("======================================================\n")

trControl <- trainControl(
  method          = "cv",
  number          = 10,
  classProbs      = TRUE,
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

# --- Árbol del modelo tuneado ---
rpart.plot(model_cv$finalModel,
           main = "Árbol Tuneado con CV (mejor CP)",
           type = 3, extra = 104, fallen.leaves = TRUE, shadow.col = "gray"
)

# --- Variabilidad de métricas por fold (boxplot) ---
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
  labs(title = "Variabilidad en Cross-Validation (10-Folds)", x = "Métrica", y = "Valor") +
  theme_minimal(base_size = 13)
print(p_vars)

# --- Confusion Matrix y Indicadores del modelo tuneado ---
cat("\n--- CONFUSION MATRIX MODELO TUNEADO (TEST) ---\n")
pred_cv_test <- predict(model_cv, test_df)
cm_cv        <- caret::confusionMatrix(pred_cv_test, test_df$economy_f)
print(cm_cv)

cat("\n--- INDICADORES MODELO TUNEADO (TEST) ---\n")
cat(sprintf("  Accuracy  : %.4f\n", cm_cv$overall["Accuracy"]))
cat(sprintf("  Recall    : %.4f\n", cm_cv$byClass["Recall"]))
cat(sprintf("  Precision : %.4f\n", cm_cv$byClass["Precision"]))
cat(sprintf("  F1 Score  : %.4f\n", cm_cv$byClass["F1"]))


# =========================================================================
# 3. PODA DEL ÁRBOL — Pruning con minbucket
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
# 4. DECISION BOUNDARY — Proyección 2D sobre PC1 y PC2
#    Fondo: fronteras de clasificación aprendidas por el árbol
#    Puntos: clase real de cada observación (aciertos y errores visibles)
# =========================================================================
cat("\n--- DECISION BOUNDARY (ESPACIO PCA) ---\n")

vars_num <- c("travelDistance", "layoverNumber", "elapsedDays", "seatsLeft")
if ("log_price" %in% names(train_df)) vars_num <- c("log_price", vars_num)

pca_res <- prcomp(train_df[, vars_num], scale. = TRUE)

var_exp <- round(summary(pca_res)$importance[2, 1:2] * 100, 1)
cat(sprintf("  Varianza explicada — PC1: %.1f%%  PC2: %.1f%%\n", var_exp[1], var_exp[2]))

df_pca_train <- data.frame(
  PC1       = pca_res$x[, 1],
  PC2       = pca_res$x[, 2],
  economy_f = train_df$economy_f
)

dt_pca <- rpart(economy_f ~ PC1 + PC2, data = df_pca_train,
                method = "class", control = rpart.control(cp = 0.005))

grid_pc1            <- seq(min(df_pca_train$PC1), max(df_pca_train$PC1), length.out = 200)
grid_pc2            <- seq(min(df_pca_train$PC2), max(df_pca_train$PC2), length.out = 200)
mesh_pca            <- expand.grid(PC1 = grid_pc1, PC2 = grid_pc2)
mesh_pca$pred_class <- predict(dt_pca, newdata = mesh_pca, type = "class")

p_boundary <- ggplot() +
  geom_tile(data  = mesh_pca,     aes(x = PC1, y = PC2, fill  = pred_class), alpha = 0.35) +
  geom_point(data = df_pca_train, aes(x = PC1, y = PC2, color = economy_f),  size = 0.8, alpha = 0.6) +
  scale_fill_manual(values  = c("Economy" = "#A8C8F9", "Premium" = "#F9A8A8"), name = "Frontera") +
  scale_color_manual(values = c("Economy" = "#1a5fa8", "Premium" = "#c0392b"), name = "Clase real") +
  labs(
    title    = "Decision Boundary en Espacio PCA",
    subtitle = sprintf("PC1 (%.1f%% var) vs PC2 (%.1f%% var)", var_exp[1], var_exp[2]),
    x = "PC1", y = "PC2"
  ) +
  theme_minimal(base_size = 13) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.6)))
print(p_boundary)


# =========================================================================
# 5. ÁRBOL DE REGRESIÓN — Target: log_price
# =========================================================================
cat("\n======================================================\n")
cat("      DECISION TREE: REGRESIÓN (log_price)\n")
cat("======================================================\n")

pred_reg    <- predictores[!predictores %in% c("log_price", "taxAmount")]
formula_reg <- as.formula(paste("log_price ~", paste(pred_reg, collapse = " + ")))

set.seed(1994)
dt_reg <- rpart(formula_reg, data = train_df,
                method  = "anova",
                control = rpart.control(cp = 0.005, maxdepth = 5))

rpart.plot(dt_reg,
           main = "Árbol de Regresión — log(Precio Vuelo)",
           type = 3, fallen.leaves = TRUE, shadow.col = "gray"
)


# --- Importancia de Variables ---
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
  labs(title = "Importancia de Variables — Árbol de Regresión", x = NULL, y = "Importancia") +
  theme_minimal(base_size = 13)
print(p_imp_reg)


# --- Métricas: RMSE, MAE, R² ---
pred_reg_test <- predict(dt_reg, test_df)

rmse   <- sqrt(mean((test_df$log_price - pred_reg_test)^2))
mae    <- mean(abs(test_df$log_price - pred_reg_test))
ss_res <- sum((test_df$log_price - pred_reg_test)^2)
ss_tot <- sum((test_df$log_price - mean(test_df$log_price))^2)
r2     <- 1 - (ss_res / ss_tot)

cat("\n--- MÉTRICAS DE REGRESIÓN (TEST) ---\n")
cat(sprintf("  RMSE : %.4f\n", rmse))
cat(sprintf("  MAE  : %.4f\n", mae))
cat(sprintf("  R²   : %.4f\n", r2))


# --- Plot: Predicho vs Real ---
p_reg <- ggplot(
  data.frame(real = test_df$log_price, predicho = pred_reg_test),
  aes(x = real, y = predicho)
) +
  geom_point(alpha = 0.3, color = "#1a7a4a", size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title    = "Árbol de Regresión: Predicho vs Real",
    subtitle = sprintf("R² = %.4f  |  RMSE = %.4f", r2, rmse),
    x = "log(Precio Real)", y = "log(Precio Predicho)"
  ) +
  theme_minimal(base_size = 13)
print(p_reg)

cat("\n======================================================\n")
cat("      Decision Tree COMPLETADO\n")
cat("======================================================\n")
