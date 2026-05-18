# -------------------------------------------------------------------------
# 16. XGBOOST: Clasificación y Regresión — Flight Prices
# -------------------------------------------------------------------------

rm(list=ls())

# 0. Carga de librerías (Añadidos DiagrammeR, pdp y tidyr por requerimiento)
list.of.packages <- c("xgboost", "caret", "ggplot2", "dplyr", "tidyr", "pROC", "Matrix", "DiagrammeR", "pdp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages, dependencies = TRUE)
invisible(lapply(list.of.packages, require, character.only = TRUE))

load("data/interim/model_data.RData")


# =========================================================================
# 1. PREPARACIÓN DE DATOS (Data Leakage Prevention)
# =========================================================================
# Excluimos log_price (Target de regresión) y taxAmount de los predictores
cols_class <- grep("log_price|taxAmount", colnames(X_train_xgb), invert = TRUE, value = TRUE)

x_train_class <- X_train_xgb[, cols_class]
x_test_class  <- X_test_xgb[, cols_class]

# Matrices DMatrix optimizadas
dtrain <- xgb.DMatrix(data = x_train_class, label = y_train_xgb)
dtest  <- xgb.DMatrix(data = x_test_class, label = y_test_xgb)


# =========================================================================
# 2. XGBOOST CLASIFICACIÓN: EARLY STOPPING & LEARNING CURVE
# =========================================================================
cat("\n======================================================\n")
cat("      XGBOOST CLASIFICACIÓN (EARLY STOPPING)\n")
cat("======================================================\n")

set.seed(42)
params_class <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = 0.05,
  max_depth = 4,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Entrenamiento con Watchlist para Early Stopping
xgb_es <- xgb.train(
  params = params_class,
  data = dtrain,
  nrounds = 500,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 30, # Frena si el AUC en test no mejora en 30 rondas
  verbose = 0
)

cat("Mejor iteración óptima:", xgb_es$best_iteration, "\n")
cat("Mejor AUC en Test:", xgb_es$best_score, "\n")

# Curva de aprendizaje
eval_log <- xgb_es$evaluation_log
p_learning <- ggplot(eval_log, aes(x = iter)) +
  geom_line(aes(y = train_auc, color = "Train")) +
  geom_line(aes(y = test_auc, color = "Test")) +
  labs(title = "Curva de Aprendizaje (Early Stopping)", x = "Nº de Árboles", y = "AUC") +
  theme_minimal()
print(p_learning)


# =========================================================================
# 3. XGBOOST CLASIFICACIÓN: HYPERPARAMETER TUNING (CARET)
# =========================================================================
cat("\n--- TUNING CON CARET ---\n")

control <- trainControl(
  method = "cv", number = 3, # Reducido a 3-Folds para agilizar la ejecución
  classProbs = TRUE, summaryFunction = twoClassSummary
)

grid_xgb <- expand.grid(
  nrounds = c(50, 100),
  max_depth = c(3, 5),
  eta = c(0.05, 0.1),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

# Preparamos dataframe de training con el Target como factor válido para caret
train_xgb_df <- as.data.frame(x_train_class)
train_xgb_df$Target <- factor(ifelse(y_train_xgb == 1, "Economy", "Premium"))

set.seed(42)
xgb_caret <- train(
  Target ~ ., data = train_xgb_df,
  method = "xgbTree", metric = "ROC",
  trControl = control, tuneGrid = grid_xgb
)

print(xgb_caret$bestTune)


# =========================================================================
# 4. EVALUACIÓN, IMPORTANCIA Y EXTRACCIÓN TOPOLÓGICA
# =========================================================================
pred_prob <- predict(xgb_es, dtest)
pred_class <- factor(ifelse(pred_prob >= 0.5, "Economy", "Premium"), levels = c("Premium", "Economy"))
y_test_factor <- factor(ifelse(y_test_xgb == 1, "Economy", "Premium"), levels = c("Premium", "Economy"))

cat("\n--- MATRIZ DE CONFUSIÓN ---\n")
print(confusionMatrix(pred_class, y_test_factor))

# Importancia (Gain, Cover, Frequency)
imp_matrix <- xgb.importance(feature_names = colnames(x_train_class), model = xgb_es)
xgb.plot.importance(imp_matrix, top_n = 10, main = "Top 10 Variables (XGBoost Gain)")

# Renderizado del Primer Árbol
xgb.plot.tree(feature_names = colnames(x_train_class), model = xgb_es, trees = 0)


# =========================================================================
# 5. PARTIAL DEPENDENCE PLOT (PDP)
# =========================================================================
# Analizamos el impacto de travelDistance manteniendo el resto constante
pdp_dist <- pdp::partial(
  object = xgb_es,
  pred.var = "travelDistance",
  train = x_train_class,
  prob = TRUE, # Forzamos salida en probabilidad
  grid.resolution = 30
)

p_pdp <- ggplot(pdp_dist, aes(x = travelDistance, y = yhat)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "PDP: Efecto de la Distancia en la Probabilidad de Economy",
       x = "Travel Distance", y = "Probabilidad Predicha") + theme_minimal()
print(p_pdp)


# =========================================================================
# 6. EXPLICABILIDAD GLOBAL Y LOCAL (SHAP VALUES)
# =========================================================================
cat("\n--- SHAP VALUES (TEORÍA DE JUEGOS) ---\n")

# Calculamos matriz SHAP sobre una muestra representativa del test
set.seed(42)
idx_shap <- sample(seq_len(nrow(x_test_class)), min(1000, nrow(x_test_class)))
x_shap <- x_test_class[idx_shap, ]

shap_matrix <- predict(xgb_es, x_shap, predcontrib = TRUE)

# Limpieza y pivoteo
shap_df <- as.data.frame(shap_matrix)
shap_df$BIAS <- NULL # Eliminamos el intercepto base
shap_long <- shap_df %>% mutate(id = row_number()) %>% pivot_longer(-id, names_to = "variable", values_to = "shap")

# Importancia SHAP Global (Media absoluta)
shap_imp <- shap_long %>% group_by(variable) %>% summarise(mean_abs_shap = mean(abs(shap))) %>% arrange(desc(mean_abs_shap))

p_shap_bar <- ggplot(head(shap_imp, 15), aes(x = mean_abs_shap, y = reorder(variable, mean_abs_shap))) +
  geom_col(fill = "darkred") +
  labs(title = "Importancia SHAP Media Absoluta", x = "|SHAP Value|", y = "") + theme_minimal()
print(p_shap_bar)

# Distribución SHAP (Boxplots)
top_vars <- head(shap_imp$variable, 10)
p_shap_box <- shap_long %>% filter(variable %in% top_vars) %>%
  ggplot(aes(x = shap, y = reorder(variable, abs(shap), FUN = median))) +
  geom_boxplot(fill = "lightblue", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Distribución de Contribuciones SHAP", x = "SHAP Value (Impacto en Log-Odds)", y = "") + theme_minimal()
print(p_shap_box)


# =========================================================================
# 7. DECISION BOUNDARY PLOT (PCA)
# =========================================================================
cat("\n--- DECISION BOUNDARY PCA ---\n")
vars_num <- c("travelDistance", "layoverNumber", "elapsedDays", "seatsLeft")
pca_res <- prcomp(train_df[, vars_num], scale. = TRUE)
df_pca <- data.frame(PC1 = pca_res$x[, 1], PC2 = pca_res$x[, 2])

xgb_pca <- xgb.train(
  params = list(objective = "binary:logistic", eta = 0.1, max_depth = 4),
  data = xgb.DMatrix(data = as.matrix(df_pca), label = y_train_xgb),
  nrounds = 50
)

grid_pc1 <- seq(min(df_pca$PC1), max(df_pca$PC1), length.out = 150)
grid_pc2 <- seq(min(df_pca$PC2), max(df_pca$PC2), length.out = 150)
mesh_pca <- expand.grid(PC1 = grid_pc1, PC2 = grid_pc2)
mesh_pca$pred_class <- factor(ifelse(predict(xgb_pca, xgb.DMatrix(as.matrix(mesh_pca))) >= 0.5, "Economy", "Premium"))

df_pca$economy_f <- y_test_factor[1:nrow(df_pca)] # Placeholder color

p_boundary <- ggplot() +
  geom_tile(data = mesh_pca, aes(x = PC1, y = PC2, fill = pred_class), alpha = 0.3) +
  scale_fill_manual(values = c("Economy" = "#A8C8F9", "Premium" = "#F9A8A8")) +
  labs(title = "XGBoost: Decision Boundary (PCA)", x = "PC1", y = "PC2") + theme_minimal()
print(p_boundary)


# =========================================================================
# 8. XGBOOST REGRESIÓN (Target: log_price)
# =========================================================================
cat("\n======================================================\n")
cat("      XGBOOST REGRESIÓN (log_price)\n")
cat("======================================================\n")

# Para regresión, el label es log_price
dtrain_reg <- xgb.DMatrix(data = x_train_class, label = train_df$log_price)
dtest_reg  <- xgb.DMatrix(data = x_test_class, label = test_df$log_price)

xgb_reg <- xgb.train(
  params = list(objective = "reg:squarederror", eta = 0.05, max_depth = 5),
  data = dtrain_reg, nrounds = 500,
  watchlist = list(train = dtrain_reg, test = dtest_reg),
  early_stopping_rounds = 20, verbose = 0
)

pred_reg <- predict(xgb_reg, dtest_reg)
rmse <- sqrt(mean((test_df$log_price - pred_reg)^2))
r2 <- 1 - (sum((test_df$log_price - pred_reg)^2) / sum((test_df$log_price - mean(test_df$log_price))^2))

cat(sprintf("Métricas de Regresión -> RMSE: %.4f | R-Squared: %.4f\n", rmse, r2))

p_reg <- ggplot(data.frame(Real = test_df$log_price, Pred = pred_reg), aes(x = Real, y = Pred)) +
  geom_point(alpha = 0.3, color = "darkgreen") + geom_abline(color = "red", linetype = "dashed") +
  labs(title = "XGBoost Regresión: Predicho vs Real", x = "log(Precio Real)", y = "Predicción") + theme_minimal()
print(p_reg)