# =========================================================================
# RANDOM FOREST: Clasificación y Regresión — Flight Prices
# Adaptado con Validación Cruzada y Tuning Avanzado (Caret)
# =========================================================================

# --- 0. SETUP Y CARGA DE DATOS ---
rm(list = ls())

# Instalador automático de paquetes (Blindado)
list.of.packages <- c(
  "randomForest", "ranger", "caret", "ggplot2", "dplyr",
  "reshape2", "vip", "pdp", "iml", "rpart", "rpart.plot", "future", "MLmetrics"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages) > 0) install.packages(new.packages, dependencies = TRUE)
invisible(lapply(list.of.packages, require, character.only = TRUE))

# 1. Cargar el entorno de trabajo generado por 10_Partition_Data.R
cat("\nCargando datos particionados desde model_data.RData...\n")
load("data/interim/model_data.RData")

# 2. Asegurar que las variables categóricas sean factores limpios
train_df$airline   <- as.factor(train_df$airline)
train_df$nonStop   <- as.factor(train_df$nonStop)
train_df$economy_f <- as.factor(train_df$economy_f)

test_df$airline    <- as.factor(test_df$airline)
test_df$nonStop    <- as.factor(test_df$nonStop)
test_df$economy_f  <- as.factor(test_df$economy_f)

# Limpieza estricta de NAs aislando variables (Evita errores de variables no encontradas)
train_df_clean <- na.omit(train_df[, c("economy_f", pred_base)])
train_df_clean$economy_f <- droplevels(train_df_clean$economy_f)
test_df_clean  <- na.omit(test_df[, c("economy_f", pred_base)])


# =========================================================================
# 1. ANÁLISIS DESCRIPTIVO AVANZADO (Nuevo bloque de la guía)
# =========================================================================
cat("\n======================================================\n")
cat("      ANÁLISIS DESCRIPTIVO AVANZADO\n")
cat("======================================================\n")

cat("Dimensiones train:", nrow(train_df_clean), "x", ncol(train_df_clean), "\n")
cat("Dimensiones test :", nrow(test_df_clean),  "x", ncol(test_df_clean),  "\n\n")

# Proporciones de las clases (Estratificación)
cat("Proporción de clases en Train:\n")
print(prop.table(table(train_df_clean$economy_f)))
cat("\nProporción de clases en Test:\n")
print(prop.table(table(test_df_clean$economy_f)))

# Resumen estadístico de las variables numéricas (Extraído de la guía)
cat("\nResumen descriptivo de predictores numéricos:\n")
train_df_clean %>%
  select(where(is.numeric)) %>%
  summary() %>%
  print()

# Gráfico de distribución de la variable objetivo
p_dist_class <- ggplot(train_df_clean, aes(x = economy_f, fill = economy_f)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = c("Economy" = "#1a5fa8", "Premium" = "#c0392b")) +
  labs(title = "Distribución de la variable objetivo (economy_f)", x = "Tipo de billete", y = "Observaciones") +
  theme_minimal(base_size = 13)
print(p_dist_class)


# =========================================================================
# 2. RANDOM FOREST — CLASIFICACIÓN
# =========================================================================
cat("\n======================================================\n")
cat("      RANDOM FOREST: CLASIFICACIÓN\n")
cat("======================================================\n")

formula_class <- as.formula(paste("economy_f ~", paste(pred_base, collapse = " + ")))
cat("Fórmula clasificación:\n"); print(formula_class)

# --- 2A. Entrenamiento básico ---
set.seed(1994)
rf_class <- randomForest(
  formula_class,
  data       = train_df_clean,
  ntree      = 300, 
  importance = TRUE
)
cat("\n--- MODELO BÁSICO ENTRADO ---\n")
print(rf_class)


# --- 2B. NUEVO BLOQUE: VALIDACIÓN CRUZADA (Caret) ---
cat("\n--- EJECUTANDO VALIDACIÓN CRUZADA (5-FOLD CV) ---\n")
trControl <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = multiClassSummary,
  savePredictions = "final"
)

set.seed(1994)
# Nota: Reducimos ntree a 150 en CV para optimizar tiempo de cómputo en datasets grandes
rf_cv <- train(
  formula_class,
  data      = train_df_clean,
  method    = "rf",
  metric    = "Accuracy",
  trControl = trControl,
  ntree     = 150
)
print(rf_cv)

# Gráfico de la validación cruzada
p_cv <- ggplot(rf_cv) + labs(title = "Validación Cruzada de Random Forest (caret)") + theme_minimal()
print(p_cv)


# --- 2C. NUEVO BLOQUE: AJUSTE DE HIPERPARÁMETROS (Tuning mtry) ---
cat("\n--- OPTIMIZANDO HIPERPARÁMETRO MTRY ---\n")
# Creamos una rejilla con valores candidatos para mtry (tus variables base son 6)
rf_grid <- expand.grid(mtry = c(2, 3, 4, 5))

set.seed(1994)
rf_tuned <- train(
  formula_class,
  data      = train_df_clean,
  method    = "rf",
  metric    = "Accuracy",
  trControl = trControl,
  tuneGrid  = rf_grid,
  ntree     = 150
)
print(rf_tuned)
cat("\nMejor valor de mtry seleccionado:", rf_tuned$bestTune$mtry, "\n")

# Gráfico del Tuning
p_tuned <- ggplot(rf_tuned) + labs(title = "Optimización del parámetro mtry (caret)") + theme_minimal()
print(p_tuned)


# --- 2D. Predicciones y Matrices de Confusión (Train vs Test de la guía) ---
pred_train_class <- predict(rf_class, train_df_clean)
pred_test_class  <- predict(rf_class, test_df_clean)

cat("\n--- MATRIZ DE CONFUSIÓN (TRAIN) ---\n")
print(caret::confusionMatrix(pred_train_class, train_df_clean$economy_f))

cat("\n--- MATRIZ DE CONFUSIÓN (TEST) ---\n")
cm_test <- caret::confusionMatrix(pred_test_class, test_df_clean$economy_f)
print(cm_test)

# Plot Matriz de Confusión (Test)
CM_df <- as.data.frame(cm_test$table)
p_cm_class <- ggplot(CM_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 6, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "#A8C8F9", high = "#1a5fa8") +
  labs(title = "Matriz de Confusión — Random Forest (Test)", x = "Clase Real", y = "Predicción") +
  theme_minimal(base_size = 13)
print(p_cm_class)


# =========================================================================
# 3. IMPORTANCIA DE VARIABLES (Doble enfoque de la guía)
# =========================================================================
cat("\n======================================================\n")
cat("      IMPORTANCIA DE VARIABLES\n")
cat("======================================================\n")

# 3A. Importancia por Impureza (Gini)
imp_class <- as.data.frame(rf_class$importance)
imp_class$Variable <- rownames(imp_class)
imp_class_top <- imp_class %>%
  arrange(desc(MeanDecreaseGini)) %>%
  mutate(Variable = reorder(Variable, MeanDecreaseGini))

p_imp_class <- ggplot(imp_class_top, aes(x = Variable, y = MeanDecreaseGini, fill = MeanDecreaseGini)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "#A8C8F9", high = "#1a5fa8") +
  coord_flip() +
  labs(title = "Importancia de Variables — Mean Decrease Gini", x = NULL, y = "Gini") +
  theme_minimal(base_size = 13)
print(p_imp_class)

# 3B. Importancia por Permutación (Vía Ranger, sugerido por la guía)
cat("\nCalculando Importancia por Permutación con Ranger...\n")
set.seed(1994)
rf_ranger <- ranger(
  formula_class,
  data       = train_df_clean,
  num.trees  = 300,
  importance = "permutation"
)
p_imp_perm <- vip(rf_ranger) + labs(title = "Importancia por Permutación (Ranger)") + theme_minimal()
print(p_imp_perm)


# =========================================================================
# 4. EXTRACCIÓN DE UN ÁRBOL AUXILIAR — CLASIFICACIÓN
# =========================================================================
cat("\n======================================================\n")
cat("      EXTRACCIÓN DE ÁRBOL AUXILIAR\n")
cat("======================================================\n")

set.seed(1994)
arbol_aux_class <- rpart(
  formula_class,
  data    = train_df_clean,
  method  = "class",
  control = rpart.control(maxdepth = 3, minbucket = 100, cp = 0.001)
)
rpart.plot(arbol_aux_class, main = "Árbol Auxiliar para Interpretación", type = 3, extra = 104, fallen.leaves = TRUE, shadow.col = "gray")


# =========================================================================
# 5. PARTIAL DEPENDENCE PLOTS (PDP) Y SHAP VALUES
# =========================================================================
cat("\n======================================================\n")
cat("      INTERPRETABILIDAD: PDP Y SHAP\n")
cat("======================================================\n")

# Partial Dependence Plot para travelDistance (Muestra cómo afecta al tipo de billete)
p_pdp <- partial(rf_class, pred.var = "travelDistance", train = train_df_clean, which.class = "Economy", prob = TRUE)
print(autoplot(p_pdp) + labs(title = "PDP: Efecto de travelDistance sobre Clase Economy", y = "Probabilidad Media"))

# SHAP Local (Muestra aleatoria de 200 observaciones para estabilidad de memoria)
future::plan(future::sequential)
options(future.globals.maxSize = 2 * 1024^3)

set.seed(1994)
X_train_class <- train_df_clean %>% select(all_of(pred_base))
y_train_class <- train_df_clean$economy_f

idx_shap <- sample(seq_len(nrow(X_train_class)), size = min(200, nrow(X_train_class)))
X_shap   <- X_train_class[idx_shap, , drop = FALSE]
y_shap   <- y_train_class[idx_shap]

predict_fun_economy <- function(model, newdata) { predict(model, newdata, type = "prob")[, "Economy"] }

predictor_economy <- Predictor$new(
  model = rf_class, data = X_shap, y = as.numeric(y_shap == "Economy"),
  predict.function = predict_fun_economy, type = "prob"
)

set.seed(1994)
shap_obs_class <- Shapley$new(predictor_economy, x.interest = test_df_clean[1, pred_base, drop = FALSE], sample.size = 100)
print(plot(shap_obs_class) + labs(title = "SHAP Local para observación Test[1] — Clase Economy"))


# =========================================================================
# 6. RANDOM FOREST — REGRESIÓN (log_price)
# =========================================================================
cat("\n======================================================\n")
cat("      RANDOM FOREST: REGRESIÓN (log_price)\n")
cat("======================================================\n")

# En regresión el tipo de asiento (economy_f) sí es un predictor válido para el precio
pred_reg <- c(pred_base, "economy_f")
formula_reg <- as.formula(paste("log_price ~", paste(pred_reg, collapse = " + ")))
cat("Fórmula regresión:\n"); print(formula_reg)

train_df_reg <- na.omit(train_df[, c("log_price", pred_reg)])

set.seed(1994)
rf_reg <- randomForest(
  formula_reg,
  data       = train_df_reg,
  ntree      = 300,
  importance = TRUE
)

cat("\n--- MODELO REGRESIÓN ENTRADO ---\n")
print(rf_reg)

# Importancia para regresión (Aumento del error cuadrático medio %IncMSE)
varImpPlot(rf_reg, main = "Importancia de Variables (Random Forest Regresión)")