# ==============================================================================
#                      LINEAR & QUADRATIC DISCRIMINANT ANALYSIS
# ==============================================================================

# --- 0. SETUP Y CARGA DE DATOS ---
rm(list = ls())

# Instalador automático de paquetes (Blindado y estructurado)
list.of.packages <- c("MASS", "caret", "ggplot2", "dplyr", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages) > 0) install.packages(new.packages, dependencies = TRUE)
invisible(lapply(list.of.packages, require, character.only = TRUE))

# Aseguramos la instalación de librerías avanzadas para gráficos y supuestos
if (!requireNamespace("klaR", quietly = TRUE)) install.packages("klaR", dependencies = TRUE)
if (!requireNamespace("biotools", quietly = TRUE)) install.packages("biotools", dependencies = TRUE)

library(klaR)
library(biotools)

# Cargamos el entorno de datos oficial del proyecto
load("data/interim/model_data.RData")

# El target de clasificación ahora es economy_f (limpiando niveles para caret)
train_df$economy_f <- as.factor(make.names(train_df$economy_f))
test_df$economy_f  <- as.factor(make.names(test_df$economy_f))

# ACTUALIZADO: Seleccionamos las columnas numéricas usando log_price en lugar de totalPrice
vars_num <- c("elapsedDays", "taxAmount", "log_price", 
              "travelDistance", "segmentDistance", "layoverNumber")


# ==============================================================================
#                        1. ANÁLISIS EXPLORATORIO INICIAL
# ==============================================================================
cat("\n--- ANÁLISIS EXPLORATORIO INICIAL ---\n")

# Frecuencias de la variable objetivo en entrenamiento
print(table(train_df$economy_f))

# ACTUALIZADO: Medias por grupo usando log_price
vuelos_medias <- train_df %>%
  group_by(economy_f) %>%
  summarise(
    log_price_media = mean(log_price),
    taxAmount_media = mean(taxAmount),
    elapsedDays_media = mean(elapsedDays),
    travelDistance_media = mean(travelDistance),
    .groups = "drop"
  )
print(vuelos_medias)

# ACTUALIZADO: Boxplot de la variable más crítica (log_price) según la clase de asiento
ggplot(train_df, aes(x = economy_f, y = log_price, fill = economy_f)) +
  geom_boxplot(alpha = 0.75) +
  labs(title = "Distribución de log_price por clase de vuelo", x = "Clase", y = "Log(Precio)") +
  theme_minimal() + 
  theme(legend.position = "none")

# ACTUALIZADO: Relación bivariante inicial entre distancia y log_price
ggplot(train_df, aes(x = travelDistance, y = log_price, color = economy_f)) +
  geom_point(size = 2, alpha = 0.6) +
  labs(title = "Separación visual de clases usando dos variables", x = "Distancia", y = "Log(Precio)") +
  theme_minimal()


# ==============================================================================
#                        2. LINEAR DISCRIMINANT ANALYSIS (LDA)
# ==============================================================================
cat("\n======================================================\n")
cat("            LINEAR DISCRIMINANT ANALYSIS (LDA)\n")
cat("======================================================\n")

# Ajuste del modelo LDA
modelo_lda <- lda(economy_f ~ ., data = train_df[, c("economy_f", vars_num)])
print(modelo_lda)

# --- Proyección en el espacio discriminante ---
# Al tener 2 clases, hay exactamente K-1 = 1 función discriminante (LD1).
lda_train_pred <- predict(modelo_lda, newdata = train_df[, vars_num])

# Histograma discriminante
ldahist(lda_train_pred$x[, 1], g = train_df$economy_f, ymax = 1,
        main = "Distribución de las clases sobre el eje discriminante LD1")


# --- Clasificación y Evaluación sobre Test ---
lda_test_pred <- predict(modelo_lda, newdata = test_df[, vars_num])

# Matriz de confusión básica y Accuracy en test
MC_lda <- table(Real = test_df$economy_f, Predicho = lda_test_pred$class)
cat("\nMatriz de Confusión Básica LDA (Test):\n")
print(MC_lda)

accuracy_lda <- sum(diag(MC_lda)) / sum(MC_lda)
cat(sprintf("Accuracy LDA en Test: %.4f\n", accuracy_lda))

# Evaluación formal con la suite de Caret
cat("\nMétricas de rendimiento detalladas (LDA):\n")
print(confusionMatrix(data = lda_test_pred$class, reference = test_df$economy_f))

# Visualización gráfica de la matriz de confusión LDA ---
conf_tbl_lda <- table(Predicted = lda_test_pred$class, Actual = test_df$economy_f)
conf_df_lda  <- as.data.frame(conf_tbl_lda)
colnames(conf_df_lda) <- c("Predicted", "Actual", "Freq")

plot_lda_cm <- ggplot(conf_df_lda, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "#f7fbff", high = "#08519c") + # Tono azul para LDA
  labs(title = "Matriz de Confusión LDA", x = "Valor Real", y = "Predicción") +
  theme_minimal()
print(plot_lda_cm)


# --- Visualización de Fronteras de Decisión LDA ---
cat("\nGenerando regiones de clasificación bivariantes (LDA)...\n")
# ACTUALIZADO: partimat con log_price
partimat(
  economy_f ~ log_price + travelDistance + taxAmount + elapsedDays,
  data = train_df[, c("economy_f", vars_num)],
  method = "lda",
  prec = 150,
  image.colors = c("skyblue2", "snow2"),
  col.mean = "firebrick"
)


# ==============================================================================
#                        3. COMPROBACIÓN DE SUPUESTOS
# ==============================================================================
cat("\n======================================================\n")
cat("               COMPROBACIÓN DE SUPUESTOS\n")
cat("======================================================\n")

# Test de Box's M para la igualdad de matrices de covarianza
# H0: Las matrices de covarianza son iguales (Ideal para LDA)
# H1: Las matrices de covarianza difieren (Apoya el uso de QDA)
cat("\nEjecutando Test de Box's M...\n")
resultado_boxm <- boxM(train_df[, vars_num], train_df$economy_f)
print(resultado_boxm)


# ==============================================================================
#                        4. QUADRATIC DISCRIMINANT ANALYSIS (QDA)
# ==============================================================================
cat("\n======================================================\n")
cat("           QUADRATIC DISCRIMINANT ANALYSIS (QDA)\n")
cat("======================================================\n")

# Ajuste del modelo QDA (Relaja el supuesto al permitir covarianzas individuales)
modelo_qda <- qda(economy_f ~ ., data = train_df[, c("economy_f", vars_num)])
print(modelo_qda)


# --- Predicción y Evaluación sobre Test ---
qda_test_pred <- predict(modelo_qda, newdata = test_df[, vars_num])

# Matriz de confusión básica y Accuracy en test
MC_qda <- table(Real = test_df$economy_f, Predicho = qda_test_pred$class)
cat("\nMatriz de Confusión Básica QDA (Test):\n")
print(MC_qda)

accuracy_qda <- sum(diag(MC_qda)) / sum(MC_qda)
cat(sprintf("Accuracy QDA en Test: %.4f\n", accuracy_qda))

# Evaluación formal con Caret
cat("\nMétricas de rendimiento detalladas (QDA):\n")
print(confusionMatrix(data = qda_test_pred$class, reference = test_df$economy_f))

# Visualización gráfica de la matriz de confusión QDA ---
conf_tbl_qda <- table(Predicted = qda_test_pred$class, Actual = test_df$economy_f)
conf_df_qda  <- as.data.frame(conf_tbl_qda)
colnames(conf_df_qda) <- c("Predicted", "Actual", "Freq")

plot_qda_cm <- ggplot(conf_df_qda, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "#fff5eb", high = "#d94801") + # Tono naranja para QDA
  labs(title = "Matriz de Confusión QDA", x = "Valor Real", y = "Predicción") +
  theme_minimal()
print(plot_qda_cm)


# --- Visualización de Fronteras de Decisión QDA ---
cat("\nGenerando regiones de clasificación curvas (QDA)...\n")
# ACTUALIZADO: partimat con log_price
partimat(
  economy_f ~ log_price + travelDistance + taxAmount + elapsedDays,
  data = train_df[, c("economy_f", vars_num)],
  method = "qda",
  prec = 150,
  image.colors = c("skyblue2", "snow2"),
  col.mean = "firebrick"
)


# ==============================================================================
#                        5. COMPARACIÓN FINAL LDA vs QDA
# ==============================================================================
cat("\n======================================================\n")
cat("             COMPARACIÓN DE RENDIMIENTO\n")
cat("======================================================\n")

# Estructura del dataframe comparativo
resultados <- data.frame(
  Modelo = c("LDA", "QDA"),
  Accuracy = c(accuracy_lda, accuracy_qda),
  Error = c(1 - accuracy_lda, 1 - accuracy_qda)
)
print(resultados)

# Gráfico de barras comparativo con ggplot2
ggplot(resultados, aes(x = Modelo, y = Accuracy, fill = Modelo)) +
  geom_col(alpha = 0.85, width = 0.4) +
  ylim(0, 1) +
  labs(title = "Comparación de Accuracy en Test (Flight Prices)", x = "Modelo", y = "Accuracy") +
  scale_fill_manual(values = c("dodgerblue3", "darkorange2")) +
  theme_minimal() +
  theme(legend.position = "none")