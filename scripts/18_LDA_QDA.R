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

# CORREGIDO: El target de clasificación ahora es economy_f (limpiando niveles para caret)
train_df$economy_f <- as.factor(make.names(train_df$economy_f))
test_df$economy_f  <- as.factor(make.names(test_df$economy_f))

# Seleccionamos las columnas numéricas cuantitativas (totalPrice incluido como predictor)
vars_num <- c("elapsedDays", "taxAmount", "totalPrice", 
              "travelDistance", "segmentDistance", "layoverNumber")


# ==============================================================================
#                        1. ANÁLISIS EXPLORATORIO INICIAL
# ==============================================================================
cat("\n--- ANÁLISIS EXPLORATORIO INICIAL ---\n")

# Frecuencias de la variable objetivo en entrenamiento
print(table(train_df$economy_f))

# Medias por grupo de las principales variables químicas/numéricas de los vuelos
vuelos_medias <- train_df %>%
  group_by(economy_f) %>%
  summarise(
    totalPrice_media = mean(totalPrice),
    taxAmount_media = mean(taxAmount),
    elapsedDays_media = mean(elapsedDays),
    travelDistance_media = mean(travelDistance),
    .groups = "drop"
  )
print(vuelos_medias)

# Boxplot de la variable más crítica (totalPrice) según la clase de asiento
ggplot(train_df, aes(x = economy_f, y = totalPrice, fill = economy_f)) +
  geom_boxplot(alpha = 0.75) +
  labs(title = "Distribución de totalPrice por clase de vuelo", x = "Clase", y = "Precio (€)") +
  theme_minimal() + 
  theme(legend.position = "none")

# Relación bivariante inicial entre distancia y precio total
ggplot(train_df, aes(x = travelDistance, y = totalPrice, color = economy_f)) +
  geom_point(size = 2, alpha = 0.6) +
  labs(title = "Separación visual de clases usando dos variables", x = "Distancia", y = "Precio Total") +
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

# Histograma discriminante (Equivalente al ldahist del documento Wine)
ldahist(lda_train_pred$x[, 1], g = train_df$economy_f, ymax = 1,
        main = "Distribución de las clases sobre el eje discriminante LD1")


# --- Clasificación y Evaluación sobre Test ---
lda_test_pred <- predict(modelo_lda, newdata = test_df[, vars_num])

# Matriz de confusión básica y Accuracy en test
MC_lda <- table(Real = test_df$economy_f, Predicho = lda_test_pred$class)
cat("\nMatriz de Confusión LDA (Test):\n")
print(MC_lda)

accuracy_lda <- sum(diag(MC_lda)) / sum(MC_lda)
cat(sprintf("Accuracy LDA en Test: %.4f\n", accuracy_lda))

# Evaluación formal con la suite de Caret
cat("\nMétricas de rendimiento detalladas (LDA):\n")
print(confusionMatrix(data = lda_test_pred$class, reference = test_df$economy_f))


# --- Visualización de Fronteras de Decisión LDA ---
cat("\nGenerando regiones de clasificación bivariantes (LDA)...\n")
partimat(
  economy_f ~ totalPrice + travelDistance + taxAmount + elapsedDays,
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
cat("\nMatriz de Confusión QDA (Test):\n")
print(MC_qda)

accuracy_qda <- sum(diag(MC_qda)) / sum(MC_qda)
cat(sprintf("Accuracy QDA en Test: %.4f\n", accuracy_qda))

# Evaluación formal con Caret
cat("\nMétricas de rendimiento detalladas (QDA):\n")
print(confusionMatrix(data = qda_test_pred$class, reference = test_df$economy_f))


# --- Visualización de Fronteras de Decisión QDA ---
cat("\nGenerando regiones de clasificación curvas (QDA)...\n")
partimat(
  economy_f ~ totalPrice + travelDistance + taxAmount + elapsedDays,
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