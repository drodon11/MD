# ==============================================================================
#                             NAIVE BAYES & kNN
# ==============================================================================

# --- SETUP Y CARGA DE DATOS ---
rm(list = ls())

list.of.packages <- c("naivebayes", "VIM", "caret")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages) > 0) install.packages(new.packages, dependencies = TRUE)
invisible(lapply(list.of.packages, require, character.only = TRUE))

# Cargamos las particiones
load("data/interim/model_data.RData")

# Aseguramos que la clase a predecir sea un factor
train_df$airline <- as.factor(train_df$airline)
test_df$airline <- as.factor(test_df$airline)

# Seleccionamos las columnas numéricas que venimos usando
vars_num <- c("elapsedDays", "taxAmount", "totalPrice", 
              "travelDistance", "segmentDistance", "layoverNumber")
formula_class <- as.formula(paste("economy_f ~ log_price +", paste(pred_base, collapse = " + ")))

# ==============================================================================
#                               NAIVE BAYES
# ==============================================================================

# -- 1A. Naive Bayes Estándar --
nb <- naive_bayes(formula_class, data = train_df, laplace = 1)

# Gráficos de distribución por clase para algunas variables
plot(nb, legend = TRUE)

# Predicción y matriz de confusión
pred_nb <- predict(nb, test_df)
cat("\n--- Matriz de Confusión: Naive Bayes ---\n")
MC_nb <- table(Prediccion = pred_nb, Real = test_df$airline)
print(MC_nb)
cat(sprintf("Accuracy: %.4f\n", sum(diag(MC_nb))/sum(MC_nb)))

# Mostrar tablas de probabilidad condicional (para las primeras variables)
tables(nb, 1:4) # Descomentar para ver las tablas en la consola


# -- 1B. Naive Bayes con Kernel (Fronteras Suavizadas) --
cat("\n--- Naive Bayes usando KERNEL ---\n")
nb_kernel <- naive_bayes(formula_class, data = train_df, usekernel = TRUE, laplace = 1)

# Plot de distribuciones con kernel
plot(nb_kernel, legend = TRUE)

# Predicción y matriz de confusión
pred_kernel <- predict(nb_kernel, test_df)
MC_kernel <- table(Prediccion = pred_kernel, Real = test_df$airline)
print(MC_kernel)
cat(sprintf("Accuracy Kernel: %.4f\n", sum(diag(MC_kernel))/sum(MC_kernel)))

cat("\n======================================================\n")
cat("          2. kNN CLASIFICACIÓN (Paquete VIM)\n")
cat("======================================================\n")
# Unimos los datos temporalmente
df_aux <- rbind(train_df[, c("airline", vars_num)], 
                test_df[, c("airline", vars_num)])

# Identificamos qué filas corresponden al test y las marcamos como NA 
test_idx <- (nrow(train_df) + 1):nrow(df_aux)
df_aux$airline[test_idx] <- NA


# -- kNN estándar por defecto (K=5) --
cat("\n--- kNN (K=5 por defecto) ---\n")
result_knn <- kNN(df_aux, variable = "airline")
table(Prediccion = result_knn$airline[test_idx], Real = test_df$airline)

# -- kNN con k=3 --
cat("\n--- kNN (K=3) ---\n")
result_knn3 <- kNN(df_aux, variable = "airline", k = 3)
table(Prediccion = result_knn3$airline[test_idx], Real = test_df$airline)

# -- kNN con k=1 --
cat("\n--- kNN (K=1) ---\n")
result_knn1 <- kNN(df_aux, variable = "airline", k = 1)
table(Prediccion = result_knn1$airline[test_idx], Real = test_df$airline)


cat("\n======================================================\n")
cat("          3. kNN REGRESIÓN (Predicción de totalPrice)\n")
cat("======================================================\n")
df_reg <- rbind(train_df[, c("airline", vars_num)], 
                test_df[, c("airline", vars_num)])

# Guardamos los valores reales y ocultamos los del test
y_real <- test_df$totalPrice
df_reg$totalPrice[test_idx] <- NA

# Ejecutamos kNN para estimar el precio (K=1)
result_reg <- kNN(df_reg, variable = "totalPrice", k = 1)

y_pred <- result_reg$totalPrice[test_idx]

# Gráficos de evaluación
par(mfrow=c(1,2))
plot(y_pred, y_real, main = "kNN Regresión: Predicho vs Real", 
     xlab = "Predicción de Precio", ylab = "Precio Real", col = "blue", pch = 20)
abline(0, 1, col = "red", lwd = 2) # Línea ideal (lo predicho = lo real)

plot(y_pred - y_real, main = "Errores Residuales", 
     ylab = "Diferencia (Predicho - Real)", col = "darkgreen", pch = 20)
abline(h = 0, col = "red", lwd = 2)
par(mfrow=c(1,1))

# Cálculo de Métricas de Regresión
mse <- sum((y_real - y_pred)^2) / length(y_real)
rmse <- sqrt(mse)
var_y <- sum((y_real - mean(y_real))^2) / length(y_real)
r.square <- 1 - (mse / var_y)

cat("\n--- RESULTADOS DE REGRESIÓN kNN (K=1) ---\n")
cat(sprintf(" RMSE (Error Cuadrático Medio): %.2f\n", rmse))
cat(sprintf(" R-Cuadrado (R2): %.4f\n", r.square))
