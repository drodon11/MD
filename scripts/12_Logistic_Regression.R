# -------------------------------------------------------------------------
# 14. REGRESIÓN LOGÍSTICA: Clasificación de Clase Economy
# -------------------------------------------------------------------------

# --- 0. PREPARACIÓN DEL ENTORNO ---
list.of.packages <- c("tidyverse", "broom", "pROC", "caret", "ResourceSelection", "Epi")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages) > 0) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = TRUE))

# Carga de la matriz topológica validada por el profesor
dd <- readRDS("data/interim/flightprices_preprocessed.rds")

# Nos aseguramos de que la variable objetivo sea numérica (0/1) para el modelo
# y creamos una versión factor para la evaluación posterior
dd$economy_num <- ifelse(dd$economy == "Basic Economy" | dd$economy == "Economy", 1, 0)
dd$economy_f <- factor(ifelse(dd$economy_num == 1, "Sí", "No"), levels = c("No", "Sí"))


# --- 1. ESPECIFICACIÓN Y AJUSTE DEL MODELO (14.a, 14.b, 14.c) ---
# Aplicamos transformación logarítmica al precio para linealizar el logit
formula_logistica <- economy_num ~ log(totalPrice) + travelDistance + layoverNumber + 
  airline + nonStop + elapsedDays + seatsLeft

modelo_logit <- glm(formula_logistica, data = dd, family = binomial)

cat("\n--- RESUMEN DEL MODELO LOGÍSTICO ---\n")
summary(modelo_logit)

# Test de la Razón de Verosimilitud (Deviance respecto al modelo nulo)
cat("\n--- TEST DE VEROSIMILITUD (ANÁLISIS DE DEVIANCE) ---\n")
print(anova(modelo_logit, test = "Chisq"))

# Pseudo R-cuadrado de McFadden
modelo_nulo <- glm(economy_num ~ 1, data = dd, family = binomial)
pseudo_r2 <- 1 - logLik(modelo_logit) / logLik(modelo_nulo)
cat("\n--- PSEUDO R2 DE McFADDEN ---\n")
print(pseudo_r2)


# --- 2. INTERPRETACIÓN DE ODDS RATIOS (14.c, 14.e) ---
cat("\n--- TABLA DE ODDS RATIOS CON INTERVALOS DE CONFIANZA ---\n")
tabla_or <- tidy(modelo_logit) %>%
  mutate(
    odds_ratio = exp(estimate),
    OR_inf = exp(estimate - 1.96 * std.error),
    OR_sup = exp(estimate + 1.96 * std.error)
  ) %>%
  select(term, estimate, p.value, odds_ratio, OR_inf, OR_sup)

print(tabla_or)


# --- 3. PREDICCIÓN Y EVALUACIÓN DEL RENDIMIENTO (14.d) ---
# Cálculo de probabilidades en el hiperespacio
dd$prob_pred <- predict(modelo_logit, type = "response")

# Clasificación binaria (Umbral = 0.5)
dd$pred_clase <- factor(ifelse(dd$prob_pred >= 0.5, "Sí", "No"), levels = c("No", "Sí"))

cat("\n--- MATRIZ DE CONFUSIÓN ---\n")
conf_mat <- confusionMatrix(dd$pred_clase, dd$economy_f, positive = "Sí")
print(conf_mat)

# Visualización 1: Distribución de probabilidades predichas
p1 <- ggplot(dd, aes(x = prob_pred)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Probabilidades Predichas",
       subtitle = "Probabilidad de pertenencia a clase Economy",
       x = "Probabilidad Predicha", y = "Frecuencia") +
  theme_minimal()
print(p1)

# Visualización 2: Curvas ROC y cálculo de AUC
roc_obj <- roc(dd$economy_num, dd$prob_pred)
cat("\n--- ÁREA BAJO LA CURVA (AUC) ---\n")
print(auc(roc_obj))

# Gráfico ROC nativo
plot(roc_obj, main = paste("Curva ROC - AUC:", round(auc(roc_obj), 4)), col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")

# Gráfico ROC alternativo (Paquete Epi - Requisito del profesor)
ROC(form = formula_logistica, data = dd, plot = "ROC", las = 1)


# --- 4. DIAGNÓSTICO DEL MODELO (14.f) ---
cat("\n--- TEST DE HOSMER-LEMESHOW (BONDAD DE AJUSTE) ---\n")
hl_test <- hoslem.test(modelo_logit$y, modelo_logit$fitted.values)
print(hl_test)

# Visualización 3: Residuos de Deviance
res_dev <- residuals(modelo_logit, type = "deviance")
p2 <- ggplot(data.frame(fitted = fitted(modelo_logit), resid = res_dev), 
             aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.5, color = "darkred") +
  geom_hline(yintercept = 0, linetype = 2, color = "black", size = 1) +
  labs(title = "Diagnóstico: Residuos de Deviance",
       x = "Valores Ajustados (Probabilidades)", y = "Residuos de Deviance") +
  theme_minimal()
print(p2)

# Visualización 4: Observaciones Influyentes (Distancia de Cook)
cooks <- cooks.distance(modelo_logit)
df_cooks <- data.frame(index = 1:length(cooks), cooks = cooks)
threshold <- 4 / nrow(dd) # Límite matemático estricto

p3 <- ggplot(df_cooks, aes(x = index, y = cooks)) +
  geom_segment(aes(xend = index, yend = 0), alpha = 0.4) +
  geom_point(data = subset(df_cooks, cooks > threshold), color = "red", size = 2) +
  geom_hline(yintercept = threshold, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Diagnóstico: Distancia de Cook",
       subtitle = "Observaciones por encima de la línea roja (4/n) son estadísticamente influyentes",
       x = "Índice de Vuelo", y = "Distancia de Cook") +
  theme_minimal()
print(p3)