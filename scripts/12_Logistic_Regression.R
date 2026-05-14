# -------------------------------------------------------------------------
# 14. REGRESIÓN LOGÍSTICA: Clasificación de Basic Economy
# -------------------------------------------------------------------------

# --- 0. PREPARACIÓN DEL ENTORNO ---
list.of.packages <- c("tidyverse", "broom", "pROC", "caret", "ResourceSelection", "Epi")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages) > 0) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = TRUE))

# Carga de la matriz topológica validada por el profesor
dd <- readRDS("data/interim/flightprices_preprocessed.rds")

# En tu dataset, economy viene de isBasicEconomy y puede estar como factor FALSE/TRUE
# Lo convertimos correctamente a 0/1
dd$economy_num <- ifelse(as.character(dd$economy) == "TRUE", 1, 0)
dd$economy_f <- factor(ifelse(dd$economy_num == 1, "Sí", "No"), levels = c("No", "Sí"))

# Comprobación: ROC y matriz de confusión necesitan dos clases
cat("\n--- COMPROBACIÓN DE LA VARIABLE OBJETIVO ---\n")
print(table(dd$economy, useNA = "ifany"))
print(table(dd$economy_num, useNA = "ifany"))
print(table(dd$economy_f, useNA = "ifany"))

if (length(unique(na.omit(dd$economy_num))) < 2) {
  stop("La variable objetivo economy_num solo tiene una clase. Revisa los valores de dd$economy.")
}


# --- 1. ESPECIFICACIÓN Y AJUSTE DEL MODELO (14.a, 14.b, 14.c) ---
# Aplicamos transformación logarítmica al precio para linealizar el logit
formula_logistica <- economy_num ~ log(totalPrice) + travelDistance + layoverNumber + 
  airline + nonStop + elapsedDays + seatsLeft

modelo_logit <- glm(formula_logistica, data = dd, family = binomial)

cat("\n--- RESUMEN DEL MODELO LOGÍSTICO ---\n")
summary(modelo_logit)

# Test de la Razón de Verosimilitud
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
dd$prob_pred <- predict(modelo_logit, type = "response")

dd$pred_clase <- factor(
  ifelse(dd$prob_pred >= 0.5, "Sí", "No"),
  levels = c("No", "Sí")
)

cat("\n--- MATRIZ DE CONFUSIÓN ---\n")
conf_mat <- confusionMatrix(dd$pred_clase, dd$economy_f, positive = "Sí")
print(conf_mat)

# Visualización 1: Distribución de probabilidades predichas
p1 <- ggplot(dd, aes(x = prob_pred)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribución de Probabilidades Predichas",
    subtitle = "Probabilidad de pertenencia a Basic Economy",
    x = "Probabilidad Predicha",
    y = "Frecuencia"
  ) +
  theme_minimal()

print(p1)

# Visualización 2: Curva ROC y AUC
roc_obj <- roc(response = dd$economy_f, predictor = dd$prob_pred, levels = c("No", "Sí"))

cat("\n--- ÁREA BAJO LA CURVA (AUC) ---\n")
print(auc(roc_obj))

plot(
  roc_obj,
  main = paste("Curva ROC - AUC:", round(auc(roc_obj), 4)),
  col = "blue",
  lwd = 2
)
abline(a = 0, b = 1, lty = 2, col = "gray")

# Gráfico ROC alternativo con Epi
ROC(form = formula_logistica, data = dd, plot = "ROC", las = 1)


# --- 4. DIAGNÓSTICO DEL MODELO (14.f) ---
cat("\n--- TEST DE HOSMER-LEMESHOW (BONDAD DE AJUSTE) ---\n")
hl_test <- hoslem.test(modelo_logit$y, modelo_logit$fitted.values)
print(hl_test)

# Visualización 3: Residuos de Deviance
res_dev <- residuals(modelo_logit, type = "deviance")

p2 <- ggplot(
  data.frame(fitted = fitted(modelo_logit), resid = res_dev),
  aes(x = fitted, y = resid)
) +
  geom_point(alpha = 0.5, color = "darkred") +
  geom_hline(yintercept = 0, linetype = 2, color = "black", linewidth = 1) +
  labs(
    title = "Diagnóstico: Residuos de Deviance",
    x = "Valores Ajustados (Probabilidades)",
    y = "Residuos de Deviance"
  ) +
  theme_minimal()

print(p2)

# Visualización 4: Observaciones Influyentes
cooks <- cooks.distance(modelo_logit)

df_cooks <- data.frame(
  index = 1:length(cooks),
  cooks = cooks
)

threshold <- 4 / nrow(dd)

p3 <- ggplot(df_cooks, aes(x = index, y = cooks)) +
  geom_segment(aes(xend = index, yend = 0), alpha = 0.4) +
  geom_point(data = subset(df_cooks, cooks > threshold), color = "red", size = 2) +
  geom_hline(yintercept = threshold, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Diagnóstico: Distancia de Cook",
    subtitle = "Observaciones por encima de la línea roja (4/n) son estadísticamente influyentes",
    x = "Índice de Vuelo",
    y = "Distancia de Cook"
  ) +
  theme_minimal()

print(p3)