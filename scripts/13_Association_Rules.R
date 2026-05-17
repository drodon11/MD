# scripts/03_association_rules.R
# Reglas de Asociación sobre datos de precios de vuelos
# Ejecutar después de 02_preprocessing.R
# Cubre: Apriori, Eclat, métricas, visualización, filtrado y diagnóstico
# ---------- 0. PAQUETES ----------
list.of.packages <- c(
  "arules", "arulesViz",
  "dplyr", "tidyr", "stringr", "ggplot2",
  "tibble", "purrr", "knitr", "scales", "forcats", "RColorBrewer"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages) > 0) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = TRUE))
rm(list.of.packages, new.packages)

# ---------- 1. CARGA DE DATOS ----------
input_path <- file.path(getwd(), "data", "interim", "flightprices_preprocessed.rds")
if (!file.exists(input_path)) stop("Ejecuta primero 02_preprocessing.R")

dd <- readRDS(input_path)

cat("\nDimensiones del dataset preprocesado:\n")
print(dim(dd))
cat("\nPrimeras filas:\n")
print(head(dd))
cat("\nTipos de variables:\n")
print(sapply(dd, class))

# ---------- 2. DISCRETIZACIÓN DE VARIABLES NUMÉRICAS ----------
# Las reglas de asociación requieren items discretos.
# Transformamos variables continuas en categorías interpretables.

cat("\n--- 2. DISCRETIZACIÓN ---\n")

dd_disc <- dd

# 1. totalPrice → precio_bajo / precio_medio / precio_alto
# <200: bajo, 200-400: medio, >400: alto
if ("totalPrice" %in% names(dd_disc)) {
  dd_disc$precio_grupo <- cut(
    dd_disc$totalPrice,
    breaks = c(-Inf, 200, 400, Inf),
    labels = c("precio_bajo", "precio_medio", "precio_alto")
  )
}

# 2. taxAmount → impuesto_bajo / impuesto_medio / impuesto_alto
#mediana ~44, Max ~192
if ("taxAmount" %in% names(dd_disc)) {
  dd_disc$impuesto_grupo <- cut(
    dd_disc$taxAmount,
    breaks = c(-Inf, 35, 60, Inf),
    labels = c("impuesto_bajo", "impuesto_medio", "impuesto_alto")
  )
}

# 3. travelDistance → distancia_corta / distancia_media / distancia_larga
#Mediana ~1473, Media ~1631, Max ~4173
if ("travelDistance" %in% names(dd_disc)) {
  dd_disc$distancia_grupo <- cut(
    dd_disc$travelDistance,
    breaks = c(-Inf, 1000, 2000, Inf),
    labels = c("distancia_corta", "distancia_media", "distancia_larga")
  )
}

# 4. seatsLeft → pocas_plazas / plazas_medias / muchas_plazas
# Rango [0, 10].
if ("seatsLeft" %in% names(dd_disc)) {
  dd_disc$plazas_grupo <- cut(
    dd_disc$seatsLeft,
    breaks = c(-Inf, 3, 7, Inf), # 0-3: pocas, 4-7: medias, 8-10: muchas
    labels = c("pocas_plazas", "plazas_medias", "muchas_plazas")
  )
}

# 5. layoverNumber → sin_escala / una_escala / multiples_escalas
# Según tu metadata: Rango [0, 3]. Lo tratamos como categorías lógicas.
if ("layoverNumber" %in% names(dd_disc)) {
  dd_disc$escala_grupo <- factor(
    ifelse(dd_disc$layoverNumber == 0, "sin_escala",
           ifelse(dd_disc$layoverNumber == 1, "una_escala", "dos_o_mas_escalas")),
    levels = c("sin_escala", "una_escala", "dos_o_mas_escalas")
  )
}

# 6. elapsedDays → mismo_dia / dia_siguiente
# Según tu metadata: Rango [0, 1] y media 0.14. Casi todos son 0.
if ("elapsedDays" %in% names(dd_disc)) {
  dd_disc$duracion_dias_grupo <- factor(
    ifelse(dd_disc$elapsedDays == 0, "mismo_dia", "dia_siguiente"),
    levels = c("mismo_dia", "dia_siguiente")
  )
}

# 7. Variables categóricas/booleanas explícitas
if ("economy" %in% names(dd_disc)) {
  dd_disc$economy_lbl <- factor(
    ifelse(as.logical(dd_disc$economy), "economy_si", "economy_no")
  )
}

if ("nonStop" %in% names(dd_disc)) {
  dd_disc$vuelo_tipo <- factor(
    ifelse(as.logical(dd_disc$nonStop), "vuelo_directo", "vuelo_con_escala")
  )
}

# Seleccionamos solo las columnas discretizadas para construir transacciones
# (Asegúrate de que no incluimos columnas numéricas originales)
cols_trans <- intersect(
  c("startApt", "destApt", "airline", "equipment",
    "precio_grupo", "impuesto_grupo", "distancia_grupo",
    "plazas_grupo", "escala_grupo", "duracion_dias_grupo",
    "economy_lbl", "vuelo_tipo"),
  names(dd_disc)
)

dd_trans_df <- dd_disc[, cols_trans, drop = FALSE]

# Convertimos todo a character para la limpieza antes de las transacciones
dd_trans_df[] <- lapply(dd_trans_df, as.character)

cat("\nColumnas usadas para las transacciones:\n")
print(cols_trans)

# Eliminamos filas con NA residuales
dd_trans_df <- na.omit(dd_trans_df)
# Volvemos a factor (requerido por el paquete arules)
dd_trans_df[] <- lapply(dd_trans_df, as.factor)

# ---------- 3. CONVERSIÓN A OBJETO transactions ----------
cat("\n--- 3. CREACIÓN DEL OBJETO TRANSACTIONS ---\n")

trans <- as(dd_trans_df, "transactions")
cat("\nResumen del objeto transactions:\n")
print(trans)
print(summary(trans))

# ---------- 4. EXPLORACIÓN INICIAL ----------
cat("\n--- 4. EXPLORACIÓN INICIAL ---\n")

n_transacciones <- length(trans)
n_items        <- length(itemLabels(trans))
cat("\nTotal transacciones:", n_transacciones)
cat("\nTotal items distintos:", n_items, "\n")

# Primeras transacciones
cat("\nPrimeras 5 transacciones:\n")
inspect(trans[1:5])

# Distribución del tamaño de las transacciones
tamanyos <- size(trans)
cat("\nResumen del tamaño de transacción:\n")
print(summary(tamanyos))

ggplot(data.frame(tamanyo = tamanyos), aes(x = tamanyo)) +
  geom_histogram(binwidth = 1, boundary = 0, color = "white", fill = "steelblue") +
  labs(
    title    = "Distribución del número de items por transacción",
    subtitle = "Datos de precios de vuelos",
    x        = "Número de items",
    y        = "Frecuencia"
  ) +
  theme_minimal()

# Frecuencia de items
itemFrequencyPlot(
  trans,
  topN  = 20,
  type  = "relative",
  main  = "Top 20 items más frecuentes",
  ylab  = "Soporte relativo",
  col   = "steelblue"
)

# Tabla de los 20 items más frecuentes
freq_items <- itemFrequency(trans, type = "relative") |>
  sort(decreasing = TRUE) |>
  head(20)

cat("\nTop 20 items por soporte relativo:\n")
print(
  tibble(item = names(freq_items), soporte = round(as.numeric(freq_items), 4))
)

# ---------- 5. EFECTO DEL SOPORTE Y CONFIANZA ----------
cat("\n--- 5. EXPLORACIÓN DE UMBRALES ---\n")

param_grid <- expand.grid(
  soporte   = c(0.001, 0.005, 0.01, 0.02),
  confianza = c(0.2, 0.4, 0.6, 0.8)
)

conteo_reglas <- param_grid |>
  mutate(
    n_reglas = map2_int(soporte, confianza, function(s, c) {
      reglas_tmp <- apriori(
        trans,
        parameter = list(supp = s, conf = c, minlen = 2),
        control   = list(verbose = FALSE)
      )
      length(reglas_tmp)
    })
  )

cat("\nNúmero de reglas según soporte y confianza:\n")
print(conteo_reglas)

ggplot(conteo_reglas, aes(x = soporte, y = n_reglas, color = factor(confianza))) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title  = "Efecto del soporte y la confianza en el número de reglas",
    x      = "Soporte mínimo",
    y      = "Número de reglas generadas",
    color  = "Confianza mínima"
  ) +
  theme_minimal()

#---------------------
#confianza: 0.6 y soporte 2%
#---------------------


# ---------- 6. EXTRACCIÓN DE REGLAS CON APRIORI ----------
cat("\n--- 6. APRIORI: EXTRACCIÓN GENERAL DE REGLAS ---\n")

reglas <- apriori(
  trans,
  parameter = list(
    supp   = 0.02,
    conf   = 0.60,
    minlen = 2,
    maxlen = 5
  )
)

cat("\nResumen de reglas generadas:\n")
print(reglas)
print(summary(reglas))

# ---------- 7. INSPECCIÓN Y ORDENACIÓN ----------
cat("\n--- 7. INSPECCIÓN DE REGLAS ---\n")

cat("\nTop 10 reglas por LIFT:\n")
reglas_lift <- sort(reglas, by = "lift", decreasing = TRUE)
inspect(head(reglas_lift, 10))

cat("\nTop 10 reglas por CONFIANZA:\n")
reglas_conf <- sort(reglas, by = "confidence", decreasing = TRUE)
inspect(head(reglas_conf, 10))

cat("\nTop 10 reglas por SOPORTE:\n")
reglas_supp <- sort(reglas, by = "support", decreasing = TRUE)
inspect(head(reglas_supp, 10))

# Convertir a data.frame para análisis tabular
reglas_df <- as(reglas, "data.frame") |> as_tibble()
cat("\nTop 15 reglas por lift (tabla):\n")
print(
  reglas_df |>
    arrange(desc(lift)) |>
    slice(1:15) |>
    mutate(across(where(is.numeric), ~ round(.x, 4)))
)

# ---------- 8. VISUALIZACIÓN ----------
cat("\n--- 8. VISUALIZACIÓN ---\n")

# Scatter: soporte vs confianza, coloreado por lift
plot(
  reglas,
  method  = "scatterplot",
  measure = c("support", "confidence"),
  shading = "lift",
  main    = "Reglas de asociación – soporte vs confianza (color: lift)"
)

# Two-key plot
plot(reglas, method = "two-key plot", main = "Two-key plot")

# Grafo de las 20 mejores por lift
reglas_top20 <- head(sort(reglas, by = "lift", decreasing = TRUE), 20)

plot(
  reglas_top20,
  method = "graph",
  engine = "htmlwidget"
)

# Gráfico agrupado
#plot(reglas_top20, method = "grouped", main = "Reglas agrupadas por consecuente")

# ---------- 9. FILTRADO DE REGLAS ----------
cat("\n--- 9. FILTRADO DE REGLAS ---\n")

# 9a. Reglas con precio_alto como consecuente
cat("\nReglas con consecuente 'precio_alto':\n")
reglas_precio_alto <- apriori(
  trans,
  parameter  = list(supp = 0.005, conf = 0.4, minlen = 2),
  appearance = list(rhs = "precio_grupo=precio_alto", default = "lhs"),
  control    = list(verbose = FALSE)
)

reglas_precio_alto <- reglas_precio_alto[!is.redundant(reglas_precio_alto)]
reglas_precio_alto <- sort(reglas_precio_alto, by = "lift", decreasing = TRUE)
inspect(head(reglas_precio_alto, 15))

# 9b. Reglas con precio_bajo como consecuente
cat("\nReglas con consecuente 'precio_bajo':\n")
reglas_precio_bajo <- apriori(
  trans,
  parameter  = list(supp = 0.005, conf = 0.4, minlen = 2),
  appearance = list(rhs = "precio_grupo=precio_bajo", default = "lhs"),
  control    = list(verbose = FALSE)
)

reglas_precio_bajo <- reglas_precio_bajo[!is.redundant(reglas_precio_bajo)]

reglas_precio_bajo <- sort(reglas_precio_bajo, by = "lift", decreasing = TRUE)
inspect(head(reglas_precio_bajo, 15))


# 9d. 
cat("\nReglas de alta calidad (lift > 2.0 & support > 0.05 & confidence > 0.7):\n")

reglas_interesantes <- subset(
  reglas,
  subset = lift > 2.0 & support > 0.05 & confidence > 0.7
)

# Poda de redundantes obligatoria
reglas_interesantes <- reglas_interesantes[!is.redundant(reglas_interesantes)]

cat("Número de reglas interesantes tras el filtro estricto:", length(reglas_interesantes), "\n")
inspect(sort(reglas_interesantes, by = "lift", decreasing = TRUE))

# ---------- 10. REGLAS REDUNDANTES ----------
cat("\n--- 10. REGLAS REDUNDANTES ---\n")

redundantes <- is.redundant(reglas)
cat("Distribución redundantes / no redundantes:\n")
print(table(redundantes))

reglas_nr <- reglas[!redundantes]
cat("Reglas originales:", length(reglas), "| Tras eliminar redundantes:", length(reglas_nr), "\n")

cat("\nTop 15 reglas no redundantes por lift:\n")
inspect(head(sort(reglas_nr, by = "lift", decreasing = TRUE), 15))

# ---------- 11. ITEMSETS FRECUENTES CON APRIORI ----------
cat("\n--- 11. ITEMSETS FRECUENTES (Apriori) ---\n")

itemsets_frec <- apriori(
  trans,
  parameter = list(target = "frequent itemsets", supp = 0.02, minlen = 1),
  control   = list(verbose = FALSE)
)

cat("\nTop 20 itemsets frecuentes por soporte:\n")
inspect(head(sort(itemsets_frec, by = "support", decreasing = TRUE), 20))

# ---------- 12. ECLAT ----------
cat("\n--- 12. ECLAT: ITEMSETS FRECUENTES ---\n")

itemsets_eclat <- eclat(
  trans,
  parameter = list(supp = 0.02, minlen = 2)
)

cat("\nResumen Eclat:\n")
print(itemsets_eclat)

cat("\nTop 15 itemsets Eclat por soporte:\n")
inspect(head(sort(itemsets_eclat, by = "support", decreasing = TRUE), 15))

# Conversión del top 5
top5_eclat <- sort(itemsets_eclat)[1:5]
cat("\nTop 5 – como lista:\n")
print(as(items(top5_eclat), "list"))
cat("\nTop 5 – como matriz binaria:\n")
print(as(items(top5_eclat), "matrix"))

# ---------- 13. VALIDACIÓN TRAIN / TEST ----------
cat("\n--- 13. VALIDACIÓN TRAIN / TEST ---\n")

set.seed(123)
idx_train   <- sample(seq_along(trans), size = round(0.7 * length(trans)))
trans_train <- trans[idx_train]
trans_test  <- trans[-idx_train]

reglas_train <- apriori(
  trans_train,
  parameter = list(supp = 0.01, conf = 0.5, minlen = 2),
  control   = list(verbose = FALSE)
)
reglas_train <- sort(reglas_train, by = "lift", decreasing = TRUE)
reglas_train_top <- head(reglas_train, 20)

medidas_test  <- interestMeasure(
  reglas_train_top,
  transactions = trans_test,
  measure      = c("support", "confidence", "lift", "count")
)
medidas_train <- quality(reglas_train_top)[, c("support", "confidence", "lift", "count")]

comparacion <- bind_cols(
  regla            = labels(reglas_train_top),
  train_support    = round(medidas_train$support,    4),
  train_confidence = round(medidas_train$confidence, 4),
  train_lift       = round(medidas_train$lift,       4),
  test_support     = round(medidas_test$support,     4),
  test_confidence  = round(medidas_test$confidence,  4),
  test_lift        = round(medidas_test$lift,        4)
)

cat("\nComparación train vs test (top 20 reglas por lift en train):\n")
print(comparacion)

# ---------- 14. ANÁLISIS DIRIGIDO: PRECIO ALTO ----------
cat("\n--- 14. ANÁLISIS DIRIGIDO: ¿QUÉ COMBINACIONES PREDICEN PRECIO ALTO? ---\n")

# Reglas más informativas hacia precio_alto con soporte más bajo para capturar más patrones
reglas_ph_final <- apriori(
  trans,
  parameter  = list(supp = 0.003, conf = 0.35, minlen = 2),
  appearance = list(rhs = "precio_grupo=precio_alto", default = "lhs"),
  control    = list(verbose = FALSE)
)
reglas_ph_final <- reglas_ph_final[!is.redundant(reglas_ph_final)]
reglas_ph_final <- sort(reglas_ph_final, by = "lift", decreasing = TRUE)

cat("\nTop 15 reglas hacia precio_alto (sin redundantes):\n")
inspect(head(reglas_ph_final, 15))

cat("\nVisualización de reglas hacia precio_alto:\n")
if (length(reglas_ph_final) > 0) {
  plot(
    head(sort(reglas_ph_final, by = "lift", decreasing = TRUE), 20),
    method = "graph",
    engine = "htmlwidget"
  )
}

# ---------- 15. GUARDADO DE RESULTADOS ----------
cat("\n--- 15. GUARDADO ---\n")

out_dir <- file.path(getwd(), "data", "results")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Guardamos reglas principales como data.frame
reglas_export <- as(reglas_nr, "data.frame") |>
  as_tibble() |>
  arrange(desc(lift))

write.csv(
  reglas_export,
  file      = file.path(out_dir, "association_rules.csv"),
  row.names = FALSE
)

# Guardamos también el objeto transactions por si se reutiliza
saveRDS(trans, file.path(out_dir, "transactions.rds"))

cat("\nArchivos guardados en:", normalizePath(out_dir, winslash = "/"), "\n")
cat("  - association_rules.csv\n")
cat("  - transactions.rds\n")

cat("\n===== FIN DEL ANÁLISIS DE REGLAS DE ASOCIACIÓN =====\n")