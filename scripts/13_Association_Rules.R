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
partition_path <- file.path(getwd(), "data", "interim", "model_data.RData")
if (!file.exists(partition_path)) stop("Ejecuta primero el script de partición única para generar model_data.RData")

# Carga train_df y test_df
load(partition_path)

cat("\nDimensiones del conjunto de entrenamiento:", dim(train_df))
cat("\nDimensiones del conjunto de prueba:", dim(test_df), "\n")

# ---------- 2. FUNCIÓN DE DISCRETIZACIÓN ----------
# Definimos la lógica una vez para aplicarla de forma idéntica a Train y Test
discretize_dataset <- function(data) {
  df_disc <- data
  
  # 1. totalPrice
  if ("totalPrice" %in% names(df_disc)) {
    df_disc$precio_grupo <- cut(
      df_disc$totalPrice,
      breaks = c(-Inf, 200, 400, Inf),
      labels = c("precio_bajo", "precio_medio", "precio_alto")
    )
  }
  
  # 2. taxAmount
  if ("taxAmount" %in% names(df_disc)) {
    df_disc$impuesto_grupo <- cut(
      df_disc$taxAmount,
      breaks = c(-Inf, 35, 60, Inf),
      labels = c("impuesto_bajo", "impuesto_medio", "impuesto_alto")
    )
  }
  
  # 3. travelDistance
  if ("travelDistance" %in% names(df_disc)) {
    df_disc$distancia_grupo <- cut(
      df_disc$travelDistance,
      breaks = c(-Inf, 1000, 2000, Inf),
      labels = c("distancia_corta", "distancia_media", "distancia_larga")
    )
  }
  
  # 4. seatsLeft
  if ("seatsLeft" %in% names(df_disc)) {
    df_disc$plazas_grupo <- cut(
      df_disc$seatsLeft,
      breaks = c(-Inf, 3, 7, Inf),
      labels = c("pocas_plazas", "plazas_medias", "muchas_plazas")
    )
  }
  
  # 5. layoverNumber
  if ("layoverNumber" %in% names(df_disc)) {
    df_disc$escala_grupo <- factor(
      ifelse(df_disc$layoverNumber == 0, "sin_escala",
             ifelse(df_disc$layoverNumber == 1, "una_escala", "dos_o_mas_escalas")),
      levels = c("sin_escala", "una_escala", "dos_o_mas_escalas")
    )
  }
  
  # 6. elapsedDays
  if ("elapsedDays" %in% names(df_disc)) {
    df_disc$duracion_dias_grupo <- factor(
      ifelse(df_disc$elapsedDays == 0, "mismo_dia", "dia_siguiente"),
      levels = c("mismo_dia", "dia_siguiente")
    )
  }
  
  # 7. Variables booleanas
  if ("economy" %in% names(df_disc)) {
    df_disc$economy_lbl <- factor(
      ifelse(as.logical(df_disc$economy), "economy_si", "economy_no")
    )
  }
  
  if ("nonStop" %in% names(df_disc)) {
    df_disc$vuelo_tipo <- factor(
      ifelse(as.logical(df_disc$nonStop), "vuelo_directo", "vuelo_con_escala")
    )
  }
  
  # Selección de columnas discretas para transacciones
  cols_trans <- intersect(
    c("startApt", "destApt", "airline", "equipment",
      "precio_grupo", "impuesto_grupo", "distancia_grupo",
      "plazas_grupo", "escala_grupo", "duracion_dias_grupo",
      "economy_lbl", "vuelo_tipo"),
    names(df_disc)
  )
  
  df_trans_df <- df_disc[, cols_trans, drop = FALSE]
  df_trans_df[] <- lapply(df_trans_df, as.character)
  df_trans_df <- na.omit(df_trans_df)
  df_trans_df[] <- lapply(df_trans_df, as.factor)
  
  return(df_trans_df)
}

# Aplicamos la función a Train y Test por separado
train_disc <- discretize_dataset(train_df)
test_disc  <- discretize_dataset(test_df)


# ---------- 3. CONVERSIÓN A OBJETO transactions ----------
cat("\n--- 3. CREACIÓN DEL OBJETO TRANSACTIONS ---\n")

trans_train <- as(train_disc, "transactions")
trans_test  <- as(test_disc, "transactions")

cat("\nResumen Transacciones Entrenamiento (Train):\n")
print(trans_train)
cat("\nResumen Transacciones Prueba (Test):\n")
print(trans_test)

# ---------- 4. EXPLORACIÓN INICIAL (TRAIN) ----------
cat("\n--- 4. EXPLORACIÓN INICIAL ---\n")

n_transacciones <- length(trans_train)
n_items        <- length(itemLabels(trans_train))
cat("\nTotal transacciones (Train):", n_transacciones)
cat("\nTotal items distintos:", n_items, "\n")

# Primeras transacciones
cat("\nPrimeras 5 transacciones de entrenamiento:\n")
inspect(trans_train[1:5])

# Distribución del tamaño de las transacciones
tamanyos <- size(trans_train)
cat("\nResumen del tamaño de transacción (Train):\n")
print(summary(tamanyos))

# Histograma de tamaño de transacciones
p_size <- ggplot(data.frame(tamanyo = tamanyos), aes(x = tamanyo)) +
  geom_histogram(binwidth = 1, boundary = 0, color = "white", fill = "steelblue") +
  labs(
    title    = "Distribución del número de items por transacción (Train)",
    x        = "Número de items",
    y        = "Frecuencia"
  ) +
  theme_minimal()
print(p_size)

# Frecuencia de items
itemFrequencyPlot(
  trans_train,
  topN  = 20,
  type  = "relative",
  main  = "Top 20 items más frecuentes (Train)",
  ylab  = "Soporte relativo",
  col   = "steelblue"
)

# Tabla de los 20 items más frecuentes
freq_items <- itemFrequency(trans_train, type = "relative") |>
  sort(decreasing = TRUE) |>
  head(20)

cat("\nTop 20 items por soporte relativo en Train:\n")
print(
  tibble(item = names(freq_items), soporte = round(as.numeric(freq_items), 4))
)

# ---------- 5. EFECTO DEL SOPORTE Y CONFIANZA ----------
cat("\n--- 5. EXPLORACIÓN DE UMBRALES ---\n")

param_grid <- expand.grid(
  soporte   = c(0.001, 0.005, 0.01, 0.02,  0.05, 0.1),
  confianza = c(0.2, 0.4, 0.6, 0.8)
)

conteo_reglas <- param_grid |>
  mutate(
    n_reglas = map2_int(soporte, confianza, function(s, c) {
      reglas_tmp <- apriori(
        trans_train,
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
  trans_train,
  parameter = list(
    supp   = 0.02,
    conf   = 0.60,
    minlen = 2,
    maxlen = 5
  )
)

cat("\nResumen de reglas generadas en Train:\n")
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

# Scatter
plot(
  reglas,
  method  = "scatterplot",
  measure = c("support", "confidence"),
  shading = "lift",
  main    = "Reglas de asociación (Train) – Soporte vs Confianza"
)

# Two-key plot
plot(reglas, method = "two-key plot", main = "Two-key plot")

# Grafo interactivo
reglas_top20 <- head(sort(reglas, by = "lift", decreasing = TRUE), 20)
plot(reglas_top20, method = "graph", engine = "htmlwidget")


# ---------- 9. FILTRADO DE REGLAS ----------
cat("\n--- 9. FILTRADO DE REGLAS ---\n")

# 9a. Reglas con precio_alto como consecuente
cat("\nReglas con consecuente 'precio_alto':\n")
reglas_precio_alto <- apriori(
  trans_train,
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
  trans_train,
  parameter  = list(supp = 0.005, conf = 0.4, minlen = 2),
  appearance = list(rhs = "precio_grupo=precio_bajo", default = "lhs"),
  control    = list(verbose = FALSE)
)
reglas_precio_bajo <- reglas_precio_bajo[!is.redundant(reglas_precio_bajo)]
reglas_precio_bajo <- sort(reglas_precio_bajo, by = "lift", decreasing = TRUE)
inspect(head(reglas_precio_bajo, 15))

# 9c. Reglas con economy_si como consecuente
cat("\nReglas con consecuente 'economy_si':\n")
reglas_economy_si <- apriori(
  trans_train,
  parameter  = list(supp = 0.01, conf = 0.5, minlen = 2),
  appearance = list(rhs = "economy_lbl=economy_si", default = "lhs"),
  control    = list(verbose = FALSE)
)
reglas_economy_si <- reglas_economy_si[!is.redundant(reglas_economy_si)]
reglas_economy_si <- sort(reglas_economy_si, by = "lift", decreasing = TRUE)
inspect(head(reglas_economy_si, 10))

# 9d. Reglas con economy_no como consecuente (Premium)
cat("\nReglas con consecuente 'economy_no':\n")
reglas_economy_no <- apriori(
  trans_train,
  parameter  = list(supp = 0.01, conf = 0.5, minlen = 2),
  appearance = list(rhs = "economy_lbl=economy_no", default = "lhs"),
  control    = list(verbose = FALSE)
)
reglas_economy_no <- reglas_economy_no[!is.redundant(reglas_economy_no)]
reglas_economy_no <- sort(reglas_economy_no, by = "lift", decreasing = TRUE)
inspect(head(reglas_economy_no, 10))

#  Reglas interesantes
cat("\nReglas de alta calidad (lift > 2.0 & support > 0.05 & confidence > 0.7):\n")
reglas_interesantes <- subset(
  reglas,
  subset = lift > 2.0 & support > 0.05 & confidence > 0.7
)
reglas_interesantes <- reglas_interesantes[!is.redundant(reglas_interesantes)]
cat("Número de reglas interesantes tras filtro estricto:", length(reglas_interesantes), "\n")
inspect(sort(reglas_interesantes, by = "lift", decreasing = TRUE))

# ---------- 10. REGLAS REDUNDANTES ----------
cat("\n--- 10. REGLAS REDUNDANTES ---\n")
redundantes <- is.redundant(reglas)
cat("Distribución redundantes / no redundantes:\n")
print(table(redundantes))

reglas_nr <- reglas[!redundantes]
cat("Reglas originales:", length(reglas), "| Tras eliminar redundantes:", length(reglas_nr), "\n")


# ---------- 11. ITEMSETS FRECUENTES CON APRIORI ----------
cat("\n--- 11. ITEMSETS FRECUENTES (Apriori) ---\n")
itemsets_frec <- apriori(
  trans_train,
  parameter = list(target = "frequent itemsets", supp = 0.02, minlen = 1),
  control   = list(verbose = FALSE)
)
cat("\nTop 20 itemsets frecuentes por soporte:\n")
inspect(head(sort(itemsets_frec, by = "support", decreasing = TRUE), 20))

# ---------- 12. ECLAT ----------
cat("\n--- 12. ECLAT: ITEMSETS FRECUENTES ---\n")

itemsets_eclat <- eclat(
  trans_train,
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
# Usamos directamente 'trans_train' y 'trans_test' generados desde model_data.RData

reglas_train <- apriori(
  trans_train,
  parameter = list(supp = 0.01, conf = 0.5, minlen = 2),
  control   = list(verbose = FALSE)
)
reglas_train <- sort(reglas_train, by = "lift", decreasing = TRUE)
reglas_train_top <- head(reglas_train, 20)

# Medición en el conjunto de prueba (Test) oficial
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

cat("\nComparación Train vs Test (Top 20 reglas basadas en la partición model_data.RData):\n")
print(comparacion)

# ---------- 14. ANÁLISIS DIRIGIDO: PRECIO ALTO ----------
cat("\n--- 14. ANÁLISIS DIRIGIDO: ¿QUÉ COMBINACIONES PREDICEN PRECIO ALTO? ---\n")
reglas_ph_final <- apriori(
  trans_train,
  parameter  = list(supp = 0.003, conf = 0.35, minlen = 2),
  appearance = list(rhs = "precio_grupo=precio_alto", default = "lhs"),
  control    = list(verbose = FALSE)
)
reglas_ph_final <- reglas_ph_final[!is.redundant(reglas_ph_final)]
reglas_ph_final <- sort(reglas_ph_final, by = "lift", decreasing = TRUE)

cat("\nTop 15 reglas hacia precio_alto (sin redundantes) en Train:\n")
inspect(head(reglas_ph_final, 15))

# ---------- 14b. ANÁLISIS DIRIGIDO: PERFIL DE AEROLÍNEAS ----------
cat("\n--- 14b. ANÁLISIS DIRIGIDO: ¿QUÉ DEFINE A CADA AEROLÍNEA? ---\n")

# Analizamos las aerolíneas dominantes que aparecieron en tu exploración inicial
aerolineas <- c("airline=Delta", "airline=American Airlines", "airline=United", "airline=Spirit Airlines")

for (aero in aerolineas) {
  cat("\nReglas que predicen:", aero, "\n")
  
  reglas_aero <- apriori(
    trans_train,
    parameter  = list(supp = 0.01, conf = 0.4, minlen = 2),
    appearance = list(rhs = aero, default = "lhs"),
    control    = list(verbose = FALSE)
  )
  
  reglas_aero <- reglas_aero[!is.redundant(reglas_aero)]
  reglas_aero <- sort(reglas_aero, by = "lift", decreasing = TRUE)
  
  # Mostramos las 5 mejores por aerolínea
  inspect(head(reglas_aero, 5))
}

# ---------- 15. GUARDADO DE RESULTADOS ----------
cat("\n--- 15. GUARDADO DE RESULTADOS ---\n")
out_dir <- file.path(getwd(), "data", "interim")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Guardar reglas del conjunto Train sin redundancia
reglas_export <- as(reglas_nr, "data.frame") |>
  as_tibble() |>
  arrange(desc(lift))

write.csv(
  reglas_export,
  file      = file.path(out_dir, "association_rules.csv"),
  row.names = FALSE
)

# Guardar las transacciones de entrenamiento
saveRDS(trans_train, file.path(out_dir, "transactions.rds"))

