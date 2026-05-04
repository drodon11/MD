# scripts/02_preprocessing.R
# Script de Preprocesamiento: Codificación de NAs, Detección de Outliers e Imputación (KNN)

list.of.packages = c("ggplot2", "dplyr", "visdat", "naniar", "DataExplorer", 
                     "VIM", "tidyr", "mice") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages, dependencies = TRUE)
lapply(list.of.packages, require, character.only = TRUE)

input_path <- file.path(getwd(), "data", "interim", "flightprices_interim.rds")
if(!file.exists(input_path)) stop("Ejecuta primero 01_clean_interim.R")
dd <- readRDS(input_path)

# Convertir variables cualitativas a factor
cat("\n--- 0. CONVERSIÓN DE FACTORES ---\n")
cols_factor <- c("startApt", "destApt", "airline", "equipment", "economy", "nonStop")
dd[cols_factor] <- lapply(dd[cols_factor], as.factor)

# -------------------------------------------------------------------------
# 1. VISUALIZACIÓN EDA (Exploratory Data Analysis) INICIAL
# -------------------------------------------------------------------------
cat("\n--- 1. VISUALIZACIÓN EDA INICIAL ---\n")

# Distribución general de los tipos de datos
vis_dat(dd)

# Distribución de valores perdidos (NAs originales)
vis_miss(dd) + labs(title = "Missing Data Inicial")

# Histograma de variables numéricas
plot_histogram(dd)

# -------------------------------------------------------------------------
# 2. CODIFICACIÓN DE LOS NA (Valores perdidos encubiertos)
# -------------------------------------------------------------------------
cat("\n--- 2. CODIFICACIÓN DE NAs OCULTOS ---\n")

# Vacíos o "None" en equipment -> NA
dd$equipment <- as.character(dd$equipment)
total_err_eq <- sum(dd$equipment %in% c("", "None"), na.rm=TRUE)
dd <- dd %>% replace_with_na(replace = list(equipment = c("", "None")))
dd$equipment <- as.factor(dd$equipment)
cat("Corregidos", total_err_eq, "valores vacíos/None en 'equipment'.\n")

# Ceros imposibles en distancias -> NA
total_err_dist <- sum(dd$travelDistance == 0, na.rm=TRUE)
dd <- dd %>% replace_with_na(replace = list(travelDistance = 0))
cat("Corregidos", total_err_dist, "valores '0' en 'travelDistance'.\n")

# -------------------------------------------------------------------------
# 3. IMPUTACIÓN DE VALORES PERDIDOS MEDIANTE MICE
# -------------------------------------------------------------------------
cat("\n--- 3. IMPUTACIÓN MICE ---\n")

# Seleccionamos solo las variables numéricas para la imputación
tipos <- sapply(dd, class)
varNum <- names(tipos)[which(tipos %in% c("numeric", "integer", "numeric"))]

# Aplicamos MICE
cat("Imputando variables numéricas con MICE...\n")
library(mice)

mice_plot <- aggr(dd, col = c("navyblue", "yellow"), numbers = TRUE, sortVars = TRUE,
                  labels = names(dd), cex.axis = .7, gap = 3, ylab = c("Missing data", "Pattern"))

# Multiple impute the missing values.
# m = 5 indica que generamos 5 datasets imputados.
# maxit = 50 fija el número máximo de iteraciones.
# method = 'pmm' usa Predictive Mean Matching.
imputed_Data <- mice(dd, m = 5, maxit = 50, method = "pmm", ridge = 1e-3, 
                     seed = 500)

summary(imputed_Data)

stripplot(imputed_Data, travelDistance, pch = 19, xlab = "Imputation number")
stripplot(imputed_Data, equipment, pch = 19, xlab = "Imputation number")
stripplot(imputed_Data, segmentDistance, pch = 19, xlab = "Imputation number")

dd <- mice::complete(imputed_Data, action = "long")

# -------------------------------------------------------------------------
# 4. DETECCIÓN Y TRATAMIENTO DE OUTLIERS
# -------------------------------------------------------------------------

detectar_clasificar_outliers <- function(dd, varNum, alpha = 0.975, z_cut = 3, k_lof = 5, lof_cut = 1.5, plot_pca = TRUE) {
  
  # Paquetes necesarios
  if (!requireNamespace("robustbase", quietly = TRUE)) stop("Falta instalar 'robustbase'")
  if (!requireNamespace("dbscan", quietly = TRUE)) stop("Falta instalar 'dbscan'")
  
  # 1. Selección de variables numéricas
  datos_num <- dd[, varNum, drop = FALSE]
  
  # 2. Escalado
  data_scaled <- scale(datos_num)
  
  # 3. Mahalanobis robusto
  cov_rob <- robustbase::covMcd(data_scaled)
  md <- mahalanobis(data_scaled, center = cov_rob$center, cov = cov_rob$cov)
  threshold <- qchisq(alpha, df = ncol(data_scaled))
  out_md <- which(md > threshold)
  
  # 4. LOF
  lof_scores <- dbscan::lof(data_scaled, k = k_lof)
  out_lof <- which(lof_scores > lof_cut)
  
  # 5. Número de variables extremas por individuo
  vars_extremas_lista <- apply(data_scaled, 1, function(x) which(abs(x) > z_cut))
  n_vars_extremas <- sapply(vars_extremas_lista, length)
  
  # 6. Nombre de las variables extremas
  nombres_vars_extremas <- lapply(vars_extremas_lista, function(idx) {
    if (length(idx) == 0) return(NA_character_)
    colnames(data_scaled)[idx]
  })
  
  # 7. Clasificación
  clasificacion <- rep("no_outlier", nrow(dd))
  
  for (i in seq_len(nrow(dd))) {
    
    en_md <- i %in% out_md
    en_lof <- i %in% out_lof
    n_ext <- n_vars_extremas[i]
    
    if (en_md && en_lof) {
      if (n_ext == 1) {
        clasificacion[i] <- "posible_error_imputar"
      } else if (n_ext >= 2) {
        clasificacion[i] <- "outlier_real"
      } else {
        clasificacion[i] <- "outlier_real"
      }
    } else if (en_md && !en_lof) {
      if (n_ext == 1) {
        clasificacion[i] <- "posible_error_imputar"
      } else {
        clasificacion[i] <- "dudoso_global"
      }
    } else if (!en_md && en_lof) {
      clasificacion[i] <- "outlier_local"
    }
  }
  
  # 8. Tabla resumen
  resumen <- data.frame(
    id = seq_len(nrow(dd)),
    md = md,
    md_outlier = seq_len(nrow(dd)) %in% out_md,
    lof = lof_scores,
    lof_outlier = seq_len(nrow(dd)) %in% out_lof,
    n_vars_extremas = n_vars_extremas,
    clasificacion = clasificacion,
    stringsAsFactors = FALSE
  )
  
  # Añadimos variables extremas en texto
  resumen$variables_extremas <- sapply(nombres_vars_extremas, function(x) {
    if (all(is.na(x))) return(NA_character_)
    paste(x, collapse = ", ")
  })
  
  # 9. PCA opcional
  pca <- prcomp(data_scaled)
  
  if (plot_pca) {
    plot(
      pca$x[, 1], pca$x[, 2],
      col = "black", pch = 1,
      xlab = "PC1", ylab = "PC2",
      main = "PCA con outliers detectados"
    )
    points(
      pca$x[out_md, 1], pca$x[out_md, 2],
      col = "red", pch = 19
    )
    points(
      pca$x[out_lof, 1], pca$x[out_lof, 2],
      col = "blue", pch = 17
    )
    legend(
      "topright",
      legend = c("Normal", "Mahalanobis", "LOF"),
      col = c("black", "red", "blue"),
      pch = c(1, 19, 17)
    )
  }
  
  # 10. Subconjuntos útiles
  posibles_errores <- resumen[resumen$clasificacion == "posible_error_imputar", ]
  outliers_reales <- resumen[resumen$clasificacion == "outlier_real", ]
  dudosos_globales <- resumen[resumen$clasificacion == "dudoso_global", ]
  outliers_locales <- resumen[resumen$clasificacion == "outlier_local", ]
  
  # 11. Salida
  return(list(
    resumen = resumen,
    posibles_errores = posibles_errores,
    outliers_reales = outliers_reales,
    dudosos_globales = dudosos_globales,
    outliers_locales = outliers_locales,
    out_md = out_md,
    out_lof = out_lof,
    threshold_md = threshold,
    pca = pca,
    data_scaled = data_scaled
  ))
}

cat("\n--- 3. DETECCIÓN DE OUTLIERS (LOF) ---\n")

resultado_outliers <- detectar_clasificar_outliers(dd, varNum)

## Ver la tabla completa
resultado_outliers$resumen

## Ver solo los posibles errores para imputar
resultado_outliers$posibles_errores

## Ver outliers reales
resultado_outliers$outliers_reales

## Ver dudosos globales
resultado_outliers$outliers_reales

## Ver dudosos globales
resultado_outliers$dudosos_globales

## Ver outliers locales
resultado_outliers$outliers_locales

# Si quieres convertir a NA los posibles errores
dd_limpio <- dd

for (fila in resultado_outliers$posibles_errores$id) {
  vars_fila <- unlist(strsplit(resultado_outliers$resumen$variables_extremas[fila], ", "))
  dd_limpio[fila, vars_fila] <- NA
}


### Lógica que está usando la función
## Está siguiendo exactamente la idea:

###### detectado por ambos + varias variables extremas → outlier_real
###### detectado por ambos + una sola variable extrema → posible_error_imputar
###### solo Mahalanobis + una sola variable extrema → posible_error_imputar
###### solo Mahalanobis + varias variables extremas → dudoso_global
###### solo LOF → outlier_local

### Importante: Esto no sustituye criterio de dominio. Es una clasificación asistida, 
### no una verdad absoluta.

## La decisión final debe ser:
## imputar si parece error puntual o valor inconsistente
## mantener si representa un caso real extremo
## revisar manualmente si cae en dudoso_global

table(resultado_outliers$resumen$clasificacion)

# -------------------------------------------------------------------------
# 5. IMPUTACIÓN DE VALORES PERDIDOS POR OUTLIERS
# -------------------------------------------------------------------------
cat("\n--- 5. IMPUTACIÓN VALORES PERDIDOS -----\n")

imputed_Data <- mice(dd_limpio, m = 5, maxit = 50, method = "pmm", ridge = 1e-3, 
                     seed = 500)

dd <- mice::complete(imputed_Data, 2)

# Eliminamos las variables que no son necesarias 
dd[, c(".imp", ".id")] <- NULL

out_path_clean <- file.path(getwd(), "data", "interim", "flightprices_preprocessed.rds")
saveRDS(dd, out_path_clean)

