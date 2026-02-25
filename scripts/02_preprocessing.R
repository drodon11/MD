# scripts/02_preprocessing.R
# Script de Preprocesamiento: Codificación de NAs, Detección de Outliers e Imputación (KNN)

list.of.packages = c("ggplot2", "dplyr", "visdat", "naniar", "DataExplorer", 
                     "VIM", "tidyr") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)
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
# 3. DETECCIÓN Y TRATAMIENTO DE OUTLIERS
# -------------------------------------------------------------------------
cat("\n--- 3. DETECCIÓN DE OUTLIERS (IQR) ---\n")
nas_previs <- sum(is.na(dd))

# Guardamos copia para comparar distribuciones luego
dd_original <- dd 

# Función para eliminar outliers basada en el rango intercuartílico (IQR) del profesor
remove_outliers_iqr <- function(data, variable) {
  IQ <- IQR(data[[variable]], na.rm = TRUE)
  intInf <- quantile(data[[variable]], probs = 0.25, na.rm = TRUE) - 1.5 * IQ
  intSup <- quantile(data[[variable]], probs = 0.75, na.rm = TRUE) + 1.5 * IQ
  
  outliers_count <- sum(data[[variable]] < intInf | data[[variable]] > intSup, na.rm = TRUE)
  
  # Visualizar el corte
  print(
    ggplot(data, aes(y = .data[[variable]])) +
      geom_boxplot(fill = "orange") +
      geom_hline(yintercept = c(intInf, intSup), color="red", linetype="dashed") +
      labs(title = paste("Boxplot de", variable, "con umbrales IQR")) +
      theme_minimal()
  )
  
  data[[variable]][data[[variable]] < intInf | data[[variable]] > intSup] <- NA
  cat("Outliers convertidos a NA en", variable, ":", outliers_count, "\n")
  return(data)
}

# Aplicamos a variables clave (Precios altos, Vuelos excesivamente largos)
dd <- remove_outliers_iqr(dd, "totalPrice")
dd <- remove_outliers_iqr(dd, "travelDistance")

cat("Nuevos NAs generados por outliers:", sum(is.na(dd)) - nas_previs, "\n")

# -------------------------------------------------------------------------
# 4. IMPUTACIÓN DE VALORES PERDIDOS MEDIANTE KNN (con VIM)
# -------------------------------------------------------------------------
cat("\n--- 4. IMPUTACIÓN KNN (VIM) ---\n")

# Seleccionamos solo las variables numéricas para la imputación
tipos <- sapply(dd, class)
varNum <- names(tipos)[which(tipos %in% c("numeric", "integer", "numeric"))]

# OJO: Excluimos las fechas y horas
varNum <- setdiff(varNum, c("departure", "arrival", "flightDate"))

# Aplicamos KNN (K=5) usando VIM
cat("Imputando variables numéricas con KNN (k=5)...\n")

# VIM::kNN imputa directamente las columnas seleccionadas en un nuevo dataframe
# imp_var = FALSE evita que cree columnas extra con booleanos de qué se imputó
dd_imputed <- kNN(dd, variable = varNum, k = 5, imp_var = FALSE)

# Visualización sugerida por el profe: Comparar distribución Original vs Imputada
df_compare <- data.frame(
  Valor = c(dd_original$totalPrice, dd_imputed$totalPrice),
  Estado = rep(c("1. Original (Con NAs y Outliers)", "2. Imputado (Sin Outliers)"), each = nrow(dd))
)

print(
  ggplot(df_compare, aes(x = Valor, fill = Estado)) +
    geom_density(alpha = 0.4) +
    labs(title = "Impacto del Preprocesamiento en la Distribución de totalPrice",
         x = "Precio ($)", y = "Densidad") +
    theme_minimal() +
    scale_fill_manual(values = c("red", "blue"))
)

# Actualizamos el dataset
dd <- dd_imputed

