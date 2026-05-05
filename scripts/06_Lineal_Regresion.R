# Cargar librerias
list.of.packages <- c(
  "dplyr", "fpc", "reshape2", "tidyr", "ggplot2", "stats", 
  "cluster", "factoextra", "colorspace", "patchwork", 
  "tidyverse", "ggpubr", "NbClust", "HDclassif", "clustMixType", 
  "clusterSim", "pracma", "DataVisualizations", "entropy",
  "clevr", "dendextend", "ggdendro", "gridExtra", "idealista18",
  "GGally", "ggplot2", "caret")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages) > 0) {
  install.packages(new.packages)
}

invisible(lapply(list.of.packages, require, character.only = TRUE))
rm(list.of.packages, new.packages)

# Cargar la base de datos
input_path <- file.path(getwd(), "data", "interim", "flightprices_preprocessed.rds")
dd <- readRDS(input_path)

# Estructura del dataset
str(dd)
summary(dd)

# Conversión de variables categóricas
# Para que ninguna variable categórica se quede como numerica
tipos <- sapply(dd, class)
varCat <- names(tipos)[which(tipos %in% c("character", "factor"))]
varNum <- names(tipos)[which(tipos %in% c("integer", "numeric"))]
for(var in varCat) {
  dd[, var] <- as.factor(as.character(dd[,var]))
}

# Analisis Explorativo Previo
# Matriz de dispersión

# Permite detectar:
  # Relaciones aproximadamente lineales
  # Asociaciones fuertes entre predictores
  # Posibles observaciones atípicas
  # Estructuras no lineales

GGally::ggpairs(dd, columns = varNum)

# Correlaciones entre variables numericas
cor(dd[,varNum])

# Regresion Lineal Simple
simple_model <- lm()
summary(simple_model)

# Ecuacion estimada
coef(simple_model)

grafico + geom_smooth(method = "lm")



  





