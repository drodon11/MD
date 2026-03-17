# ==============================================================================
#                           Profiling de los Clústeres
# ==============================================================================

# Carga de paquetes
list.of.packages <- c("FactoMineR", "dplyr") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

library(FactoMineR)
library(dplyr)

# ------------------------------------------------------------------------------
# Cargar los datos y preparar variables
# ------------------------------------------------------------------------------
cat("\n--- Cargando datos clusterizados ---\n")
input_path <- file.path(getwd(), "data", "interim", "flightprices_clustered.rds")
dd_clust <- readRDS(input_path)

# Separamos las variables según su tipo
varNum <- names(select_if(dd_clust, is.numeric))
varCat <- names(select_if(dd_clust, is.factor))
varCat <- varCat[varCat != "cluster"] # Quitamos la variable objetivo

# ==============================================================================
# Script Karina (Basado en ProfilingCleanRevisited.R)
# Generación masiva de Plots y Test Clásicos (ANOVA / Chi-cuadrado)
# ==============================================================================

# Profiling de Variables Numéricas (Boxplots y ANOVA)
cat("\n--- Analizando Variables Numéricas ---\n")
for(v in varNum) {
  # Test ANOVA para ver si la media de la variable cambia según el clúster
  p_val <- summary(aov(dd_clust[[v]] ~ dd_clust$cluster))[[1]][["Pr(>F)"]][1]
  
  # Si el p-value es < 0.05, la variable es significativa para diferenciar grupos
  if(!is.na(p_val) && p_val < 0.05) {
    cat("Significativa:", v, "(p-value:", format(p_val, scientific = TRUE), ")\n")
    
    # Boxplot de la variable numérica dividida por clúster
    boxplot(dd_clust[[v]] ~ dd_clust$cluster, 
            main = paste("Boxplot de", v, "por Clúster"),
            xlab = "Clúster", ylab = v, col = 2:4) # Colores para los 3 clústeres
  }
}

# Profiling de Variables Categóricas (Barplots y Chi-Cuadrado)
cat("\n--- Analizando Variables Categóricas ---\n")
for(v in varCat) {
  # Test Chi-cuadrado para ver si la proporción cambia según el clúster
  # suppressWarnings para evitar el aviso si alguna categoría tiene pocos vuelos
  p_val <- suppressWarnings(chisq.test(table(dd_clust[[v]], dd_clust$cluster))$p.value)
  
  if(!is.na(p_val) && p_val < 0.05) {
    cat("Significativa:", v, "(p-value:", format(p_val, scientific = TRUE), ")\n")
    
    # Barplot apilado (Spineplot) de proporciones
    tabla_prop <- prop.table(table(dd_clust[[v]], dd_clust$cluster), margin = 2)
    barplot(tabla_prop, 
            main = paste("Distribución de", v, "por Clúster"),
            xlab = "Clúster", ylab = "Proporción", 
            col = heat.colors(length(unique(dd_clust[[v]]))),
            legend = rownames(tabla_prop), args.legend = list(x = "topright", cex = 0.7))
    # Histograma de proporciones para mayor claridad
    tabla_freq <- table(dd_clust$cluster, dd_clust[[v]])
    barplot(tabla_freq, 
            beside = TRUE,
            main = paste("Distribución de", v, "por Clúster (Agrupado)"),
            xlab = v, ylab = "Cantidad de Vuelos", 
            col = c("#4DAF4A", "#377EB8", "#E41A1C"),
            legend = paste("Clúster", rownames(tabla_freq)), 
            args.legend = list(x = "topright", cex = 0.8))
  }
}

# ==============================================================================
# Script Sergi (Lebart)
# Perfilado profundo (Sobre/Infrarrepresentación)
# ==============================================================================

# Buscamos en qué columna está el clúster
idx_cluster <- which(names(dd_clust) == "cluster")

# Ejecutamos el test de Lebart
res_catdes <- catdes(dd_clust, num.var = idx_cluster)

cat("\n--- Variables que más definen a CADA CLÚSTER (v.test) ---\n")
cat("* Si v.test > 1.96: La característica es inusualmente ALTA en ese grupo.\n")
cat("* Si v.test < -1.96: La característica es inusualmente BAJA en ese grupo.\n\n")

# Imprimimos los resultados
print(res_catdes)

plot(res_catdes)
