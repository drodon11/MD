# ==============================================================================
#                             Clustering Jerárquico
# ==============================================================================

# Instalación y carga de paquetes necesarios
list.of.packages <- c("cluster", "dplyr") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

library(cluster)
library(dplyr)

# ------------------------------------------------------------------------------
# Carga de datos
# ------------------------------------------------------------------------------
input_path <- file.path(getwd(), "data", "interim", "flightprices_interim.rds")
dd <- readRDS(input_path)

# ------------------------------------------------------------------------------
# 10a. Descripción precisa de los datos usados
# ------------------------------------------------------------------------------
cat("\n--- 10a. Preparando datos y ajustando variables ---\n")

# Excluimos las fechas y horas porque distorsionan la métrica de distancia de Gower.
vars_to_exclude <- c("flightDate", "departure", "arrival")
dd_clust <- dd %>% select(-any_of(vars_to_exclude))

# Forzamos a que todas las variables cualitativas (texto, lógicas o binarias) sean factores.
dd_clust <- dd_clust %>% 
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.logical, as.factor) %>%
  # Convierte automáticamente cualquier columna con solo 2 valores a factor
  mutate(across(where(~ length(unique(na.omit(.x))) == 2), as.factor))

cat(paste("Dataset listo con", nrow(dd_clust), "registros (Base de datos completa).\\n"))

# ------------------------------------------------------------------------------
# 10b. Método de clustering, métricas y criterios de agregación
# ------------------------------------------------------------------------------
cat("\n--- 10b. Calculando Distancia de Gower y Método de Ward ---\n")

# Métrica: Disimilitud de Gower (la indicada para datos mixtos: numéricos y factores)
mat_dist <- daisy(dd_clust, metric = "gower")
mat_dist <- mat_dist^2

# Agregación: Método de Ward (ward.D2), que minimiza la varianza intra-clúster
res_hc <- hclust(mat_dist, method = "ward.D2")
cat("Modelo de Clustering Jerárquico creado exitosamente.\n")

# ------------------------------------------------------------------------------
# 10c. Dendrograma Resultante
# ------------------------------------------------------------------------------
cat("\n--- 10c. Generando Dendrograma ---\n")

# Gráfico del dendrograma (Exportar para la memoria del proyecto)
plot(res_hc, main = "Dendrograma de Vuelos (Métrica: Gower, Agregación: Ward)", 
     xlab = "Vuelos", sub = "", ylab = "Distancia (Inercia)", cex = 0.5)

# ------------------------------------------------------------------------------
# Validación matemática de k mediante el Coeficiente de Silueta
# ------------------------------------------------------------------------------
cat("\n--- Evaluando diferentes k con el Coeficiente de Silueta ---\n")

# Calculamos la silueta para k=2
grupos_k2 <- cutree(res_hc, k = 2)
sil_k2 <- silhouette(grupos_k2, mat_dist)
media_k2 <- summary(sil_k2)$avg.width

# Calculamos la silueta para k=3
grupos_k3 <- cutree(res_hc, k = 3)
sil_k3 <- silhouette(grupos_k3, mat_dist)
media_k3 <- summary(sil_k3)$avg.width

# Calculamos la silueta para k=4
grupos_k4 <- cutree(res_hc, k = 4)
sil_k4 <- silhouette(grupos_k4, mat_dist)
media_k4 <- summary(sil_k4)$avg.width

# Calculamos la silueta para k=5
grupos_k5 <- cutree(res_hc, k = 5)
sil_k5 <- silhouette(grupos_k5, mat_dist)
media_k5 <- summary(sil_k5)$avg.width

cat("Silueta media para k = 2:", media_k2, "\n")
cat("Silueta media para k = 3:", media_k3, "\n")
cat("Silueta media para k = 4:", media_k4, "\n")
cat("Silueta media para k = 5:", media_k5, "\n")

# ------------------------------------------------------------------------------
# 10d. Discusión del número final de clústeres
# ------------------------------------------------------------------------------
# Tras observar los saltos de inercia en el dendrograma y usando el
# método de Silhouette, definimos el punto de corte.
k_elegido <- 3

# Dibujamos las fronteras de los grupos en el dendrograma para visualizar el corte
rect.hclust(res_hc, k = k_elegido, border = 2:(k_elegido+1))

# Asignamos a cada vuelo su respectivo clúster y lo guardamos como factor
dd_clust$cluster <- as.factor(cutree(res_hc, k = k_elegido))

# ------------------------------------------------------------------------------
# 10e. Tabla con la descripción del tamaño de los clústeres
# ------------------------------------------------------------------------------
cat(paste("\n--- 10e. Tabla de tamaño de clústeres (k =", k_elegido, ") ---\n"))
tabla_tamanos <- table(dd_clust$cluster)
print(tabla_tamanos)

# Guardamos el dataset final con los clústeres para usarlo posteriormente
saveRDS(dd_clust, file.path(getwd(), "data", "interim", "flightprices_clustered.rds"))
cat("\nClustering finalizado y guardado en flightprices_clustered.rds\n")

