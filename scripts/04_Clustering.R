# ==============================================================================
#                             Clustering
# ==============================================================================

# Instalación y carga de paquetes necesarios
list.of.packages <- c("dplyr", "fpc", "reshape2", "tidyr", "ggplot2", "stats", 
                      "cluster", "factoextra", "colorspace", "patchwork", 
                      "tidyverse", "ggpubr", "NbClust", "HDclassif", "clustMixType", 
                      "clusterSim", "pracma", "DataVisualizations", "entropy",
                      "clevr", "dendextend", "ggdendro", "gridExtra", "mclust", "clusterSim") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

library(cluster)
library(dplyr)
library(ggplot2)
library(ggdendro)
library(factoextra)
library(mclust)
library(DataVisualizations)
library(clusterSim)
library(fpc)

# ------------------------------------------------------------------------------
# Carga de datos (CORREGIDO AL RDS PREPROCESADO)
# ------------------------------------------------------------------------------
input_path <- file.path(getwd(), "data", "interim", "flightprices_preprocessed.rds")
dd <- readRDS(input_path)

# 1 - Extraer las variables numéricas del dataset original
cat("\n--- Extrayendo las variables numéricas del dataset original ---\n")
varNum <- which(sapply(dd, is.numeric))
dd_num <- dd[, varNum]

# 2 - Escalar los datos numéricos (media 0, desviación típica 1)
cat("\n--- Escalando los datos numéricos ---\n")
dd_num_scaled <- scale(dd_num)


# ==============================================================================
# CLUSTERING K-MEANS (Solo numéricas)
# ==============================================================================
cat("\n--- Evaluando K Óptimo para K-Means ---\n")
set.seed(101) 
(km_clusters <- kmeans(x = dd_num_scaled, centers = 4, nstart = 50, trace = FALSE))

# A. Búsqueda de hiperparámetros (Codo y Silueta)
plot_codo_km <- fviz_nbclust(dd_num_scaled, kmeans, method = "wss", k.max = 10) +
  labs(title = "K-Means: Elbow Method")
print(plot_codo_km)

plot_sil_km <- fviz_nbclust(dd_num_scaled, kmeans, method = "silhouette", k.max = 10) +
  labs(title = "K-Means: Silhouette method")

print(plot_sil_km)

# B. Ejecución del modelo K-Means Final
# Con los resultados de antes vemos que debe ser k = 3
k_kmeans <- 3
km_clusters <- kmeans(x = dd_num_scaled, centers = k_kmeans, nstart = 50, trace = FALSE)

# C. Validación: Silueta individual
sil <- silhouette(km_clusters$cluster, dist(dd_num_scaled))
fviz_silhouette(sil) + labs(title = "Silhouette Analysis for k=3")

# D. Validación: Calinski-Harabasz (Índice de separación)
ch_index <- fpc::calinhara(dd_num_scaled, km_clusters$cluster)
cat("\nCalinski-Harabasz Index:", ch_index, "\n")

# E. SSE: Sum of Squared Errors
(sse <- km_clusters$tot.withinss)

# F. Método del codo
sse <- c()
for (k in 1:10) {
  kmeans_model <- kmeans(dd_num_scaled, k)
  sse[k] <- kmeans_model$tot.withinss
}

plot <- ggplot(data.frame(x = 1:10, y = sse), aes(x, y)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of clusters", y = "SSE", title = "Elbow Method") 

print(plot)

# G. Método del gap estadístico
gap_stat <- clusGap(dd_num_scaled, FUN = kmeans, nstart = 10, K.max = 10, B = 10)
plot(gap_stat, main="Gap statistic plot")

# H. Índice de Davies-Bouldin
DB_index <- clusterSim::index.DB(dd_num_scaled, km_clusters$cluster)
DB_index

DB_indexes <- vector()
for (k in 1:10) {
  kmeans_result <- kmeans(dd_num_scaled, centers = k)
  DB_index <- clusterSim::index.DB(dd_num_scaled, kmeans_result$cluster)
  DB_indexes[k] <- DB_index$DB
}

DB_df <- data.frame(k = 1:10, DB = DB_indexes)
ggplot(DB_df, aes(x = k, y = DB)) + geom_point() + geom_line()

# I. Entropia
cluster_labels <- km_clusters$cluster

cluster_entropy <- entropy::entropy(table(cluster_labels))
cluster_entropy <- sapply(unique(cluster_labels), function(i) entropy::entropy(cluster_labels == i))

mean_entropy <- mean(cluster_entropy)
mean_entropy

entropy_values <- vector()
for (k in 1:10) {
  kmeans_result <- kmeans(dd_num_scaled, centers = k)
  cluster_labels <- kmeans_result$cluster
  cluster_entropy <- sapply(unique(cluster_labels), function(i) 
    entropy::entropy(cluster_labels == i))
  entropy_values[k] <- mean(cluster_entropy)
}

entropy_df <- data.frame(k = 1:10, entropy = entropy_values)
ggplot(entropy_df, aes(x = k, y = entropy)) + geom_point() + geom_line()

# J. Representación bidimensional del K-Means
plot_kmeans <- fviz_cluster(
  list(data = dd_num_scaled, cluster = km_clusters$cluster), 
  ellipse.type = "norm",
  geom = "point",
  stand = FALSE,
  palette = "jco", 
  ggtheme = theme_classic(),
  main = "Proyección Espacial K-Means (k=3)"
)

print(plot_kmeans)

cat("\nModelo K-Means finalizado. Inercia explicada:", 
    round(km_clusters$betweenss / km_clusters$totss * 100, 2), "%\n")


# ==============================================================================
# CLUSTERING JERÁRQUICO (solamente variables numéricas)
# ==============================================================================

# ==============================================================================
# CLUSTERING JERÁRQUICO (solamente variables numéricas)
# ==============================================================================

# 1 - Calcular la matriz de distancias usando la dist. euclideana
cat("\n--- Calculando la matriz de distancias usando la dist. euclideana ---\n")
distancia_Euc <- dist(dd_num_scaled, method="euclidean")

# 2 - escalar los datos numéricos (media 0, desviación típica 1)
cat("\n--- Escalando los datos numéricos ---\n")
dd_num_scaled <- scale(dd_num)

# 3 - calcular la matriz de distancias usando la dist. euclideana
cat("\n--- Calculando la matriz de distancias usando la dist. euclideana ---\n")
distancia_Euc<- dist(dd_num_scaled, method="euclidean")

# 4 - agrupación single (une grupos a partir de la menor distancia entre elementos de ambos clusters)
# Suele producir el efecto de encadenamiento
cat("\n--- Realizando agrupación single ---\n")
agr_Single <- hclust(distancia_Euc, method="single")

# 5 - agrupación complete (usa la mayor distancia entre elementos de dos grupos)
# Tiende a formar clusters más compactos
cat("\n--- Realizando agrupación complete ---\n")
agr_Complete <- hclust(distancia_Euc, method="complete")

# 6 - enlace promedio (toma la distancia media entre todos los pares de observaciones de dos clusters)
# Compromiso habitual entre el 'single' y 'complete'
cat("\n--- Realizando agrupación average ---\n")
agr_Average <- hclust(distancia_Euc, method="average")

# 7 - Mcquitty (actualiza las distancias entre clusters promediando de forma recursiva según las fusiones previas)
cat("\n--- Realizando agrupación mcquitty ---\n")
agr_Mcquitty <- hclust(distancia_Euc, method="mcquitty")

# 8 - Criterio Median (calcula las distancias a partir de centroides corregidos y puede verse afectado por inversiones en el dendograma)
cat("\n--- Realizando agrupación median ---\n")
agr_Median <- hclust(distancia_Euc, method="median")

# 9 - Centroide (depende de la distancia entre centroides de clusters)
# Es intuitivo, aunque no siempre conserva bien la estructura jerárquica
cat("\n--- Realizando agrupación centroide ---\n")
agr_Centroid <- hclust(distancia_Euc, method="centroid")

# 10 - Ward.D o Ward.D2 (minimiza el incremento de variabilidad interna en cada fusión)
# Suele producir grupos compactos y bien separados --> método más usado
cat("\n--- Realizando agrupación Ward ---\n")
agr_Ward <- hclust(distancia_Euc, method="ward.D2")

# 11 - Representación del dendograma de Ward
# Para inspeccionar visualmente posibles cortes del árbol
cat("\n--- Representación del dendograma de Ward ---\n")
plot(agr_Ward, main="H.Clustering with euclidean distance and WARD method")
rect.hclust(agr_Ward, k=3, border=3)

# Por si queremos hacer la representación de todos los dendogramas en una sola imagen
cat("\n--- Representación de todos los dendogramas realizados ---\n")
### Función para convertir hclust a ggplot
plot_dendrogram <- function(hclust_obj, method_name) {
  dendro_data <- ggdendro::dendro_data(hclust_obj)
  
  ggplot(ggdendro::segment(dendro_data)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    theme_minimal() +
    ggtitle(method_name) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    )
}

### Lista de dendrogramas con nombres
dendograms <- list(
  plot_dendrogram(agr_Single, "Single"),
  plot_dendrogram(agr_Complete, "Complete"),
  plot_dendrogram(agr_Average, "Average"),
  plot_dendrogram(agr_Mcquitty, "McQuitty"),
  plot_dendrogram(agr_Median, "Median"),
  plot_dendrogram(agr_Centroid, "Centroid"),
  plot_dendrogram(agr_Ward, "Ward.D2")
)

# mostrar todos los dendogramas
gridExtra::grid.arrange(grobs = dendograms, ncol = 3)

# cálculo de correlaciones (cuánto mayor sea la correlación, mejor refleja el árbol la estructura aproximada de los datos)
info <- data.frame(metricas = c("Single", "Complete", "Average", "McQuitty", "Median", 
                                "Centroid", "Ward"), 
                   correlaciones = c(cor(distancia_Euc, cophenetic(agr_Single)), 
                                     cor(distancia_Euc, cophenetic(agr_Complete)), 
                                     cor(distancia_Euc, cophenetic(agr_Average)), 
                                     cor(distancia_Euc, cophenetic(agr_Mcquitty)), 
                                     cor(distancia_Euc, cophenetic(agr_Median)), 
                                     cor(distancia_Euc, cophenetic(agr_Centroid)), 
                                     cor(distancia_Euc, cophenetic(agr_Ward))))

(info <- info[order(info$correlaciones, decreasing = TRUE), ])

# Coeficiente de Silhouette
grupos_hc_num <- cutree(agr_Ward, k = 3)
sil <- cluster::silhouette(grupos_hc_num, dist(dd_num_scaled))
avg_sil <- mean(sil[, 3])
summary(sil)

Silhouetteplot(as.matrix(dd_num_scaled), Cls = sil[, "cluster"], main='Silhouetteplot')

# Índice Davies-Bouldin

k_num <- 3
grupos_hc_num <- cutree(agr_Ward, k = k_num)

DB_index_hc <- clusterSim::index.DB(x = dd_num_scaled, cl = grupos_hc_num)
cat("El Índice Davies-Bouldin para k =", k_num, "es:", round(DB_index_hc$DB, 4), "\n")

DB_indexes_hc <- vector()

for (k in 2:10) {
  grupos_temp <- cutree(agr_Ward, k = k)
  DB_temp <- clusterSim::index.DB(x = dd_num_scaled, cl = grupos_temp)
  DB_indexes_hc[k] <- DB_temp$DB
}

DB_df_hc <- data.frame(k = 2:10, DB = DB_indexes_hc[2:10])

plot_DB_hc <- ggplot(DB_df_hc, aes(x = k, y = DB)) + 
  geom_point(color = "darkred", size = 3) + 
  geom_line(color = "red") +
  theme_minimal() +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "Índice Davies-Bouldin (H. Clustering)",
       x = "Number of clusters (k)", 
       y = "Davies-Bouldin Index (DB)")

print(plot_DB_hc)

# Índice de Calinski-Harabasz
CH_index_hc <- fpc::calinhara(dd_num_scaled, grupos_hc_num)
cat("El Índice Calinski-Harabasz para k =", k_num, "es:", round(CH_index_hc, 4), "\n")

CH_indexes_hc <- vector()

for (k in 2:10) {
  grupos_temp <- cutree(agr_Ward, k = k)
  CH_indexes_hc[k] <- fpc::calinhara(dd_num_scaled, grupos_temp)
}

CH_df_hc <- data.frame(k = 2:10, CH = CH_indexes_hc[2:10])

plot_CH_hc <- ggplot(CH_df_hc, aes(x = k, y = CH)) + 
  geom_point(color = "darkgreen", size = 3) + 
  geom_line(color = "green4") +
  theme_minimal() +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "Calinski-Harabasz Index (H. Clustering)",
       x = "Número de clústeres (k)", 
       y = "Índice Calinski-Harabasz (CH)")

print(plot_CH_hc)

# Entropía



# Adjusted Rand Index (K-Means vs Jerárquico)
grupos_jerarquico <- cutree(agr_Ward, k = 3)

ari_algoritmos <- adjustedRandIndex(km_clusters$cluster, grupos_jerarquico)
cat("ARI (K-Means vs Jerárquico):", round(ari_algoritmos, 4), "\n")
# ------------------------------------------------------------------------------
# CLUSTERING JERÁRQUICO (variables mixtas)
# ------------------------------------------------------------------------------

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