# ==============================================================================
#                 ACP: Análisis de Componentes Principales
# ==============================================================================
# Paquetes
library(dplyr)
library(ggplot2)
library(factoextra)

# Cargamos el rds después del preprocessing
input_path <- file.path(getwd(), "data", "interim", "flightprices_preprocessed.rds")
dd <- readRDS(input_path)

# Extraemos las variables numéricas
varNum <- which(sapply(dd, is.numeric))
dcon <- dd[, varNum]

cat("Variables numéricas usadas en el PCA:\n")
print(names(dcon))

# Estandarizar (media 0, sd 1)
dcon_scaled <- scale(dcon)
<<<<<<< HEAD
=======

as_tibble(head(dcon)) %>% print(n=6)
apply(dcon_scaled, 2, sd)

pca <- prcomp(dcon, center = TRUE, scale. = TRUE)
>>>>>>> main

# Vista rápida
as_tibble(head(dcon)) %>% print(n=6)

apply(dcon_scaled, 2, sd)

# AJUSTE DEL ACP

#prcomp usa SVD; center=TRUE, scale.=TRUE también estandariza internamente
pca <- prcomp(dcon_scaled, center = TRUE, scale. = TRUE)

#variancia explicada
var_exp <- pca$sdev^2
var_exp_ratio <- var_exp / sum(var_exp)
cum_exp_ratio <- cumsum(var_exp_ratio)

cat("\n--- Varianza Explicada por Componente (Ratio) ---\n")
print(round(var_exp_ratio, 4))

cat("\n--- Varianza Acumulada (Ratio) ---\n")
print(round(cum_exp_ratio, 4))

# SCREE PLOT Y VARIANZA ACUMULADA

scree_df <- data.frame(
  PC = paste0("PC", seq_along(var_exp_ratio)),
  VarExp = var_exp_ratio,
  CumExp = cum_exp_ratio
)

#scree plot
plot_marginal <- ggplot(scree_df, aes(x = seq_along(VarExp), y = VarExp)) +
  geom_line(color = "blue") + 
  geom_point(size = 2, color = "darkblue") +
  labs(x = "Componente Principal", y = "Proporción varianza explicada",
       title = "Scree plot (Inercia Marginal)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq_along(scree_df$VarExp))

print(plot_marginal)

#varianza acumulada
plot_acumulada <- ggplot(scree_df, aes(x = seq_along(CumExp), y = CumExp)) +
  geom_line(color = "red") + 
  geom_point(size = 2, color = "darkred") +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "black") +
  labs(x = "Componente Principal", y = "Varianza acumulada",
       title = "Varianza Acumulada (>80%)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq_along(scree_df$CumExp))

print(plot_acumulada)

# CARGAS (loadings) Y SCORES

#revisamos cómo contribuyen las variables originales a los 2 primeros componentes
k <- which(cum_exp_ratio >= 0.80)[1]
cat("\n--- Loadings (Pesos de las variables en PC1, PC2, PC3 y PC4) ---\n")
loadings <- pca$rotation[, 1:4]
print(round(loadings, 3))

scores <- pca$x
print(head(scores[, 1:4]))

# biplot
<<<<<<< HEAD
biplot(pca, cex = c(0.5, 0.7), main = "Biplot PCA (PC1 vs PC2)")

# PROYECCIÓN Y RECONSTRUCCIÓN

#proyectar datos a k componentes
scores_k <- pca$x[, 1:k, drop = FALSE]

#reconstrucción aproximada (desde scores_k -> espacio original estandarizado)
Xstd_hat <- scores_k %*% t(pca$rotation[, 1:k])

#"desestandarizar" para volver a escala original
means <- attr(dcon_scaled, "scaled:center")
sds <- attr(dcon_scaled, "scaled:scale")
Xrec <- sweep(Xstd_hat, 2, sds, `*`)
Xrec <- sweep(Xrec, 2, means, `+`)

cat("\n--- Datos Reconstruidos (primeras 6 filas) ---\n")
as_tibble(head(Xrec))

# PIPELINE PARA EL USO DE MODELOS

pc_df <- as.data.frame(pca$x[, 1:k])
varCat <- which(!sapply(dd, is.numeric))
df_model <- cbind(pc_df, dd[, varCat, drop=FALSE])

cat("\n--- Dataset listo ---\n")
print(head(df_model))

# interpretación de cargas y biplot
  # Vectores largos: variables con alta contribución al componente
  # Ángulos pequeños entre vectores: variables CORRELACIONADAS
  # Puntos (observaciones) próximos: perfiles similares en variables originales
  # Signo de la carga: dirrección de la relación con el componente

=======
biplot <- fviz_pca_biplot(pca, 
                                     geom.ind = "point",   
                                     col.ind = "black",    
                                     alpha.ind = 0.2,      
                                     col.var = "red",      
                                     repel = TRUE,         
                                     title = "Biplot PCA (PC1 vs PC2")

print(biplot)
>>>>>>> main
