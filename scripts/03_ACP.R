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

as_tibble(head(dcon)) %>% print(n=6)
apply(dcon_scaled, 2, sd)

pca <- prcomp(dcon, center = TRUE, scale. = TRUE)

# Variancia explicada
var_exp <- pca$sdev^2
var_exp_ratio <- var_exp / sum(var_exp)
cum_exp_ratio <- cumsum(var_exp_ratio)

cat("\n--- Varianza Explicada por Componente (Ratio) ---\n")
print(round(var_exp_ratio, 4))

cat("\n--- Varianza Acumulada (Ratio) ---\n")
print(round(cum_exp_ratio, 4))

#Scree plot y variancia acumulada
scree_df <- data.frame(
  PC = paste0("PC", seq_along(var_exp_ratio)),
  VarExp = var_exp_ratio,
  CumExp = cum_exp_ratio
)

# Scree plot
plot_marginal <- ggplot(scree_df, aes(x = seq_along(VarExp), y = VarExp)) +
  geom_line(color = "blue") + 
  geom_point(size = 2, color = "darkblue") +
  labs(x = "Main Component", y = "Proportion of the explained variance",
       title = "Scree plot") +
  theme_minimal() +
  scale_x_continuous(breaks = seq_along(scree_df$VarExp))

print(plot_marginal)

# Varianza acumulada
plot_acumulada <- ggplot(scree_df, aes(x = seq_along(CumExp), y = CumExp)) +
  geom_line(color = "red") + 
  geom_point(size = 2, color = "darkred") +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "black") +
  labs(x = "Main Component", y = "Cumulative Variance",
       title = "Cumulative Variance (>80%)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq_along(scree_df$CumExp))

print(plot_acumulada)

# Loadings
# Revisamos cómo contribuyen las variables originales a los 2 primeros componentes
cat("\n--- Loadings (Pesos de las variables en PC1 y PC2) ---\n")
loadings <- pca$rotation[, 1:4]

loadings_df <- as.data.frame(loadings) %>%
  tibble::rownames_to_column(var = "Variable") %>%
  tidyr::pivot_longer(
    cols = starts_with("PC"),
    names_to = "Componente",
    values_to = "Carga"
  )

# Invertimos el orden de las variables para que al girar el gráfico 
# la primera variable de tu matriz aparezca arriba del todo, y no abajo.
loadings_df$Variable <- factor(loadings_df$Variable, levels = rev(rownames(loadings)))

# Generamos el grafico
plot_loadings <- ggplot(loadings_df, aes(x = Variable, y = Carga, fill = Carga)) +
  geom_col() + 
  
  # Añadimos una línea punteada en el 0 para distinguir visualmente positivo de negativo
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.6) +
  
  # Ponemos los 4 gráficos en una cuadrícula de 2x2
  facet_wrap(~ Componente, ncol = 2) + 
  
  # Giramos el gráfico para que las barras sean horizontales
  coord_flip() + 
  
  # Colores: Rojo (negativo), Blanco (cero), Azul (positivo)
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC", 
    midpoint = 0, guide = "none"
  ) +
  
  theme_bw() + 
  theme(
    strip.background = element_rect(fill = "#f0f0f0", color = "NA"),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.text.x = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(color = "gray80")
  ) +
  
  # Etiquetas
  labs(
    title = "Original Variables Contribution (Loadings)",
    subtitle = "Variables direction and magnitude in all 4 Factorial Planes",
    x = NULL,
    y = "Value of the Variable (Loading)"
  )

print(plot_loadings)

print(round(loadings, 3))

scores <- pca$x
print(head(scores[, 1:4]))

# biplot
biplot <- fviz_pca_biplot(pca, 
                                     geom.ind = "point",   
                                     col.ind = "black",    
                                     alpha.ind = 0.2,      
                                     col.var = "red",      
                                     repel = TRUE,         
                                     title = "Biplot PCA (PC1 vs PC2")

print(biplot)