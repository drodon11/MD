# ==============================================================================
#                 ACP: Análisis de Componentes Principales
# ==============================================================================
# Paquetes
library(dplyr)
library(ggplot2)

# Cargamos el rds después del preprocessing
input_path <- file.path(getwd(), "data", "interim", "flightprices_preprocessed.rds")
dd <- readRDS(input_path)

# Extraemos las variables numéricas
varNum <- which(sapply(dd, is.numeric))
dcon <- dd[, varNum]

cat("Variables numéricas usadas en el PCA:\n")
print(names(dcon))

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
  labs(x = "Componente Principal", y = "Proporción varianza explicada",
       title = "Scree plot (Inercia Marginal)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq_along(scree_df$VarExp))

print(plot_marginal)

# Varianza acumulada
plot_acumulada <- ggplot(scree_df, aes(x = seq_along(CumExp), y = CumExp)) +
  geom_line(color = "red") + 
  geom_point(size = 2, color = "darkred") +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "black") +
  labs(x = "Componente Principal", y = "Varianza acumulada",
       title = "Varianza Acumulada (>80%)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq_along(scree_df$CumExp))

print(plot_acumulada)

# Loadings
# Revisamos cómo contribuyen las variables originales a los 2 primeros componentes
cat("\n--- Loadings (Pesos de las variables en PC1 y PC2) ---\n")
loadings <- pca$rotation[, 1:4]
print(round(loadings, 3))

scores <- pca$x
print(head(scores[, 1:4]))

# biplot
biplot(pca, cex = c(0.5, 0.7), main = "Biplot PCA (PC1 vs PC2)")