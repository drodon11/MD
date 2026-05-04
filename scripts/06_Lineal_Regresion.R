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

