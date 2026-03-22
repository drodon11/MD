# ==============================================================================
#                                 Profiling
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. LIBRERÍAS Y CARGA DE DATOS
# ------------------------------------------------------------------------------
library(FactoMineR) 
library(factoextra)  
library(ggplot2)     
library(dplyr)      

setwd(getwd()) 
input_path <- file.path("data", "interim", "flightprices_clustered.rds")
dd_clust <- readRDS(input_path)

# Aseguramos que el clúster es un factor
dd_clust$cluster <- as.factor(dd_clust$cluster)

# Variables comunes
dades <- dd_clust
P <- dades$cluster
nameP <- "cluster"
nc <- length(levels(factor(P)))
vars_analisis <- setdiff(names(dades), "cluster")
K <- length(vars_analisis)


# ==============================================================================
# Script Karina
# ==============================================================================

# Funciones matemáticas originales de Karina
ValorTestXnum <- function(Xnum,P){
  nk <- as.vector(table(P)); n <- sum(nk); xk <- tapply(Xnum,P,mean)
  txk <- (xk-mean(Xnum))/(sd(Xnum)*sqrt((n-nk)/(n*nk))); pxk <- pt(txk,n-1,lower.tail=F)
  for(c in 1:length(levels(as.factor(P)))){if (pxk[c]>0.5){pxk[c]<-1-pxk[c]}}
  return (pxk)
}

ValorTestXquali <- function(P,Xquali){
  taula <- table(P,Xquali); n <- sum(taula); pk <- apply(taula,1,sum)/n
  pj <- apply(taula,2,sum)/n; pf <- taula/(n*pk)
  pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2], byrow=TRUE); dpf <- pf - pjm 
  dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj))); zkj <- dpf
  zkj[dpf!=0]<-dpf[dpf!=0]/dvt[dpf!=0]; pzkj <- pnorm(zkj,lower.tail=F)
  for(c in 1:length(levels(as.factor(P)))){for (s in 1:length(levels(Xquali))){if (pzkj[c,s]> 0.5){pzkj[c,s]<-1- pzkj[c,s]}}}
  return (list(rowpf=pf,vtest=zkj,pval=pzkj))
}

pvalk <- matrix(data=0, nrow=nc, ncol=K, dimnames=list(levels(P), vars_analisis))
n_filas <- dim(dades)[1]
# Para que se guarde el output en un pdf y en txt, se puede descomentar esto
# pdf("1_Profiling_Karina_Graficos.pdf", width=10, height=7)
# sink("1_Profiling_Karina_Resultados.txt")

for(k_idx in 1:K){
  v_name <- vars_analisis[k_idx]
  v_data <- dades[[v_name]]
  
  if (is.numeric(v_data)){ 
    print(paste("Anàlisi Variable Numèrica:", v_name))
    boxplot(v_data~P, main=paste("Boxplot of", v_name, "vs", nameP ), horizontal=TRUE, col=rainbow(nc))
    barplot(tapply(v_data, P, mean), main=paste("Means of", v_name, "by", nameP ), col=rainbow(nc))
    abline(h=mean(v_data, na.rm=TRUE), lwd=2, lty=2)
    
    o <- oneway.test(v_data~P)
    print(paste("p-value ANOVA:", o$p.value))
    
    pvalk[,k_idx] <- ValorTestXnum(v_data, P)
    print("p-values ValorsTest: "); print(pvalk[,k_idx])      
    
  } else if (inherits(v_data, "Date") || inherits(v_data, "POSIXt")) {
    print(paste("Anàlisi Variable Data:", v_name))
    hist(v_data, breaks="weeks", main=paste("Histogram of", v_name))
    
  } else {
    print(paste("Variable Qualitativa:", v_name))
    v_data <- as.factor(v_data)
    
    paleta <- rainbow(length(levels(v_data)))
    barplot(table(v_data, as.factor(P)), beside=TRUE, col=paleta, main=paste("Grouped Barplot:", v_name))
    legend("topright", levels(v_data), pch=15, cex=0.6, col=paleta)
    
    print("Test Chi quadrat: ")
    print(suppressWarnings(chisq.test(v_data, as.factor(P))))
    
    vt_res <- ValorTestXquali(P, v_data)
    print("valorsTest:"); print(vt_res)
    pvalk[,k_idx] <- apply(vt_res$pval, 1, min, na.rm=TRUE)
  }
}
cat("\n==============================================================\n")
cat(" RESUM FINAL KARINA: P-VALUES PER CLASSE (ORDENATS)\n")
for (c in 1:nc) {
  if(!is.na(levels(as.factor(P))[c])){
    print(paste("P.values per class:", levels(as.factor(P))[c]))
    print(sort(pvalk[c,]), digits=3) 
  }
}
# Para que se guarde el output en un pdf y en txt, se puede descomentar esto
#sink()
#dev.off() 


# ==============================================================================
# Script Sergi
# ==============================================================================

# Para que se guarde el output en un pdf y en txt, se puede descomentar esto
# pdf("2_Profiling_Sergi_Graficos.pdf", width=12, height=8)
# sink("2_Profiling_Sergi_Resultados.txt")

var_num <- vars_analisis[sapply(dd_clust[vars_analisis], is.numeric)]
var_cat <- vars_analisis[sapply(dd_clust[vars_analisis], is.factor)]
colores <- c("#00AFBB", "#E7B800", "#FC4E07", "#868686", "#4DAF4A", "#984EA3")

# --- VISUALIZACIÓN DESCRIPTIVA ---
cat("\n--- VISUALIZACIÓN DESCRIPTIVA BIVARIANTE ---\n")
for (v in var_num) {
  p1 <- ggplot(dd_clust, aes_string(x = "cluster", y = v, fill = "cluster")) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = colores) +
    theme_minimal() +
    labs(title = paste("Boxplot of", v, "for each Cluster"), x = "Cluster", y = v) +
    theme(legend.position = "none")
  print(p1) 
}

for (v in var_cat) {
  p2 <- ggplot(dd_clust, aes_string(x = "cluster", fill = v)) +
    geom_bar(position = "fill", alpha = 0.8) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    labs(title = paste("Composition of", v, "inside every cluster"),
         x = "Cluster", y = "Proportion") +
    theme(axis.text.x = element_text(angle = 0))
  
  if (length(levels(dd_clust[[v]])) > 10) {
    p2 <- p2 + theme(legend.text = element_text(size = 7), legend.key.size = unit(0.5, "cm"))
  }
  print(p2) 
}

# --- SIGNIFICACIÓN GLOBAL Y PERFILADO ---
cat("\n--- SIGNIFICACIÓN GLOBAL Y PERFILADO ESTADÍSTICO (catdes) ---\n")
idx_cluster <- which(names(dd_clust) == "cluster")
res.catdes <- catdes(dd_clust, num.var = idx_cluster, prob = 0.05)

cat("\n--- RANKING DE VARIABLES CATEGÓRICAS (Chi-cuadrado) ---\n")
print(res.catdes$test.chi2)

cat("\n--- RANKING DE VARIABLES NUMÉRICAS (Eta-cuadrado) ---\n")
print(res.catdes$quanti.var)

cat("\n--- DESCRIPCIÓN DE CADA CLÚSTER (V-TESTS) ---\n")
for (i in 1:length(res.catdes$category)) {
  cat("\n-------------------------------------------------\n")
  cat(" DESCRIPCIÓN DEL CLÚSTER", names(res.catdes$category)[i], "\n")
  cat("-------------------------------------------------\n")
  
  cat("\n> Categorías sobrerrepresentadas / subrepresentadas:\n")
  print(res.catdes$category[[i]])
  
  if (!is.null(res.catdes$quanti[[i]])) {
    cat("\n> Variables numéricas caracterizadoras:\n")
    print(res.catdes$quanti[[i]])
  }
}

# Gráficos automáticos de catdes (v-tests)
# plot(res.catdes, show = "all", barplot = TRUE)

# Para que se guarde el output en un pdf y en txt, se puede descomentar esto
# sink()
# dev.off()

# cat("Se han generado 4 archivos en tu carpeta de trabajo:\n")
# cat("1. 1_Profiling_Karina_Graficos.pdf\n")
# cat("2. 1_Profiling_Karina_Resultados.txt\n")
# cat("3. 2_Profiling_Sergi_Graficos.pdf\n")
# cat("4. 2_Profiling_Sergi_Resultados.txt\n")
