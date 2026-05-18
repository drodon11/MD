# install.packages("MASS")
library(MASS)
library(FactoMineR)

input_path <- file.path(getwd(), "data", "interim", "flightprices_preprocessed.rds")
vuelos <- readRDS(input_path)

names(vuelos)

vuelos <- vuelos[, c("airline", "elapsedDays", "taxAmount", "totalPrice", 
                     "seatsLeft", "travelDistance", "segmentDistance", "layoverNumber")]

vuelos <- na.omit(vuelos)
# Asegurarnos de que la clase a predecir sea un factor
vuelos$airline <- as.factor(vuelos$airline)

# Hay categorias de aerolineas con muy pocos vuelos, hay que agrupar ("otras")
print("Frecuencias antes de agrupar:")
print(table(vuelos$airline))

# Identificamos las aerolíneas que tienen más de 50 vuelos
conteos <- table(vuelos$airline)
aerolineas_grandes <- names(conteos[conteos > 50])

# Convertimos a texto temporalmente para modificar
vuelos$airline <- as.character(vuelos$airline)

# Si la aerolínea no está en las grandes, la llamamos "Otras"
vuelos$airline[!(vuelos$airline %in% aerolineas_grandes)] <- "Otras"

# Volvemos a convertir a factor (obligatorio para los modelos)
vuelos$airline <- as.factor(vuelos$airline)

print("Frecuencias DESPUÉS de agrupar:")
print(table(vuelos$airline))


# ==============================================================================
#                                      LDA
# ==============================================================================
vuelos.lda <- lda(airline ~ elapsedDays + taxAmount + totalPrice + seatsLeft + 
                    travelDistance + segmentDistance + layoverNumber, data = vuelos)

vuelos.lda

# coeficients de la funcio discriminant
vuelos.lda$scaling[,2]
vuelos.lda$scaling[,1]

vuelos.lda$scaling[,1:2]

# valors de cada cas per la primera funcio discriminant
vuelos.lda.values <- predict(vuelos.lda, vuelos[2:8])

vuelos$LDA2 <- vuelos.lda.values$x[,2]
vuelos.lda.values$x[,2]
vuelos$LDA1 <- vuelos.lda.values$x[,1]
vuelos.lda.values$x[,1]

calcWithinGroupsVariance <- function(variable,groupvariable)
{
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- variable[groupvariable==leveli,]
    levelilength <- length(levelidata)
    if (levelilength > 1) { 
      sdi <- sd(levelidata)
      numi <- (levelilength - 1)*(sdi * sdi)
      denomi <- levelilength
      numtotal <- numtotal + numi
      denomtotal <- denomtotal + denomi
    }
  }
  Vw <- numtotal / (denomtotal - numlevels)
  return(Vw)
}

groupStandardise <- function(variables, groupvariable)
{
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  variablenames <- colnames(variables)
  for (i in 1:numvariables)
  {
    variablei <- variables[i]
    variablei_name <- variablenames[i]
    variablei_Vw <- calcWithinGroupsVariance(variablei, groupvariable)
    variablei_mean <- mean(as.matrix(variablei))  
    variablei_new <- (variablei - variablei_mean)/(sqrt(variablei_Vw))
    data_length <- nrow(variablei)
    if (i == 1) { variables_new <- data.frame(row.names=seq(1,data_length)) }
    variables_new[`variablei_name`] <- variablei_new
  }
  return(variables_new)
}

# si s'estandarditzen les variables es solen obtenir valors mes interpretables
groupstandardisedconcentrations <- groupStandardise(vuelos[2:8], vuelos[1])

calcBetweenGroupsVariance <- function(variable,groupvariable)
{
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  grandmean <- mean(as.matrix(variable) )         
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- variable[groupvariable==leveli,]
    levelilength <- length(levelidata)
    if (levelilength > 0) {
      meani <- mean( as.matrix(levelidata) )
      sdi <- sd(levelidata)
      numi <- levelilength * ((meani - grandmean)^2)
      denomi <- levelilength
      numtotal <- numtotal + numi
      denomtotal <- denomtotal + denomi
    }
  }
  Vb <- numtotal / (numlevels - 1)
  Vb <- Vb[[1]]
  return(Vb)
}

calcSeparations <- function(variables,groupvariable)
{
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  variablenames <- colnames(variables)
  for (i in 1:numvariables)
  {
    variablei <- variables[i]
    variablename <- variablenames[i]
    Vw <- calcWithinGroupsVariance(variablei, groupvariable)
    Vb <- calcBetweenGroupsVariance(variablei, groupvariable)
    sep <- Vb/Vw
    print(paste("variable",variablename,"Vw=",Vw,"Vb=",Vb,"separation=",sep))
  }
}

# separacio que donen les dues funcions discriminants
calcSeparations(vuelos.lda.values$x, vuelos[1])


hist(vuelos.lda.values$x[,2])
hist(vuelos.lda.values$x[,1])

# histograma multiple entre la funcio discriminant i la resposta
#par("mar")
#par(mar=c(1,1,1,1))
#par(mar=c(5.1,4.1,4.1,2.1))
#par(mar=c(3,2.5,1.5,1))

pdf("Histogramas_LDA1.pdf", width = 8, height = 15)
par(mar=c(3, 3, 2, 1))
ldahist(data = vuelos.lda.values$x[,1], g=vuelos$airline, ymax=1)
dev.off()

pdf("Histogramas_LDA2.pdf", width = 8, height = 15)
par(mar=c(3, 3, 2, 1))
ldahist(data = vuelos.lda.values$x[,2], g=vuelos$airline)
dev.off()

# plot de les dues components discriminants (etiquetem els grups)
plot(vuelos.lda.values$x[,1], vuelos.lda.values$x[,2]) 

plot(vuelos$LDA1, vuelos$LDA2)
text(vuelos.lda.values$x[,1], vuelos.lda.values$x[,2], vuelos$airline, cex=0.7, pos=4, col=as.numeric(vuelos$airline)) 

plot(vuelos$LDA1, vuelos$LDA2, type="n")
text(vuelos.lda.values$x[,1], vuelos.lda.values$x[,2], vuelos$airline, cex=0.7, pos=4, col="red") 

# utilitzar les regles per estimar el grup de cada cas
# par(mfrow=c(1,2))
# ldahist(data = vuelos$LDA2, g=vuelos$airline)
# ldahist(data = vuelos$LDA1, g=vuelos$airline)
# par(mfrow=c(1,1))

pdf("Histogramas_Comparativa_LDA1_LDA2.pdf", width = 14, height = 16) 

par(mfrow=c(1,2))

par(mar=c(3, 3, 2, 1))

ldahist(data = vuelos$LDA2, g=vuelos$airline)
ldahist(data = vuelos$LDA1, g=vuelos$airline)

par(mfrow=c(1,1))

dev.off()

vuelos$Prediccion <- vuelos.lda.values$class

# matriu de confusio
table(vuelos[,1])
MC <- table(vuelos[,1], vuelos$Prediccion)
MC

# accuracy
accuracy <- sum(diag(MC))/dim(vuelos)[1]
accuracy

# compute missclassification rate
MR <- 1-accuracy
MR

printMeanAndSdByGroup <- function(variables,groupvariable)
{
  variablenames <- c(names(groupvariable),names(as.data.frame(variables)))
  groupvariable <- groupvariable[,1] 
  means <- aggregate(as.matrix(variables) ~ groupvariable, FUN = mean)
  names(means) <- variablenames
  print(paste("Means:"))
  print(means)
  sds <- aggregate(as.matrix(variables) ~ groupvariable, FUN = sd)
  names(sds) <- variablenames
  print(paste("Standard deviations:"))
  print(sds)
  samplesizes <- aggregate(as.matrix(variables) ~ groupvariable, FUN = length)
  names(samplesizes) <- variablenames
  print(paste("Sample sizes:"))
  print(samplesizes)
}

# mitjanes de les funcions discriminants per grups
printMeanAndSdByGroup(vuelos.lda.values$x, vuelos[1])


# ==============================================================================
#                      PREPOCESAMIENTO & CARGA DE DATOS
# ==============================================================================
list.of.packages <- c("caret", "MASS", "klaR", "ggplot2", "ggpubr", "reshape2") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
invisible(lapply(list.of.packages, require, character.only = TRUE))

load("data/interim/model_data.RData")

# Asegurarnos de que las clases son factor
train_df$airline <- as.factor(train_df$airline)
test_df$airline <- as.factor(test_df$airline)


# ESCALADO DE DATOS
vars_para_escalar <- c("elapsedDays", "taxAmount", "totalPrice", "seatsLeft", 
                       "travelDistance", "segmentDistance", "layoverNumber")

# Entrenamos el escalador SOLO con el train_df
preproc_param <- caret::preProcess(x = train_df[, vars_para_escalar], method = c("center", "scale"))

# Transformamos train_df y test_df
train_df[, vars_para_escalar] <- predict(preproc_param, train_df[, vars_para_escalar])
test_df[, vars_para_escalar]  <- predict(preproc_param, test_df[, vars_para_escalar])

# Densidades bivariantes (Usando train_df)
p1 <- ggplot(data = train_df, aes(x = totalPrice, fill = airline, colour = airline)) +
  geom_density(alpha = 0.3) + theme_bw()
p2 <- ggplot(data = train_df, aes(x = travelDistance, fill = airline, colour = airline)) +
  geom_density(alpha = 0.3) + theme_bw()
p3 <- ggplot(data = train_df, aes(x = taxAmount, fill = airline, colour = airline)) +
  geom_density(alpha = 0.3) + theme_bw()
p4 <- ggplot(data = train_df, aes(x = elapsedDays, fill = airline, colour = airline)) +
  geom_density(alpha = 0.3) + theme_bw()

ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

# Pairs plot (Usando train_df)
pairs(x = train_df[, c("totalPrice", "travelDistance", "taxAmount", "elapsedDays")], 
      col = as.numeric(train_df[, 'airline']), pch = 20)


# ==============================================================================
#                                   LDA 2
# ==============================================================================
options(digits = 4)

# Entrenamos LDA con train_df
modelo_lda2 <- lda(airline ~ elapsedDays + taxAmount + totalPrice + seatsLeft + 
                     travelDistance + segmentDistance + layoverNumber, data = train_df)
modelo_lda2

# Gráfico LDA
datos_lda <- cbind(train_df, predict(modelo_lda2)$x)
ggplot(datos_lda, aes(LD1, LD2)) +
  geom_point(aes(color = airline)) +
  ggtitle("Gráfico LDA (Train Set)")

klaR::partimat(airline ~ totalPrice + travelDistance, data = train_df, method = "lda", 
               image.colors = heat.colors(length(levels(train_df$airline))), col.mean = "black")

# Predicciones sobre test_df
predicciones_lda <- predict(modelo_lda2, test_df)
table(test_df$airline, predicciones_lda$class, dnn = c("Grupo real", "Grupo pronosticado"))

cat(sprintf("Accuracy LDA: %.4f\n", mean(predicciones_lda$class == test_df$airline)))


# ==============================================================================
#                                    QDA 
# ==============================================================================
options(digits = 4)

print("Frecuencias originales en train_df:")
print(table(train_df$airline))

# Identificamos las aerolíneas "grandes" (ej. más de 50 vuelos en el train)
# Si te sigue dando error, sube este número de 50 a 100
conteos <- table(train_df$airline)
aerolineas_grandes <- names(conteos[conteos > 50])

# Convertimos a texto temporalmente
train_df$airline <- as.character(train_df$airline)
test_df$airline <- as.character(test_df$airline)

# Agrupamos en "Otras" las que no son grandes
train_df$airline[!(train_df$airline %in% aerolineas_grandes)] <- "Otras"
test_df$airline[!(test_df$airline %in% aerolineas_grandes)] <- "Otras"

# Volvemos a convertir a factor (esencial para LDA/QDA)
train_df$airline <- as.factor(train_df$airline)
test_df$airline <- as.factor(test_df$airline)

print("Frecuencias DESPUÉS de agrupar en train_df:")
print(table(train_df$airline))

# Entrenamos QDA con train_df (excluimos seatsLeft)
modelo_qda <- qda(airline ~ elapsedDays + taxAmount + totalPrice + 
                    travelDistance + segmentDistance, data = train_df)
modelo_qda

partimat(airline ~ totalPrice + travelDistance, data = train_df, method = "qda", 
         image.colors = heat.colors(length(levels(train_df$airline))), col.mean = "black")

# Predicciones sobre test_df
predicciones_qda <- predict(modelo_qda, test_df)

# Matriz de confusión estilo caret
matriz_confusion_qda <- caret::confusionMatrix(
  factor(predicciones_qda$class, levels=levels(test_df$airline)), 
  factor(test_df$airline, levels=levels(test_df$airline))
)

print(matriz_confusion_qda)
