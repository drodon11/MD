# -------------------------------------------------------------------------
# SCRIPT DE PARTICIÓN ÚNICA: Generación de model_data.RData
# -------------------------------------------------------------------------

rm(list=ls())

# Instalador automático de paquetes (Blindado con dependencias)
list.of.packages <- c("caret", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages, dependencies = TRUE)
invisible(lapply(list.of.packages, require, character.only = TRUE))
# 
# 1. Carga del dataset preprocesado
dd <- readRDS("data/interim/flightprices_preprocessed.rds")

# 2. Definición de Targets y Predictores
dd$economy_f <- factor(ifelse(dd$economy %in% c("Basic Economy", "Economy"), "Economy", "Premium"))
dd$log_price <- log(dd$totalPrice)

predictores <- c("log_price", "travelDistance", "layoverNumber", "airline", "nonStop", "elapsedDays", "seatsLeft")

# 3. Creación de la partición (Holdout 70/30)
set.seed(42) # Semilla fija para reproducibilidad total
train_idx <- createDataPartition(dd$economy_f, p = 0.8, list = FALSE)

train_df <- dd[train_idx, ]
test_df  <- dd[-train_idx, ]

# 4. Preparación específica para XGBoost (Matrices numéricas One-Hot)
# XGBoost no acepta factores, así que pre-calculamos las matrices de diseño
formula_xgb <- as.formula(economy_f ~ log_price + travelDistance + layoverNumber + airline + nonStop + elapsedDays + seatsLeft)
dummy_model <- dummyVars(formula_xgb, data = dd)

X_train_xgb <- predict(dummy_model, newdata = train_df)
X_test_xgb  <- predict(dummy_model, newdata = test_df)

# Labels binarios (0,1)
y_train_xgb <- as.numeric(train_df$economy_f) - 1
y_test_xgb  <- as.numeric(test_df$economy_f) - 1

# 5. Guardado del entorno de trabajo
save(train_df, test_df, X_train_xgb, X_test_xgb, y_train_xgb, y_test_xgb, predictores,
     file = "data/interim/model_data.RData")

cat("Particiones guardadas con éxito en data/interim/model_data.RData\n")