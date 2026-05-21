# ==============================================================================
#        LINEAR MODELS - FLIGHT PRICES
# ==============================================================================

rm(list = ls())

# ==============================================================================
# 1. PACKAGES AND CENTRALIZED DATA LOADING
# ==============================================================================

list.of.packages <- c(
  "dplyr", "ggplot2", "tidyr", "tibble", "GGally",
  "caret", "car", "lmtest", "ggfortify"
)

new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[, "Package"])
]

if (length(new.packages) > 0) {
  install.packages(new.packages)
}

invisible(lapply(list.of.packages, require, character.only = TRUE))

# Load the Data Bundle: 80/20 partition
load("data/interim/model_data.RData")

cat("\n================ CHECK DATA LOADED ================\n")
cat("Rows in train_df:", nrow(train_df), "\n")
cat("Rows in test_df:", nrow(test_df), "\n")
cat("Total rows train + test:", nrow(train_df) + nrow(test_df), "\n")
cat("Columns in train_df:", ncol(train_df), "\n")
cat("Columns in test_df:", ncol(test_df), "\n")
cat("===================================================\n")

# To keep compatibility with the rest of the script, we use the full dataset
dd <- bind_rows(train_df, test_df)

cat("\nDimensions of the full modelling dataset:\n")
print(dim(dd))

# ==============================================================================
# 3. INITIAL PREPARATION
# ==============================================================================

dd <- na.omit(dd)

dd <- dd |>
  mutate(
    across(where(is.character), as.factor),
    across(where(is.logical), as.factor)
  )

if (!("totalPrice" %in% names(dd))) {
  stop("Variable 'totalPrice' does not exist. Check the exact name of the response variable.")
}

dd$totalPrice <- as.numeric(dd$totalPrice)

# Remove variables that may introduce data leakage
dd <- dd |> select(-any_of(c("taxAmount", "log_price")))

types <- sapply(dd, class)

cat_vars <- names(types)[types %in% c("character", "factor")]
num_vars <- names(types)[types %in% c("integer", "numeric")]
num_predictors <- setdiff(num_vars, "totalPrice")

cat("\nDimensions after removing missing values:\n")
print(dim(dd))

cat("\nCategorical variables:\n")
print(cat_vars)

cat("\nNumerical variables:\n")
print(num_vars)

cat("\nNumerical predictor variables:\n")
print(num_predictors)

variable_table <- data.frame(
  Variable = names(dd),
  Type = sapply(dd, class)
)

cat("\nVariable table:\n")
print(variable_table)

# ==============================================================================
# 4. EXPLORATORY ANALYSIS
# ==============================================================================

ggplot(dd, aes(x = totalPrice)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of total flight price",
    x = "Total price",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(dd, aes(y = totalPrice)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Boxplot of total flight price",
    y = "Total price"
  ) +
  theme_minimal()

if (length(num_vars) > 1) {
  cor_matrix <- cor(dd[, num_vars], use = "complete.obs")
  cat("\nCorrelation matrix:\n")
  print(round(cor_matrix, 3))
}

vars_ggpairs <- head(num_vars, 6)

if (length(vars_ggpairs) >= 2) {
  GGally::ggpairs(dd, columns = vars_ggpairs)
}

# ==============================================================================
# 5. VARIABLE SELECTION FOR SIMPLE MODEL
# ==============================================================================

if ("travelDistance" %in% num_predictors) {
  x_simple <- "travelDistance"
} else if ("seatsLeft" %in% num_predictors) {
  x_simple <- "seatsLeft"
} else if ("elapsedDays" %in% num_predictors) {
  x_simple <- "elapsedDays"
} else if ("baseFare" %in% num_predictors) {
  x_simple <- "baseFare"
} else {
  x_simple <- num_predictors[1]
}

cat("\nVariable used for the simple model:\n")
print(x_simple)

simple_plot <- ggplot(dd, aes(x = .data[[x_simple]], y = totalPrice)) +
  geom_point(alpha = 0.4) +
  labs(
    title = paste("Total price versus", x_simple),
    x = x_simple,
    y = "Total price"
  ) +
  theme_minimal()

print(simple_plot)

# ==============================================================================
# 6. SIMPLE LINEAR REGRESSION
# ==============================================================================

formula_simple <- as.formula(
  paste("totalPrice ~", x_simple)
)

simple_model <- lm(formula_simple, data = dd)

cat("\nSimple linear model summary:\n")
print(summary(simple_model))

cat("\nSimple model coefficients:\n")
print(coef(simple_model))

simple_plot +
  geom_smooth(method = "lm", se = TRUE, color = "red")

cat("\nFirst residuals of the simple model:\n")
print(head(data.frame(
  observed = dd$totalPrice,
  fitted = fitted(simple_model),
  residual = residuals(simple_model)
)))

autoplot(simple_model) +
  theme_minimal()

cat("\nBreusch-Pagan test for homoscedasticity:\n")
print(lmtest::bptest(simple_model))

simple_residuals <- residuals(simple_model)

set.seed(2108)

cat("\nShapiro-Wilk normality test:\n")
if (length(simple_residuals) > 5000) {
  print(shapiro.test(sample(simple_residuals, 5000)))
} else {
  print(shapiro.test(simple_residuals))
}

cat("\nR2 simple model:\n")
print(summary(simple_model)$r.squared)

cat("\nAdjusted R2 simple model:\n")
print(summary(simple_model)$adj.r.squared)

cat("\nConfidence intervals simple model:\n")
print(confint(simple_model))

new_flight_simple <- data.frame(
  value = median(dd[[x_simple]], na.rm = TRUE)
)

names(new_flight_simple) <- x_simple

cat("\nPoint prediction for new flight:\n")
print(predict(simple_model, newdata = new_flight_simple))

cat("\nConfidence interval for new flight:\n")
print(predict(
  simple_model,
  newdata = new_flight_simple,
  interval = "confidence"
))

cat("\nPrediction interval for new flight:\n")
print(predict(
  simple_model,
  newdata = new_flight_simple,
  interval = "prediction"
))

# ==============================================================================
# 7. MULTIPLE LINEAR REGRESSION
# ==============================================================================

preferred_vars <- c(
  "elapsedDays",
  "economy",
  "nonStop",
  "baseFare",
  "seatsLeft",
  "travelDistance",
  "airline",
  "equipment",
  "startApt",
  "destApt",
  "departure_raw",
  "arrival_raw",
  "segmentDistance_raw"
)

vars_multiple_model <- preferred_vars[preferred_vars %in% names(dd)]

if (length(vars_multiple_model) < 3) {
  other_vars <- setdiff(names(dd), c("totalPrice", vars_multiple_model))
  vars_multiple_model <- unique(c(vars_multiple_model, head(other_vars, 5)))
}

cat("\nVariables used in the multiple model:\n")
print(vars_multiple_model)

formula_multiple <- as.formula(
  paste("totalPrice ~", paste(vars_multiple_model, collapse = " + "))
)

multiple_model <- lm(formula_multiple, data = dd)

cat("\nMultiple linear model summary:\n")
print(summary(multiple_model))

cat("\nR2 simple model:\n")
print(summary(simple_model)$r.squared)

cat("\nR2 multiple model:\n")
print(summary(multiple_model)$r.squared)

cat("\nAdjusted R2 multiple model:\n")
print(summary(multiple_model)$adj.r.squared)

# ==============================================================================
# 8. CATEGORICAL VARIABLES
# ==============================================================================

if (length(cat_vars) > 0) {
  
  cat_var <- cat_vars[1]
  
  formula_factor <- as.formula(
    paste("totalPrice ~", x_simple, "+", cat_var)
  )
  
  factor_model <- lm(formula_factor, data = dd)
  
  cat("\nCategorical variable used:\n")
  print(cat_var)
  
  cat("\nModel summary with categorical variable:\n")
  print(summary(factor_model))
  
} else {
  cat("\nNo categorical variables available.\n")
}

# ==============================================================================
# 9. INTERACTIONS
# ==============================================================================

if (length(cat_vars) > 0) {
  
  formula_interaction <- as.formula(
    paste("totalPrice ~", x_simple, "*", cat_var)
  )
  
  interaction_model <- lm(formula_interaction, data = dd)
  
  cat("\nModel summary with interaction:\n")
  print(summary(interaction_model))
  
  top_cats <- names(sort(table(dd[[cat_var]]), decreasing = TRUE))[
    1:min(4, length(unique(dd[[cat_var]])))
  ]
  
  dd_inter <- dd |>
    filter(.data[[cat_var]] %in% top_cats)
  
  ggplot(
    dd_inter,
    aes(x = .data[[x_simple]], y = totalPrice, color = .data[[cat_var]])
  ) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = paste("Interaction between", x_simple, "and", cat_var),
      x = x_simple,
      y = "Total price",
      color = cat_var
    ) +
    theme_minimal()
}

# ==============================================================================
# 10. MULTICOLLINEARITY
# ==============================================================================

cat("\nVIF of the multiple model:\n")
print(try(car::vif(multiple_model)))

# ==============================================================================
# 11. FULL MODEL
# ==============================================================================

full_model <- lm(totalPrice ~ ., data = dd)

cat("\nFull model summary:\n")
print(summary(full_model))

# ==============================================================================
# 12. VARIABLE SELECTION BY AIC
# ==============================================================================

step_model <- step(full_model, trace = 1)

cat("\nAIC-selected model summary:\n")
print(summary(step_model))

# ==============================================================================
# 13. FORMAL MODEL COMPARISON
# ==============================================================================

cat("\nANOVA between simple and multiple model:\n")
print(anova(simple_model, multiple_model))

# ==============================================================================
# 14. PREDICTIVE EVALUATION TRAIN / TEST
# ==============================================================================

train <- train_df
test  <- test_df

train <- train |>
  mutate(
    across(where(is.character), as.factor),
    across(where(is.logical), as.factor)
  )

test <- test |>
  mutate(
    across(where(is.character), as.factor),
    across(where(is.logical), as.factor)
  )

train$totalPrice <- as.numeric(train$totalPrice)
test$totalPrice  <- as.numeric(test$totalPrice)

train <- train |> select(-any_of(c("taxAmount", "log_price")))
test  <- test  |> select(-any_of(c("taxAmount", "log_price")))

train_model <- lm(formula(step_model), data = train)

pred_test <- predict(train_model, newdata = test)

rmse <- sqrt(mean((test$totalPrice - pred_test)^2))
mae  <- mean(abs(test$totalPrice - pred_test))
mse  <- mean((test$totalPrice - pred_test)^2)

r2_test <- 1 - sum((test$totalPrice - pred_test)^2) /
  sum((test$totalPrice - mean(test$totalPrice))^2)

test_metrics <- data.frame(
  MAE = mae,
  MSE = mse,
  RMSE = rmse,
  R2_test = r2_test
)

cat("\nTest metrics:\n")
print(test_metrics)

test_results <- data.frame(
  Actual = test$totalPrice,
  Predicted = pred_test,
  Error = test$totalPrice - pred_test
)

ggplot(test_results, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Actual versus predicted values",
    subtitle = "The red line represents perfect prediction",
    x = "Actual price",
    y = "Predicted price"
  ) +
  theme_minimal()

ggplot(test_results, aes(x = Predicted, y = Error)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_hline(
    yintercept = 0,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Residuals on the test set",
    x = "Predicted price",
    y = "Error"
  ) +
  theme_minimal()

# ==============================================================================
# 15. CROSS-VALIDATION
# ==============================================================================

set.seed(2108)

control <- trainControl(method = "cv", number = 5)

cv_model <- train(
  formula(step_model),
  data = train,
  method = "lm",
  trControl = control
)

cat("\nCross-validation:\n")
print(cv_model)

# ==============================================================================
# 16. LOG-LINEAR MODEL
# ==============================================================================

log_model <- lm(
  update(formula(step_model), log(totalPrice) ~ .),
  data = dd
)

cat("\nLog-linear model summary:\n")
print(summary(log_model))

autoplot(simple_model) +
  theme_minimal()

# ==============================================================================
# 17. QUADRATIC MODEL
# ==============================================================================

formula_quadratic <- as.formula(
  paste("totalPrice ~", x_simple, "+ I(", x_simple, "^2)")
)

quadratic_model <- lm(formula_quadratic, data = dd)

cat("\nQuadratic model summary:\n")
print(summary(quadratic_model))

cat("\nANOVA between simple and quadratic model:\n")
print(anova(simple_model, quadratic_model))

# ==============================================================================
# 18. OUTLIERS, LEVERAGE AND INFLUENCE
# ==============================================================================

cooks <- cooks.distance(multiple_model)

df_cooks <- data.frame(
  index = 1:length(cooks),
  cooks = cooks
)

threshold <- 4 / nrow(dd)

cat("\nTop 20 most influential observations according to Cook's Distance:\n")
print(head(df_cooks[order(-df_cooks$cooks), ], 20))

ggplot(df_cooks, aes(x = index, y = cooks)) +
  geom_segment(aes(xend = index, yend = 0), alpha = 0.6) +
  geom_hline(
    yintercept = threshold,
    color = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Cook's Distance",
    subtitle = "The red line represents the 4/n threshold",
    x = "Observation",
    y = "Cook's Distance"
  ) +
  theme_minimal()

ggplot(df_cooks, aes(x = index, y = cooks)) +
  geom_segment(aes(xend = index, yend = 0), alpha = 0.6) +
  geom_point(
    data = subset(df_cooks, cooks > threshold),
    aes(x = index, y = cooks),
    color = "red",
    size = 2
  ) +
  geom_hline(
    yintercept = threshold,
    color = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Detection of influential observations",
    subtitle = "Red points exceed the 4/n threshold",
    x = "Observation",
    y = "Cook's Distance"
  ) +
  theme_minimal()

df_cooks_sorted <- df_cooks[order(-df_cooks$cooks), ]

top_cooks <- df_cooks_sorted[1:min(50, nrow(df_cooks_sorted)), ]

ggplot(top_cooks, aes(x = reorder(index, -cooks), y = cooks)) +
  geom_col(fill = "darkorange") +
  geom_hline(
    yintercept = threshold,
    color = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Top most influential observations",
    x = "Observation",
    y = "Cook's Distance"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ==============================================================================
# 19. GLOBAL MODEL COMPARISON
# ==============================================================================

model_summary <- data.frame(
  Model = c(
    "Simple",
    "Multiple",
    "Full",
    "Stepwise",
    "Log-linear",
    "Quadratic"
  ),
  R2 = c(
    summary(simple_model)$r.squared,
    summary(multiple_model)$r.squared,
    summary(full_model)$r.squared,
    summary(step_model)$r.squared,
    summary(log_model)$r.squared,
    summary(quadratic_model)$r.squared
  ),
  Adjusted_R2 = c(
    summary(simple_model)$adj.r.squared,
    summary(multiple_model)$adj.r.squared,
    summary(full_model)$adj.r.squared,
    summary(step_model)$adj.r.squared,
    summary(log_model)$adj.r.squared,
    summary(quadratic_model)$adj.r.squared
  ),
  AIC = c(
    AIC(simple_model),
    AIC(multiple_model),
    AIC(full_model),
    AIC(step_model),
    AIC(log_model),
    AIC(quadratic_model)
  )
)

cat("\nComparative model summary:\n")
print(model_summary)

# ==============================================================================
# 20. FINAL SUMMARY
# ==============================================================================

cat("\n============================================================\n")
cat("FINAL SUMMARY\n")
cat("============================================================\n")

cat("\nResponse variable: totalPrice\n")

cat("\nSimple model used:\n")
print(formula_simple)

cat("\nMultiple model used:\n")
print(formula_multiple)

cat("\nFinal stepwise model:\n")
print(formula(step_model))

cat("\nTest metrics for stepwise model:\n")
print(test_metrics)

cat("\nGlobal model comparison:\n")
print(model_summary)

cat("\n============================================================\n")
cat("END OF SCRIPT\n")
cat("============================================================\n")