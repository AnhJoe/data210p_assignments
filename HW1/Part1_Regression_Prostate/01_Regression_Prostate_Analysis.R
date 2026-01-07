# Load libraries
library(tidyverse)
library(caret)

# Load data
df <- read_csv("prostate.csv")

# Split data into training and test sets
train_df <- df %>% filter(train == TRUE)
test_df  <- df %>% filter(train == FALSE)

# Validate ntrain and ntest
ntrain <- nrow(train_df)
ntest  <- nrow(test_df)
ntrain
ntest

# Separate train/test predictors (X) and outcome (Y)
y_train <- train_df$lpsa
y_test  <- test_df$lpsa
# Filter out non-predictor columns
X_train <- train_df %>%
  select(-lpsa, -train, -id)
X_test <- test_df %>%
  select(-lpsa, -train, -id)

# Validate
dim(X_train)
length(y_train)
dim(X_test)
length(y_test)

# Simple Linear Regression (SLR) with the lcavol predictor
slr_fit <- lm(lpsa ~ lcavol, data = train_df)
summary(slr_fit)

# Plot SLR results on training set
ggplot(train_df, aes(x = lcavol, y = lpsa)) +
  geom_point(alpha = 0.7) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    level = 0.95,
    color = "blue"
  ) +
  labs(
    title = "Simple Linear Regression: lpsa vs lcavol (Training Set)",
    x = "lcavol (log cancer volume)",
    y = "lpsa (log PSA)"
  )

# Generate predictions using the SLR model
pred_test_slr <- predict(slr_fit, newdata = test_df)

# Evaluate SLR model performance on test set
rmse_slr <- RMSE(pred_test_slr, test_df$lpsa)
r2_slr   <- R2(pred_test_slr, test_df$lpsa)
rmse_slr
r2_slr

# Plot predicted vs actual for SLR model on test set
tibble(
  actual = test_df$lpsa,
  predicted = as.numeric(pred_test_slr)
) %>%
  ggplot(aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  labs(
    title = "SLR Test Set: Predicted vs Actual lpsa",
    x = "Actual lpsa",
    y = "Predicted lpsa")

# SLR Conclusion: The SLR model using lcavol as a predictor in the test set shows moderate 
# generalization performance on the test set, with RMSE (0.69) and R-squared (0.54) values.
# With a RMSE of 0.69 on lpsa which translates to about a multiplicative factor of 2 in relative PSA error, 
# we can capture the order of magnitude to reasonably separate low vs. high PSA, but yet, 
# we cannot reliably distinguish between 6 vs. 9 ng/mL PSA values for clinical decision-making.
# While the R2 tells us that lcavol explains a good portion of variability in lpsa, the remaining 46% remains unexplained. 
# These results indicate room for improvement with more predictors in Multiple Linear Regression (MLR) model.

# Multiple Linear Regression (MLR) with all predictors except id, train, and lpsa
mlr_fit <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, data = train_df)
summary(mlr_fit)

# Optional: confint(mlr_fit, level = 0.95)
beta_svi <- coef(mlr_fit)["svi"]
exp(beta_svi)
beta_lcavol <- coef(mlr_fit)["lcavol"]
exp(beta_lcavol)

# Generate predictions using the MLR model
pred_test_mlr <- predict(mlr_fit, newdata = test_df)

# Evaluate MLR model performance on test set
rmse_mlr <- RMSE(pred_test_mlr, test_df$lpsa)
r2_mlr   <- R2(pred_test_mlr, test_df$lpsa)
rmse_mlr
r2_mlr

# Plot predicted vs actual for MLR model on test set
tibble(
  actual = test_df$lpsa,
  predicted = as.numeric(pred_test_mlr)
) %>%
  ggplot(aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  labs(
    title = "MLR Test Set: Predicted vs Actual lpsa",
    x = "Actual lpsa",
    y = "Predicted lpsa"
  )