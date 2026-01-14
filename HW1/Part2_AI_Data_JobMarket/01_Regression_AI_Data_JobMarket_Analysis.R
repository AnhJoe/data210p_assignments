# Load libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(broom)
library(dplyr)
library(ggrepel)
library(patchwork)
library(lmtest)
library(car)

# Load data
test_df <- read_csv("ai_jobs_test.csv")
train_df <- read_csv("ai_jobs_train.csv")

# Validate ntrain and ntest
ntrain <- nrow(train_df)
ntest  <- nrow(test_df)
ntrain
ntest

# Separate train/test predictors (X) and response (y)
y_train_salary_min <- train_df$log_salary_min
y_test_salary_min  <- test_df$log_salary_min
y_train_salary_max <- train_df$log_salary_max
y_test_salary_max  <- test_df$log_salary_max


# Filter out non-predictor columns
X_train <- train_df %>%
  select(-job_id)
X_test <- test_df %>%
  select(-job_id)

# Validate
dim(X_train)
dim(X_test)
length(y_train_salary_max)
length(y_test_salary_max)
length(y_train_salary_min)
length(y_test_salary_min)

# Simple Linear Regression (SLR) with the predictors
slr_fit_salary_min <- lm(log_salary_min ~ min_experience_years, data = train_df)
summary(slr_fit_salary_min)
# 95% confidence intervals for beta parameters
confint(slr_fit_salary_min, level = 0.95)

slr_fit_salary_max <- lm(log_salary_max ~ min_experience_years, data = train_df)
summary(slr_fit_salary_max)
# 95% confidence intervals for beta parameters
confint(slr_fit_salary_max, level = 0.95)

# Plot SLR results on training set for log_salary_min
ggplot(train_df, aes(x = min_experience_years, y = log_salary_min)) +
  geom_point(alpha = 0.7) +
  geom_smooth(
    aes(color = "Linear Regression Line"),
    method = "lm",
    se = TRUE,
    level = 0.95
  ) +
  scale_color_manual(values = c("Linear Regression Line" = "blue")) +
  labs(
    title = "Simple Linear Regression (SLR): log_salary_min vs. Minimum Experience Required (Training Set)",
    x = "Minimum Experience Required (Years)",
    y = "Log Salary Minimum",
    color = "Legend"
  )

# Plot SLR results on training set for log_salary_max
ggplot(train_df, aes(x = min_experience_years, y = log_salary_max)) +
  geom_point(alpha = 0.7) +
  geom_smooth(
    aes(color = "Linear Regression Line"),
    method = "lm",
    se = TRUE,
    level = 0.95
  ) +
  scale_color_manual(values = c("Linear Regression Line" = "blue")) +
  labs(
    title = "Simple Linear Regression (SLR): log_salary_max vs. Minimum Experience Required (Training Set)",
    x = "Minimum Experience Required (Years)",
    y = "Log Salary Maximum",
    color = "Legend"
  )

# Generate predictions using the SLR model
pred_test_slr_salary_min <- predict(slr_fit_salary_min, newdata = test_df)
pred_test_slr_salary_max <- predict(slr_fit_salary_max, newdata = test_df)

# Evaluate SLR model performance on test set for log_salary_min
rmse_slr_salary_min <- RMSE(pred_test_slr_salary_min, test_df$log_salary_min)
r2_slr_salary_min   <- R2(pred_test_slr_salary_min, test_df$log_salary_min)
rmse_slr_salary_min
r2_slr_salary_min

# Evaluate SLR model performance on test set for log_salary_max
rmse_slr_salary_max <- RMSE(pred_test_slr_salary_max, test_df$log_salary_max)
r2_slr_salary_max   <- R2(pred_test_slr_salary_max, test_df$log_salary_max)
rmse_slr_salary_max
r2_slr_salary_max

# Plot predicted vs actual for SLR model on test set for log_salary_min
ref_line <- tibble(
  label = "Perfect Prediction",
  slope = 1,
  intercept = 0
)

tibble(
  actual = test_df$log_salary_min,
  predicted = as.numeric(pred_test_slr_salary_min)
) %>%
  ggplot(aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.7) +
  geom_abline(
    data = ref_line,
    aes(slope = slope, intercept = intercept, color = label),
    linewidth = 1
  ) +
  scale_color_manual(values = c("Perfect Prediction" = "blue")) +
  labs(
    title = "Simple Linear Regression (SLR): Predicted vs Actual log_salary_min (Test Set)",
    x = "Actual log_salary_min",
    y = "Predicted log_salary_min",
    color = "Legend"
  )


# Plot predicted vs actual for SLR model on test set for log_salary_min
ref_line <- tibble(
  label = "Perfect Prediction",
  slope = 1,
  intercept = 0
)

tibble(
  actual = test_df$log_salary_min,
  predicted = as.numeric(pred_test_slr_salary_min)
) %>%
  ggplot(aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.7) +
  geom_abline(
    data = ref_line,
    aes(slope = slope, intercept = intercept, color = label),
    linewidth = 1
  ) +
  scale_color_manual(values = c("Perfect Prediction" = "blue")) +
  labs(
    title = "Simple Linear Regression (SLR): Predicted vs Actual log_salary_min (Test Set)",
    x = "Actual log_salary_min",
    y = "Predicted log_salary_min",
    color = "Legend"
  )

# Plot predicted vs actual for SLR model on test set for log_salary_max
ref_line <- tibble(
  label = "Perfect Prediction",
  slope = 1,
  intercept = 0
)

tibble(
  actual = test_df$log_salary_max,
  predicted = as.numeric(pred_test_slr_salary_max)
) %>%
  ggplot(aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.7) +
  geom_abline(
    data = ref_line,
    aes(slope = slope, intercept = intercept, color = label),
    linewidth = 1
  ) +
  scale_color_manual(values = c("Perfect Prediction" = "blue")) +
  labs(
    title = "Simple Linear Regression (SLR): Predicted vs Actual log_salary_max (Test Set)",
    x = "Actual log_salary_max",
    y = "Predicted log_salary_max",
    color = "Legend"
  )

# Assumptions of Linear Regression: homoskedasticity, influence, visual plots

# --- Homoskedasticity test (Breusch–Pagan) ---
# H0: Var(eps_i | x_i) = constant (sigma^2)
bptest(slr_fit_salary_min)
bptest(slr_fit_salary_max)

# --------------------------------------------
# Cook's Distance: top 5 most influential obs
# --------------------------------------------

# Cook's Distance for log_salary_min
cooks_vals_min <- cooks.distance(slr_fit_salary_min)

top5_idx_min <- order(cooks_vals_min, decreasing = TRUE)[1:5]

top5_influential_min <- tibble(
  obs = top5_idx_min,
  job_id = train_df$job_id[top5_idx_min],
  job_title = train_df$job_title[top5_idx_min],
  country = train_df$country[top5_idx_min],
  city = train_df$city[top5_idx_min],
  min_experience_years = train_df$min_experience_years[top5_idx_min],
  log_salary_min = train_df$log_salary_min[top5_idx_min],
  log_salary_max = train_df$log_salary_max[top5_idx_min],
  cooks_d = cooks_vals_min[top5_idx_min]
) %>%
  arrange(desc(cooks_d))

top5_influential_min

# Cook's Distance: top 5 most influential obs for log_salary_max
cooks_vals_max <- cooks.distance(slr_fit_salary_max)

top5_idx_max <- order(cooks_vals_min, decreasing = TRUE)[1:5]

top5_influential_max <- tibble(
  obs = top5_idx_max,
  job_id = train_df$job_id[top5_idx_max],
  job_title = train_df$job_title[top5_idx_max],
  country = train_df$country[top5_idx_max],
  city = train_df$city[top5_idx_max],
  min_experience_years = train_df$min_experience_years[top5_idx_max],
  log_salary_min = train_df$log_salary_min[top5_idx_max],
  log_salary_max = train_df$log_salary_max[top5_idx_max],
  cooks_d = cooks_vals_min[top5_idx_max]
) %>%
  arrange(desc(cooks_d))

top5_influential_max


# 2x2 ggplot Diagnostics Panel (log_salary_min SLR)
library(tidyverse)
library(ggplot2)
library(broom)
library(dplyr)
library(patchwork)

# Build diagnostics data from the fitted model
diag_min <- broom::augment(slr_fit_salary_min) %>%
  dplyr::mutate(obs = dplyr::row_number())

# 1) Residuals vs Fitted (linearity + mean-zero)
p1 <- ggplot(diag_min, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE) +
  labs(
    title = "Residuals vs Fitted",
    x = "Fitted values",
    y = "Residuals"
  )

# 2) Normal Q-Q (normality of residuals)
p2 <- ggplot(diag_min, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.35) +
  stat_qq_line() +
  labs(
    title = "Normal Q-Q",
    x = "Theoretical Quantiles",
    y = "Standardized Residuals"
  )

# 3) Scale-Location (homoskedasticity)
p3 <- ggplot(diag_min, aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
  geom_point(alpha = 0.35) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Scale-Location",
    x = "Fitted values",
    y = "Sqrt(|Standardized Residuals|)"
  )

# 4) Residuals vs Leverage (influence)
p4 <- ggplot(diag_min, aes(x = .hat, y = .std.resid)) +
  geom_point(alpha = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE) +
  labs(
    title = "Residuals vs Leverage",
    x = "Leverage",
    y = "Standardized Residuals"
  )

# Combine into a 2x2 panel
(p1 | p2) / (p3 | p4) +
  plot_annotation(title = "SLR Diagnostics (log_salary_min vs. min_experience_years)")


# SLR Diagnostics (log_salary_max)
# Build diagnostics data from the fitted model
diag_max <- broom::augment(slr_fit_salary_max) %>%
  dplyr::mutate(obs = dplyr::row_number())

# 1) Residuals vs Fitted (linearity + mean-zero)
q1 <- ggplot(diag_max, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE) +
  labs(
    title = "Residuals vs Fitted",
    x = "Fitted values",
    y = "Residuals"
  )

# 2) Normal Q-Q (normality of residuals)
q2 <- ggplot(diag_max, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.35) +
  stat_qq_line() +
  labs(
    title = "Normal Q-Q",
    x = "Theoretical Quantiles",
    y = "Standardized Residuals"
  )

# 3) Scale-Location (homoskedasticity)
q3 <- ggplot(diag_max, aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
  geom_point(alpha = 0.35) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Scale-Location",
    x = "Fitted values",
    y = "Sqrt(|Standardized Residuals|)"
  )

# 4) Residuals vs Leverage (influence)
q4 <- ggplot(diag_max, aes(x = .hat, y = .std.resid)) +
  geom_point(alpha = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE) +
  labs(
    title = "Residuals vs Leverage",
    x = "Leverage",
    y = "Standardized Residuals"
  )

# Combine into a 2x2 panel
(q1 | q2) / (q3 | q4) +
  plot_annotation(title = "SLR Diagnostics (log_salary_max vs. min_experience_years)")

