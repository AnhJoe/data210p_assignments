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
df <- read_csv("prostate.csv")

# Split data into training and test sets
train_df <- df %>% filter(train == TRUE)
test_df  <- df %>% filter(train == FALSE)

# Validate ntrain and ntest
ntrain <- nrow(train_df)
ntest  <- nrow(test_df)
ntrain
ntest

# Separate train/test predictors (X) and response (y)
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
    aes(color = "Linear Regression Line"),
    method = "lm",
    se = TRUE,
    level = 0.95
  ) +
  scale_color_manual(values = c("Linear Regression Line" = "blue")) +
  labs(
    title = "Simple Linear Regression (SLR): lpsa vs lcavol (Training Set)",
    x = "lcavol (log cancer volume)",
    y = "lpsa (log PSA)",
    color = "Legend"
  )


# Generate predictions using the SLR model
pred_test_slr <- predict(slr_fit, newdata = test_df)

# Evaluate SLR model performance on test set
rmse_slr <- RMSE(pred_test_slr, test_df$lpsa)
r2_slr   <- R2(pred_test_slr, test_df$lpsa)
rmse_slr #average prediction error in outcome units (lpsa)
r2_slr 

# Plot predicted vs actual for SLR model on test set
ref_line <- tibble(
  label = "Perfect Prediction",
  slope = 1,
  intercept = 0
)

tibble(
  actual = test_df$lpsa,
  predicted = as.numeric(pred_test_slr)
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
    title = "Simple Linear Regression (SLR): Predicted vs Actual lpsa (Test Set)",
    x = "Actual lpsa (Log PSA)",
    y = "Predicted lpsa (Log PSA)",
    color = "Legend"
  )


# SLR Conclusion: The SLR model using lcavol as a predictor in the test set shows moderate 
# generalization performance on the test set, with RMSE (0.69) and R-squared (0.54) values.
# With a RMSE of 0.69 on lpsa which translates to about a multiplicative factor of 2 in relative PSA error, 
# we can capture the order of magnitude to reasonably separate low vs. high PSA, but yet, 
# we cannot reliably distinguish between 6 vs. 9 ng/mL PSA values for clinical decision-making.
# While the R2 tells us that lcavol explains a good portion of variability in lpsa, the remaining 46% remains unexplained. 
# These results indicate room for improvement with more predictors in Multiple Linear Regression (MLR) model.

# Multiple Linear Regression (MLR) with all predictors except id, train, and lpsa
mlr_fit <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, data = train_df)

# Coefficient estimates, standard errors, t-tests
summary(mlr_fit)$coefficients

# 95% confidence intervals for beta parameters
confint(mlr_fit, level = 0.95)

# Generate predictions using the MLR model
pred_test_mlr <- predict(mlr_fit, newdata = test_df)


# Evaluate MLR model performance on test set
rmse_mlr <- RMSE(pred_test_mlr, test_df$lpsa)
r2_mlr   <- R2(pred_test_mlr, test_df$lpsa)
rmse_mlr
r2_mlr

# MLR fit for top 3 predictors lcavol, svi, lweight
mlr_fit_best <- lm(lpsa ~ lcavol + svi + lweight, data = train_df)
pred_test_mlr_best <- predict(mlr_fit_best, newdata = test_df)
# Evaluate MLR (best 3 predictors: lcavol, lweight, svi) model performance on test set
rmse_mlr_best <- RMSE(pred_test_mlr_best, test_df$lpsa)
r2_mlr_best   <- R2(pred_test_mlr_best, test_df$lpsa)
rmse_mlr_best
r2_mlr_best

# Plot predicted vs actual for MLR model on test set
tibble(
  actual = test_df$lpsa,
  predicted = as.numeric(pred_test_mlr)
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
    title = "Multiple Linear Regression (MLR): Predicted vs Actual lpsa (Test Set)",
    x = "Actual lpsa (Log PSA)",
    y = "Predicted lpsa (Log PSA)",
    color = "Legend"
  )


#VIF check for multicollinearity
# Rule-of-thumb interpretation:
# VIF ~ 1     : no multicollinearity
# VIF > 5    : moderate multicollinearity
# VIF > 10   : serious multicollinearity
vif(mlr_fit)

# MLR Conclusion: The MLR model using all predictors in the test set shows worse performance compared to the SLR.
# The RMSE increased from 0.6926 to 0.7220, and R-squared decreased from 0.5438 to 0.5052.
# Based on these metrics, the SLR explains more variance in the outcome than MLR and has lower prediction error.
# This suggests that adding more predictors introduced noise and overfitting rather than improving generalization. 


# Assumptions of Linear Regression: Normality, homoskedasticity, independence

# Create diagnostic plots using base R plotting
par(mfrow = c(2, 2)) # set plotting layout to 2x2
plot(slr_fit)  # residuals vs fitted, QQ plot, scale-location, leverage
par(mfrow = c(1, 1)) # reset plotting layout

# The built-in diagnostic plots are useful but can be improved with ggplot2 for better aesthetics and customization.
gg_lm_diagnostics <- function(
    fit,
    label_top = 5,
    show_cooks = TRUE,
    colors = list(
      points = "#2C7BB6",
      loess = "#D55E00",
      ref = "gray",
      qq = "darkgreen",
      cooks = "black",
      labels = "white"
    )
) {
  # Required: ggplot2, broom, dplyr, ggrepel, patchwork
  
  aug <- broom::augment(fit) %>%
    dplyr::mutate(
      sqrt_abs_std_resid = sqrt(abs(.std.resid)),
      obs = dplyr::row_number()
    )
  
  lab <- aug %>% dplyr::slice_max(.cooksd, n = label_top)
  
  theme_diag <- ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      axis.title = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 9),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 9),
      plot.margin = ggplot2::margin(6, 6, 6, 6)
    )
  
  legend_colors <- c(
    "LOESS smooth"      = colors$loess,
    "Reference (y = 0)" = colors$ref,
    "Q–Q reference"     = colors$qq
  )
  
  # 1) Residuals vs Fitted  (THIS panel owns the shared COLOR legend)
  p1 <- ggplot2::ggplot(aug, ggplot2::aes(.fitted, .resid)) +
    ggplot2::geom_point(color = colors$points, alpha = 0.35, size = 1.6) +
    ggplot2::geom_smooth(
      ggplot2::aes(color = "LOESS smooth"),
      method = "loess", se = FALSE, linewidth = 1
    ) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = 0, color = "Reference (y = 0)"),
      linetype = 2, linewidth = 0.8
    ) +
    ggplot2::scale_color_manual(
      values = legend_colors,
      breaks = c("LOESS smooth", "Reference (y = 0)")
    ) +
    ggplot2::labs(
      title = "Residuals vs Fitted",
      x = "Fitted values",
      y = "Residuals"
    ) +
    theme_diag +
    ggplot2::guides(
      color = ggplot2::guide_legend(order = 1, ncol = 1)
    )
  
  # 2) Normal Q–Q  (draw Q–Q line, but DON'T contribute a COLOR legend entry)
  p2 <- ggplot2::ggplot(aug, ggplot2::aes(sample = .std.resid)) +
    ggplot2::stat_qq(color = colors$points, alpha = 0.35, size = 1.6) +
    ggplot2::stat_qq_line(
      ggplot2::aes(color = "Q–Q reference"),
      linewidth = 0.9
    ) +
    ggplot2::labs(
      title = "Normal Q–Q",
      x = "Theoretical quantiles",
      y = "Standardized residuals"
    ) +
    theme_diag +
    ggplot2::scale_color_manual(
      values = legend_colors,
      breaks = "Q–Q reference"
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(order = 1, ncol = 1))
  
  
  # 3) Scale–Location  (draw LOESS, but DON'T contribute a COLOR legend entry)
  p3 <- ggplot2::ggplot(aug, ggplot2::aes(.fitted, sqrt_abs_std_resid)) +
    ggplot2::geom_point(color = colors$points, alpha = 0.35, size = 1.6) +
    ggplot2::geom_smooth(
      ggplot2::aes(color = "LOESS smooth"),
      method = "loess", se = FALSE, linewidth = 1
    ) +
    ggplot2::labs(
      title = "Scale–Location",
      x = "Fitted values",
      y = expression(sqrt("|Standardized residuals|"))
    ) +
    theme_diag +
    ggplot2::scale_color_manual(values = legend_colors, breaks = NULL) +
    ggplot2::guides(color = "none")
  
  # 4) Residuals vs Leverage
  n <- stats::nobs(fit)
  p <- length(stats::coef(fit))
  cook_levels <- c(0.5, 1)
  
  cook_contour <- function(h, c, p) sqrt(c * p * (1 - h) / h)
  h_grid <- seq(max(1e-6, min(aug$.hat, na.rm = TRUE)),
                max(aug$.hat, na.rm = TRUE),
                length.out = 300)
  
  contour_df <- bind_rows(lapply(cook_levels, function(c) {
    data.frame(.hat = h_grid,
               y = cook_contour(h_grid, c, p),
               level = paste0("Cook's D = ", c))
  })) %>%
    bind_rows(lapply(cook_levels, function(c) {
      data.frame(.hat = h_grid,
                 y = -cook_contour(h_grid, c, p),
                 level = paste0("Cook's D = ", c))
    }))
    
  p4 <- ggplot2::ggplot(aug, ggplot2::aes(.hat, .std.resid)) +
    ggplot2::geom_point(aes(size = .cooksd), color = colors$points, alpha = 0.35) +
    ggplot2::scale_size_continuous(name   = "Cook's D (points)", range  = c(1, 6),breaks = c(0.5, 1), labels = c("0.5", "1"))+
    ggplot2::geom_hline(aes(yintercept = 0, color = "Reference (y = 0)"),linetype = 2, linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = 2 * p / n, linetype = 3, color = colors$ref, linewidth = 0.8) +
    ggplot2::scale_color_manual(values = legend_colors) +
    ggplot2::labs(title = "Residuals vs Leverage", x = "Leverage (hat values)", y = "Standardized residuals") +
    ggplot2::geom_line(
        data = contour_df,
        ggplot2::aes(.hat, y, linetype = level),
        color = colors$cooks,
        inherit.aes = FALSE,
        linewidth = 0.8
      ) +
    ggplot2::scale_linetype_manual(
      name = "Cook's D =",
      values = c(
        "Cook's D = 0.5" = "solid",
        "Cook's D = 1"   = "dashed")) +
    ggrepel::geom_text_repel(
      data = lab,
      ggplot2::aes(label = obs),
      color = colors$labels,
      size = 3,
      max.overlaps = Inf) +
    ggplot2::guides(
      color = "none",
      size = ggplot2::guide_legend(order = 2),
      linetype = ggplot2::guide_legend(order = 3)) +
    theme_diag
    
  # Build 2×2 and collect legends into one
  (p1 | p2) / (p3 | p4) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")
}

# Plot MLR diagnostics
gg_lm_diagnostics(mlr_fit)

# Identify top 3 influential points based on Cook's distance
aug <- broom::augment(mlr_fit, data = train_df)
k <- 5
top3 <- aug %>%
  dplyr::arrange(dplyr::desc(.cooksd)) %>%
  dplyr::select(id, .cooksd, .hat, .std.resid, .resid, .fitted) %>%
  dplyr::slice_head(n = k)
top3_rows <- train_df %>%
  dplyr::inner_join(top3 %>% dplyr::select(id), by = "id")
top3
top3_rows


# --- Homoskedasticity test (Breusch–Pagan) ---
# H0: Var(eps_i | x_i) = constant (sigma^2)
bptest(mlr_fit)

# Plot Residuals vs. lcavol, lweight, svi (partial residuals)
# Augment model
aug <- broom::augment(mlr_fit)

# Extract coefficients
beta_lcavol  <- coef(mlr_fit)["lcavol"]
beta_lweight <- coef(mlr_fit)["lweight"]
beta_svi     <- coef(mlr_fit)["svi"]

# Reusable diagnostic color palette
diag_colors <- c(
  "Observations" = "#2C7BB6",
  "LOESS smooth" = "#D55E00",
  "Linear trend" = "gray40"
)

# Partial residuals vs lcavol
aug %>%
  mutate(partial_resid_lcavol = .resid + beta_lcavol * lcavol) %>%
  ggplot(aes(lcavol, partial_resid_lcavol)) +
  geom_point(
    aes(color = "Observations"),
    alpha = 0.25
  ) +
  geom_smooth(
    aes(color = "LOESS smooth"),
    method = "loess",
    se = FALSE,
    linewidth = 1
  ) +
  geom_smooth(
    aes(color = "Linear trend"),
    method = "lm",
    se = FALSE,
    linetype = 2,
    linewidth = 0.9
  ) +
  scale_color_manual(values = diag_colors) +
  labs(
    title = "Partial Residuals vs lcavol (MLR-adjusted)",
    x = "lcavol",
    y = "Partial residuals",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 10),
    legend.position = "bottom"
  )


# Partial residuals vs lweight
aug %>%
  mutate(partial_resid_lweight = .resid + beta_lweight * lweight) %>%
  ggplot(aes(lweight, partial_resid_lweight)) +
  geom_point(
    aes(color = "Observations"),
    alpha = 0.25
  ) +
  geom_smooth(
    aes(color = "LOESS smooth"),
    method = "loess",
    se = FALSE,
    linewidth = 1
  ) +
  geom_smooth(
    aes(color = "Linear trend"),
    method = "lm",
    se = FALSE,
    linetype = 2,
    linewidth = 0.9
  ) +
  scale_color_manual(values = diag_colors) +
  labs(
    title = "Partial Residuals vs lweight (MLR-adjusted)",
    x = "lweight",
    y = "Partial residuals",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 10),
    legend.position = "bottom"
  )

# Partial residuals vs svi
aug %>%
  mutate(partial_resid_svi = .resid + beta_svi * svi) %>%
  ggplot(aes(svi, partial_resid_svi)) +
  geom_point(
    aes(color = "Observations"),
    alpha = 0.25
  ) +
  geom_smooth(
    aes(color = "Linear trend"),
    method = "lm",
    se = FALSE,
    linetype = 2,
    linewidth = 0.9
  ) +
  scale_color_manual(values = diag_colors) +
  labs(
    title = "Partial Residuals vs svi (MLR-adjusted)",
    x = "svi",
    y = "Partial residuals",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 10),
    legend.position = "bottom"
  )


# # --- Normality test (Shapiro–Wilk) ---
# # H0: errors are normally distributed
# #shapiro.test(resid(fit1)): will give an error, because it 
# #cannot handle more than 5000 residuals.
# 
# set.seed(210)
# # For large datasets, sample 5000 residuals for the test
# n <- length(res)
# shapiro.test(sample(resid(mlr_fit), size = min(5000, n)))
# 
# 
# # --- Independence / autocorrelation (Durbin–Watson) ---
# # NOTE: DW is meaningful for ordered observations (e.g., time series).
# # Here, recordings come from multiple subjects over time.
# # A simple illustration: sort within subject by test_time.
# pd_ord <- pd %>%
#   arrange(subject., test_time)
# 
# fit1_ord <- lm(total_UPDRS ~ PPE, data = pd_ord)
# dwtest(fit1_ord)
# 
# # --- Addressing non-normality: Box–Cox transformation ---
# # Requires strictly positive response; total_UPDRS is nonnegative in practice.
# min(pd$total_UPDRS)
# 
# # Estimate lambda by maximizing the profile log-likelihood
# bc <- boxcox(fit1, lambda = seq(-2, 2, by = 0.05), plotit = TRUE)
# lambda_hat <- bc$x[which.max(bc$y)]
# lambda_hat
# 
# # Transform the response using Box–Cox definition
# boxcox_transform <- function(y, lambda) {
#   if (abs(lambda) < 1e-8) return(log(y))
#   (y^lambda - 1) / lambda
# }
# 
# # Apply transformation
# pd$Y_bc <- boxcox_transform(pd$total_UPDRS, lambda_hat)
# 
# # Fit linear model with transformed response
# fit1_bc <- lm(Y_bc ~ PPE, data = pd)
# summary(fit1_bc)
# 
# # Compare diagnostics pre/post transformation
# par(mfrow = c(2, 2)) # set plotting layout to 2x2
# plot(fit1) # original
# plot(fit1_bc) # Box–Cox transformed
# par(mfrow = c(1, 1)) # reset plotting layout