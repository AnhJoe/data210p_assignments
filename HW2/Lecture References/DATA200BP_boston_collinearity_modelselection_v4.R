#!/usr/bin/env Rscript
# DATA 200BP — Collinearity & Model Selection (Boston Housing)
#
# This script reproduces the lecture workflow in R:
#   1) Full multiple regression: MEDV ~ all predictors
#   2) Collinearity diagnostics: correlations, VIF
#   3) Stepwise selection: AIC and BIC
#   4) Regularization: ridge and lasso via glmnet (CV; lambda.min and lambda.1se)
#
# Dataset: boston.xls (note: despite the extension, this file is CSV text)

# -----------------------------
# Setup
# -----------------------------
DATA_PATH <- "boston.xls"
OUTDIR <- "boston_figs"
dir.create(OUTDIR, showWarnings = FALSE)

# Helper: safe package loading
use_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Package '", pkg, "' not installed. Install with install.packages('", pkg, "')"))
  }
}

# -----------------------------
# Load data
# -----------------------------
boston <- read.csv(DATA_PATH)
stopifnot("MEDV" %in% names(boston))

# -----------------------------
# 1) Full OLS model
# -----------------------------
lm_full <- lm(MEDV ~ ., data = boston)
cat("\n=== Full OLS: MEDV ~ all predictors ===\n")
print(summary(lm_full))

# -----------------------------
# 2) Collinearity diagnostics
# -----------------------------

# Correlation heatmap (base R)
png(file.path(OUTDIR, "corr_heatmap_baseR.png"), width = 1100, height = 900, res = 140)
corr <- cor(boston[, setdiff(names(boston), "MEDV")])
image(1:ncol(corr), 1:ncol(corr), corr,
      axes = FALSE, xlab = "", ylab = "",
      main = "Predictor Correlation Heatmap")
axis(1, at = 1:ncol(corr), labels = colnames(corr), las = 2, cex.axis = 0.7)
axis(2, at = 1:ncol(corr), labels = colnames(corr), las = 2, cex.axis = 0.7)
box()
dev.off()

# Scatter: RAD vs TAX
r <- cor(boston$RAD, boston$TAX)
png(file.path(OUTDIR, "scatter_rad_tax_R.png"), width = 800, height = 600, res = 140)
plot(boston$TAX, boston$RAD, pch = 19, cex = 0.6,
     main = paste0("RAD vs TAX (corr = ", round(r, 2), ")"),
     xlab = "TAX", ylab = "RAD")
dev.off()

# VIF
# Option A: car::vif (if available)
vif_table <- NULL
if (requireNamespace("car", quietly = TRUE)) {
  vif_vals <- car::vif(lm_full)
  vif_table <- sort(vif_vals, decreasing = TRUE)
} else {
  # Option B: compute VIF manually: VIF_j = 1/(1 - R_j^2)
  Xnames <- setdiff(names(boston), "MEDV")
  vif_vals <- sapply(Xnames, function(v) {
    others <- setdiff(Xnames, v)
    m <- lm(as.formula(paste(v, "~", paste(others, collapse = "+"))), data = boston)
    r2 <- summary(m)$r.squared
    1 / (1 - r2)
  })
  vif_table <- sort(vif_vals, decreasing = TRUE)
}
cat("\n=== VIF (Full Model) ===\n")
print(vif_table)

png(file.path(OUTDIR, "vif_full_R.png"), width = 900, height = 600, res = 140)
par(mar = c(5, 8, 3, 2))
barplot(rev(vif_table), horiz = TRUE, las = 1,
        main = "Variance Inflation Factors (Full Model)",
        xlab = "VIF")
abline(v = 5, lty = 2)
abline(v = 10, lty = 2)
dev.off()

# -----------------------------
# 3) Stepwise selection: AIC and BIC
# -----------------------------
use_pkg("MASS")

# Stepwise AIC (default)
lm_aic <- step(lm_full, direction = "both", trace = 0)
cat("\n=== Stepwise AIC model ===\n")
print(formula(lm_aic))

# Stepwise BIC: use k = log(n)
n <- nrow(boston)
lm_bic <- step(lm_full, direction = "both", k = log(n), trace = 0)
cat("\n=== Stepwise BIC model ===\n")
print(formula(lm_bic))

# VIF after AIC (optional)
if (requireNamespace("car", quietly = TRUE)) {
  vif_aic <- sort(car::vif(lm_aic), decreasing = TRUE)
  png(file.path(OUTDIR, "vif_stepwise_aic_R.png"), width = 900, height = 600, res = 140)
  par(mar = c(5, 8, 3, 2))
  barplot(rev(vif_aic), horiz = TRUE, las = 1,
          main = "VIF After Stepwise AIC (May Still Be Large)",
          xlab = "VIF")
  abline(v = 5, lty = 2)
  abline(v = 10, lty = 2)
  dev.off()
}

# -----------------------------
# 4) Ridge and Lasso via glmnet
# -----------------------------
use_pkg("glmnet")

x <- model.matrix(MEDV ~ . , data = boston)[, -1]  # drop intercept column
y <- boston$MEDV

set.seed(200)

# Ridge (alpha=0)
cv_ridge <- glmnet::cv.glmnet(x, y, alpha = 0, nfolds = 5, standardize = TRUE)
cat("\n=== Ridge CV ===\n")
cat("lambda.min:", cv_ridge$lambda.min, "\n")
cat("lambda.1se:", cv_ridge$lambda.1se, "\n")

png(file.path(OUTDIR, "ridge_cv_R.png"), width = 900, height = 650, res = 140)
plot(cv_ridge, main = "Ridge CV (alpha=0)")
dev.off()

png(file.path(OUTDIR, "ridge_path_R.png"), width = 900, height = 650, res = 140)
plot(glmnet::glmnet(x, y, alpha = 0, standardize = TRUE),
     xvar = "lambda", main = "Ridge Coefficient Paths")
dev.off()

# Lasso (alpha=1)
cv_lasso <- glmnet::cv.glmnet(x, y, alpha = 1, nfolds = 5, standardize = TRUE)
cat("\n=== Lasso CV ===\n")
cat("lambda.min:", cv_lasso$lambda.min, "\n")
cat("lambda.1se:", cv_lasso$lambda.1se, "\n")

png(file.path(OUTDIR, "lasso_cv_R.png"), width = 900, height = 650, res = 140)
plot(cv_lasso, main = "Lasso CV (alpha=1)")
dev.off()

png(file.path(OUTDIR, "lasso_path_R.png"), width = 900, height = 650, res = 140)
plot(glmnet::glmnet(x, y, alpha = 1, standardize = TRUE),
     xvar = "lambda", main = "Lasso Coefficient Paths")
dev.off()

# Show selected variables at lambda.1se
coef_1se <- coef(cv_lasso, s = "lambda.1se")
selected <- rownames(coef_1se)[which(coef_1se != 0)]
selected <- setdiff(selected, "(Intercept)")
cat("\nSelected (nonzero) predictors at lambda.1se:\n")
print(selected)

cat("\nDone. Figures saved to:", OUTDIR, "\n")


# ------------------------------------------------------------
# Geometry demo: collinearity creates elongated RSS contours
# ------------------------------------------------------------

make_geometry_collinearity_figure <- function(outpath = file.path(output_dir, "geometry_collinearity.png"), seed = 0){
  set.seed(seed)
  n <- 120
  z <- rnorm(n)
  x1 <- z + 0.05*rnorm(n)
  x2 <- 1.02*z + 0.05*rnorm(n)  # highly collinear with x1

  beta0 <- 0; beta1 <- 2; beta2 <- -2
  y <- beta0 + beta1*x1 + beta2*x2 + 0.4*rnorm(n)

  b1 <- seq(-6, 6, length.out = 240)
  b2 <- seq(-6, 6, length.out = 240)
  RSS <- outer(b1, b2, Vectorize(function(B1, B2){
    sum((y - (beta0 + B1*x1 + B2*x2))^2)
  }))

  png(outpath, width = 900, height = 650, res = 160)
  contour(b1, b2, RSS, nlevels = 18, xlab = expression(beta[1]), ylab = expression(beta[2]),
          main = "RSS contours under strong collinearity")

  # Ridge (L2) circle
  r <- 3.2
  t <- seq(0, 2*pi, length.out = 400)
  lines(r*cos(t), r*sin(t), lty = 2, lwd = 2)

  # Lasso (L1) diamond
  d <- 3.2
  diamond <- rbind(c(0,d), c(d,0), c(0,-d), c(-d,0), c(0,d))
  lines(diamond[,1], diamond[,2], lty = 3, lwd = 2)

  legend("topright", legend = c("L2 ball (ridge)", "L1 ball (lasso)"),
         lty = c(2,3), lwd = c(2,2), bty = "n")
  dev.off()
}

make_geometry_collinearity_figure()
