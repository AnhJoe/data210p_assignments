#!/usr/bin/env python3
"""
DATA 200BP — Collinearity & Model Selection (Boston Housing)

This script reproduces the core analyses used in the lecture:
  1) Multiple regression with MEDV as response
  2) Collinearity diagnostics (correlations, scatterplots, VIF)
  3) Model selection (stepwise AIC/BIC; forward/backward by CV MSE)
  4) Regularization (ridge + lasso with cross-validation; lambda_min and lambda_1se)

Dataset: boston.xls (note: despite the extension, this file is CSV-formatted text).
"""

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

import statsmodels.api as sm
from statsmodels.stats.outliers_influence import variance_inflation_factor

from sklearn.model_selection import KFold
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
from sklearn.linear_model import LinearRegression, RidgeCV, LassoCV, Lasso, Ridge
from sklearn.linear_model import lasso_path


# -----------------------------
# Configuration
# -----------------------------
DATA_PATH = "boston.xls"
OUTDIR = "boston_figs"
RANDOM_STATE = 200

# CV choices (adjust as desired)
K_FOLDS_SELECTION = 3   # used for forward/backward selection plots in the deck
K_FOLDS_PENALTY = 5     # used for ridge/lasso CV


# -----------------------------
# Utilities
# -----------------------------
def add_const(X: pd.DataFrame) -> pd.DataFrame:
    """Add intercept column for statsmodels OLS."""
    return sm.add_constant(X, has_constant="add")


def compute_vif(X: pd.DataFrame) -> pd.DataFrame:
    """
    Compute VIFs for each column in X (excluding intercept).
    NOTE: VIF uses an intercept in the auxiliary regressions, so we include const here.
    """
    Xc = add_const(X)
    out = []
    for i, col in enumerate(Xc.columns):
        if col == "const":
            continue
        out.append((col, float(variance_inflation_factor(Xc.values, i))))
    return pd.DataFrame(out, columns=["variable", "VIF"]).sort_values("VIF", ascending=False)


def stepwise_selection(df: pd.DataFrame, y_col: str, x_cols: list[str],
                       criterion: str = "aic") -> tuple[list[str], float]:
    """
    Greedy stepwise selection (both directions) using AIC or BIC.
    Returns: (selected_vars, final_score)
    """
    y = df[y_col]
    predictors = list(x_cols)

    def fit(cols):
        X = add_const(df[cols])
        return sm.OLS(y, X).fit()

    def score(cols):
        m = fit(cols)
        return float(m.aic) if criterion.lower() == "aic" else float(m.bic)

    included: list[str] = []
    changed = True

    while changed:
        changed = False

        # ---- forward step ----
        excluded = [c for c in predictors if c not in included]
        if excluded:
            best_var = None
            best_score = np.inf
            for new_var in excluded:
                sc = score(included + [new_var])
                if sc < best_score:
                    best_score = sc
                    best_var = new_var

            current_score = np.inf if not included else score(included)
            if best_score < current_score - 1e-8:
                included.append(best_var)  # type: ignore[arg-type]
                changed = True

        # ---- backward step ----
        if len(included) > 1:
            current_score = score(included)
            best_score = current_score
            worst_var = None
            for var in list(included):
                cols = [c for c in included if c != var]
                sc = score(cols)
                if sc < best_score - 1e-8:
                    best_score = sc
                    worst_var = var
            if worst_var is not None:
                included.remove(worst_var)
                changed = True

    return included, score(included)


def cv_mse_numpy(X: np.ndarray, y: np.ndarray, cols_idx: list[int], folds) -> float:
    """
    Fast-ish CV MSE for linear regression using numpy least squares.
    Uses an intercept in each fold by adding a column of ones.
    """
    n = X.shape[0]
    Xi = X[:, cols_idx]
    Xd = np.column_stack([np.ones(n), Xi])

    mses = []
    for tr, te in folds:
        beta, *_ = np.linalg.lstsq(Xd[tr], y[tr], rcond=None)
        pred = Xd[te] @ beta
        mses.append(np.mean((y[te] - pred) ** 2))
    return float(np.mean(mses))


# -----------------------------
# Main
# -----------------------------
def main():
    os.makedirs(OUTDIR, exist_ok=True)

    # Load data (CSV-formatted)
    df = pd.read_csv(DATA_PATH)
    y = df["MEDV"].values
    X = df.drop(columns=["MEDV"])
    predictors = list(X.columns)

    # ========== 1) Full OLS ==========
    ols = sm.OLS(df["MEDV"], add_const(X)).fit()
    print("\n=== Full OLS: MEDV ~ all predictors ===")
    print(ols.summary())

    # ========== 2) Collinearity diagnostics ==========
    # Correlation heatmap (simple matplotlib version)
    corr = X.corr().values
    plt.figure(figsize=(10, 8))
    plt.imshow(corr, aspect="auto")
    plt.colorbar(shrink=0.8)
    plt.xticks(range(len(predictors)), predictors, rotation=90)
    plt.yticks(range(len(predictors)), predictors)
    plt.title("Predictor Correlation Heatmap")
    plt.tight_layout()
    plt.savefig(os.path.join(OUTDIR, "corr_heatmap_matplotlib.png"), dpi=200)
    plt.close()

    # Scatter: RAD vs TAX
    r = np.corrcoef(df["RAD"], df["TAX"])[0, 1]
    plt.figure(figsize=(6, 4.5))
    plt.scatter(df["TAX"], df["RAD"], alpha=0.6, s=18)
    plt.title(f"RAD vs TAX (corr = {r:.2f})")
    plt.xlabel("TAX")
    plt.ylabel("RAD")
    plt.tight_layout()
    plt.savefig(os.path.join(OUTDIR, "scatter_rad_tax.png"), dpi=200)
    plt.close()

    # VIF (full model)
    vif_full = compute_vif(X)
    print("\n=== VIF (Full Model) ===")
    print(vif_full)

    plt.figure(figsize=(7, 4.8))
    v = vif_full.sort_values("VIF", ascending=True)
    plt.barh(v["variable"], v["VIF"])
    plt.axvline(5, linestyle="--", linewidth=1)
    plt.axvline(10, linestyle="--", linewidth=1)
    plt.title("Variance Inflation Factors (Full Model)")
    plt.xlabel("VIF")
    plt.tight_layout()
    plt.savefig(os.path.join(OUTDIR, "vif_full.png"), dpi=200)
    plt.close()

    # ========== 3) Model selection ==========
    aic_vars, aic_val = stepwise_selection(df, "MEDV", predictors, criterion="aic")
    bic_vars, bic_val = stepwise_selection(df, "MEDV", predictors, criterion="bic")

    print("\n=== Stepwise AIC selected variables ===")
    print(aic_vars)
    print("AIC:", aic_val)

    print("\n=== Stepwise BIC selected variables ===")
    print(bic_vars)
    print("BIC:", bic_val)

    vif_aic = compute_vif(df[aic_vars])
    plt.figure(figsize=(7, 4.8))
    v = vif_aic.sort_values("VIF", ascending=True)
    plt.barh(v["variable"], v["VIF"])
    plt.axvline(5, linestyle="--", linewidth=1)
    plt.axvline(10, linestyle="--", linewidth=1)
    plt.title("VIF After Stepwise AIC (May Still Be Large)")
    plt.xlabel("VIF")
    plt.tight_layout()
    plt.savefig(os.path.join(OUTDIR, "vif_stepwise_aic.png"), dpi=200)
    plt.close()

    # Forward/backward selection by CV MSE (greedy; for illustration)
    Xmat = X.values
    cv = KFold(n_splits=K_FOLDS_SELECTION, shuffle=True, random_state=RANDOM_STATE)
    folds = list(cv.split(Xmat))

    # forward
    remaining = predictors.copy()
    selected = []
    forward_mse = []
    for _ in range(len(predictors)):
        best_var, best_mse = None, np.inf
        for cand in remaining:
            idx = [predictors.index(c) for c in (selected + [cand])]
            mse = cv_mse_numpy(Xmat, y, idx, folds)
            if mse < best_mse:
                best_mse, best_var = mse, cand
        selected.append(best_var)  # type: ignore[arg-type]
        remaining.remove(best_var)  # type: ignore[arg-type]
        forward_mse.append(best_mse)

    plt.figure(figsize=(7, 4.6))
    plt.plot(range(1, len(forward_mse) + 1), forward_mse, marker="o")
    plt.title(f"Forward Selection: {K_FOLDS_SELECTION}-Fold CV MSE vs Model Size")
    plt.xlabel("Number of predictors")
    plt.ylabel("CV MSE")
    plt.tight_layout()
    plt.savefig(os.path.join(OUTDIR, "forward_cv_mse.png"), dpi=200)
    plt.close()

    # backward
    selected = predictors.copy()
    backward_mse = []
    while True:
        idx = [predictors.index(c) for c in selected]
        backward_mse.append(cv_mse_numpy(Xmat, y, idx, folds))
        if len(selected) == 1:
            break
        best_remove, best_mse = None, np.inf
        for cand in selected:
            subset = [c for c in selected if c != cand]
            idx_sub = [predictors.index(c) for c in subset]
            mse = cv_mse_numpy(Xmat, y, idx_sub, folds)
            if mse < best_mse:
                best_mse, best_remove = mse, cand
        selected.remove(best_remove)  # type: ignore[arg-type]

    sizes = list(range(len(predictors), 0, -1))
    plt.figure(figsize=(7, 4.6))
    plt.plot(sizes, backward_mse, marker="o")
    plt.title(f"Backward Elimination: {K_FOLDS_SELECTION}-Fold CV MSE vs Model Size")
    plt.xlabel("Number of predictors")
    plt.ylabel("CV MSE")
    plt.tight_layout()
    plt.savefig(os.path.join(OUTDIR, "backward_cv_mse.png"), dpi=200)
    plt.close()

    # ========== 4) Ridge and Lasso ==========
    alphas = np.logspace(-3, 3, 200)

    ridge_cv = RidgeCV(alphas=alphas, cv=K_FOLDS_PENALTY, scoring="neg_mean_squared_error")
    pipe_ridge = Pipeline([("scaler", StandardScaler()), ("ridgecv", ridge_cv)])
    pipe_ridge.fit(Xmat, y)
    best_alpha_ridge = float(pipe_ridge.named_steps["ridgecv"].alpha_)
    print("\n=== Ridge CV ===")
    print("best lambda (alpha):", best_alpha_ridge)

    # Ridge coefficient paths
    scaler = StandardScaler()
    Xstd = scaler.fit_transform(Xmat)
    alphas_path = np.logspace(-3, 3, 100)
    coefs = []
    for a in alphas_path:
        m = Ridge(alpha=a)
        m.fit(Xstd, y)
        coefs.append(m.coef_)
    coefs = np.array(coefs)

    plt.figure(figsize=(7.4, 4.8))
    for j in range(coefs.shape[1]):
        plt.plot(np.log10(alphas_path), coefs[:, j], linewidth=1)
    plt.axvline(np.log10(best_alpha_ridge), linestyle="--", linewidth=1)
    plt.title("Ridge: Coefficient Paths vs log10(lambda)")
    plt.xlabel("log10(lambda)")
    plt.ylabel("Standardized coefficient")
    plt.tight_layout()
    plt.savefig(os.path.join(OUTDIR, "ridge_path.png"), dpi=200)
    plt.close()

    # Lasso CV
    lasso_cv = LassoCV(cv=K_FOLDS_PENALTY, random_state=RANDOM_STATE, max_iter=20000)
    pipe_lasso = Pipeline([("scaler", StandardScaler()), ("lassocv", lasso_cv)])
    pipe_lasso.fit(Xmat, y)
    best_alpha_lasso = float(pipe_lasso.named_steps["lassocv"].alpha_)
    print("\n=== Lasso CV ===")
    print("lambda_min (alpha):", best_alpha_lasso)

    # 1-SE rule (largest alpha within 1 SE of minimum CV error)
    lassocv = pipe_lasso.named_steps["lassocv"]
    mse_path = lassocv.mse_path_              # (n_alphas, n_folds)
    alphas_cv = lassocv.alphas_               # descending
    mean_mse = mse_path.mean(axis=1)
    se_mse = mse_path.std(axis=1, ddof=1) / np.sqrt(mse_path.shape[1])
    min_idx = int(np.argmin(mean_mse))
    threshold = float(mean_mse[min_idx] + se_mse[min_idx])
    candidate_idxs = np.where(mean_mse <= threshold)[0]
    alpha_1se = float(alphas_cv[candidate_idxs[0]])
    print("lambda_1se (alpha):", alpha_1se)

    # Fit lasso at 1-SE alpha for a sparser model
    lasso_1se = Pipeline([("scaler", StandardScaler()),
                          ("lasso", Lasso(alpha=alpha_1se, max_iter=20000))])
    lasso_1se.fit(Xmat, y)
    coef_1se = lasso_1se.named_steps["lasso"].coef_
    selected_vars = [predictors[i] for i, c in enumerate(coef_1se) if abs(c) > 1e-6]
    print("Selected (nonzero) predictors at lambda_1se:", selected_vars)

    # Lasso coefficient path
    alphas_lp = np.logspace(-3, 1.2, 120)
    _, coef_path, _ = lasso_path(Xstd, y, alphas=alphas_lp, max_iter=20000)

    plt.figure(figsize=(7.4, 4.8))
    for j in range(coef_path.shape[0]):
        plt.plot(np.log10(alphas_lp), coef_path[j, :], linewidth=1)
    plt.axvline(np.log10(best_alpha_lasso), linestyle="--", linewidth=1)
    plt.axvline(np.log10(alpha_1se), linestyle="--", linewidth=1)
    plt.title("Lasso: Coefficient Paths vs log10(lambda)")
    plt.xlabel("log10(lambda)")
    plt.ylabel("Standardized coefficient")
    plt.tight_layout()
    plt.savefig(os.path.join(OUTDIR, "lasso_path.png"), dpi=200)
    plt.close()

    # Lasso CV curve (with error bars)
    plt.figure(figsize=(7.2, 4.8))
    plt.errorbar(np.log10(alphas_cv), mean_mse, yerr=se_mse,
                 fmt="o", markersize=3, capsize=2)
    plt.axvline(np.log10(best_alpha_lasso), linestyle="--", linewidth=1)
    plt.axvline(np.log10(alpha_1se), linestyle="--", linewidth=1)
    plt.title("Lasso CV: MSE vs log10(lambda) (min vs 1-SE)")
    plt.xlabel("log10(lambda)")
    plt.ylabel("CV MSE")
    plt.tight_layout()
    plt.savefig(os.path.join(OUTDIR, "lasso_cv.png"), dpi=200)
    plt.close()

    print("\nDone. Figures saved to:", OUTDIR)


if __name__ == "__main__":
    main()


# ------------------------------------------------------------
# Geometry demo: collinearity creates elongated RSS contours
# ------------------------------------------------------------

import numpy as np
import matplotlib.pyplot as plt

def make_geometry_collinearity_figure(outpath="geometry_collinearity.png", seed=0):
    np.random.seed(seed)
    n = 120
    z = np.random.normal(size=n)
    x1 = z + 0.05*np.random.normal(size=n)
    x2 = 1.02*z + 0.05*np.random.normal(size=n)  # highly collinear with x1

    X = np.column_stack([np.ones(n), x1, x2])
    beta_true = np.array([0.0, 2.0, -2.0])
    y = X @ beta_true + 0.4*np.random.normal(size=n)

    b1 = np.linspace(-6, 6, 240)
    b2 = np.linspace(-6, 6, 240)
    B1, B2 = np.meshgrid(b1, b2)

    RSS = np.zeros_like(B1)
    for i in range(B1.shape[0]):
        pred = beta_true[0] + (B1[i, :][None, :]*x1[:, None]) + (B2[i, :][None, :]*x2[:, None])
        RSS[i, :] = np.sum((y[:, None] - pred)**2, axis=0)

    plt.figure(figsize=(7.5, 5.5))
    plt.contour(B1, B2, RSS, levels=18)
    plt.xlabel(r"$\beta_1$")
    plt.ylabel(r"$\beta_2$")
    plt.title("RSS contours under strong collinearity")

    # Ridge (L2) ball and Lasso (L1) ball
    r = 3.2
    t = np.linspace(0, 2*np.pi, 400)
    plt.plot(r*np.cos(t), r*np.sin(t), linestyle="--", linewidth=1.5, label=r"$\ell_2$ ball (ridge)")

    d = 3.2
    diamond = np.array([[0, d], [d, 0], [0, -d], [-d, 0], [0, d]])
    plt.plot(diamond[:, 0], diamond[:, 1], linestyle="-.", linewidth=1.5, label=r"$\ell_1$ ball (lasso)")

    plt.legend(loc="upper right", frameon=True)
    plt.tight_layout()
    plt.savefig(outpath, dpi=200)
    plt.close()

# Generate the figure
make_geometry_collinearity_figure(outpath=os.path.join(OUTPUT_DIR, "geometry_collinearity.png"))
