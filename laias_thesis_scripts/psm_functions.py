import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np

def plot_covariate_balance(matched_df, unmatched_df, covariates):
    # Reset index to avoid duplicate index issues
    matched_df = matched_df.copy().reset_index(drop=True)
    unmatched_df = unmatched_df.copy().reset_index(drop=True)
    
    # Add dataset labels
    matched_df["dataset"] = "Matched"
    unmatched_df["dataset"] = "Unmatched"
    
    # Combine safely
    df = pd.concat([matched_df, unmatched_df], axis=0, ignore_index=True)
    
    n = len(covariates)
    ncols = 2
    nrows = (n + 1) // 2
    
    fig, axes = plt.subplots(nrows=nrows, ncols=ncols, figsize=(16, 5 * nrows))
    axes = axes.flatten()
    
    for i, col in enumerate(covariates):
        sns.boxplot(
            x="dataset",
            y=col,
            hue="treated",
            data=df,
            ax=axes[i]
        )
        
        axes[i].set_title(f"Balance Check: {col}")
        axes[i].set_xlabel("")
        axes[i].set_ylabel(col)
    
    # Clean empty axes
    for j in range(i + 1, len(axes)):
        fig.delaxes(axes[j])
    
    plt.tight_layout()
    plt.show()

def compute_smd(treated: pd.Series, control: pd.Series) -> float:
    """
    Compute the absolute standardized mean difference.
    Binary variables use the pooled proportion denominator;
    continuous variables use the pooled standard deviation.
    """
    mean_t, mean_c = treated.mean(), control.mean()
    is_binary = set(treated.dropna().unique()).issubset({0, 1}) and \
                set(control.dropna().unique()).issubset({0, 1})

    if is_binary:
        p_t, p_c = mean_t, mean_c
        pooled_sd = np.sqrt((p_t * (1 - p_t) + p_c * (1 - p_c)) / 2)
    else:
        pooled_sd = np.sqrt((treated.var() + control.var()) / 2)

    return 0.0 if pooled_sd == 0 else abs((mean_t - mean_c) / pooled_sd)


def smd_table_from_pymatch(m, covariates: list[str]) -> pd.DataFrame:
    """
    Build a covariate balance table from a fitted Pymatch Matcher object.

    Parameters
    ----------
    m : pymatch.Matcher.Matcher
        A Matcher instance after calling m.match().
    covariates : list[str]
        Covariate column names to include in the table.

    Returns
    -------
    pd.DataFrame
        One row per covariate: pre/post means and SMDs.
    """
    # ── Pre-match data ───────────────────────────────────────────────────────
    # Pymatch stores original treated/control in m.test and m.control
    treated_pre = m.test
    control_pre = m.control

    # ── Post-match data ──────────────────────────────────────────────────────
    # After m.match(), matched records live in m.matched_data
    matched    = m.matched_data
    treated_post = matched[matched[m.yvar] == 1]
    control_post = matched[matched[m.yvar] == 0]

    rows = []
    for cov in covariates:
        t_pre = treated_pre[cov].dropna()
        c_pre = control_pre[cov].dropna()
        t_post = treated_post[cov].dropna()
        c_post = control_post[cov].dropna()

        rows.append({
            "Covariate":             cov,
            "Mean Treated (Before)": round(t_pre.mean(),  3),
            "Mean Control (Before)": round(c_pre.mean(),  3),
            "SMD (Before)":          round(compute_smd(t_pre, c_pre), 3),
            "Mean Treated (After)":  round(t_post.mean(), 3),
            "Mean Control (After)":  round(c_post.mean(), 3),
            "SMD (After)":           round(compute_smd(t_post, c_post), 3),
        })

    result = pd.DataFrame(rows).set_index("Covariate")

    # Flag imbalanced covariates (|SMD| >= 0.10 after matching)
    result["Balanced"] = result["SMD (After)"].apply(
        lambda x: "✓" if x < 0.10 else "✗"
    )
    return result
