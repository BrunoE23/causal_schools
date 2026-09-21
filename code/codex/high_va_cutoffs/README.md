# High-VA cutoff exploration

Run `Rscript code/codex/high_va_cutoffs/01_plot_distributions.R` from the repository root.
Requires data.table and ggplot2. Reads the five primary `stata_eb_school_values_*.csv`
files in `output/tables/empirical_bayes_school_va`, using All-sample estimates and
`va_eb_centered`. These are observational EB estimates, not the newer RC-VAM estimates.

Produces histograms under equal school and outcome-specific student weights, plus
ordered school values to inspect gaps without histogram bin sensitivity, in
`output/figures/high_va_cutoffs`. Summary CSV goes to `data/clean/high_va_cutoffs`.
The entire observed range is retained. Quantiles use the inverse weighted empirical CDF.
Zero, median, and 75th percentile are reference conventions only; no threshold is
selected and no binary indicators are created. Visual separation alone does not
establish distinct latent school types; estimation noise and EB shrinkage can affect shape.

## Selected binary definition

Run `Rscript code/codex/high_va_cutoffs/02_build_indicators.R` to create the
user-selected top-quartile indicators. For each of the five outcomes, high VA
equals 1 if `va_eb_centered > P75`, otherwise 0. Cutoffs use equal school weights
and the inverse empirical CDF (R quantile type 1), matching the plots. Schools
exactly at the cutoff receive 0; ties are not arbitrarily split. Unavailable
or nonfinite VA remains missing. The reference population is all schools with
finite All-sample EB VA for that outcome, without an SAE restriction.

Outputs in `data/clean/high_va_cutoffs`:
- `school_high_va.csv`: one row per school, five `high_va_*` indicators.
- `school_high_va_long.csv`: source VA, outcome, cutoff, and indicator by school/outcome.
- `high_va_cutoffs.csv`: thresholds, counts, missingness, ties, and high-VA shares.

Original VA files and downstream regressions are unchanged.

For the median alternative, set `HIGH_VA_PERCENTILE=50` before running
`02_build_indicators.R`. The same five VA inputs and equal-school empirical
quantile convention are used, with strict VA > P50 and ties coded 0. All three
indicator outputs go to `data/clean/high_va_cutoffs/median/`, preserving P75.
Unset the variable or set it to 75 to use the original top-quartile default.
