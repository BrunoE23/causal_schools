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
