# Results-section table builders

`11_format_primary_seven_va_distribution.R` formats the paper-facing Table 5
from `output/tables/results_section/primary_seven_va_distribution.csv`. The
input combines seven primary outcomes with their EB posterior value-added
distributions and cross-cohort RSS latent standard deviations. The formatter
keeps P10, P50, and P90, reports student sample sizes, and places the RSS
bootstrap standard error below its standard deviation to keep the table
readable.

The source CSV is built from the outcome-specific EB outputs and
`data/clean/rss_debiased_varcov/four_year_no_sae/rss_varcov_matrix.csv`.
