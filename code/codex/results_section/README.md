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

`12_run_unshrunk_cross_iv_rss_validation.R` estimates the full directed
seven-by-seven cross-outcome IV system using unshrunk school value added. It
compares each direct IV coefficient with the prediction obtained by multiplying
the corresponding RSS implied gain by the destination outcome's unshrunk
same-outcome IV pass-through. The script saves the full estimates, a compact
comparison table, and summary agreement statistics. Equality inference is not
reported because it requires a joint bootstrap of the RSS and IV estimators.

`13_format_unshrunk_cross_iv_rss_ratios.R` reformats that validation exercise
as a seven-by-seven matrix of direct IV gains divided by RSS-implied causal
gains. A value of one indicates exact agreement. The diagonal equals one by
construction, and cells with predictions close to zero should be interpreted
from the levels table rather than from the ratio alone.
