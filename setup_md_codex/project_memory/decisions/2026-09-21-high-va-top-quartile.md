# Decision: Define high VA as the top quartile

**Date:** 2026-09-21
**Status:** active

After inspecting the five primary EB VA distributions, Bruno confirmed that
"top 25%" meant above P75, rather than above P25. The plots did not reveal a
clear natural separation, so this is an explicit classification convention.

Implementation uses equal school weights, as stated when implementing the
confirmation. Each outcome's cutoff is its inverse empirical CDF at .75
(R quantile type 1), matching the plotted equal-school reference lines.
The population is schools with finite All-sample `va_eb_centered` in the
existing five primary `stata_eb_school_values_*.csv` inputs: math, language,
highinst, highpay, and program_income_full. No SAE restriction is added.
These are observational EB estimates, not RC-VAM estimates.

Indicator = 1 strictly above the cutoff and 0 otherwise; ties at the cutoff
receive 0. Missing or nonfinite VA remains missing. Consequently the high
share can be slightly below 25% from discrete school counts or ties.
Code: `code/codex/high_va_cutoffs/02_build_indicators.R`.
Outputs: `data/clean/high_va_cutoffs/school_high_va.csv`, long-form source
values and indicators, and `high_va_cutoffs.csv`. No downstream regression
specification has yet been changed.
