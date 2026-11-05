# Family-background heterogeneity in EB VA pass-through

Run from the repository root with R, data.table, fixest and ggplot2:

```powershell
$env:R_LIBS_USER = 'C:/Users/xd-br/AppData/Local/R/win-library/4.4'
& 'C:/Program Files/R/R-4.4.1/bin/Rscript.exe' code/codex/family_background_heterogeneity/01_estimate_and_plot.R
```

## Reference figure and current estimation data

The visual reference is the paper's five-outcome EB achievement-quintile figure
(`04_run_primary_five_grade4_quintile_iv_eb.R`), with staggered outcome estimates,
robust 95% confidence intervals and a horizontal reference at theta = 1.
Its original Stata main-five regression dataframe is unavailable locally and
its high-premium-outcome samples predate the current exam-taker rule. These new
plots explicitly use the **current** main-IV sample rules and saved R EB values,
rather than silently mixing those earlier runs.

Input is `data/clean/peer_lottery_variation/main_regression_df_with_peer_shocks.csv`,
the saved current main-IV frame with peer diagnostics left-joined. Only original
main-IV columns are selected. No peer variable enters a regression or sample
restriction. Override this frame with `FAMILY_HET_REGRESSION_DF` if needed.
It contains unique students from timely at-risk SAE processes 2018-2020.

The five series are Math, Verbal, High-premium institution, High-premium field,
and log projected program income. Existing `program_income_adj_eb` is used:
its outcome is verified identical to `log_program_income_full_clp_m1`, and
attended/offered school values are verified against the corresponding
`log_program_income_clp_m1` row in the unified R EB file, along with the four
other series.

Each IV uses attended-school EB VA instrumented by offered-school EB VA,
controlling for its expected EB VA, cohort indicators, linear Grade 4 math and
verbal SIMCE, gender indicators and Grade 8 age indicators. All five outcomes
use exam takers and outcome/control/instrument/treatment complete cases,
matching the current main runner. Standard errors are fixest `vcov="hetero"`
with its existing default finite-sample correction; intervals are beta +/-
1.96 SE, matching the reference plot. School values are fixed across background
groups; VA is not re-estimated or rescaled within groups.

## Background definitions

Background comes from selected columns of Dropbox `data/clean/univ_gr8_df.csv`
and is uniquely merged by mrun. Override the Dropbox root with
`CAUSAL_SCHOOLS_DATA_WD`.

- Income: `income_mid_imputed`, the baseline Grade 4 SIMCE parent-survey family
  income midpoint after the existing project imputation.
- Education: row mean of `father_educ_years_imputed` and
  `mother_educ_years_imputed`, following the existing peer-family-education
  convention. One observed parent is used if only one is available; both
  missing stays missing. These are the questionnaire's approximate years.

For both backgrounds, quintiles are constructed **once in the pooled saved
lottery frame before outcome-specific filtering**, using average ranks and
`ceiling(5*rank/N)`, exactly the reference achievement-quintile rule. Ties are
kept together, so group sizes need not be exactly 20%. These are within-frame
income quintiles, distinct from simply pairing the broad-sample income deciles
in the earlier student income-distribution table. Missing background is
excluded only from that background's regressions. Education-year ranges appear
on its x axis and in `quintile_definitions.csv`.

## Outputs and validation

Figures (PNG/PDF) are under `output/figures/family_background_heterogeneity/`;
derived data are under `data/clean/family_background_heterogeneity/`:

- `family_background_heterogeneity_estimates.csv`: all 50 subgroup estimates,
  robust SEs/intervals, regression Ns and robust first-stage F statistics.
- `quintile_definitions.csv`, `background_coverage.csv`,
  `student_background_groups.csv`: grouping reference, coverage and assignments.
- `pooled_estimates.csv`, `pooled_main_iv_parity.csv`: pooled estimates and
  exact beta/SE/N checks against the four corresponding current saved main
  results (the saved main table does not include projected program income).
- `school_value_source_validation.csv`: all five outcomes' attended/offered
  school-value equality checks against the unified R EB file.
- `group_sample_partition_checks.csv`: outcome-specific quintile regression Ns
  sum to the same background-observed complete-case sample before splitting.

Original figures, main regression data and raw source files are preserved.
