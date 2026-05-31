# Scalar School-Value IV

This folder contains the construction and estimation code for the scalar school-value IV regressions.

## Current Default: Expected-VA Risk Control

The current preferred estimation path is the expected-VA scalar-risk-control approach implemented in:

- `08_run_expected_va_scalar_iv.R`
- `09_run_expected_va_quintile_heterogeneity.R`
- `10_run_expected_va_grade4_quintile_heterogeneity.R`
- `11_run_expected_va_grade4_tercile_heterogeneity.R`

Use this path for new main estimates and heterogeneous-effects tables. It replaces the high-dimensional `prob_*` and `iszero_*` controls with one scalar expected-value risk control per school-value metric:

`expected_VA_i = sum_s p_is * V_s`

where `p_is` is the simulated assignment probability and `V_s` is the All-sample school value for that metric.

The expected-VA outputs are:

- `data/clean/scalar_school_value_iv/scalar_school_value_iv_expected_va.csv`
- `data/clean/scalar_school_value_iv/scalar_school_value_iv_expected_va_diagnostics.csv`
- `output/tables/scalar_school_value_iv/scalar_school_value_iv_results_expected_va.csv`
- `output/tables/scalar_school_value_iv/scalar_school_value_iv_main_results_expected_va.csv`
- `output/tables/scalar_school_value_iv/scalar_school_value_iv_main_results_expected_va.tex`
- `output/tables/scalar_school_value_iv/scalar_school_value_iv_expected_va_by_simce8_math_quintile.csv`
- `output/tables/scalar_school_value_iv/scalar_school_value_iv_expected_va_by_simce8_math_quintile_main.csv`
- `output/tables/scalar_school_value_iv/scalar_school_value_iv_expected_va_by_simce8_math_quintile_main.tex`
- `output/tables/scalar_school_value_iv/scalar_school_value_iv_expected_va_by_simce4_math_quintile_same_sample.csv`
- `output/tables/scalar_school_value_iv/scalar_school_value_iv_expected_va_by_simce4_math_quintile_same_sample_main.csv`
- `output/tables/scalar_school_value_iv/scalar_school_value_iv_expected_va_by_simce4_math_quintile_same_sample_main.tex`
- `output/tables/scalar_school_value_iv/scalar_school_value_iv_expected_va_by_simce4_math_tercile_same_sample.csv`
- `output/tables/scalar_school_value_iv/scalar_school_value_iv_expected_va_by_simce4_math_tercile_same_sample_main.csv`
- `output/tables/scalar_school_value_iv/scalar_school_value_iv_expected_va_by_simce4_math_tercile_same_sample_main.tex`

Do not use the older wide `prob_*` / `iszero_*` Stata path for new heterogeneity work unless the task is explicitly a robustness or legacy comparison.

## Legacy Wide-Probability-Control Path

The construction layer is:

- `01_build_scalar_school_value_iv_df.R`

The estimation layer is:

- `02_run_scalar_school_value_iv.do`
- `03_append_gender_gap_by_gender.do`

These scripts implement the older wide probability-control design. They are kept for reproducibility and robustness checks, not as the default path for new estimates.

The R construction script starts from:

- `data/clean/univ_gr8_df.csv`
- `data/clean/DA_probs/probs_columns_wide_k<support>_timely_risk.csv`
- `data/clean/DA_probs/probability_support_k<support>_timely_risk.csv`
- `data/clean/school_rbd_observational_values/school_rbd_observational_values.csv`

It writes the regression-ready dataframe to:

- `data/clean/scalar_school_value_iv/scalar_school_value_iv_k<support>_timely_risk.csv`

The exported dataframe is intentionally compact: it keeps identifiers, first-pass outcomes and controls, probability controls, diagnostic probability-mass variables, and the constructed `d_*`/`z_*` scalar value variables. It does not carry the full `univ_gr8_df` string and metadata columns into Stata.

It also writes:

- `data/clean/scalar_school_value_iv/scalar_school_value_iv_k25_timely_risk_baselines.csv`
- `data/clean/scalar_school_value_iv/scalar_school_value_iv_k25_timely_risk_diagnostics.csv`

The Stata script imports that R-created CSV once and saves a local Stata copy:

- `data/clean/scalar_school_value_iv/scalar_school_value_iv_k25_timely_risk.dta`

The first-pass specifications use the value of the attended school as the endogenous scalar treatment:

- `D_i = V_{most_time_RBD}`

The scalar instrument is the value of the first-round offered school:

- `Z_i = V_{rbd_treated_1R}`

The scalar offer instrument is coded relative to the outside-support offered-school baseline. For each school-value measure, the script computes the mean value of first-round offered RBDs that are not in the k=25 supported probability-control set among timely at-risk students. It then codes:

- supported offered RBD: `Z_i = V_{rbd_treated_1R} - mean(V_outside_support_offered)`
- outside-support offered RBD: `Z_i = 0`
- no first-round offer: `Z_i = 0`

The endogenous attended-school value is centered using the same outside-support offered-school baseline:

- `D_i = V_{most_time_RBD} - mean(V_outside_support_offered)`

Thus the instrument is nonzero only for supported lottery-school offers whose school value differs from the outside-support offered-school baseline.

The requested treatment variants are:

- math value, unadjusted
- math value, adjusted
- verbal/language value, unadjusted
- verbal/language value, adjusted
- STEM enrollment value, unadjusted
- STEM enrollment value, adjusted
- STEM enrollment gender-gap value, adjusted

The output table is written to:

- `output/tables/scalar_school_value_iv/scalar_school_value_iv_results_k<support>_timely_risk.csv`
- `output/tables/scalar_school_value_iv/scalar_school_value_iv_results_k<support>_timely_risk.dta`

The script reports:

- `n_obs`: IV estimation sample size
- `fs_beta`: first-stage coefficient on the scalar offer instrument
- `fs_se`: robust first-stage standard error
- `fs_f`: robust first-stage F statistic from testing the scalar offer instrument
- `iv_seconds` and `fs_seconds`: elapsed time for the IV and first-stage diagnostic regressions

The legacy Stata scripts include both `prob_*` and `iszero_*` probability controls and write to `scalar_school_value_iv_results_k100_timely_risk_prob_iszero.*`, so earlier `prob_*`-only estimates are not overwritten. This path should not be the starting point for current heterogeneity work.

The append script `03_append_gender_gap_by_gender.do` runs `stem_adj` and `stem_gap_adj` separately for boys and girls, then appends those rows to the existing results table without rerunning the main seven specifications. It assumes `GEN_ALU == 1` for boys and `GEN_ALU == 2` for girls.

## Expected-VA Risk-Control Details

The expected-VA variant is implemented in:

- `08_run_expected_va_scalar_iv.R`

This variant drops the k-support probability-control restriction. Instead of carrying `prob_<rbd>` and `iszero_<rbd>` controls for a supported school set, it uses all numeric RBDs appearing in the long DA probability files and constructs one scalar risk control per value metric:

`expected_VA_i = sum_s p_is * V_s`

where `p_is` is the simulated assignment probability and `V_s` is the All-sample school value for that metric. Non-school `unmatched` probability mass contributes zero.

For heterogeneous-effects tables, split or interact on the expected-VA regression dataframe and keep the matching `expected_<metric>` control for each `d_<metric>` / `z_<metric>` pair.

`09_run_expected_va_quintile_heterogeneity.R` implements the first grade-8 SIMCE heterogeneity table. It merges `simce8_math_quintile` from `data/clean/simce8_heterogeneity/cohort_2019_math_heterogeneity.csv` into the expected-VA regression dataframe and reports theta by grade-8 math quintile. The main table uses adjusted Math, Language, and STEM school values; the full CSV also keeps the unadjusted specifications.

`10_run_expected_va_grade4_quintile_heterogeneity.R` repeats the exercise by grade-4 math SIMCE quintile. It keeps the same SIMCE-8-linked 2019 sample as the grade-8 quintile table, excluding students with grade-4 controls but missing grade-8 math quintile so the grade-4 and grade-8 splits are directly comparable.

`11_run_expected_va_grade4_tercile_heterogeneity.R` uses the same sample as `10_run_expected_va_grade4_quintile_heterogeneity.R`, but constructs terciles directly from the grade-4 math score rather than collapsing deciles.
