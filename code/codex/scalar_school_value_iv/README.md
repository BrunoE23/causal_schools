# Scalar School-Value IV

This folder contains the construction and estimation code for the scalar school-value IV regressions.

The construction layer is:

- `01_build_scalar_school_value_iv_df.R`

The estimation layer is:

- `02_run_scalar_school_value_iv.do`
- `03_append_gender_gap_by_gender.do`

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

The current Stata scripts include both `prob_*` and `iszero_*` probability controls and write to `scalar_school_value_iv_results_k100_timely_risk_prob_iszero.*`, so earlier `prob_*`-only estimates are not overwritten. If this is too slow, temporarily change `local prob_controls prob_* iszero_*` to `local prob_controls prob_*` and change `controls_stub` to a matching label.

The append script `03_append_gender_gap_by_gender.do` runs `stem_adj` and `stem_gap_adj` separately for boys and girls, then appends those rows to the existing results table without rerunning the main seven specifications. It assumes `GEN_ALU == 1` for boys and `GEN_ALU == 2` for girls.
