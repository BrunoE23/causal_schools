# Decision: Use imputed SIMCE income midpoint to build the VA income decile control

**Date:** 2026-05-13
**Status:** active

## Context

The school observational value-added regressions already condition on 4th-grade SIMCE math and language scores. The parent-survey income variable, `income_decile`, was previously excluded because it had missing values and had not been cleaned enough for controlled VA use.

A diagnostic on the current `univ_gr8_df` found that, among students with both baseline SIMCE score controls, `74,287` students were missing `income_decile`. This is a moderate but not prohibitive amount of missingness.

With the `n = 15` rule, the diagnostic imputed:

- `46,310` from 4th-grade SIMCE school x grade-8 student comuna x SIMCE year
- `14,099` from 4th-grade SIMCE school x grade-8 student comuna
- `10,032` from 4th-grade SIMCE school x SIMCE year
- `2,346` from 4th-grade SIMCE school

This left `1,500` students missing income among the baseline-SIMCE-score population in the diagnostic run.

## Decision

Use an imputed income control in controlled school value-added regressions.

The imputed variable is:

- `income_decile_imputed`

It is built by first imputing the SIMCE parent-survey income midpoint:

- `income_mid_imputed`

The script preserves observed `income_mid` when available. For missing values, it imputes only among students who have both baseline SIMCE score controls. After midpoint imputation, `income_decile_imputed` is recomputed from the observed-plus-imputed `income_mid_imputed` distribution among students with both baseline SIMCE score controls.

The imputation uses median observed `income_mid` with a minimum donor-cell size of `15`, in this order:

1. 4th-grade SIMCE school x grade-8 student comuna x SIMCE year
2. 4th-grade SIMCE school x grade-8 student comuna
3. 4th-grade SIMCE school x SIMCE year
4. 4th-grade SIMCE school

Cases still missing after these four steps remain missing. The rule intentionally does not fill with a broad year, comuna-only, or national median.

## Rationale

The imputation variables are pre-treatment relative to the high-school value-added object. The 4th-grade SIMCE school comes from the baseline SIMCE file, and the grade-8 student comuna is observed before later higher-education outcomes and before the post-grade-8 school-value object is formed.

Using a minimum donor-cell size avoids relying on very thin baseline-context cells. The `n = 15` threshold recovers nearly all missing income cases among students who otherwise have both baseline SIMCE score controls while still leaving thin cells missing.

## Implementation

Implemented in:

- `code/clean_simce_survey.R`
- `code/universe_reg_df.R`
- `code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R`

`clean_simce_survey.R` now keeps the 4th-grade SIMCE school identifier as:

- `simce_rbd_4to`

`universe_reg_df.R` creates:

- `income_mid_observed`
- `income_mid_imputed`
- `income_mid_missing`
- `income_mid_impute_source`
- `income_mid_impute_n`
- `income_mid_was_imputed`
- `income_decile_observed`
- `income_decile_imputed`
- `income_decile_was_imputed`
- `income_decile_imputation_min_n`
- `income_mid_missing_after_impute`
- `income_decile_missing_after_impute`

The controlled VA script includes `income_decile_imputed` in the main control set. In the gender-gap VA model, it is included in the baseline controls and is interacted with gender under the current interaction rule.

## Risks and Follow-Up

The imputed income control is still a proxy based on survey response and baseline-context medians. It should be used with the imputation-source and donor-count diagnostics available for sensitivity checks.

The CPAD files also contain parent-education information that may be useful for future SES controls. Those variables were noted but not added in this implementation.
