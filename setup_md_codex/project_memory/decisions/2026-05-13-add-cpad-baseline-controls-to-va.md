# Decision: Add imputed CPAD baseline controls to VA

**Date:** 2026-05-13
**Status:** active

## Context

The 4th-grade SIMCE parent survey (`_cpad`) contains baseline family and early-childhood variables that can improve the school value-added control set. After adding an imputed income control, the next useful controls are parent education, parent indigenous-status indicators, and early-childhood attendance indicators.

Household size is not included in the current control set because, in the current 4th-grade CPAD files, it is only available for 2015-2016 rather than all 2013-2016 baseline SIMCE years.

## Decision

Add the following CPAD-derived controls to the controlled VA regressions:

- `father_educ_years_imputed`
- `mother_educ_years_imputed`
- `father_indigenous_imputed`
- `mother_indigenous_imputed`
- `sala_cuna_imputed`
- `jardin_imputed`
- `prekinder_imputed`
- `kinder_imputed`

Missing values are imputed using the same baseline-context hierarchy and minimum donor-cell size as income:

1. 4th-grade SIMCE school x grade-8 student comuna x SIMCE year
2. 4th-grade SIMCE school x grade-8 student comuna
3. 4th-grade SIMCE school x SIMCE year
4. 4th-grade SIMCE school

The minimum donor-cell size is `15`. Remaining missing values are left missing.

## Education-Year Assumptions

Father and mother education categories are converted into approximate years of education before imputation.

For 2013, 2015, and 2016:

- `1` no schooling -> `0`
- basic-school year categories -> corresponding completed years `1` through `8`
- high-school year categories -> `9` through `12`
- incomplete technical/professional postsecondary -> `13`
- completed technical/professional postsecondary or incomplete university -> `14`
- completed university -> `16`
- master's degree -> `18`
- doctorate -> `21`
- empty, double mark, and "do not know / do not remember" -> missing

For 2014, the first education category is 1st grade rather than no schooling, so categories `1` through `8` map directly to years `1` through `8`; later categories follow the same approximate-year interpretation where possible.

These are control-variable approximations, not final descriptive education measures.

## Binary Controls and Median Imputation

Parent indigenous-status variables and early-childhood attendance variables are cleaned as:

- `1` yes -> `1`
- `2` no -> `0`
- empty, double mark, and "do not know / do not remember" -> missing

Median imputation for binary controls can produce `0.5` when the donor cell is tied. These fractional values are intentionally kept for now as baseline-context control values rather than rounded.

Diagnostic counts from the current baseline-SIMCE population showed exact `0.5` imputations were rare:

- father indigenous: `400`
- mother indigenous: `483`
- sala cuna: `233`
- jardin infantil: `1,900`
- prekinder: `56`
- kinder: `13`

## Implementation

Implemented in:

- `code/clean_simce_survey.R`
- `code/universe_reg_df.R`
- `code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R`

The universe build writes `_observed`, `_missing`, `_impute_source`, `_impute_level`, `_impute_n`, `_was_imputed`, and `_missing_after_impute` companion fields for each CPAD-imputed control.

The `_impute_level` fields are coded as:

- `0`: observed
- `1`: 4th-grade SIMCE school x grade-8 student comuna x SIMCE year
- `2`: 4th-grade SIMCE school x grade-8 student comuna
- `3`: 4th-grade SIMCE school x SIMCE year
- `4`: 4th-grade SIMCE school
- missing: still missing or outside the baseline-SIMCE imputation population

## Risks and Follow-Up

The education-year conversion is intentionally coarse. If parent education becomes a main descriptive variable rather than a control, revisit the category mapping and document it separately.

The binary imputation leaves `0.5` ties as fractional controls. This is acceptable for the current linear-control VA specification, but sensitivity checks could round ties or include missing/source flags explicitly.
