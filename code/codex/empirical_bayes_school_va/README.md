# Empirical-Bayes School Value Added

This folder implements an Empirical-Bayes robustness path for the scalar school-value IV design.

The goal is to shrink the stage-1 observational school value-added estimates before using them as the scalar school-quality index in the expected-VA IV.

## Workflow

Run these scripts in order.

```r
source("code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R")
source("code/codex/empirical_bayes_school_va/01_construct_eb_school_values.R")
source("code/codex/empirical_bayes_school_va/02_run_expected_va_scalar_iv_eb.R")
```

The first command must be rerun because the school-value constructor now exports `controlled_value_added_se`, `controlled_value_added_resid_sd`, and `controlled_value_added_se_method`.

## Stage-1 SE Export

The existing school-value constructor estimates adjusted school value added from individual-level regressions with high-school RBD and middle-school RBD fixed effects.

The constructor now appends a regression-derived SE for each student-weighted centered school effect.

```text
controlled_value_added_se = SE(controlled_value_added_centered_student)
```

Because `fixef()` returns fixed-effect point estimates but not their SEs, the constructor runs an auxiliary regression with `school_rbd` entered as explicit dummies and the remaining fixed effects still absorbed.
Operationally, the script uses `lfe::felm()` and `lfe::getfe(se = TRUE)` with a custom estimable function for the student-weighted centered school effect.
This matters: the SE used for EB is not the SE of a raw reference-normalized fixed-effect level; it is the regression-derived SE of `school FE - student-weighted mean(school FE)`, matching `controlled_value_added_centered_student`.
The current stage-1 SE method label is `lfe_getfe_school_rbd_centered_student_iid_bN100`.
`controlled_value_added_resid_sd` is kept only as a diagnostic.

## EB Shrinkage

`01_construct_eb_school_values.R` reads:

```text
data/clean/school_rbd_observational_values/school_rbd_observational_values.csv
```

It keeps All-sample adjusted VA for:

- `z_year_math_max`
- `z_year_leng_max`
- `stem_enrollment_m1`
- `log_program_income_clp_m1`
- `program_certified_years_m1`
- `inst_certified_years_m1`

For each outcome, it estimates the latent across-school variance as:

```text
tau_m^2 = max(weighted Var(Vhat_s^m) - weighted mean(se_s^2), 0)
```

using `n_students_regression` as the weight.

The school-specific reliability is:

```text
lambda_s^m = tau_m^2 / (tau_m^2 + se_s^2)
```

The EB value is:

```text
V_EB_s^m = mean_m + lambda_s^m * (Vhat_s^m - mean_m)
```

The output is recentered to preserve the student-weighted zero-mean convention used in the current school VA.

Outputs:

```text
data/clean/empirical_bayes_school_va/eb_school_rbd_observational_values.csv
data/clean/empirical_bayes_school_va/eb_school_rbd_observational_values_diagnostics.csv
```

Diagnostics include the original and EB-shrunken SDs, the estimated `tau`, and reliability quantiles.

## EB Expected-VA IV

`02_run_expected_va_scalar_iv_eb.R` repeats the expected-VA scalar IV after replacing the school-value index with `controlled_value_added_eb_centered_student`.

For each metric:

```text
A_i^EB = V_EB_{most_time_RBD}^m
O_i^EB = V_EB_{rbd_treated_1R}^m
E_i^EB = sum_s p_is V_EB_s^m
```

The IV regression instruments `A_i^EB` with `O_i^EB` and controls for `E_i^EB` plus the same core covariates used in the current expected-VA IV.

Outputs:

```text
data/clean/empirical_bayes_school_va/scalar_school_value_iv_expected_va_eb.csv
data/clean/empirical_bayes_school_va/scalar_school_value_iv_expected_va_eb_diagnostics.csv
output/tables/empirical_bayes_school_va/scalar_school_value_iv_results_expected_va_eb.csv
output/tables/empirical_bayes_school_va/scalar_school_value_iv_main_four_expected_va_eb.csv
output/tables/empirical_bayes_school_va/scalar_school_value_iv_main_four_expected_va_eb.tex
output/tables/empirical_bayes_school_va/scalar_school_value_iv_accreditation_expected_va_eb.csv
output/tables/empirical_bayes_school_va/scalar_school_value_iv_accreditation_expected_va_eb.tex
output/tables/empirical_bayes_school_va/scalar_school_value_iv_program_income_expected_va_eb.csv
output/tables/empirical_bayes_school_va/scalar_school_value_iv_program_income_expected_va_eb.tex
```

When Stata is unavailable, all regressions in this workflow are run in R.
The program-income EB-IV run can be restricted to the new outcome with environment variables, for example:

```powershell
$env:SCHOOL_VA_OUTCOMES='log_program_income_clp_m1'
$env:SCHOOL_RBD_VALUES_OUTPUT_PATH='C:/Users/brunem/Dropbox/causal_schools/data/clean/school_rbd_observational_values/school_rbd_observational_values_program_income.csv'
$env:SCHOOL_VA_SE_EF='school_centered_student'
$env:SCHOOL_VA_SE_BOOTSTRAP_REPS='100'
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R

$env:SCHOOL_RBD_VALUES_INPUT_PATH='C:/Users/brunem/Dropbox/causal_schools/data/clean/school_rbd_observational_values/school_rbd_observational_values_program_income.csv'
$env:EB_SCHOOL_VALUES_OUTPUT_PATH='C:/Users/brunem/Dropbox/causal_schools/data/clean/empirical_bayes_school_va/eb_school_rbd_observational_values_program_income.csv'
$env:EB_SCHOOL_VA_OUTCOMES='log_program_income_clp_m1'
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' code/codex/empirical_bayes_school_va/01_construct_eb_school_values.R

$env:EB_SCHOOL_VALUES_INPUT_PATH='C:/Users/brunem/Dropbox/causal_schools/data/clean/empirical_bayes_school_va/eb_school_rbd_observational_values_program_income.csv'
$env:EB_IV_VALUE_SPECS='program_income_adj_eb'
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' code/codex/empirical_bayes_school_va/02_run_expected_va_scalar_iv_eb.R
```

## Interpretation

The resulting coefficient is a pass-through coefficient per unit of EB-shrunken observational school VA.

Because EB shrinkage compresses the school-value scale, EB and non-EB coefficients should be compared together with the SD of the corresponding school-value index.
The EB coefficient may be mechanically larger if the same causal movement is measured against a more compressed index.

This workflow does not implement hybrid causal EB or IV VAM.
It validates an EB-shrunken observational school-value index along the SAE lottery margin.
