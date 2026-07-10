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
- `admission_exam_taker`
- `higher_ed_enrolled_m1`
- `stem_enrollment_m1`
- `log_program_income_area_clp_m1`
- `log_program_income_institution_clp_m1`
- `log_program_income_full_clp_m1`
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
output/tables/empirical_bayes_school_va/scalar_school_value_iv_higher_ed_enrollment_expected_va_eb.csv
output/tables/empirical_bayes_school_va/scalar_school_value_iv_higher_ed_enrollment_expected_va_eb.tex
```

The current Stata one-outcome path also writes repo-local outputs named
`stata_eb_school_values_<outcome>.csv`. After running the one-outcome Stata jobs,
combine them with:

```r
source("code/codex/empirical_bayes_school_va/06_combine_stata_va_eb_outputs.R")
```

Program income now has three Stata outcome keys:

```text
program_income_area
program_income_institution
program_income_full
```

The Stata regressions use short variable aliases to satisfy Stata's 32-character
variable-name limit, but the exported VA/EB `outcome` labels use the canonical
research names above.

The expected-VA IV runner also supports the corresponding EB scalar specs:

```text
program_income_area_adj_eb
program_income_institution_adj_eb
program_income_full_adj_eb
```

When using the Stata VA/EB estimates, point `EB_SCHOOL_VALUES_INPUT_PATH` to
`output/tables/empirical_bayes_school_va/stata_eb_school_rbd_observational_values_for_iv.csv`.

Program-income-full can also be decomposed into mutually orthogonal EB VA
components with:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' code/codex/empirical_bayes_school_va/09_decompose_program_income_full_va.R
```

The decomposition is school-level, uses the Stata EB VA values, weights by the
`program_income_full` regression N, and applies this ordered Gram-Schmidt basis:

```text
math VA
institution-income VA residualized on math VA
area-income VA residualized on math VA and institution-income VA
```

For the simpler exam-first version currently under consideration, set:

```powershell
$env:PROGRAM_INCOME_DECOMP_ORDER='exam,math,area,institution'
```

This uses:

```text
exam-taking VA
math VA residualized on exam-taking VA
area-income VA residualized on exam-taking VA and math VA
institution-income VA residualized on exam-taking VA, math VA, and area-income VA
```

It writes an extended IV input file:

```text
output/tables/empirical_bayes_school_va/stata_eb_school_rbd_observational_values_for_iv_with_program_income_decomposition.csv
```

The optional decomposition IV specs are:

```text
program_income_full_math_component_adj_eb
program_income_full_institution_component_adj_eb
program_income_full_area_component_adj_eb
```

The exam-first version also supports:

```text
program_income_full_exam_component_adj_eb
```

They are not part of the default IV run; request them with `EB_IV_VALUE_SPECS`
and point `EB_SCHOOL_VALUES_INPUT_PATH` to the extended file above.
The residual component is kept in the decomposition output only as a
reconstruction check; it is not used as an IV scalar.

For the `math -> area -> institution` or `exam -> math -> area -> institution`
ordering, the no-residual joint IV is run
with:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' code/codex/empirical_bayes_school_va/10_run_program_income_decomposition_joint_iv_eb.R
```

Set `PROGRAM_INCOME_DECOMP_JOINT_COMPONENTS` to the ordered components to include
in that joint regression. This regression jointly instruments the attended
decomposition components with the offered-school decomposition components and
excludes the residual component.

The higher-education enrollment outcome is:

```text
higher_ed_enrolled_m1 = 1[COD_SIES_m1 is observed]
```

The high-paying-field enrollment outcome is:

```text
high_paying_field_m1 = 1[student matriculates in Science, Law,
Engineering/Manufacturing/Construction, or Medicine+]
```

`Medicine+` is the presentation group containing `Medicina`, `Quimica y
Farmacia`, `Enfermeria`, `Obstetricia y Puericultura`, `Tecnologia Medica`, and
`Odontologia`. Other health-related and technical health programs are grouped as
`Other Healthcare` and are not included in the high-paying-field binary. No
observed matriculation is coded `0`; matriculated students with insufficient
field classification remain missing.
The variable used by the VA/EB runner is only `1`, `0`, or missing.

When ready, estimate the high-paying-field VA/EB with the one-outcome Stata
runner:

```stata
do code/codex/empirical_bayes_school_va/05_construct_va_eb_one_outcome_stata.do highpay 1
```

This has not been run yet in the current iteration.

The exam-taking outcome is:

```text
admission_exam_taker = 1[math_max or leng_max is observed]
```

Each outcome should use the maximum valid follow-up available for that outcome.
The current principal VA/EB stage-1 sample is grade-8 cohorts 2017-2020. These
are the four cohorts with the current grade-4 SIMCE controls. Cohort 2021 is in
the rebuilt universe and has PAES 2026 records, but it is excluded from the
principal VA/EB specification until a comparable grade-4 control strategy is
chosen. Cohort 2017 remains in VA/EB; any later exclusion of 2017 from SAE/IV
comes from missing `DA_probs_2017` support, not from the VA stage.

Within the 2017-2020 VA sample, `admission_exam_taker` is estimated on the full
eligible school/age/control sample. Score outcomes are estimated on students
with observed scores. SIES-based outcomes such as `stem_enrollment_m1`,
`high_paying_field_m1`, and `log_program_income_full_clp_m1` use all students
with complete SIES follow-up. Cohort 2021 must not be silently coded as zero for
SIES outcomes while full `Matricula-Ed-Superior-2026` is missing.

The higher-education enrollment definition uses the direct SIES program-code signal from the clean
higher-education matricula file. It should not be replaced with
`field_reclassified_m1`, accreditation fields, or program-income availability,
because those variables can be missing for data-quality or classification
reasons after enrollment has already been observed.

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
