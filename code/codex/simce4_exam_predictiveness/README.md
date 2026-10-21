# Predicted entrance-exam score and realized gap

The current exercise is implemented by `07_construct_regress_bg.R` and
`08_verify_report_bg.R`. Scripts 00-06 and their earlier outputs are
pre-existing exploratory analyses and are not rerun by this exercise.

## Inputs and sample

Read selected columns from the paper's linked master,
`C:/Users/brunem/Box/causal_schools/data/clean/univ_gr8_df.csv`.
Keep grade-8 cohorts 2017 and 2019 with both required entrance-exam
components in the expected admission year (2022 and 2024), both grade-8
SIMCE scores, and the existing complete baseline-control vector. This is
the broad administrative universe, not the main SAE applicant sample.
The two grade-8 SIMCE extracts are the existing staged 2017/2019 files in
`tmp/simce8_inputs/`; stage them with script 00 if needed.

All HE outcomes come from the master. `he_applied` is one if
`year_1st_app` is observed: a regular centralized university application
in the recorded 2021-2026 window. It does not cover every direct/IP/CFT
application and is not financial-aid application. `premium_field` copies
`high_paying_field_m1`, the paper's first-observed-enrollment outcome;
non-enrollees retain its existing zero. Preserve genuine missing values.
Both regressions use one common outcome-complete sample.

## Score prediction

Standardize math and verbal entrance-exam scores within admission year
among timely takers in the two master cohorts. Average the two z-scores
and standardize that composite to SD one within year. These are PDT 2022
and PAES 2024, using the paper's best-available component scores.

Predict this composite using cubic math and reading SIMCE scores in
grades 4 and 8, existing parent/background controls, middle-school GPA
and attendance, and fixed effects for cohort, gender, age, home comuna,
middle-school observation years and most-attended middle school. This
matches the rich pre-high-school predictor set in the earlier analysis.
No high-school identifiers or HE outcomes enter prediction.

Use five deterministic student folds (`abs(mrun) %% 5 + 1`), training on
the other four folds. Save every fitted model. `b_i` is the out-of-fold
prediction and `g_i = actual_score - b_i`. A student with a fixed-effect
level absent from that fold's training sample cannot receive a prediction
and is explicitly counted as excluded. Neither component is rescaled
separately; both retain units of one actual composite-score SD.

## Requested regressions

For each binary outcome estimate the additive linear-probability model
`Y_i = alpha + beta_b b_i + beta_g g_i + error_i` with an intercept and
no other second-stage controls. Cluster SEs by most-attended middle
school, with correction `G/(G-1) * (N-1)/(N-3)` and t inference using
`G-1` degrees of freedom. These SEs are conditional on the generated
scores and do not incorporate first-stage estimation uncertainty.
The report also tests equality of the two slopes.

Application follow-up covers 2021-2026 and first-enrollment data cover
2022-2025, so follow-up length differs by cohort. These are descriptive
conditional associations; the score gap is not a causal school effect.

## Run and outputs

From the repository root:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/simce4_exam_predictiveness/07_construct_regress_bg.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/simce4_exam_predictiveness/08_verify_report_bg.R
```

Derived student scores, fold models, regression models and covariance
matrices, normalization, sample accounting, manifests and checks are in
`data/clean/simce4_exam_predictiveness/bg_decomposition/`.
The report is
`output/reports/simce4_exam_predictiveness/predicted_score_and_gap.html`.
Its companion table is
`output/tables/simce_prediction/predicted_score_and_gap.csv`.
The existing preference-model report remains unchanged.

The current run retains 233,787 students at 5,647 middle schools after
excluding 353 out-of-fold predictions with unseen fixed-effect levels.
Cross-fitted score R-squared is 72.0% overall (2017: 69.2%; 2019: 74.5%).
Construction passes 22 checks and independent verification passes 52.
