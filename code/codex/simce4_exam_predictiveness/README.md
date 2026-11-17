# Predicted entrance-exam score and realized gap

Script `07_construct_regress_bg.R` builds the score decomposition.
The current logistic outcome models are implemented by `09_fit_logit_bg.R`
and `10_verify_report_logit_bg.R`. Script `08_verify_report_bg.R` verifies
and reports the preceding linear-probability models in archived outputs.
Scripts 00-06 and their earlier outputs are
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

At the user's request, fit logistic regressions for each binary outcome:
`Pr(Y_i=1) = logit_inverse(alpha + beta_b b_i + beta_g g_i)`.
The intercept and the two existing score components are the only
second-stage regressors. Reuse the cached student scores unchanged;
no prediction refitting or master-data reads are needed for this change.
Cluster the logit score sandwich by most-attended middle school, with
correction `G/(G-1) * (N-1)/(N-3)` and t inference using `G-1` degrees
of freedom. These SEs condition on the generated scores and do not
incorporate first-stage estimation uncertainty.

The main table shows average marginal effects (AMEs) in percentage
points, not log-odds coefficients or discrete one-SD probability changes.
For a continuous score component k, the AME is
`beta_k * mean(p_i * (1-p_i))`. Delta-method covariance uses the full
middle-school-clustered coefficient covariance and the derivative of
the mean logistic weight. Raw logit coefficients and odds ratios are
available separately. Test equality of the two logit slopes as the
primary comparison; the two AMEs share a common positive multiplier.
The earlier LPMs remain saved as a historical comparison.

Application follow-up covers 2021-2026 and first-enrollment data cover
2022-2025, so follow-up length differs by cohort. These are descriptive
conditional associations; the score gap is not a causal school effect.

## Run and outputs

From the repository root:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/simce4_exam_predictiveness/07_construct_regress_bg.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/simce4_exam_predictiveness/09_fit_logit_bg.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/simce4_exam_predictiveness/10_verify_report_logit_bg.R
```

Derived student scores, fold models, regression models and covariance
matrices, normalization, sample accounting, manifests and checks are in
`data/clean/simce4_exam_predictiveness/bg_decomposition/`.
Logistic model objects, covariance matrices, AMEs and preservation hashes
are in the `logit/` subfolder. Run only scripts 09/10 to refit the current
outcome models when `student_scores.rds` already exists.
The report is
`output/reports/simce4_exam_predictiveness/predicted_score_and_gap.html`.
Its companion table is
`output/tables/simce_prediction/predicted_score_and_gap.csv`.
Archived LPM report/table paths have the suffix `_lpm`; script 08 now
writes those archive paths, preserving the active logistic report.
Script 09 archives existing LPM reports before the first logit update.
The existing preference-model report remains unchanged.

The current run retains 233,787 students at 5,647 middle schools after
excluding 353 out-of-fold predictions with unseen fixed-effect levels.
Cross-fitted score R-squared is 72.0% overall (2017: 69.2%; 2019: 74.5%).
Score construction passes 22 checks; the earlier independent LPM
verification passes 52. The logistic fitter separately verifies convergence,
score equations, covariance identification and preservation of earlier inputs.
The current logit run passes 14 fit checks and 28 independent verification
checks. AMEs for b_i/g_i are 21.71/19.40 pp for centralized HE application
and 13.86/12.36 pp for premium-field enrollment. Both coefficient-equality
tests reject at p<0.001, so the baseline component now has the larger
association for both outcomes in this specification.
