# Premium enrollment on entrance-exam scores and family-income deciles

The user stopped the plot work and requested four regressions for premium
institution, field, either, and both on PAES scores and family-income decile.
They explicitly selected both math and verbal, decile indicators with decile 1
as reference, and OLS linear probability models rather than logit.

Include linear math and verbal scores and admission-year indicators. Keep the
existing admission-year broad-panel score standardization and original imputed
grade-4 household-income deciles. All four equations use the same complete
score/income sample from the broad 2017-2020 cohort panel (ages 12-16, attended
high school observed). Do not impose a high-score threshold, HE-enrollment
condition, or school-VA-availability condition. Use unchanged first-enrollment
premium flags and their inclusive union/intersection; non-enrollees are zero.

Use attended-high-school-clustered CR1 covariance and t(G-1) inference. This
allows residual dependence within schools, which matter for these choices.
Count the full explicit design's K parameters in the finite-sample correction.
Report percentage-point coefficients and SEs, retaining unscaled estimates and
the full covariance locally. These are conditional descriptive associations.

Unlike the previous logit analysis, retain admission-year 2026 in OLS despite
uniformly zero recorded premium outcomes; year effects remain included.
The assistant stated this choice before fitting, after the user selected OLS.
Flag these recorded zeros without claiming that they establish lifetime
non-enrollment. No extra sensitivity regressions are included in the requested
four-model table.

Validate normal equations and the actual regression-based clustered sandwich
covariance. With a common design, verify the coefficient identity for the
inclusive union. Save fitted-value range diagnostics without clipping.

Completed estimation uses 565,040 students, 3,372 attended-school clusters,
and 17 parameters per equation, including 97,717 non-enrollees and 5,105
admission-2026 observations. All four OLS normal-equation and independently
reconstructed CR1 covariance checks pass, as does the coefficient identity.
The source cache remains unchanged. Report all nine decile contrasts and
both score slopes in one table, with full coefficients/covariances saved locally.

Code: `code/codex/premium_enrollment_scores_income/`.
Data: `data/clean/premium_enrollment_scores_income/`.
Table: `output/reports/premium_enrollment_scores_income/regression_table.html`.
