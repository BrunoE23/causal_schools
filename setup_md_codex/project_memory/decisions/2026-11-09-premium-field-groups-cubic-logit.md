# Cubic-score logits for the four premium-field groups

The user requested logit because the preceding cubic OLS R-squared seemed low.
Retain the four Science/Law/Engineering/Medicine+ outcomes, full bivariate
total-degree-three math/verbal polynomial, income-decile indicators (D1 reference),
and admission-year indicators. Preserve normalization, field definitions, and
non-enrollee zeros. These are four independent binary logits, not multinomial logit.

The admission-2026 cell has zero outcomes for every field, so its fixed-effect
logit coefficient has no finite maximum-likelihood estimate. Exclude its 5,105
observations from all four models, preserving a common 559,935-student sample
with 3,363 school clusters. This was communicated before fitting. Refit the four
cubic OLS models on that identical sample to separate model changes from sample
changes. Retain prior OLS results with 2026 for traceability.

Use the actual logit clustered score-sandwich covariance with the observed
information evaluated at the converged coefficients, CR1 correction
G/(G-1)*(N-1)/(N-K), and normal delta-method intervals. Compare with fixest
covariance, which may retain preceding-IRLS Hessian weights. No residual-spread
or other proxy uncertainty is used.

Report average discrete probability contrasts for D2-D10 versus D1, setting each
student to both categories while holding scores/year fixed. For math and verbal,
report average partial derivatives including all cubic, interaction and logistic-
link derivatives. Use exact analytic gradients and the full coefficient covariance
for their SEs; validate gradients by directional finite differences. Store raw
log-odds coefficients/odds ratios separately. Do not call raw logit coefficients
percentage-point effects or call score derivatives finite one-SD changes.

McFadden pseudo-R2 is 1 - full log likelihood / intercept-only log likelihood;
it is not directly comparable to OLS R2. Also report squared-error/Brier R2 with
the same observed-outcome variance denominator as OLS, plus Tjur R2 and AUC.
These are in-sample descriptive fit measures, not out-of-sample validation.
Do not assume that changing the link will substantially improve predictive fit.

Scripts: `code/codex/premium_enrollment_scores_income/07_logit_cubic_field_groups.R`
and `08_logit_cubic_field_table.R`. Data/report/table task name:
`premium_field_groups_logit_cubic_scores_income`.

All four logits converged. In Science/Law/Engineering/Medicine+ order, logit
squared-error R2 is 0.0161/0.0356/0.0423/0.0443, compared with common-sample
OLS R2 of 0.0158/0.0347/0.0426/0.0416. The link change does not substantially
improve this measure of fit. McFadden pseudo-R2 is
0.0285/0.0853/0.0435/0.0689; in-sample AUC is 0.632/0.735/0.646/0.696.
The decile-10 average probability contrasts are -1.80/+2.51/-4.36/-2.00
percentage points relative to decile 1.

Verification passed for convergence, score equations, likelihood consistency,
cluster-sandwich comparison, analytic effect gradients, and probability calibration.
Maximum directional gradient discrepancy is below 1.5e-11. The source cache
checksum is unchanged. Independent binary fitted probabilities sum above one
for about 0.033% of observations; these fits should not be treated as a joint
multinomial probability model.
