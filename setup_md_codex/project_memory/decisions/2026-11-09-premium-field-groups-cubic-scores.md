# Cubic math/verbal controls for the four field-group LPMs

The user replaced their tentative score-decile request with polynomials up to
degree three and possible math-verbal interactions. No score-decile code was
run. Use the full bivariate polynomial of total degree <= 3:
M, V, M^2, V^2, MV, M^3, V^3, M^2V, MV^2. This adds seven terms to the earlier
linear score specification. Do not include degree-four or higher products or
interactions with family income. The assistant stated this specification before
estimation.

Preserve the exact 565,040 student sample, original admission-year score
standardization, four mutually exclusive premium-field groups, nine income-
decile indicators, and admission-year indicators. Keep non-enrollees and the
5,105 admission-2026 records as before. Use the same 3,372 attended-school
clusters, CR1 finite-sample covariance with the new K=24, and t(G-1) inference.

Present income-decile coefficients and the raw score-polynomial coefficients
with clustered SEs; identify the latter as basis coefficients, not constant
one-SD changes. Compare R-squared and income coefficients with the earlier
linear-score models. Test the seven added terms jointly and the interaction
block jointly using the full clustered covariance. No graphs are requested.

Check the exact OLS normal equations, independently reconstructed clustered
covariance, unchanged sample/means, nested R-squared, and component additivity
against the aggregate field coefficient vector under the new cubic design.
Retain all prior linear-score results.

All four fits pass the normal-equation, clustered covariance, unchanged-sample,
and additivity checks. R-squared changes from 0.014671 to 0.016351 (Science),
0.028563 to 0.034688 (Law), 0.040494 to 0.044036 (Engineering), and 0.040515 to
0.042157 (Medicine+). All seven added terms are jointly significant at p<0.001
in each equation. For the three interactions jointly, Science p=0.010787;
the remaining equations have p<0.001. These tests use the clustered covariance.
Decile-10 coefficients in percentage points become -2.6023, +2.8566, -4.7880,
and -2.7625 respectively. Input caches and prior results remain unchanged.

Scripts: `code/codex/premium_enrollment_scores_income/05_cubic_field_groups.R`
and `06_cubic_field_table.R`. Data/report/table task name:
`premium_field_groups_cubic_scores_income`.
