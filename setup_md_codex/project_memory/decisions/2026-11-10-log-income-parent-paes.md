# Add PAES scores to parent-specific log projected-income OLS

User requested adding PAES scores to the two preceding parent-specific models.
Include both existing z_math and z_verbal linearly, in their original within-
admission-year SD units. Each model includes only an intercept, the selected
parent's continuous schooling years, math and verbal. Omit the other parent,
VA, interactions, and other baseline controls.

Keep exactly the previous 463,033 HE enrollees and 3,262 attended-school clusters.
The sample inherits earlier complete-PAES/full-baseline and VA availability
restrictions; no new exclusions or imputations. Preserve income projections and
natural logs. Follow prior OLS school-clustered CR1 and t(G-1) inference.

Compare each parent's coefficient and overall R2 with the parent-only regression
on the identical source sample. Verify source hashes, sample sizes, score
availability, income log identity, four-parameter designs, normal equations,
independent CR1 covariance, and nested R2. Attenuation is descriptive and not
a causal mediation decomposition; income is projected rather than realized.

Script: code/codex/premium_enrollment_scores_income/27_log_income_parent_paes.R.
Output task: log_projected_income_parent_paes in clean data/reports.

Completed on the identical sample. Father years coefficient 0.00656852
(clustered SE 0.00023825), versus 0.02902730 before; reduction 77.3713%.
Mother years 0.00626647 (0.00025338), versus 0.03033271; reduction 79.3409%.
Both p<0.001; implied geometric-mean differences 0.6590% and 0.6286% per year.
Math/verbal coefficients are 0.13966981/0.05285932 in the father model and
0.14058316/0.05323406 in the mother model. R2 rises from 0.067678/0.061657
to 0.298604/0.297916 respectively. All checks pass.
