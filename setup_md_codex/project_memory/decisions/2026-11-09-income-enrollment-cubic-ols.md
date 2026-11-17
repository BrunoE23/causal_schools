# Cubic OLS for HE enrollment and enrollee-only log projected income

The user requested the preceding income-contribution analysis for two outcomes:
log projected income only among HE enrollees, and whether students enroll in HE.
Retain the full total-degree-three math/verbal polynomial, nine income-decile
indicators (D1 reference), and admission-year indicators. Preserve original
score normalization and baseline income deciles; do not restandardize among
enrollees. No new score or school-VA restriction is introduced.

Start from the original 565,040-student OLS sample. Keep every student for HE
enrollment, including non-enrollees and admission-2026 records. For log income,
retain only HE enrollees with valid projected income; audit coverage and require
that every enrollee has it, rather than silently losing enrollees. The source
outcome is `log_program_income_full_clp_m1`, natural log of projected monthly
CLP income at first HE enrollment. Verify it against the existing positive
`program_income_full_clp_m1`. Enrollment retains the existing `COD_SIES_m1`-
based indicator. Income assigned to non-enrollees is excluded from the income
regression. Reuse the cached broad panel and check all shared covariates by ID.

For each outcome, the full and no-income models use identical observations.
The main contribution measure remains (R2_full - R2_without_income)/R2_full,
using ordinary unadjusted in-sample R2. This is income's incremental contribution
after scores and admission year, not a causal share or allocation of their
shared explanatory power. Save partial R2 separately with its different
denominator. The two outcome models intentionally have different samples.

Use actual attended-school-clustered CR1 covariance and t(G-1) inference,
recalculating N, K, and G in each outcome sample. Rebuild factor levels to
avoid empty admission-year dummies. Income coefficients are log points for
projected income and percentage points for enrollment; do not mix these scales.
Score rows average the full cubic's local derivatives per original score SD,
with exact linear-combination SEs from the full clustered covariance. They are
not finite one-SD changes.

Verify unique-ID linkage, common covariates, enrollment and income coverage,
log identity, exact sample definitions, full/reduced normal equations and
clustered covariance, nested R2, the projection identity for the R2 gain, and
unchanged input checksums. Preserve earlier models and create no plots.

Scripts: `code/codex/premium_enrollment_scores_income/11_income_enrollment_cubic_ols.R`
and `12_income_enrollment_cubic_tables.R`. Outputs use task name
`income_enrollment_cubic_income_contribution` under clean data, reports, and tables.

All four full/reduced fits pass the checks. Every one of the 467,323 enrollees
has projected income, and the income regression uses 3,327 school clusters.
Its full R2 is 0.3047985, reduced R2 is 0.3006175, and income's incremental
share of explained variation is 1.3717%. The enrollment model retains 565,040
students and 3,372 schools; its full R2 is 0.1681166, reduced R2 is 0.1679252,
and income's incremental share is 0.1138%. Income decile 10 versus 1 is about
0.074 log points for projected income and 0.30 percentage points for enrollment.
The 5,105 admission-2026 students are all non-enrollees, so this year is absent
from the income sample. Source checksums remain unchanged.
