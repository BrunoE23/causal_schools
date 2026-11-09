# Premium enrollment on scores and family income

The user chose four OLS linear probability models, with both math and verbal
entrance-exam scores and income-decile indicators (decile 1 reference).
Outcomes are premium institution, premium field, either (inclusive union), and both.

Run from the repository root:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/01_regress.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/02_table.R
```

Requires data.table and fixest. `CAUSAL_SCHOOLS_REPO_WD` overrides the repository.
Reuse the 23 MB broad-panel cache from `all_students_by_attended_va`, before
its school-VA merge. No master reread or plot generation is needed.

## Specification and sample

For each binary outcome, estimate OLS on an intercept, linear standardized math
and verbal scores, income-decile indicators 2-10, and admission-year indicators.
Use original within-admission-year broad-panel score normalization and the
existing imputed baseline household-income deciles; do not re-rank or normalize
in the regression sample. No score-income interactions or school fixed effects.

All four models use a common complete sample from the paper's broad panel
(grade-8 cohorts 2017-2020, ages 12-16, attended school observed). Require both
positive observed test scores and a valid income decile. There is no high-math
threshold, HE-enrollment restriction, or requirement for estimated school VA.
Preserve existing premium flags; non-enrollees are zero for all four outcomes.

Retain admission 2026, whose recorded premium outcomes are uniformly zero.
The earlier logit analysis omitted this cell because of separation; OLS can
include it. Document this inclusion explicitly rather than silently treating
the recorded zeros as evidence about future enrollment.

Standard errors cluster by attended high school (`most_time_RBD`), allowing
arbitrary within-school residual correlation. Use the CR1 correction
`G/(G-1) * (N-1)/(N-K)` with K including intercept and all year/decile indicators;
confidence intervals and p-values use t(G-1). Tables show coefficients and SEs
multiplied by 100, in percentage points.

The fitted sample has 565,040 students in 3,372 attended high schools, including
97,717 non-enrollees and 5,105 admission-2026 records. Each equation has 17
parameters. The input cache is unchanged, and all four regressions pass the
normal-equation, covariance, and common-design coefficient-identity checks.

## Checks and outputs

Check binary flags, union/intersection identities, non-enrollee zeros, common
sample, matrix rank, and OLS normal equations. Independently reconstruct each
clustered sandwich covariance and compare with fixest. The common-X OLS
coefficients must obey `either = institution + field - both`. Save outside-[0,1]
fitted-value rates without clipping predictions.

Models, analysis sample, full coefficients (including year effects), covariance
matrices, confidence intervals, joint tests and sample diagnostics are local in
`data/clean/premium_enrollment_scores_income/`. Rendered table:
`output/reports/premium_enrollment_scores_income/regression_table.html`.
Paper table fragment: `output/tables/premium_enrollment_scores_income/regression_table.tex`.
