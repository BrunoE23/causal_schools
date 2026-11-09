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

## Four premium-field groups

The follow-up runs the same specification and covariance on Science, Law,
Engineering/Manufacturing/Construction, and Medicine+. Run:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/03_field_groups.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/04_field_table.R
```

Reuse the exact prior 565,040 student IDs and all covariates. Read only five
master columns to recover the earlier field definitions. Science, Law and
Engineering use their existing first-enrollment flags. Medicine+ is the
intersection of `high_paying_field_m1` and `field_reclassified_m1 == "Medicine"`,
preserving the original medicine, pharmacy, nursing, midwifery, medical
technology and dentistry list. Each indicator is zero for non-enrollees and
students enrolling outside that group. Do not condition on premium-field
enrollment or change the sample or score normalization.

Verify individual-level exclusivity and exact summation to the old field flag.
The four estimated coefficient vectors must sum to the previous premium-field
coefficient vector, including intercept and year effects. Each regression has
its own school-clustered covariance; component SEs must not be added together.

Save the new models, estimates, sample and audits under
`data/clean/premium_field_groups_scores_income/`, and the HTML/LaTeX tables under
the corresponding `output/reports/` and `output/tables/` task folders. Earlier
four-outcome regression outputs remain unchanged.

## Cubic score controls and interactions

The user replaced the proposed score-decile specification with polynomials up
to degree three and math-verbal interactions. Scripts `05_cubic_field_groups.R`
and `06_cubic_field_table.R` fit and present the four field-group OLS models with
the full bivariate cubic basis:
`M, V, M^2, V^2, M*V, M^3, V^3, M^2*V, M*V^2`.
M and V retain the existing standardized scores. This is total degree at most
three, not a tensor-product polynomial through degree six. There are no score
deciles or score-income interactions. Keep all income indicators, admission-year
effects, the exact student sample, and school-clustered CR1 inference.

Run `05_cubic_field_groups.R` followed by `06_cubic_field_table.R` using the same
Rscript command as above. Input is the small saved field-group analysis cache;
no external master reread is needed. Reuse one 24-column design matrix for the
four fits and covariance checks. Verify normal equations, clustered covariance,
nested R-squared improvement, unchanged means/sample, and component additivity
against the aggregate cubic OLS normal-equation solution. Report clustered Wald
F tests for all seven added terms, the three interaction terms, and the four
degree-three terms. Save income-coefficient comparisons to the linear-score
models. Raw polynomial coefficients are not constant one-SD marginal effects.

Outputs use task name `premium_field_groups_cubic_scores_income` under
`data/clean/`, `output/reports/` and `output/tables/`; previous results are retained.

## Logit replacement and fit comparison

The user requested logit after seeing the low OLS R-squared. Run
`07_logit_cubic_field_groups.R` and then `08_logit_cubic_field_table.R`.
Reuse the field-group cache and the full cubic score basis, decile indicators,
and admission-year effects. Restrict all four models to a common set of year
cells with outcome variation in every field. The all-zero admission-2026 cell
has no finite logit year effect and is excluded. Refit the four cubic OLS
comparators on exactly the same remaining sample; preserve prior OLS outputs.

Estimate four independent binary logits, with attended-school-clustered CR1
covariance. Recompute the Hessian/clustered-score sandwich at the converged
coefficients and compare with fixest covariance. Store this reported covariance
separately from the fitted objects, whose package covariance may use nearly
identical weights from the preceding IRLS step. Delta-method intervals use
normal quantiles. Check convergence, score equations and analytic effect
gradients by directional finite differences.

Main tables report probability effects: mean discrete income-decile contrasts
versus D1 over every student's observed scores/year, and average partial score
derivatives incorporating all polynomial terms and the logit link. Score AMEs
are local derivatives per SD, not finite one-SD changes. Raw log-odds coefficients,
odds ratios, full covariance and effect gradients are saved separately.

Fit diagnostics distinguish McFadden pseudo-R2 (likelihood ratio versus an
intercept-only fit), Tjur R2, AUC, and squared-error/Brier R2. The last uses
`1 - mean((Y-p)^2)/mean((Y-mean(Y))^2)` and is directly comparable with same-sample
OLS R2. All metrics are in-sample; no claim about held-out prediction is made.
Independent logits need not have additive fitted probabilities, so save a sum
diagnostic and do not apply the earlier OLS coefficient-additivity check.

Outputs use task name `premium_field_groups_logit_cubic_scores_income` under
`data/clean/`, `output/reports/` and `output/tables/`. No plots are generated.

## Return to cubic OLS: income's contribution to eight outcomes

Run `09_cubic_ols_income_contribution.R` and then
`10_cubic_ols_income_tables.R`. Both use the same Rscript command and working
directory as above. Reuse the two small original OLS analysis caches, matching
unique student IDs and checking every shared variable. Restore the original
565,040 students and 3,372 school clusters, including the 5,105 admission-2026
records. No master reread, new normalization, or plot is needed.

Estimate the full cubic-score OLS model for institution, field, either, both,
Science, Law, Engineering/Manufacturing/Construction, and Medicine+. For each,
also estimate a reduced model dropping only the nine income-decile indicators.
Both retain the same score terms and year indicators on the exact same sample.
Income's reported fraction is `(R2_full - R2_without_income) / R2_full`, expressed
as a percentage. It measures incremental explanatory power after scores/year;
it does not allocate their shared variation and is not a causal contribution.
Use ordinary unadjusted R2. Also save, separately, income's partial R2 with
denominator `1 - R2_without_income`; do not confuse this with the reported share.

Keep actual school-clustered CR1 inference and t(G-1), with K=24 for full models
and K=15 for reduced models. Tables show all income-decile coefficients plus
average local math/verbal derivatives from the cubic polynomial, with their
exact linear-combination clustered SEs. Raw basis coefficients remain in the
HTML and coefficient CSV. Verify normal equations and covariance for both
models, nested-model projection identities, union and component coefficient
identities, and exact reproduction of the previous cubic OLS field estimates.

Outputs use task name `premium_cubic_income_contribution` under `data/clean/`,
`output/reports/`, and `output/tables/`. The HTML report combines the R2
contribution summary and both four-column regression tables. Three LaTeX table
fragments provide the contribution summary, main outcomes, and field groups.
Model objects, covariance matrices, all coefficients, clustered income Wald
tests, source checksums, and diagnostics stay in the clean-data task folder.
