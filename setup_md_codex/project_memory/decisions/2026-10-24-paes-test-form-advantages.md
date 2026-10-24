# PAES form-score diagnostic (2026-10-24)

## Request and decision

Bruno asked whether the PAES records test form and whether people receive an
advantage based on it. The raw 2023-2026 admissions score files contain form
codes, item-level response strings, correct/incorrect/omitted counts, and
same-sitting reported scores. Existing project clean college-score files retain
best scores and are unsuitable for directly pairing a particular form with its
outcome. Use raw same-sitting fields in an isolated diagnostic.

Bruno clarified that the question is variation across forms within each year,
not whether a form is always better. The separate year-subject regressions
already implement this. Center the report on those joint tests; do not frame
changes in form rankings across years as an additional research question.

## Definition

Primary family: eight subjects/modules by four regular admissions processes
(2023-2026), 32 tests. Reading, M1, M2, History, and the four separate science
modules. Supplementary family: 24 winter PAES tests, 2024-2026; do not include
winter admissions 2023, which was PDT. Codes are specific to subject/year/sitting.

Retain valid form/score, school, baseline controls, and within-school variation
in form. Complete controls are GPA 4-7, ranking 100-1000, sex 1/2, age 14-90,
graduation year 1930 through admissions year minus one. Score scale: 100-1000.
No SAE or university-application selection. Analyze all qualifying takers as
primary; current graduates are robustness, aligning with the project cohort
interpretation without limiting the broad form diagnostic.

Regress reported score on form, attended-school effects, cubic GPA, cubic ranking
score, cubic age, sex, graduation-year and educational-track indicators. Compare
with same-sample unadjusted and school-only results. Correct counts are secondary
outcomes, never controls. Balance outcomes: GPA, sex, age, recent-graduate status,
with school effects only. Age is at December of sitting year (a common shift for
winter that does not alter a cubic specification).

## Inference and validation

Cluster regression-derived SEs by attended school, not by form. Wald tests use
F(q,G-1). Holm correction covers 32 regular or 24 winter joint tests per
specification/outcome. All baseline outcomes are one family within sitting.
All 102 primary regular pairwise contrasts receive simultaneous Bonferroni 95%
intervals, including when reporting the data-selected maximum fitted difference.
Maximum point estimates are still subject to selection bias.

Do not regularize the coefficient covariance. Full nuisance-block covariances
can be numerically singular; negative eigenvalues have relative magnitude below
2.4e-16. Tested form submatrices are positive definite. Independent within-school
QR regression and a manual cluster sandwich reproduce Reading 2026 coefficients
and form SEs (maximum SE discrepancy 2.7e-14).

## Findings

- 13/32 primary regular tests survive Holm; 12/32 for current graduates.
- Reading 2026: form 103 minus 106 = 7.4405 points, SE 0.5006, simultaneous CI
  [5.6936,9.1874], N=247,204 in 3,443 schools, 0.0595 score SD.
- Reading 2026 current graduates: 8.4479 points in the within-year comparison.
- No 128-test regular baseline balance rejection after Holm; winter also none.
- One of 24 winter tests survives (biology 2024), with weaker support coverage.
- Regular retained samples cover 91.5%-99.2% of valid form-score takers; winter
  coverage is 51.4%-94.0%. Three invalid positive-form scores are explicitly
  excluded and reconciled in the sample audit.

## Limits

These are conditional form-score associations, consistent with small advantages
remaining after DEMRE equating. They are not established causal estimates:
assignment randomization has not been verified, test center/room identifiers are
absent, and school clustering cannot account for unobserved cross-school room
dependence. LOCAL_EDUCACIONAL is not treated as test-site metadata. No admission
effect or general equality of score distributions is inferred.

## Artifacts

Code: `code/codex/paes_test_forms/`.
Clean outputs: `data/clean/paes_test_forms/`.
Report: `output/reports/paes_test_forms/report.html`.
Figure: `output/figures/paes_test_forms/reading_math_form_gaps.png`.
Raw data and existing project outputs are unchanged.
