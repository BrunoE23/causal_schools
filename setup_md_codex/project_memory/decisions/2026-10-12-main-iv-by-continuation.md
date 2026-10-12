# Main first-stage and pass-through tables by origin continuation

Date: 2026-10-12

## Request

Bruno requested Tables 4 (first stage) and 5 (pass-through) separately for
school-ending and voluntary applicants, with groups as rows and the seven
main outcomes as columns. He also requested mean expected VA and the fraction
of each applicant group with assignment risk.

## Samples and fixed design

The grouping is the same as the new Table 2 variant. Among Table 2's 294,198
timely 2018-2020 grade-9 applicants (observed high-school exposure, age 12-16),
250,564 have a grade-8 origin with no listed offering above grade 8 and 38,868
have an origin offering grade 9. Another 4,766 have unresolved/other offerings.
Use cached contemporaneous SAE A1-based classifications from the participation
exercise. These operational labels do not establish motives or actual moves.

Both IV subgroups additionally require the original assignment-risk criterion,
max_j p_ij < 1, and the original outcome-specific complete-case restrictions.
Do not rebuild or recenter school VA within a subgroup. Keep the original
2017-2020 EB school VA, outcome normalizations, offered-VA zero convention,
and expected-VA construction. Controls are expected VA for that outcome,
cohort indicators, grade-4 math and verbal scores, gender indicators, and age
indicators. Math/verbal and high-premium institution/field regressions require
exam taking; exam-taking, aid-application and higher-ed-enrollment regressions
do not. All other missingness rules follow the main specification.

Use actual fixest OLS first stages and 2SLS outcome fits with vcov='hetero',
matching the paper's HC1 standard errors. Retain its normal-reference p-values
for zero and one; the single-instrument first-stage F is (kappa/SE)^2. No
between-group difference test was requested, so do not infer statistical
heterogeneity from differences in point estimates or significance alone.

## Exact source and validation

Use the seven-outcome paper regression snapshot:
`output/intermediate/empirical_bayes_school_va/va_2017_2020__sae_2018_2020/scalar_iv_regression_df.csv`.
The older Box copy omits d/z/expected_highered_adj_eb; do not silently drop that
outcome or mix snapshots. All 42 pooled beta/SE/kappa/SE/F/N statistics from
the seven-outcome snapshot reproduce the current paper within numerical
tolerance before subgroup fits run.

Every subgroup is checked as an exact subset of each original estimation
sample, allowing for unresolved/other origins. Independent matrix calculations
of 2SLS with structural-residual HC1 covariance and first-stage HC1 covariance
verify the math regressions in both subgroups. Lean fitted objects, model
formulas, coefficient tables and the compact analysis frame are saved.

## Assignment risk and expected VA

Assignment-risk fractions use all Table 2 applicants in each continuation group,
before controls/outcome restrictions. All have matched DA probabilities:

| Group | Applicants | With risk | Share with risk |
| --- | ---: | ---: | ---: |
| School ending | 250,564 | 135,477 | 54.0688% |
| Voluntary | 38,868 | 24,016 | 61.7886% |

The primary expected-VA table averages sum_j p_ij VA_j over all applicants in
each group. Additional saved/report tables use applicants with risk and the
exact outcome-specific IV samples. Reconstruct from the original All-sample EB
school values and full DA probability distributions; unmatched options and
schools with missing VA contribute zero, without renormalizing probability
mass. All seven reconstructed expected-VA fields match the original cached
inputs for all 162,633 risk applicants to <1e-10. Probability mass sums to one.

Expected achievement VA is in outcome SDs. Expected VA for the five binary
outcomes is multiplied by 100 for percentage-point presentation only. It stays
on the original student-weighted school-VA scale, not a subgroup-centered one.
Expected VA combines portfolios, priorities and assignment probabilities and
is not a pure preference measure. Show coverage: mean measured-VA probability
mass is about 91.4% for school-ending and 98.1% for voluntary applicants;
unmatched mass is 8.49% and 1.70%, respectively.

## Results

First-stage coefficients for school-ending / voluntary applicants are:
math .452/.422, verbal .452/.386, exam .438/.353, aid .418/.340,
higher-ed .440/.372, high-premium institution .259/.378, and field .488/.423.
All first-stage F statistics exceed 235.

Pass-through coefficients are respectively 1.139/1.393, .992/1.198,
.501/.419, .582/.518, .748/.772, .554/.953, and .875/.751. Voluntary estimates
are less precise; subgroup Ns range from 16,461 to 21,196 versus 73,700 to
111,952 for school-ending applicants.

All-applicant mean expected VA is -.108/-.089 for math and -.087/-.065 for
verbal (SD units), and -4.652/-1.233 for exam taking, .522/1.518 for aid,
-1.126/-.173 for higher ed, -3.246/-2.460 for high-premium institution, and
.433/.413 for field (percentage points).

## Outputs

Code: `code/codex/empirical_bayes_school_va/17_run_main_by_continuation.R`,
`18_expected_va_risk_by_continuation.R`, `19_report_main_by_continuation.R`.
Data/models/checks:
`data/clean/empirical_bayes_school_va/va_2017_2020__sae_2018_2020/by_continuation/`.
Report: `output/reports/empirical_bayes_school_va/main_by_continuation.html`
(Markdown alongside). Four table fragments in the existing cohort-pair table
directory cover first stage, pass-through, expected VA and assignment risk.
No manuscript/table replacement or additional editor tab was requested.
