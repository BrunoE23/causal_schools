# SAE applicant composition trends

Construct school-by-SAE-cohort applicant composition and test a separate linear
time trend for each of the 100 schools receiving the most submitted applications.
The three primary outcomes are baseline SIMCE, female share, and parents'
education. The user explicitly requested **unadjusted p-values** and counts of
schools with trends in one, two, or all three outcomes (threshold p < 0.05).

## Run

From the repository root, with R 4.5.1 and packages data.table, sandwich,
ggplot2 and jsonlite:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/applicant_composition_trends/01_build_panel.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/applicant_composition_trends/02_test_trends.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/applicant_composition_trends/03_report.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/applicant_composition_trends/04_verify.R
node code/codex/applicant_composition_trends/05_verify_report.js
```

Inputs default to `C:/Users/brunem/Box/causal_schools`; override with
`CAUSAL_SCHOOLS_DATA_WD`. Inputs are read-only. The 1.23 GB universe CSV is
preflighted and read with only the necessary baseline columns. New derived data
and diagnostics go in `data/clean/applicant_composition_trends/` and stay local.
Figures go in `output/figures/applicant_composition_trends/`; the main report is
`output/reports/applicant_composition_trends/report.html`.

## Population and schools

Use the existing paper roster `all_applicant_expected_va.rds`: 294,198 timely
grade-9 SAE applicants, processes 2018-2020, ages 12-16 at grade 8, and observed
positive attended high-school RBD. Retain all continuation/priority groups,
without conditioning on lottery risk or exam taking. SAE process year precedes
grade-9 entry by one year. The first observed SAE process convention is inherited.

Read raw regular-round C1 applications; require grade 9 and exclude automatically
added continuity entries (`agregada_por_continuidad == 1`). Do not exclude genuine
priority applications. Collapse programs/campuses within applicant-process-RBD.
Rank schools by pooled distinct applicant-school pairs, breaking ties by RBD;
fix the resulting 100 schools across years. This retains 99 schools from the
earlier ranking: RBD 4977 replaces RBD 52. Do not alter the earlier ranking.

The top 100 have 212,608 applicant-school pairs. There are 86 schools present in
three cohorts and 14 present in two. A school-year with no applicants remains an
explicit empty cell with missing means, never an outcome of zero.

## Outcomes

- `simce_baseline`: arithmetic mean of grade-4 math and verbal z-scores, requiring
  both subjects. These existing z-scores are standardized within baseline test
  year. Their average is **not** restandardized to unit variance.
- `female`: GEN_ALU = 2 for valid codes 1/2; other codes missing. CSV means are
  shares, with display columns in percent/percentage points.
- `parents_education`: average available mother/father reported education years
  per student, then average those student values. At least one parent is required.
  The underlying questionnaire categories have already been mapped to approximate
  years using the project's year-specific crosswalk.

Seven secondary measures retain math, verbal, mother and father separately;
both-parent complete reports; existing observed-plus-imputed parent values; and
the mean of raw SIMCE points requiring both subjects. No new imputation occurs.
Each cell/outcome records its own observed denominator, SD and coverage.

## Estimation

For each school/outcome fit `mean ~ (sae_proceso - 2018)` with equal weight for each
observed cohort. Export annual slope, observed endpoint change, standard errors,
confidence intervals and raw two-sided p-values. Keep two inference models separate:

1. **Cohort OLS (primary)**: ordinary lm covariance and t(G - 2). Three cohorts
   provide only one residual degree of freedom. With two cohorts the slope is
   identified, but this test is undefined. Constant means receive no artificial
   p-value. Saved lm objects are in `cohort_ols_models.rds`.
2. **Independent applicants (supplementary)**: regress individual outcome on year
   with weight 1/n_observed for its school-cohort. This is exactly equal-weight
   OLS on cohort means for the coefficient. Compute the actual regression HC2
   covariance and t(N - 2) p-value. It assumes independent students within school,
   without common cohort shocks or origin-school dependence. Five schools have
   constant individual gender values, for which no p-value is supplied.

The second model supports a two-cohort comparison under stronger assumptions;
it does not manufacture residual degrees of freedom for the first model. Neither
model establishes a persistent time trend outside this short period or a causal
peer effect. Overlap of applicants across schools also means tests across schools
are dependent. Per the user, no multiple-testing corrections are calculated.

## Main outputs and results (2026-10-22)

- `school_cohort_panel.csv`: 300 school-cohort cells, wide outcome means/counts/coverage.
- `school_cohort_means_long.csv`: 3,000 cells including components/sensitivity measures.
- `primary_school_trend_tests.csv`: 300 school-outcome records.
- `school_trend_tests.csv`: 1,000 records including seven secondary outcomes.
- `school_significant_trend_counts.csv`: each school's 0-3 detected trends and
  outcome flags under both inference models; unavailable tests remain missing.
- `significant_trend_count_summary.csv`: exactly one/two/three and at-least counts.
- `verification.csv`: independent aggregation, closed-form cohort OLS checks,
  explicit matrix HC2 calculations and significance-count reconciliation.

Cohort OLS: among 86 schools with tests, 8 have at least one significant trend,
all in exactly one outcome; 0 have two or three. Fourteen schools have no test and
are not included in the zero-detection group. Independent applicants: among 100,
81 have at least one, 25 have at least two, and 2 have all three; exact counts are
56/23/2. These figures use the three primary outcomes only.

2018-2020 cross-school correlations (86 schools) are 0.899 for SIMCE, 0.978 for
female share, and 0.968 for parent education. Median absolute endpoint changes
are 0.0468 average subject-z units, 2.062 percentage points and 0.437 years.
The parental-education change is mostly a 2018-2019 fall followed by a small
recovery. Coverage and questionnaire differences merit investigation before
attributing it to the full applicant population; see the project issues log.

## Admission-probability-weighted proxy (requested follow-up)

Keep the exact same top 100 schools, submitted applicant-school pairs and X
definitions. Define each school-cohort mean as `sum(p_is * X_i) / sum(p_is)` among
applicants with X observed, where `p_is` is the simulated assignment probability
to that destination RBD. Sum mutually exclusive program probabilities within RBD
before joining. This is a ratio of expected totals, used as a proxy for admitted
student composition; it does not account for take-up or random denominators.

Read the existing `DA_probs/DA_probs_2018.csv` through `DA_probs_2020.csv`.
Every one of the 115,880 distinct applicants in the top-school panel has a
probability distribution summing to one, including unmatched. Only after checking
this distribution, treat an absent destination as zero recorded simulation
probability. Preserve genuinely submitted pairs: do not add the 346 simulated
top-school destinations absent from the original submitted-choice panel.

There are 134,426 positive-weight and 78,182 zero-weight applicant-school pairs.
Nine populated school-cohort cells have no admission probability mass, and one
additional parent-education cell has no observed characteristic with positive
weight. Their weighted means are missing. Three schools have only one supported
cohort and cannot have a trend estimated. Keep all 100 school rows in the outputs;
do not silently trim low effective sample sizes or substitute zero for missing means.

Primary inference still uses equal-cohort OLS. For supplementary inference, WLS
weights are `p_is / sum(p_is)` within the observed school-cohort-outcome; exclude
zero weights. This reproduces the cohort OLS coefficient exactly. HC2 treats the
simulation probabilities as fixed and assumes independent applicants; use
`N_positive - 2` residual degrees of freedom. Effective N, `(sum p)^2 / sum(p^2)`,
is a concentration diagnostic only, never a substitute for regression covariance.
Undefined tests and constant outcomes receive missing p-values.

Run after the original panel is built:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/applicant_composition_trends/06_probability_weighted_panel.R
$env:APPLICANT_COMPOSITION_DIR = 'data/clean/applicant_composition_trends/probability_weighted'
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/applicant_composition_trends/02_test_trends.R
Remove-Item Env:APPLICANT_COMPOSITION_DIR
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/applicant_composition_trends/08_verify_probability_weights.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/applicant_composition_trends/07_probability_weighted_report.R
node code/codex/applicant_composition_trends/05_verify_report.js output/reports/applicant_composition_trends/probability_weighted.html
```

Derived outputs use `data/clean/applicant_composition_trends/probability_weighted/`.
The report is `output/reports/applicant_composition_trends/probability_weighted.html`.
`weighted_vs_unweighted_school_tests.csv` and
`weighted_vs_unweighted_count_summary.csv` compare both versions. Weighted
denominators, effective N, missingness and source linkage audits are also exported.

Using raw p < .05 across the three primary outcomes:

| Inference | Testable schools | At least one | Exactly one | Exactly two | All three | At least two |
|---|---:|---:|---:|---:|---:|---:|
| Cohort means OLS | 80 | 9 | 9 | 0 | 0 | 0 |
| Independent applicants HC2 | 97 | 68 | 47 | 20 | 1 | 21 |

The one school with all three significant HC2 trends is RBD 17788, Colegio
Amanecer. Counts are of detected trends; some schools have fewer than three
available p-values. Cohort OLS has 77 schools with all three p-values and HC2 has
91. The comparison report explicitly identifies changes in support and retains
the strong-assumption interpretation of independent-applicant inference.

Validation passed 1,828 numerical/data-integrity checks, including independent
matrix calculations for all 964 estimable weighted HC2 regressions. A separate
rerun of the generalized fitter on the original unweighted panel reproduced all
1,000 original school-outcome records and school-count summaries. Report checks
cover all ten outcome tables, RBD search and every local figure/download link.

## Three-year school composition by applicant, offer and attendance group

The follow-up request adds income decile and averages composition across the
2018-2020 cohorts. Bruno explicitly selected **all students, using the school
attended most often in high school** for the attendee population. Script 09
creates the following groups for the same fixed top 100 schools:

- `weighted_applicants`: the existing genuine submitted-choice panel, with
  destination-specific simulated admission probabilities as weights.
- `first_round_offered`: students in the full existing 294,198-person SAE paper
  roster whose recorded positive `rbd_treated_1R` equals the destination. This
  includes guaranteed offers and does not condition on attending the offered
  school. It includes 403 offers to destinations absent from the genuine-choice
  panel, which are identified in `offer_listing_audit.csv`.
- `actual_attendees`: every master-universe student in grade-8 cohorts 2018-2020
  whose `most_time_RBD` is the destination, including non-SAE students. No age,
  offer, lottery-risk, exam-taking or complete-case restriction is imposed.
- Supplement: `offered_at_risk` retains actual first-round offers with offered-
  school simulated probability strictly between zero and one, so users can
  distinguish stochastic lottery offers from all realized SAE offers.

The primary characteristics preserve the earlier reported available-parent mean,
average of both grade-4 subject z-scores, and female share (stored 0-1). Income is
the existing `income_decile_imputed` (observed plus upstream imputation), without
reranking or further imputation. Observed-only income decile is supplementary.
Each characteristic has its own nonmissing denominator.

Aggregation columns distinguish three objects:

- `mean_equal_cohort_*`: equal-weight average of the available cohort means.
  Always inspect `n_cohorts_*`; a two-year average is not labeled as a full
  three-cohort observation.
- `mean_all_three_cohorts_*`: same average, but missing unless all three means
  exist. No empty cohort is replaced with zero.
- `mean_pooled_*`: pool students and their group weights across 2018-2020;
  larger observed groups receive proportionally more weight.

Run from the repository root:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/applicant_composition_trends/09_school_group_averages.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/applicant_composition_trends/10_verify_group_averages.R
```

Outputs are in `data/clean/applicant_composition_trends/group_averages/`:

- `school_group_averages_2018_2020.csv`: main 300-row school-by-group table with
  all four primary measures, aggregation variants, cohort counts and coverage.
- `school_composition_100_schools.csv`: the same main groups/measures side by
  side, with one row per school (100 rows).
- `school_group_averages.rds`: both main tables, full long results, annual means
  and group/outcome definitions for reuse in R.
- `school_group_averages_long.csv` and `school_group_cohort_means.csv`: include
  the strictly stochastic offer subset and observed-only income, plus numerators,
  denominators and coverage. Dictionaries and population audits accompany them.

The main offer group contains 52,019 students; attendees contain 72,502, including
21,087 outside the paper SAE roster and 49 outside ages 12-16 or with age missing.
The stochastic-offer supplement contains 37,454 students. All 100 schools have
three attendee cohorts for all primary characteristics. Offers and probability-
weighted applicants can have fewer supported years. Retain the 53 actual offers
with zero recorded simulation probability in the main offer group; do not call
them stochastic offers based on their simulated zero. This is recorded in the
offer probability audit. Verification checks direct pooled weighted means,
equal-cohort averages, three-year completeness, and exact reproduction of earlier
weighted baseline means.
