# Average grade histories for the voluntary-SAE table

Date: 2026-10-10

## Request and scope

Bruno suggested combining grades 5-8 to reduce the importance of grade-specific
measures. Use one four-grade average per construct as the compact main table,
with two-period averages (5-6 and 7-8) as a timing sensitivity. This follows the
separate-predictor/PCA table without replacing its saved results.

## Construction and specifications

- Reuse the cached PCA analysis frame; no raw source data or PCA loadings change.
- For GPA, attendance, peer GPA z, peer attendance z, GPA-minus-SIMCE-math z gap,
  and GPA-minus-SIMCE-language z gap, average the finite available values across
  grades 5-8 with equal grade weights. Each grade's peer comparison still uses
  its original attended school-grade-year roster. Gaps average the existing
  grade-specific differences, not an alternative pooled peer calculation.
- Standardize each new summary using its mean and sample SD among pupils in
  the original voluntary training schools with an observed summary. Impute that
  training group's median only for a wholly missing summary. These are new
  summary moments, not the old individual-grade moments.
- Square the standardized average; do not average the grade-specific squares.
  For every included summary, add categorical controls for 0-4 observed grades
  (0-2 in the period-specific sensitivity). No separate missing dummy is needed
  because the zero-observation category covers wholly missing summaries.
- Fit six separate OLS LPMs, each with one average, its square and count controls;
  also fit one joint model with all six summaries. Keep all 313,813 pupils,
  including 45,947 applicants (14.6415%), in 2,000 origin schools.
- Reuse the same cohort-by-region, sector, gender, age/square/missingness,
  same-SIMCE-school/missingness, parent-record, survey-year, answered-fraction,
  complete-survey and annual survey-PC1 controls as the preceding table. PCA
  replaces individual survey answers. It remains fitted on complete respondents
  by year, and SEs condition on these estimated scores.
- For each academic construct, estimate a sensitivity containing its 5-6 and
  7-8 averages, squares and counts together. Another model jointly includes all
  twelve period summaries. The context/PCA controls and population are fixed.
- For the focused GPA/math-gap comparison, first restrict grade contributions
  to pairs where both measures are observed. Use the same 275,638 pupils with
  at least one paired grade in the GPA-only, gap-only and both-together models.
  Normalize these matched-grade averages separately. This preserves average
  gap = average peer GPA - average peer baseline math. Main-table averages
  need not use identical observed grades, so do not apply the identity to them
  mechanically.
- Cluster actual OLS SEs by origin school, with the package's finite-sample
  correction. Report coefficients and SEs multiplied by 100 (percentage points).
  A linear coefficient is the local slope at the reference mean; with squares,
  the fitted change from zero to one standardized unit is linear plus quadratic.

## Results and interpretation

Separate average peer-GPA: linear .023277 (SE .101992), squared -.382032
(.057520), in pp. Its near-zero linear slope is not evidence of no association;
the squared term implies an inverted U. Average math gap: linear -.709339
(.074205), squared .007717 (.044911).

The two period-specific peer-GPA linear coefficients, entered together, are
-1.741717 (.202039) for grades 5-6 and +1.664028 (.180080) for grades 7-8.
Thus averaging suppresses a timing distinction. These conditional slopes are
not causal effects or a test of a pooled common slope; period SDs differ.

With all six averages jointly, survey PC1 slopes are -1.090076 (.336556),
-1.225950 (.195193), -1.044113 (.159739), and -.672865 (.135215) for survey years
2013-2016. More favorable reports remain associated with less application.
Raw and peer-relative GPA/attendance overlap mechanically; large joint academic
coefficients must be interpreted conditional on that overlap, not as marginal
associations. Gap coefficients also change the baseline-achievement condition.

In-sample R-squared is .0282376 for one average, .0327205 for two periods, and
.0332925 for separate grades, all with annual PC1 and the same population.
Academic coverage controls differ, so this is descriptive, not a formal nested
test. No new held-out predictive-performance claim is made.

## Verification, output and limits

`16_average_grades.R` verifies observed-grade arithmetic, sample preservation,
paired-gap identity, separate-model composition, OLS normal equations, and an
independently reconstructed joint-model school-clustered covariance.
`17_report_averages.R` reads saved small coefficient tables and validates unique
model/term lookups when rendering the report.

Outputs: `data/clean/sae_participation/averaged_grades/` and
`output/reports/sae_participation/voluntary_averages.html` (Markdown alongside).
The original OLS report links to this compact update.

Annual grade-8 measures may postdate application, PC1 describes the grade-4
school, survey completeness can be selective, and available-grade means can
remain affected by missingness despite count controls. The outcome is explicit
SAE application from a school offering grade 9, not an observed move or motive.
Geography remains deferred.
