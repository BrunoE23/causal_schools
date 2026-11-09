# Descriptive high entrance-exam math scores by household income

The user requested a descriptive figure of the fraction of students with a
math score at least one SD above the mean across income deciles, and confirmed
that math refers to the PSU/PAES college entrance exam.

Use the paper's broad grade-8 panel (2017-2020, ages 12-16, positive observed
most-time high-school RBD). Standardize positive `math_max` within `psu_year`
in that broad panel before income restrictions, matching
`school_rbd_observational_values/01_construct_school_rbd_values.R` and checking
against its saved score diagnostics. The indicator is `z_math >= 1`.
Do not standardize separately within income deciles or require a verbal score
or timely exam. This is a within-paper-panel reference distribution, not the
entire national exam-taking population.

Use existing `income_decile_imputed` without re-ranking; exclude unavailable
income deciles. These deciles inherit baseline SIMCE eligibility and existing
school/context-cell income imputation from the master. Existing quantile bins
split tied income midpoint values and need not have equal sizes after this
sample restriction.

Show both observed-score and all-student denominators because exam-score
availability can vary by income. Both use the same count of observed high
scores. The all-student series is observed attainment among students with an
income decile, not an imputation of non-takers' achievement. Pool students
equally across cohorts, without causal interpretation or regression adjustment.

Implementation: `code/codex/math_high_score_by_income/01_plot.R`.
Figures: `output/figures/math_high_score_by_income/`.
Reusable counts, normalization and verification:
`data/clean/math_high_score_by_income/`.

## Conditional premium enrollment extension

The user then requested the proportion in a high-income program, institution,
either, and both, by income decile conditional on the same high-math definition.
Scripts 02 and 03 reuse the saved admission-year normalization and require
identical high-math counts in every decile. The denominator for all four series
is the full high-math group in each income decile, including non-enrollees.
No further verbal-score, timely-exam, or HE-enrollment restriction is added.

Use the existing master first-enrollment flags `high_paying_field_m1` and
`high_inst_m1`. The program flag is the established high-paying field list,
not a newly chosen monetary threshold on full projected program income.
The institution flag uses the established centered income FE > 0.1 cutoff.
Retain the master flags, including non-enrollee zeros and existing institution
FE-support coding. Compute either as the inclusive union and both as the
intersection. Verify outcome completeness rather than dropping students or
changing denominators across series. The four proportions must satisfy
P(either) = P(program) + P(institution) - P(both) in each decile.

This figure conditions only on exceeding the score cutoff; remaining score
differences within the high-math group are not regression-adjusted.
New aggregates and a compact local student cache are saved under
`data/clean/math_high_score_by_income/premium_enrollment/`.
