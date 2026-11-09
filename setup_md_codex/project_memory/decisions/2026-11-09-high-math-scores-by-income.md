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

## Four-field decomposition

The user requested a separate line for each of the four high-paying fields,
still conditional on the same math cutoff. Scripts 04 and 05 retain the
existing 93,469-student high-math cache and its income deciles, including
non-enrollees in all denominators. The outcomes are first-enrollment Science,
Law, Engineering/Manufacturing/Construction, and Medicine+.

Science, Law and Engineering use the master's existing positive-list flags.
Medicine+ is recovered exactly as the intersection of `high_paying_field_m1`
and `field_reclassified_m1 == "Medicine"`. This is algebraically equivalent
to the original six-program definition because the combined high-paying
indicator already incorporates that positive list and the other three groups
are disjoint from Medicine. Verify this decomposition for every student:
at most one component is one, and their sum equals the original combined
indicator. Also verify all ten decile numerators and denominators against
the prior figure. Do not substitute all Medicine enrollment or redefine the
program list. The four conditional shares therefore sum exactly to the
previous high-paying program curve.

## Mean scores within the selected high-math sample

The next user request is to show average math and verbal scores by income
decile in this selected sample. Scripts 06 and 07 preserve the previous
high-math student cache, existing income deciles, and math z-scores.
Standardize positive verbal scores (`leng_max`) separately within admission
year in the paper's broad panel (2017-2020 grade-8 cohorts, age 12-16,
positive attended high-school RBD), before income or high-math restrictions.
Verify verbal means and SDs against the paper's saved score diagnostics.
Thus both plotted averages are in the paper's subject-specific
admission-year SD units, without re-standardizing in the selected sample.

Retain all selected students for the math mean. If verbal scores are missing,
exclude them only from the verbal mean, report coverage by decile and in the
figure, and save the common-observed-sample math mean as a diagnostic.
Pool students with equal weight; impose no enrollment, verbal-threshold,
or further complete-control restriction. These descriptive means reveal
remaining score variation within the common high-math cutoff.

## Projected income among HE enrollees and enrollment rate

The user requested a figure version with log projected income among those
enrolling in HE and the share enrolling in HE. Scripts 08 and 09 retain the
exact selected high-math sample and the previous first-enrollment indicator.

For income, average the existing `log_program_income_full_clp_m1` over HE
enrollees only in each income decile. This is mean ln(projected monthly CLP),
not ln(mean projected income). Verify its equivalence to the natural log of
the raw projected-income column. Retain the established MiFuturo estimates
and their existing fallback sources; exclude all non-enrollee assignments
from this mean. Require income availability for every enrolled student.

For enrollment, divide the number with observed first HE enrollment by all
selected high-math students in each decile. Verify both counts against the
previous premium-enrollment figure. Use two panels with the respective units
and label deciles 1, 8 and 10, continuing the user's label preference.
