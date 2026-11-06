# Baseline composition by school EB VA quintile

Run `01_tabulate.R` from the repository root with R, data.table and ggplot2.
The main table describes the broad panel, with parallel SAE-applicant and
at-risk tables. There are four outcome-specific panels: math, verbal,
high-premium field and high-premium institution.

Use the current unified R/lfe All-sample EB column
`controlled_value_added_eb_centered_student`. School quintiles are defined
once per outcome using `ceiling(5*average_rank/N)` over all finite-VA schools,
with equal school weights and ties kept together. Cutoffs are common across
the three student populations. A student is assigned the quintile of
`most_time_RBD`. No offered-school or complier weighting is used.

The broad panel is the existing 931,547 unique students in Grade 8 cohorts
2017-2020, age 12-16, with a positive most-attended high-school RBD, including
private schools. Timely SAE applicants in cohorts 2018-2020 number 294,198.
The at-risk sample uses the existing 162,633-student full roster in the saved
main-IV frame, before exam-taking or baseline complete-case filters. Its
student IDs and SAE processes are checked against the universe. No PAES,
later enrollment, or baseline-completeness restriction enters these tables.

Read 14 selected columns of the roughly 1 GB universe. Report student/school
Ns and student-weighted means of female, Grade 8 age, Grade 8 residence in RM,
Grade 4 math and verbal z-scores, existing imputed income decile and mother's/
father's education years. Existing scores are not restandardized; existing
baseline imputations are retained without new imputation. Female is missing
for gender codes outside 1/2, including one broad-panel student; region is
defined for valid codes 1-16. Characteristic-specific nonmissing means use
their own observed denominators. Report coverage explicitly rather than
requiring a common complete-case sample.

Derived inputs, school assignments/cutoffs, variable means/SDs/nonmissing Ns,
student/school counts, matching coverage, population-specific wide tables and
parity/partition audits are under `data/clean/baseline_by_va_quintile/`.
The full interactive HTML table, with a population selector and coverage in
each mean cell, is under `output/tables/baseline_by_va_quintile/`.
Four-panel PNG tables for each population are under
`output/figures/baseline_by_va_quintile/`.

Checks reproduce unstratified legacy-coded means/Ns/coverage from the existing
balance table before VA matching (1e-10 tolerance), separately recording the
tiny differences from treating one invalid gender and eight invalid region
codes as missing rather than zero in the new tables. Other checks
verify unique keys, positive known school links, exact population sizes,
twenty school quintile definitions, conserved matched student totals, valid
coverage counts and all twelve HTML table panels. Means describe baseline
composition by subsequently attended school, not school treatment effects.

## SAE applicants by their expected-VA (E) quintile

Run `02_tabulate_expected_va.R` after script 01. It reuses the validated
baseline input and the same eight characteristics, restricting to the
294,198 timely SAE applicants from the preceding table. It reconstructs
each applicant's four E variables using the current unified R EB school
values and the original 2018-2020 assignment probabilities. Only selected
columns of the three probability files, about 30 MB total, are read; no
new universe scan is needed.

E is exactly the current main-IV object: sum of probability times school
EB VA, with unavailable school VA (including unmatched options) contributing
zero and no renormalization over covered schools. A missing probability
record leaves E missing, not zero. No new probability-coverage, PAES-taking,
baseline-completeness or at-risk restriction is imposed. Real-school unknown
VA mass and unmatched mass are separately audited. Both E and covered
probability mass must match the saved current main frame for all 162,633
at-risk students, for all four outcomes, to 1e-10.

Quintiles now rank **students by E**, rather than schools by attended VA.
They use `ceiling(5*average_rank/N)` among applicants with finite E, separately
for each outcome, with equal student weights and tied E values kept together.
Cutoffs and Ns can therefore differ by outcome and need not split tied values
into exactly equal student counts. Baseline means use the same nonmissing-
value denominators and existing imputations as script 01. The table adds
mean E in each quintile: math/verbal in test-score SD and field/institution
in percentage points of school VA. Means are not restandardized within groups.

Outputs are isolated under `expected_va/` within the preceding clean-data,
figure and table directories: derived applicant inputs, student quintile
assignments, E definitions, baseline means/SDs/Ns and coverage, main-E parity,
probability coverage and count-partition checks, wide CSV, four-panel PNG,
and HTML table with characteristic-specific availability. Regenerate script
01 first if the universe/baseline definitions change. These are descriptive
baseline differences in ex ante assignment opportunities.

All 294,198 applicants have probability records and E values in all four
outcomes. Maximum E discrepancy versus the 162,633-student saved main frame
is 5.55e-16; maximum covered-mass discrepancy is 1.11e-16. For each outcome,
782 applicants have positive probability on a real school without an available
VA, with mean unknown mass 0.001478 across all applicants. Mean unmatched mass
is 0.080284. These contributions retain the main construction's zero values.
In expected field VA Q1/Q5, female shares are 55.8/28.3%; expected math VA
Q1/Q5 have baseline math -0.38/+0.03 SD and income decile 4.20/5.16.
