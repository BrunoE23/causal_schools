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
