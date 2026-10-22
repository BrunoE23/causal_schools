# Table 2 variant: school-ending and voluntary SAE applicants

Update 2026-10-22: at the user's request, add share of SAE applicants and
non-private high-school attendance to the existing HTML/LaTeX variant.
The first row divides each SAE-subset N by all 294,198 applicants, leaving
the broad-panel cell not applicable; do not renormalize away the 1.62%
unresolved/other group. The attendance row uses most_time_RBD and the same
private-paid sector indicator from staff_lasso_va/school_predictors.csv
as the four-group applicant comparison. Means condition on observed sector,
with counts and coverage in nonprivate_attendance.csv. Attendance coverage
is over 99.94% for all columns. School-ending/voluntary rows are
85.17%/13.21% of applicants and 99.45%/98.37% non-private attendance.
All original 45 paper means and group sizes remain unchanged. Script 03
adds this small sector summary and 02 renders both new rows reproducibly.

Date: 2026-10-12

Bruno requested a version of Table 2 with columns for SAE applicants from
schools ending at grade 8 and voluntary SAE applicants, with no differences.

Retain the exact three original samples and variable constructions from
`code/claude/balance_table/01_build_balance_table.R`:

- Broad panel: cohorts 2017-2020, observed most-time high school, age 12-16.
- SAE applicants: within that panel, cohorts 2018-2020 and timely_sae = 1.
- At-risk applicants: additionally max simulated option probability < 1,
  using the full long DA distribution without a school-support threshold.

Append two subsets of the original SAE-applicant column by joining the cached
grade-8 origin classification on mrun and cohort_gr8:

- School ending: `Ends at 8 in SAE offerings`, meaning maximum listed offered
  grade <= 8 in the contemporaneous SAE A1 file.
- Voluntary: `Offers grade 9` in that file.

These are origin-school continuation proxies already used by the participation
exercise, not a new legal-school register or a claim about motives/actual moves.
Do not use the participation exercise's broader regular-plus-complementary
application flag to redefine the paper's SAE sample. Origins absent from the
offering file remain unresolved, never automatically labeled school-ending.

Keep all Table 2 rows, including block match rates, N and percentage of broad
panel. Compute means on available observations with the exact existing exam
normalization (within year after broad-panel restrictions), socioeconomic
imputation, middle-school measures and outcome coding. Omit differences, SEs
and joint tests. Risk and continuation columns overlap; they are not five
mutually exclusive groups.

The original columns reproduce all 45 saved paper means, sample sizes and
per-variable match rates within 1e-10. Broad N is 931,547, SAE N is 294,198,
and at-risk N is 162,633. Among applicants, school-ending N is 250,564 (85.17%)
and voluntary N is 38,868 (13.21%); 4,766 (1.62%) enter neither new group
(4,635 unresolved, 131 other offering patterns). The latter remain in the
original columns when eligible. Metropolitan shares are 23.9% and 35.9% in
the school-ending and voluntary applicant groups, respectively.

Outputs are a separate paper-table fragment
`output/tables/balance_table/balance_table_continuation.tex` and an HTML view
`output/reports/balance_table/table2_continuation.html`. The manuscript's
existing Table 2 is not replaced. Derived statistics and replication checks
are under `data/clean/balance_table_continuation/`; code is under
`code/codex/balance_table/`. Source data remain unchanged.
