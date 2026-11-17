# Applicant and attended-school comparison

Run from the repository root:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/applicant_priority_comparison/01_build_four_group_comparison.R
```

The script compares four groups of timely grade-9 SAE applicants in 2018-2020:

1. origin school ends at grade 8, not prioritario;
2. origin school ends at grade 8, prioritario;
3. origin school offers grade 9, not prioritario;
4. origin school offers grade 9, prioritario.

It reports applicant baseline characteristics, cohort composition, assignment risk,
expected VA of the application portfolio, attended-school sector and TP status, and
attended-school EB VA on the seven paper outcomes. The continuation labels use the
paper-style SAE offering definition. “Voluntary” describes continuation availability;
it does not prove motive or require an observed school change.

The main HTML report is written to
`output/reports/applicant_priority_comparison/applicants_and_attended_schools.html`.
The paper-ready LaTeX table is written to
`output/tables/applicant_priority_comparison/applicants_and_attended_schools_by_priority_continuation.tex`.
Reusable cell statistics and coverage diagnostics are under
`data/clean/applicant_priority_comparison/`.

To rank destination schools by the number of distinct applicants listing each
school and attach pooled 2018-2020 grade-9 vacancies, run:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/applicant_priority_comparison/02_top_sae_destination_schools.R
```

This writes the top-100 CSV and summary to
`output/tables/applicant_priority_comparison/` and an HTML report to
`output/reports/applicant_priority_comparison/top_100_sae_destination_schools.html`.

`03_count_sae_applications_by_grade.R` reports all-grade counts in the regular
SAE rounds of 2018-2020 directly from the raw C1 files. It excludes automatically
added continuity entries and reports distinct applicants, distinct applicant-school
pairs, and applicant-program rows separately. Counts cover the full SAE system in
each year, without the paper's age, observed-high-school, or earliest-process
restrictions. Pooled applicant totals count applicant-years, so a person returning
in a later year is counted again. Derived summaries are written to
`data/clean/sae_applications_by_grade/`.
The local 2018 C1 codebook confirms target-grade codes -1 = pre-kinder,
0 = kinder, 1-8 = basic education, and 9-12 = first through fourth medio,
and defines the automatically added continuity flag on its second page.
