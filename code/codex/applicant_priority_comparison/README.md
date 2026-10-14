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
