# Kinder capacity accounting

Run from the repository root:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/kinder_capacity/01_account_capacity.R
```

The script uses small annual SAE A1 regular-round offering files (2017-2021)
and school directories (2015-2025). No pupil-level files are needed. The primary
measure is declared total kinder capacity (`cupos_totales`), with `vacantes`
reported separately. Sum unique RBD-program rows for `cod_nivel=0`, which is
equivalent to ordinary preschool `cod_ense=10`, `cod_grado=5`.

National comparisons use SAE processes 2019-2021, admitting pupils for 2020-2022.
Earlier changes use only common rollout regions, with Nuble normalized to its
previous Biobio boundary. Program uniqueness, nonnegative capacity/vacancies,
vacancies <= capacity, and exact accounting identities are checked.

New positive-capacity SAE kinder listings are split into schools observed
operating previously, candidate openings (first observed operating in the
current process year or following entry year), and unresolved cases. Candidate
openings do not establish legal founding dates. School identifier changes,
reactivation, and existing schools entering SAE remain possible. Directory
preschool codes cannot establish when kindergarten specifically was introduced.

School-year capacities, exact transitions, annual and decomposed totals, input
manifests, directory history, and checks are under `data/clean/kinder_capacity/`.
The report is `output/reports/kinder_capacity/kinder_capacity_changes.html`.
The report can be rebuilt without re-reading sources by running
`02_report_capacity.R`. Methods are documented in project-memory decision
`2026-10-14-kinder-capacity-accounting.md`.
