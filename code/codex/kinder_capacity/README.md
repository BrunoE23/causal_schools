# Kinder capacity accounting

The latest user instruction is to ignore SAE years and use the school
directories. Run `04_directory_preschool_changes.R` for that standalone
national 2015-2025 exercise; it reads only the raw directories and needs none
of the earlier SAE-derived caches. Its report is
`output/reports/kinder_capacity/directory_only_preschool_changes.html`.
Derived school-year records, candidate openings, exact preschool stock-change
accounting, field availability, and source manifests are under
`data/clean/school_directory_preschool/`. The figure is
`output/figures/kinder_capacity/directory_only_preschool_changes.png` (and PDF).

Directories identify active preschool enrollment (any ENS=10 at a school
with ESTADO_ESTAB=1 and MATRICULA=1), covering all educacion parvularia levels. They
do not report a consistent annual preschool capacity measure. Hence the
greater-than-10-seat analysis cannot be replicated using directory data alone.
Instead the report explicitly labels changes in school numbers: candidate
first operations with preschool, continuing schools adding/dropping preschool,
returning operations, and schools ceasing observed operation. Their exact
accounting reproduces the stock difference every year. The first observed
operation is not a verified legal founding date. See project-memory decision
`2026-10-14-directory-only-preschool-accounting.md`.

Earlier SAE-based work is documented below for reproducibility.

### Preschool-only establishments

Run `05_count_preschool_only.R` to count operating schools with ENS=10 and no
other positive ENS code in any teaching slot. Zero/NA slots are ignored.
This describes enrolled teaching types, not every authorized offering. Active
still means ESTADO_ESTAB=1 and MATRICULA=1. New CSVs in
`data/clean/school_directory_preschool/` contain annual preschool-only versus
mixed-school counts, the 2025 school list, and its dependency split. The 2025
preschool-only sample is independently verified to have MAT_TOTAL=MAT_ENS_1
and zero enrollment in all other MAT_ENS categories. ENS10 covers all
educacion parvularia, not kindergarten alone.

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

## Large changes and longer opening series

After building the caches, run:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/kinder_capacity/03_plot_large_changes_and_openings.R
```

This reports strict changes greater than 10 seats (11 or more) at continuing
positive-capacity kinder offerings, excluding entry/exit. National changes are
available for entry 2021-2022. A longer comparable series for entry 2019-2022
keeps the five 2017 rollout regions fixed (Tarapaca, Coquimbo, O'Higgins, Los
Lagos, Magallanes). Counts, seat totals, and school lists are saved in new CSVs
under `data/clean/kinder_capacity/`; thresholded vacancy changes are separate
diagnostics and must not be called capacity changes.

Annual national candidate school openings are first observed operating with
enrollment in the 2015-2025 directory history. Exclude the left-censored 2015
baseline and report 2016-2025. The first operating year's dependency and
preschool status determine the split: private paid is COD_DEPE=4, and an
ENS code of 10 identifies any preschool (not kinder alone). Previously
registered but never previously observed operating schools are distinguished
from RBDs newly recorded in their first operating year. These are not verified
legal founding dates. The directory series is broader than SAE; kinder seat
capacity is not known for all its candidate openings.

PNG/PDF figures are under `output/figures/kinder_capacity/`. The new report is
`output/reports/kinder_capacity/large_changes_and_school_openings.html`.
