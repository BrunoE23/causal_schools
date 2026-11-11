# Secondary-school spots listed in SAE, 2016-2025

Answers the request for every year since SAE's creation, using SAE A1 offers
for secondary grades 9-12 (1-4 medio). It does not measure enrollment or use
school-directory operation status to define exits.

Run from the repository root:

```powershell
& code/codex/secondary_sae_spots/00_fetch_missing_offers.ps1
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/secondary_sae_spots/01_analyze.R
```

The first command retrieves four official public archives (about 135 MB in
total) and extracts only A1 and its documentation. The second uses data.table
and base R and completes in seconds. Existing raw files are not changed.
`CAUSAL_SCHOOLS_DATA_WD` can override the default Box data root.

## Inputs and definitions

- Existing regular-round A1 files for process years 2016-2021 in the raw data.
- Official 2022-2025 A1 sources downloaded from
  https://datosabiertos.mineduc.cl/sistema-de-admision-escolar-sae/ into the
  new task's `data/clean/secondary_sae_spots/source_offers/` cache.
- The existing school-directory cache supplies RBD names and regions only.
  Nuble is grouped with Biobio to preserve historical geographic boundaries.
- Process year t corresponds to entry year t+1. The latest release checked
  on 2026-11-11 is process 2025, entry 2026.
- `cupos_totales` measures declared total capacity. `vacantes` measures
  available places advertised for admission. Neither is observed enrollment.
- Sum unique offering programs within each RBD-grade-year, then across grades.
  Program keys are RBD+cod_curso from 2017; 2016 uses
  RBD+nivel+cod_ense+cod_grado. The script checks uniqueness before summing.
- Use initial regular-round A1 consistently, including 2024, which also has
  a separate A3 file for revised waitlist-stage offers. Do not mix rounds.

## Comparability and exits

Each annual comparison retains only region-grade combinations present in
both years before summing school capacity. Thus a regional rollout or the
first inclusion of grades 10-12 does not count as an existing school expanding.
This retains genuine addition or removal of a grade at an individual school
within already covered region-grade cells. Early pairs have different coverage.
The 2016 baseline has only grade 9 in Magallanes. Grade 9 is national in 2019;
all secondary grades are national from 2020. National full-secondary annual
changes therefore start with 2020-to-2021. Grade-9-only comparisons are also saved.

Continuers have positive capacity in both years. Expansion/downsize means any
positive/negative change; changes strictly greater than 10 seats are separate
columns. Entries/exits are transitions from/to zero capacity or no listing in
the compared coverage. Disappearance of a listing and continued listing with
zero capacity are separately retained. A school with zero vacancies and
positive capacity has not exited. Even disappearance from all SAE listings
does not establish legal or permanent school closure. The exercise is descriptive,
not an estimate of the causal effect of SAE.

## Outputs and validation

- `data/clean/secondary_sae_spots/`: annual totals, comparable annual changes,
  school-year and school-grade-year panels, school transitions, coverage audit,
  source-key audit, and input paths/hashes.
- `output/reports/secondary_sae_spots/annual_secondary_sae_spots.html`: annual
  tables, definitions, figure, downloads, and latest secondary exit list.
- `output/figures/secondary_sae_spots/secondary_sae_spots.png`: national stock
  and annual school-change plot.

Checks cover key uniqueness, grade/teaching codes, nonnegative observed capacity
and vacancies, vacancies not exceeding capacity, complete geography, and exact
school and seat accounting. A separate Python reconstruction of the raw A1
files verified all annual totals and national 2021-2025 transition counts.
See project-memory decision `2026-11-11-secondary-sae-spots.md`.
