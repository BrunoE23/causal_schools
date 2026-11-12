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

## Capacity growth and VA, 2021-2025

Run `02_correlate_va.R` after the annual capacity constructor. It reads only
the small saved school-year panel and the current Box
`data/clean/empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv`.
It does not rebuild or change VA estimates.

The primary outcomes are the four existing school-preference VA dimensions:
math, exam taking, high-premium field, and high-premium institution. Four
supplementary outcomes in the same source (language, enrollment, aid application,
and log predicted program income) are reported separately. All-sample,
student-centered EB adjusted observational VA is the main measure; unshrunk
VA is a sensitivity. Each school receives equal weight in the correlations.

Main growth is 100*(C_2025/C_2021-1), starting with all 2,511 RBDs with positive
2021 secondary capacity. The 24 schools with zero/no 2025 secondary listing
enter as -100%; new listings with no positive baseline cannot define growth.
Alternative measures are absolute seat change, Spearman rank correlation,
log growth among positive-capacity continuers, and percentage growth at schools
with at least 100 baseline seats. No matched schools are trimmed. The largest
two raw percentage growth cases have no matched VA and thus do not influence
the correlations. Outcome-specific unmatched lists are exported.

Outputs: `data/clean/secondary_sae_spots/va_correlations/` (endpoint panel,
matched long data, eight-outcome main table, 40 sensitivity rows, annual
2021-to-2022 through 2024-to-2025 correlations, coverage, and manifests).
The report is `output/reports/secondary_sae_spots/capacity_growth_vs_va_2021_2025.html`;
the plot is under the corresponding figures folder. Four plot panels show all
continuers, log growth, 20 equal-count VA-bin means, and an OLS line.

The correlations are descriptive, without controls, weights, p-values, or
measurement-error correction. VA is estimated using 2017-2020 grade-8 cohorts
and later outcomes that overlap the capacity period, so it is not a pre-2021
information set or a causal measure of schools' capacity response to quality.
Independent Python source joins reproduce all 40 Ns, Pearson correlations,
and tied-rank correlations to 1e-12.
