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

## Enrollment trends before and after SAE

The 2026-11-12 follow-up uses actual April-30 enrollment to check whether
growth differences by VA predate SAE and change at regional rollout.

```powershell
& code/codex/secondary_sae_spots/03_fetch_enrollment.ps1
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/secondary_sae_spots/04_build_enrollment.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/secondary_sae_spots/05_analyze_enrollment_trends.R
& 'C:/Users/brunem/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/python.exe' code/codex/secondary_sae_spots/06_validate_enrollment.py
```

The downloader retrieves official classroom aggregates for 2010-2025, about
1.5-2 MB compressed per year, with source PDFs and hashes. It never reads the
large student-level Rendimiento files or changes existing raw data. The R
builder sums regular youth-secondary teaching codes 310/410/510/610/710/810/910
in functioning schools. Combined classes use grade-specific counts. The 2014
missing sex details concern preschool/primary rows; secondary rows reconcile.
The gender-count reconciliation includes the new N_ALU_NB field from 2024.

The population comprises 2,621 eventually SAE-listed secondary schools;
2,419 have positive enrollment in all three years before their regional SAE
entry cohort. VA matches 2,398 schools for math/field/institution and 2,404 for
exam taking. Fixed top-quartile VA groups use the existing retrospective EB
estimates. Subsequent missing regular-secondary enrollment records enter as
zero reported enrollment, not verified legal closures. Post-baseline entrants
are excluded; this population cannot describe every pre-SAE school closure.

Normalize each school to 100 at its mean enrollment in event years -3,-2,-1.
Entry year is 2017 in Magallanes, 2018 in four regions, 2019 in the remaining
non-Metropolitana regions, and 2020 in Metropolitana. Nuble joins historical
Biobio. Use the common event window -5 through +5, equal school weights,
school and region-by-calendar-year effects, and separate high-VA differential
pre-slope, level shift and slope change. Cluster by 15 regions with t(14)
confidence intervals; all post-slope SEs use the actual covariance matrix.
Grade-9 enrollment is a separate admissions-margin outcome. Sensitivities
exclude 2020-2021, require at least 100 baseline secondary students, weight
by baseline enrollment, use continuous VA, restrict to 2021 SAE listings,
or require positive enrollment in all five pre-SAE years. Group thresholds
remain fixed across sensitivities. These are descriptive comparisons, not
SAE effects; VA uses overlapping 2017-2020 grade-8 cohorts and later outcomes.

The analysis script launches one short R worker per VA measure to limit peak
memory. A specific outcome argument reruns one worker; argument `report`
rebuilds tables/figures from the saved worker estimates. Disabling automatic
data.table indices and removing staging objects avoids unnecessary memory use.

Outputs are in `data/clean/secondary_sae_spots/enrollment_trends/`: school-year
counts, source audit/hashes, cohort and VA panel, event and calendar group
means, 224 regression contrasts from 56 models, event-study estimates, and
compact actual fixest objects in `models/`. The HTML report is
`output/reports/secondary_sae_spots/enrollment_trends_before_after_sae_va.html`;
three PNGs are in the corresponding figures directory.

Python independently re-aggregates every school's grade counts in all 16
sources (47,317 school-years), then uses the exact balanced-within-region
FWL transformation and regional cluster sandwich to reproduce all 224
coefficients/SEs to 1e-8 (observed maximum errors 1.7e-11 and 2.5e-12).
See `independent_validation.json` and project-memory decision
`2026-11-12-secondary-enrollment-trends-va.md`.
