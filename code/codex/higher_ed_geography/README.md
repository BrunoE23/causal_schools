# Higher-education campus location and home-region changes

Run from the repository root with R, data.table and sf:

```powershell
$env:R_LIBS_USER = 'C:/Users/xd-br/AppData/Local/R/win-library/4.4'
& 'C:/Program Files/R/R-4.4.1/bin/Rscript.exe' code/codex/higher_ed_geography/01_construct_home_regions.R
& 'C:/Program Files/R/R-4.4.1/bin/Rscript.exe' code/codex/higher_ed_geography/02_construct_enrollment_locations.R
```

Set `CAUSAL_SCHOOLS_DATA_WD` to override the Dropbox project root. Windows R
explicitly uses UTF-8 LC_CTYPE to access accented raw filenames. Raw/clean
source files are never modified; all new data go to repository
`data/clean/higher_ed_geography/`. Large CSVs are read with selected columns
and filtered before joins; RDS stage outputs avoid rescanning full enrollment
files during further summaries.

## Student samples

The same broad panel and applicant definitions as the earlier math, income and
region descriptives: Grade 8 cohorts 2017-2020, observed positive high-school
RBD, Grade 8 age 12-16, one row per mrun (931,547 students). SAE applicants are
timely applicants in Grade 8 cohorts 2018-2020 (294,198). The at-risk subset has
maximum simulated assignment probability below one (162,633), using the full
DA probability files and the existing student_id/SAE-process join. No private
school exclusion or baseline-score-complete restriction is added.

## Home definitions

- `home_region_sae`: the recorded SAE process's Grade 9 B1 applicant HOME
  coordinates (`lat_con_error`, `lon_con_error`), matched to the local
  `chl_admin1.shp` polygons. All 16 current regions are used for both endpoints,
  including Nuble. Exact containment only; invalid, offshore and ambiguous
  points remain missing. No nearest-region or Grade 8 fallback is used.
  `sae_georef_quality` and `sae_region_status` retain provenance/coverage.
- `home_region_paes`: `CODIGO_REGION_DOMICILIO` from raw B socioeconomic files
  at the **same process as the student's existing `psu_year`**. No fallback to a
  later address. This is not `CODIGO_REGION_EGRESO`, which describes the school.
  The raw files cover the process years present in the cohort-restricted
  universe, including PDT/PAES 2021-2026 when needed.
- `home_region_paes_expected`: parallel domicile lookup at cohort + 5, provided
  as a fixed-timing sensitivity without replacing the primary definition.
- `home_region_grade8`: original `COD_REG_ALU`, preserving its recorded code.
  This is the source used in the preceding student-region table, not a direct
  SAE observation. It is retained as a separate comparison; historical region
  coding is not harmonized for this secondary measure.

The SAE 2019 B1 local data dictionary identifies coordinates as household
coordinates with random error. Quality 1 is unique rooftop/range-interpolated
geocoding, 2 unique geometric-center geocoding, 3 a coherent response-center,
4 municipality coordinates, 5 a location shared by the user. Movement tables
also report the quality 1-or-5 subset. Apparent regional changes are recorded
address/coordinate differences, not verified physical migration.

## Enrollment indicators

The primary `m1` window uses **exactly the existing `year_info_m1` and
`COD_SIES_m1`** selected in `univ_gr8_df.csv`. Campus region is recovered from
the full SIES 2022-2025 census by MRUN x observation year x CODIGO_UNICO.
Only Carreras Profesionales and Carreras Tecnicas are eligible, matching the
project's enrollment cleaner. No new first-record ordering or expected-entry
year restriction is substituted. In the current data, all 625,616 existing
enrollees match a unique campus region and an original-entry-year record.
This is the first observed ingreso in the available 2022-2025 window, not a
claim about lifetime first enrollment before 2022.

Student-level fields:

- `enrolled_m1`: an existing first-enrollment program is recorded.
- `campus_region_m1`, `campus_modality_m1`: selected program campus and modality.
- `in_rm_m1`: enrolled at a campus with region code 13.
- `outside_home_sae_m1`: campus region differs from SAE home region.
- `outside_home_paes_m1`: campus region differs from linked PAES domicile;
  domicile measured after the first enrollment year is excluded (4 SAE
  applicants, 2 at risk). `home_region_paes_pre_entry` records the eligible home.
- `outside_home_grade8_m1`: secondary comparison with Grade 8 residence.
- `outside_home_paes_expected_m1`: fixed expected-process sensitivity, also
  excluding addresses from processes after first entry.

Non-enrollees receive zero on unconditional enrollment-location outcomes.
Enrollees with missing necessary home/campus data receive NA, never zero.
Campus location is not physical residential location; distance-learning records
are retained and their modality is recorded.

The separate `2025` suffix describes enrollment in the latest full SIES census,
including continuing students. Multiple enrollments use any qualifying campus
in RM/outside home; regions are not chosen by arbitrary row order. PAES addresses
after 2025 are excluded from its PAES-based comparison. No PAES 2026 admissions
matricula is treated as a replacement for the missing full SIES 2026 census.

## Region changes and outputs

`sae_paes_region_changed` compares the two observed home regions, with NA when
either is missing. Grade 8 and fixed-process alternatives are separate fields.

Key outputs under `data/clean/higher_ed_geography/`:

- `student_higher_ed_geography.csv` / `.rds`: all individual indicators and homes.
- `enrollment_location_summary.csv`: counts, observed-outcome denominators,
  conditional enrolled shares and coverage for broad panel, SAE applicants,
  and at risk, separately for first enrollment and 2025. `pct_all_observed`
  excludes missing outcomes; `pct_enrolled_observed` conditions on enrollment
  and observed necessary geography.
- `enrollment_location_common_sample.csv`: all three location measures computed
  on exactly the same enrolled students with both homes available before entry.
- `home_region_movement_summary.csv`: matched counts, differences and rates,
  overall, quality 1-or-5, and among enrollees.
- `sae_to_paes_region_transitions.csv`, `home_region_movement_by_origin.csv`:
  flow counts, origin shares and RM inflows, including diagonal counts.
- `sae_geocoding_coverage.csv`, `first_enrollment_region_matching.csv`,
  `enrollment_scan_summary.csv`: source and coverage audits.
- `home_regions.*`, `paes_domicile_by_process.csv`,
  `annual_enrollment_records.rds`: reusable intermediate files.

Validation covers unique student/process keys, valid probability and region
codes, unchanged applicant/risk totals, exact program/year links to raw SIES,
unique campus regions, observed entry-year consistency, and zeros for
non-enrollees. No regressions or VA estimates are changed.

## Outside-home enrollment by PAES home in RM versus elsewhere

Run `03_tabulate_by_paes_home_rm.R` after the two construction scripts. It reads
the cached student RDS and splits SAE applicants and at-risk enrollees by PAES
HOME region code 13 versus codes 1-12,14-16, not by campus location. The first
enrollment split uses `home_region_paes_pre_entry`; the 2025 split requires
PAES process <= 2025, preserving the previous timing rules. Missing PAES homes
cannot be stratified and are excluded with their counts recorded. The output
`enrollment_outside_home_by_paes_home_rm.csv` contains both outside-SAE and
outside-PAES shares, their counts/denominators, and an additional common-sample
comparison. The standard outside-SAE column excludes missing SAE homes within
each PAES-defined stratum. Assertions verify that strata reconstruct the
preceding PAES outcome's enrolled denominator and numerator for each sample/window.

`04_tabulate_non_rm_destinations.R` further splits the PAES-home-outside-RM
enrollees into any RM campus, another non-home region outside RM with no RM
enrollment, and home-region-only enrollment. It uses the same first-entry and
2025 timing/coverage rules. These categories partition the enrolled sample and
the first two sum to the preceding outside-home count. For 2025, simultaneous
RM and third-region enrollment is assigned to the any-RM category and separately
counted in `n_rm_and_other_non_home`; first-entry campus is unique. Output:
`non_rm_paes_home_enrollment_destinations.csv`, including counts, enrolled shares,
and the share of outside-home students whose campus set includes RM.
