# PAES 2026 Update

This folder tracks the July 2026 update that incorporates the new raw PAES 2026
files from:

```text
C:/Users/brunem/Dropbox/causal_schools/data/raw/2026
```

## Implemented

- `code/clean_college_apps_data.R` now reads PAES 2026:
  - `PAES-2026-Inscritos-Puntajes/A_INSCRITOS_PUNTAJES_PAES_2026_PUB_MRUN.csv`
  - `PAES-2026-Postulantes/C_POSTULANTES_SELECCION_PAES_2026_PUB_MRUN.csv`
- `data/clean/psu_students.RData` and `data/clean/psu_applications.RData`
  have been regenerated with 2026 included.
- `code/universe_reg_df.R` now constructs:

```text
expected_psu_year = cohort_gr8 + 5
timely_psu = 1[psu_year == expected_psu_year]
```

because `psu_year` is the admissions-process year.
- `data/clean/univ_gr8_df.csv` and `.dta` have been regenerated and now include
  existing-universe students observed in PAES 2026.

## Auxiliary Inputs

Run:

```r
source("code/codex/paes_2026_update/01_build_paes_2026_auxiliary_clean_inputs.R")
```

This writes:

```text
data/clean/paes_2026_update/oferta_codes_2024_2026_all.*
data/clean/paes_2026_update/oferta_codes_2024_2026_rec.*
data/clean/oferta_codes_24_26_all.rds
data/clean/oferta_codes_24_26_rec.rds
data/clean/paes_2026_update/paes_matricula_2025_2026_mapped.*
data/clean/paes_2026_update/paes_socioeconomics_2026.*
output/tables/paes_2026_update/paes_2026_auxiliary_clean_input_diagnostics.csv
```

The top-level `oferta_codes_24_26_*` files are intentionally named separately
from the old `oferta_codes_24_25_*` files so legacy 2024-2025 outputs are not
silently redefined.

Then build the canonical 2022-2025 SIES program metadata:

```r
source("code/codex/paes_2026_update/04_build_program_info_22_25.R")
```

This writes:

```text
data/clean/program_info_22-25.rds
output/tables/paes_2026_update/program_info_22_25_diagnostics.csv
output/tables/paes_2026_update/program_info_22_25_oferta_coverage.csv
```

`program_info_22-25.rds` uses full SIES `Matricula-Ed-Superior-2025`, not PAES
matricula. Against PAES oferta 2026, matched `COD_SIES` rows increase from
1,910 with `program_info_22-24.rds` to 2,060 with `program_info_22-25.rds`.

Then append first-application PAES 2026 students to the existing application
portfolio outcome:

```r
source("code/codex/paes_2026_update/02_append_2026_first_app_stem_outcome.R")
```

This keeps all existing `stem_outcome.RData` rows and appends only students
whose first regular application year is 2026. It writes:

```text
data/clean/stem_outcome.RData
data/clean/paes_2026_update/stem_outcome_pre_paes2026_append.RData
output/tables/paes_2026_update/stem_outcome_2026_append_diagnostics.csv
output/tables/paes_2026_update/stem_outcome_coverage_by_first_app_year.csv
```

After appending, rerun `code/universe_reg_df.R` so the updated
`stem_outcome.RData` is merged into `univ_gr8_df.csv` and `.dta`.

## Grade-8 2021 Universe Extension

Run:

```r
source("code/codex/paes_2026_update/05_extend_main_universe_to_2021.R")
```

This rebuilds the canonical universe, school-tracking, and SAE first-offer
inputs through grade-8 cohort 2021:

```text
data/clean/universe.csv
data/clean/universe_controls.csv
data/clean/tracking_univ8gr.RData
data/clean/rbd_universe.csv
data/clean/sae_grade9_unique_proceso.RData
data/clean/sae_grade9_students_frame.RData
data/clean/sae_binary_prep.RData
data/clean/treatment_1R_v2.RData
data/clean/offers_1R_p_proceso.RData
```

The script uses raw `student_tracking/2025`, so cohort 2021 receives the same
four-year post-grade-8 attended-school definition as earlier cohorts
(`cohort_gr8 + 1` through `cohort_gr8 + 4`). Duplicate student-year enrollment
rows are resolved deterministically by preferring nonzero attendance, passed
records, highest attendance, then RBD.

After running this script, rerun:

```r
source("code/universe_reg_df.R")
```

`universe_reg_df.R` now also adds a separate PAES-only flag,
`paes_matriculated_2026`, from mapped `PAES-2026-Matricula`. This variable is
not a full SIES substitute; `paes_matricula_is_full_sies_2026` is set to `0`.

## Audit

Run:

```r
source("code/codex/paes_2026_update/03_audit_paes_2026_integration.R")
```

This writes:

```text
output/tables/paes_2026_update/paes_2026_integration_audit.csv
output/tables/paes_2026_update/paes_2026_integration_audit.md
output/tables/paes_2026_update/paes_2026_clean_score_application_year_counts.csv
output/tables/paes_2026_update/paes_2026_universe_cohort_audit.csv
output/tables/paes_2026_update/paes_2026_rbd_match_by_cohort.csv
output/tables/paes_2026_update/paes_2026_oferta_24_26_year_counts.csv
output/tables/paes_2026_update/paes_2026_program_info_22_25_year_counts.csv
output/tables/paes_2026_update/paes_2026_program_info_oferta_coverage.csv
output/tables/paes_2026_update/paes_2026_matricula_paes_year_counts.csv
output/tables/paes_2026_update/paes_2026_stem_outcome_year_counts.csv
```

Latest audit highlights:

- Raw PAES 2026 file families present: 5/5.
- `psu_students.RData` has 215,142 clean score rows for 2026.
- `psu_applications.RData` has 1,694,081 application rows for 2026.
- `univ_gr8_df.csv` now covers grade-8 cohorts 2017-2021, with 1,197,982
  one-row-per-student records and 200,024 records observed in
  `psu_year == 2026`.
- Cohort 2021 has 253,472 students, 251,024 matched `most_time_RBD` values,
  190,187 timely PAES 2026 records, and 79,475 `paes_matriculated_2026`
  records.
- `stem_outcome.RData` has 141,178 first-application rows in 2026.
- `oferta_codes_24_26_all.rds` includes 2,150 PAES 2026 program-code rows,
  with 87 missing `COD_SIES`.
- `program_info_22-25.rds` includes 15,826 total `COD_SIES` rows after the
  most-recent collapse, including 11,743 rows whose latest source year is full
  SIES 2025. It improves PAES oferta 2026 metadata coverage from 1,910 to 2,060
  matched `COD_SIES` rows.

## Important Limitation

`PAES-2026-Matricula` is not the full SIES `Matricula-Ed-Superior` file. It is
useful as a PAES/admissions-system enrollment object and can be mapped to
`COD_SIES` through same-year oferta, but it should not silently replace the full
higher-education matricula files used for `COD_SIES_m1`, accreditation, or
program-income outcomes.

The grade-8 2021 cohort is now present in the main universe because
`student_tracking/2025` is available and `rbd_universe.csv` has been rebuilt
with the full four-year attended-school window. The remaining limitation is
higher-education enrollment coverage: full SIES `Matricula-Ed-Superior-2026` is
still absent, so PAES matricula 2026 remains a separately labeled PAES-only
source.
