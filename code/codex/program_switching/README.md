# Program Switching

This folder contains a first-pass workflow for counting higher-ed program
switching using the higher-ed matricula files for 2022-2025.

Main script:

- `01_count_program_switches.R`

Inputs:

- `data/raw/<year>/Matricula-Ed-Superior-<year>/20250729_Matricula_Ed_Superior_<year>_PUBL_MRUN.csv`
- Years currently loaded: `2022`, `2023`, `2024`, `2025`

The script reads directly from the raw higher-ed enrollment files but does not
modify anything in `data/raw`.

Outputs:

- `data/clean/program_switching/program_switching_student_summary_2022_2025.csv`
- `data/clean/program_switching/program_switching_overall_summary_2022_2025.csv`
- `data/clean/program_switching/program_switching_dimension_summary_2022_2025.csv`
- `data/clean/program_switching/potential_pipeline_programs_2022_2025.csv`

Definitions:

- The sample keeps `Carreras Profesionales` and `Carreras Tecnicas/Técnicas`.
- The main first-pass switch measure is individual-level: append all enrollment
  rows from 2022-2025 and count distinct objects per `MRUN`.
- `any_program_switch_observed` is true when a student is observed with more
  than one distinct program identifier across all enrollment records.
- `any_institution_switch_observed` is true when a student is observed with more
  than one distinct institution code.
- `any_generic_major_switch_observed` is true when a student is observed with
  more than one distinct `AREA_CARRERA_GENERICA`.
- `any_field_switch_observed` is true when a student is observed with more than
  one distinct reclassified field.
- `program_switch_same_institution_only` is true when a student has more than
  one program but only one institution.
- `program_switch_cross_institution` is true when a student has more than one
  program and more than one institution.
- `program_switch_same_generic_major_only` is true when a student has more than
  one program but only one generic major.
- `program_switch_cross_generic_major` is true when a student has more than one
  program and more than one generic major.
- `program_switch_same_field_only` is true when a student has more than one
  program but only one broad reclassified field.
- `program_switch_cross_field` is true when a student has more than one program
  and more than one broad reclassified field.

Program identifiers:

- The main program identifier is `CODIGO_UNICO`.
- If `CODIGO_UNICO` is missing, the fallback identifier is `COD_INST` plus
  `COD_CARRERA`.

Pipeline flag:

- `any_pipeline_like_program_observed` is a heuristic flag. It marks students
  ever observed in program names or generic career areas containing terms such
  as `bachiller`, `plan comun`, `ciclo basico`, `college`, or `propedeutico`.
- `potential_pipeline_programs_2022_2025.csv` lists pipeline-like programs among
  students who are also observed switching programs.

This is meant only as a diagnostic first pass. It should not be interpreted as a
final transfer or pipeline classification without checking program names and
institution-specific rules.

Run:

```r
source("code/codex/program_switching/01_count_program_switches.R")
```
