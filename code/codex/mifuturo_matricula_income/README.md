# MiFuturo Income on Matriculated Programs

This folder contains a diagnostic workflow for attaching MiFuturo labor-market
income measures to students' matriculated higher-ed programs.

It does not modify the official matricula or universe files.

## Scripts

```r
source("code/codex/mifuturo_matricula_income/01_diagnose_mifuturo_matricula_merge.R")
source("code/codex/mifuturo_matricula_income/02_regress_mifuturo_income_on_institution_carrera.R")
source("code/codex/mifuturo_matricula_income/03_construct_person_level_income_outcomes.R")
```

`01_diagnose_mifuturo_matricula_merge.R` audits possible joins from matriculated
programs to MiFuturo.

`02_regress_mifuturo_income_on_institution_carrera.R` revisits the earlier
imputation idea in `code/processing_college_apps_outcome.R`: regress observed
MiFuturo log income on institution plus carrera fixed effects, then predict
missing MiFuturo income rows only when both levels are in support.
The chosen carrera variable is `AREA_CARRERA_GENERICA`. Title/career names are
kept only as a diagnostic of within-generic-career variation.

`03_construct_person_level_income_outcomes.R` builds candidate person-level
income outcomes for the grade-8 universe. Matriculated students receive the
MiFuturo model prediction. The preferred tier is the two-way FE prediction from
`institution + AREA_CARRERA_GENERICA`; programs outside two-way support are
filled by `AREA_CARRERA_GENERICA` FE, institution FE, then the global MiFuturo
mean. Students with no observed matriculation receive a configurable wage floor.
The wage floor is not used for matriculated students.
The current default floor is `350000` CLP, labelled
`raw_minimum_wage_proxy_2020_2023`, because MiFuturo income measures likely
reflect wage years before the 2025-2026 publication window.

## Code Map

From the official higher-ed matricula dictionary:

- `CODIGO_UNICO`, renamed `COD_SIES` in project code, is the unique program code
  considering institution, sede, career/program, jornada, and version.
- `COD_CARRERA` is the career/program code.
- `CODIGO_DEMRE` is the DEMRE code and exists only for institutions attached to
  the centralized admissions system.
- `COD_INST` is the higher-ed institution code.

For applications:

- `COD_CARRERA_PREF` is the application/preference program code.
- `code/facts_major_choice_prep.R` maps `COD_CARRERA_PREF` to `COD_SIES` using
  2024/2025 oferta definitiva files.

For MiFuturo income:

- The inspected income file does not expose `COD_SIES`.
- Its `Código` behaves as an institution code, so the diagnostic treats it as
  comparable to `COD_INST` only as part of a larger key.
- The available program descriptors are institution type/name, generic career
  name, and title/career name.

## Match Keys Tested

The diagnostic tests two candidate joins from matricula to MiFuturo:

```text
inst_type_generic_title =
  COD_INST + TIPO_INST_1 + NOMB_INST + AREA_CARRERA_GENERICA + NOMB_CARRERA

inst_type_generic =
  COD_INST + TIPO_INST_1 + NOMB_INST + AREA_CARRERA_GENERICA
```

The preferred join/imputation dimension is `AREA_CARRERA_GENERICA`.
`inst_type_generic_title` remains useful for auditing multiplicity and naming
variation, but it is not the main specification.

The current clean matricula exports do not carry `AREA_CARRERA_GENERICA`, so the
diagnostic fills it from `data/clean/program_info_22-24.rds` by `COD_SIES` and
reports that source. This is a diagnostic supplement, not a silent merge rule for
the main pipeline.

## Outputs

The script writes diagnostics under:

```text
output/tables/mifuturo_matricula_income/
```

Main outputs:

- `mifuturo_matricula_match_summary.csv`
- `mifuturo_matricula_unmatched_programs.csv`
- `mifuturo_matricula_key_multiplicity.csv`
- `mifuturo_matricula_candidate_program_income.csv`
- `mifuturo_matricula_code_map.csv`
- `mifuturo_matricula_match_report.md`
- `mifuturo_income_fe_model_summary.csv`
- `mifuturo_income_fe_cv_summary.csv`
- `mifuturo_income_fe_predictions.csv`
- `mifuturo_income_fe_unsupported_missing.csv`
- `mifuturo_carrera_generica_title_variation_examples.csv`
- `mifuturo_income_fe_report.md`
- `mifuturo_income_fe_model_artifact.rds`
- `mifuturo_person_level_income_outcomes.csv`
- `mifuturo_person_level_income_source_summary.csv`
- `mifuturo_enrolled_program_income_summary.csv`
- `mifuturo_enrolled_income_unsupported_programs.csv`
- `mifuturo_person_level_income_report.md`

## Interpretation

This should be read as a merge audit.
MiFuturo income is not observed at the full `COD_SIES` level in the inspected
file, so multiple sede/jornada/version-specific `COD_SIES` values can map to one
MiFuturo income row.

Before adding income to the official person-level matricula outcomes, inspect
coverage, unmatched programs, and key multiplicity.
For the log-income imputation, use the generic-career prediction output rather
than title-specific career names.

For the all-student outcome, use:

```text
program_income_clp_m1
log_program_income_clp_m1
program_income_source_m1
```

for first enrollment, with the analogous `_ml` variables for last enrollment.
The longer `mifuturo_income_hier_or_minwage_*` variables are retained as
traceability aliases, but `program_income` is the working outcome name.
