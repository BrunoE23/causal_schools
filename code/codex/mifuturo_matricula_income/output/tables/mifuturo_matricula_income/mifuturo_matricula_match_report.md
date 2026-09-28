# MiFuturo Matricula Income Merge Diagnostic

Generated: 2026-09-21 17:45:21

This is a diagnostic merge only. It does not modify the official matricula or universe files.

## Inputs

- MiFuturo income CSV: `C:/Users/brunem/Dropbox/causal_schools/data/raw/mifuturo/Buscador_Empleabilidad_ingresos_2025_2026_SIES(Carreras e IES (2025-2026)).csv`
- First ingreso matricula: `C:/Users/brunem/Dropbox/causal_schools/data/clean/mat_ingresos_22-25/mat_1st_ing.csv`
- Last ingreso matricula: `C:/Users/brunem/Dropbox/causal_schools/data/clean/mat_ingresos_22-25/mat_last_ing.csv`
- Program info supplement: `C:/Users/brunem/Dropbox/causal_schools/data/clean/program_info_22-25.rds`

## Code Map

- Matricula `CODIGO_UNICO`, renamed `COD_SIES`, is the documented program code considering institution, sede, carrera, jornada, and version.
- Matricula `COD_CARRERA` is the career/program code.
- Matricula `CODIGO_DEMRE` is available only for institutions in the SUA admissions system.
- Matricula `COD_INST` is the higher-education institution code.
- MiFuturo income file does not expose `COD_SIES`; its `Código` behaves as an institution code and is used only together with institution type/name and career names.

## Match Keys Tested

- `inst_type_generic_title`: `COD_INST + TIPO_INST_1 + NOMB_INST + AREA_CARRERA_GENERICA + NOMB_CARRERA`
- `inst_type_generic`: `COD_INST + TIPO_INST_1 + NOMB_INST + AREA_CARRERA_GENERICA`

## Overall Summary

   enrollment_measure          match_key_name student_rows rows_matched_observed_income
               <char>                  <char>        <int>                        <int>
1:      first_ingreso       inst_type_generic      1266733                       531103
2:      first_ingreso inst_type_generic_title      1266733                       379511
3:       last_ingreso       inst_type_generic      1266733                       528939
4:       last_ingreso inst_type_generic_title      1266733                       375371
   rows_matched_missing_income rows_no_mifuturo_key rows_missing_key_component
                         <int>                <int>                      <int>
1:                      100832               634798                          0
2:                       79047               808175                          0
3:                       98468               639326                          0
4:                       76943               814419                          0
   rows_mifuturo_duplicate_key share_rows_observed_income share_rows_any_mifuturo_match
                         <int>                      <num>                         <num>
1:                           0                  0.4192699                     0.4988699
2:                           0                  0.2995983                     0.3620005
3:                           0                  0.4175616                     0.4952954
4:                           0                  0.2963300                     0.3570713

## Outputs

- `C:/Users/brunem/Research/causal_schools/code/codex/mifuturo_matricula_income/output/tables/mifuturo_matricula_income/mifuturo_matricula_match_summary.csv`
- `C:/Users/brunem/Research/causal_schools/code/codex/mifuturo_matricula_income/output/tables/mifuturo_matricula_income/mifuturo_matricula_unmatched_programs.csv`
- `C:/Users/brunem/Research/causal_schools/code/codex/mifuturo_matricula_income/output/tables/mifuturo_matricula_income/mifuturo_matricula_key_multiplicity.csv`
- `C:/Users/brunem/Research/causal_schools/code/codex/mifuturo_matricula_income/output/tables/mifuturo_matricula_income/mifuturo_matricula_candidate_program_income.csv`
- `C:/Users/brunem/Research/causal_schools/code/codex/mifuturo_matricula_income/output/tables/mifuturo_matricula_income/mifuturo_matricula_code_map.csv`

## Important Caution

MiFuturo income is not available at the full `COD_SIES` level in the file inspected here.
Even a successful match can map multiple `COD_SIES` values, such as sede/jornada/version variants, to one MiFuturo income row.
Use `mifuturo_matricula_key_multiplicity.csv` before promoting any candidate income variable into the official pipeline.
