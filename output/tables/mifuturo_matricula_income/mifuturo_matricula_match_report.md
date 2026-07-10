# MiFuturo Matricula Income Merge Diagnostic

Generated: 2026-06-30 12:12:37

This is a diagnostic merge only. It does not modify the official matricula or universe files.

## Inputs

- MiFuturo income CSV: `C:/Users/brunem/Dropbox/causal_schools/data/raw/mifuturo/Buscador_Empleabilidad_ingresos_2025_2026_SIES(Carreras e IES (2025-2026)).csv`
- First ingreso matricula: `C:/Users/brunem/Dropbox/causal_schools/data/clean/mat_ingresos_22-24/mat_1st_ing.csv`
- Last ingreso matricula: `C:/Users/brunem/Dropbox/causal_schools/data/clean/mat_ingresos_22-24/mat_last_ing.csv`
- Program info supplement: `C:/Users/brunem/Dropbox/causal_schools/data/clean/program_info_22-24.rds`

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

   enrollment_measure          match_key_name student_rows
               <char>                  <char>        <int>
1:      first_ingreso       inst_type_generic      1266733
2:      first_ingreso inst_type_generic_title      1266733
3:       last_ingreso       inst_type_generic      1266733
4:       last_ingreso inst_type_generic_title      1266733
   rows_matched_observed_income rows_matched_missing_income
                          <int>                       <int>
1:                       524844                       99414
2:                       374942                       77932
3:                       521742                       96873
4:                       370149                       75706
   rows_no_mifuturo_key rows_missing_key_component rows_mifuturo_duplicate_key
                  <int>                      <int>                       <int>
1:               618245                      24230                           0
2:               789629                      24230                           0
3:               619441                      28677                           0
4:               792201                      28677                           0
   share_rows_observed_income share_rows_any_mifuturo_match
                        <num>                         <num>
1:                  0.4143288                     0.4928095
2:                  0.2959913                     0.3575134
3:                  0.4118800                     0.4883547
4:                  0.2922076                     0.3519724

## Outputs

- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_matricula_match_summary.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_matricula_unmatched_programs.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_matricula_key_multiplicity.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_matricula_candidate_program_income.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_matricula_code_map.csv`

## Important Caution

MiFuturo income is not available at the full `COD_SIES` level in the file inspected here.
Even a successful match can map multiple `COD_SIES` values, such as sede/jornada/version variants, to one MiFuturo income row.
Use `mifuturo_matricula_key_multiplicity.csv` before promoting any candidate income variable into the official pipeline.
