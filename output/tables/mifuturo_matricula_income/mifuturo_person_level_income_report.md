# MiFuturo Person-Level Income Outcomes

Generated: 2026-07-07 22:34:32

## Rule

Working outcome name: `program_income`.

- Matriculated students use a MiFuturo model prediction.
- The preferred tier is the selected two-way FE prediction from `log(income) ~ institution + AREA_CARRERA_GENERICA`.
- Matriculated programs outside two-way FE support are filled by explicit fallback tiers: `AREA_CARRERA_GENERICA` FE, institution FE, then global MiFuturo mean.
- Students with no observed matriculation receive the configured non-matriculation floor.
- The non-matriculation floor is not used for matriculated students.

## Non-Matriculation Floor

- CLP: 350000
- Label: `raw_minimum_wage_proxy_2020_2023`
- Note: Raw 350,000 CLP proxy for non-matriculated students, chosen to align with likely 2020-2023 wage years underlying MiFuturo income measures.

## Person-Level Source Summary

                                        income_source enrollment_measure
                                               <char>             <char>
 1: matriculated_two_way_fe_institution_area_generica   first_enrollment
 2:               not_matriculated_minimum_wage_floor   first_enrollment
 3:            matriculated_fallback_area_generica_fe   first_enrollment
 4:              matriculated_fallback_institution_fe   first_enrollment
 5:        matriculated_fallback_global_mifuturo_mean   first_enrollment
 6: matriculated_two_way_fe_institution_area_generica    last_enrollment
 7:               not_matriculated_minimum_wage_floor    last_enrollment
 8:            matriculated_fallback_area_generica_fe    last_enrollment
 9:              matriculated_fallback_institution_fe    last_enrollment
10:        matriculated_fallback_global_mifuturo_mean    last_enrollment
    n_students n_matriculated n_not_matriculated
         <int>          <int>              <int>
 1:     432512         432512                  0
 2:     317948              0             317948
 3:     142206         142206                  0
 4:      40822          40822                  0
 5:      10872          10872                  0
 6:     428015         428015                  0
 7:     317948              0             317948
 8:     147898         147898                  0
 9:      38593          38593                  0
10:      11906          11906                  0
    n_strict_two_way_fe_matriculated n_missing_after_rule
                               <int>                <int>
 1:                           432512                    0
 2:                                0                    0
 3:                                0                    0
 4:                                0                    0
 5:                                0                    0
 6:                           428015                    0
 7:                                0                    0
 8:                                0                    0
 9:                                0                    0
10:                                0                    0
    share_missing_after_rule mean_income_clp median_income_clp
                       <num>           <num>             <num>
 1:                        0         1559631           1419392
 2:                        0          350000            350000
 3:                        0         1251565           1252026
 4:                        0         1636501           1643073
 5:                        0         1443833           1443833
 6:                        0         1547362           1386185
 7:                        0          350000            350000
 8:                        0         1254349           1252026
 9:                        0         1627001           1643073
10:                        0         1443833           1443833

## Enrolled Program FE Coverage

   enrollment_measure n_enrolled_students n_unique_cod_sies n_unique_inst_area
               <char>               <int>             <int>              <int>
1:   first_enrollment              626412              9325               2843
2:    last_enrollment              626412              9320               2847
   n_program_info_found n_fe_level_supported n_fe_estimable share_fe_estimable
                  <int>                <int>          <int>              <num>
1:               613844               432524         432512          0.6904593
2:               611665               428025         428015          0.6832803
   n_hier_two_way_fe n_hier_area_generica_fe n_hier_institution_fe
               <int>                   <int>                 <int>
1:            432512                  142206                 40822
2:            428015                  147898                 38593
   n_hier_global_mean n_hier_complete share_hier_complete mean_fe_income_clp
                <int>           <int>               <num>              <num>
1:              10872          626412                   1            1559631
2:              11906          626412                   1            1547362
   median_fe_income_clp mean_hier_income_clp median_hier_income_clp
                  <num>                <num>                  <num>
1:              1419392              1492694                1386394
2:              1386185              1481119                1374706

## Outputs

- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_person_level_income_outcomes.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_person_level_income_source_summary.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_enrolled_program_income_summary.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_enrolled_income_unsupported_programs.csv`
