# MiFuturo Person-Level Income Outcomes

Generated: 2026-07-10 20:44:06

## Rule

Canonical working outcome names: `program_income_area`, `program_income_institution`, and `program_income_full`.

- `program_income_area`: matriculated students use the one-way `AREA_CARRERA_GENERICA` FE prediction; non-estimable matriculated cases use the global MiFuturo mean with an explicit source flag.
- `program_income_institution`: matriculated students use the one-way institution FE prediction; non-estimable matriculated cases use the global MiFuturo mean with an explicit source flag.
- `program_income_full`: matriculated students use the selected hierarchy: two-way institution + `AREA_CARRERA_GENERICA` FE, then area FE, then institution FE, then global MiFuturo mean.
- Students with no observed matriculation receive the configured non-matriculation floor.
- The non-matriculation floor is not used for matriculated students.
- Backward-compatible `program_income` columns are retained as aliases to `program_income_full` for existing VA/Stata scripts.
- `high_paying_field_m1`: non-matriculated students are coded 0; matriculated students are coded 1 for Science, Law, Engineering/Manufacturing/Construction, or Medicine+ (`Medicina`, `Quimica y Farmacia`, `Enfermeria`, `Obstetricia y Puericultura`, `Tecnologia Medica`, `Odontologia`). Matriculated students with insufficient field classification remain missing rather than being silently coded 0.
- `high_inst_m1`: non-matriculated students are coded 0; matriculated students are coded 1 when their institution's centered MiFuturo institution FE is above 0.1 log points, and 0 otherwise, including when the matriculated institution lacks an institution FE.

## Non-Matriculation Floor

- CLP: 553553
- Label: `current_minimum_wage_proxy_553553`
- Note: Raw 553,553 CLP proxy for non-matriculated students, using the current minimum-wage value selected as the conservative floor.

## Person-Level Source Summary

    enrollment_measure              outcome_variant
                <char>                       <char>
 1:   first_enrollment strict_two_way_fe_or_minwage
 2:   first_enrollment strict_two_way_fe_or_minwage
 3:   first_enrollment strict_two_way_fe_or_minwage
 4:   first_enrollment          program_income_area
 5:   first_enrollment          program_income_area
 6:   first_enrollment          program_income_area
 7:   first_enrollment   program_income_institution
 8:   first_enrollment   program_income_institution
 9:   first_enrollment   program_income_institution
10:   first_enrollment          program_income_full
11:   first_enrollment          program_income_full
12:   first_enrollment          program_income_full
13:   first_enrollment          program_income_full
14:   first_enrollment          program_income_full
15:    last_enrollment strict_two_way_fe_or_minwage
16:    last_enrollment strict_two_way_fe_or_minwage
17:    last_enrollment strict_two_way_fe_or_minwage
18:    last_enrollment          program_income_area
19:    last_enrollment          program_income_area
20:    last_enrollment          program_income_area
21:    last_enrollment   program_income_institution
22:    last_enrollment   program_income_institution
23:    last_enrollment   program_income_institution
24:    last_enrollment          program_income_full
25:    last_enrollment          program_income_full
26:    last_enrollment          program_income_full
27:    last_enrollment          program_income_full
28:    last_enrollment          program_income_full
    enrollment_measure              outcome_variant
                                        income_source n_students n_matriculated
                                               <char>      <int>          <int>
 1:         matriculated_fe_institution_area_generica     440838         440838
 2:               not_matriculated_minimum_wage_floor     569835              0
 3:                    matriculated_missing_fe_income     187309         187309
 4:                     matriculated_area_generica_fe     587579         587579
 5:               not_matriculated_minimum_wage_floor     569835              0
 6:   matriculated_area_generica_global_mifuturo_mean      40568          40568
 7:                       matriculated_institution_fe     474332         474332
 8:               not_matriculated_minimum_wage_floor     569835              0
 9:     matriculated_institution_global_mifuturo_mean     153815         153815
10: matriculated_two_way_fe_institution_area_generica     440838         440838
11:               not_matriculated_minimum_wage_floor     569835              0
12:            matriculated_fallback_area_generica_fe     146741         146741
13:              matriculated_fallback_institution_fe      33482          33482
14:        matriculated_fallback_global_mifuturo_mean       7086           7086
15:         matriculated_fe_institution_area_generica     437351         437351
16:               not_matriculated_minimum_wage_floor     569835              0
17:                    matriculated_missing_fe_income     190796         190796
18:                     matriculated_area_generica_fe     590644         590644
19:               not_matriculated_minimum_wage_floor     569835              0
20:   matriculated_area_generica_global_mifuturo_mean      37503          37503
21:                       matriculated_institution_fe     467597         467597
22:               not_matriculated_minimum_wage_floor     569835              0
23:     matriculated_institution_global_mifuturo_mean     160550         160550
24: matriculated_two_way_fe_institution_area_generica     437351         437351
25:               not_matriculated_minimum_wage_floor     569835              0
26:            matriculated_fallback_area_generica_fe     153293         153293
27:              matriculated_fallback_institution_fe      30229          30229
28:        matriculated_fallback_global_mifuturo_mean       7274           7274
                                        income_source n_students n_matriculated
    n_not_matriculated n_strict_two_way_fe_matriculated
                 <int>                            <int>
 1:                  0                           440838
 2:             569835                                0
 3:                  0                                0
 4:                  0                           440838
 5:             569835                                0
 6:                  0                                0
 7:                  0                           440838
 8:             569835                                0
 9:                  0                                0
10:                  0                           440838
11:             569835                                0
12:                  0                                0
13:                  0                                0
14:                  0                                0
15:                  0                           437351
16:             569835                                0
17:                  0                                0
18:                  0                           437351
19:             569835                                0
20:                  0                                0
21:                  0                           437351
22:             569835                                0
23:                  0                                0
24:                  0                           437351
25:             569835                                0
26:                  0                                0
27:                  0                                0
28:                  0                                0
    n_not_matriculated n_strict_two_way_fe_matriculated
    n_model_estimable_matriculated n_missing_after_rule
                             <int>                <int>
 1:                         440838                    0
 2:                              0                    0
 3:                              0               187309
 4:                         587579                    0
 5:                              0                    0
 6:                              0                    0
 7:                         474332                    0
 8:                              0                    0
 9:                              0                    0
10:                         440838                    0
11:                              0                    0
12:                         146741                    0
13:                          33482                    0
14:                              0                    0
15:                         437351                    0
16:                              0                    0
17:                              0               190796
18:                         590644                    0
19:                              0                    0
20:                              0                    0
21:                         467597                    0
22:                              0                    0
23:                              0                    0
24:                         437351                    0
25:                              0                    0
26:                         153293                    0
27:                          30229                    0
28:                              0                    0
    n_model_estimable_matriculated n_missing_after_rule
    share_missing_after_rule mean_income_clp median_income_clp
                       <num>           <num>             <num>
 1:                        0         1564579           1426070
 2:                        0          553553            553553
 3:                        1              NA                NA
 4:                        0         1482725           1381582
 5:                        0          553553            553553
 6:                        0         1443833           1443833
 7:                        0         1509437           1595375
 8:                        0          553553            553553
 9:                        0         1443833           1443833
10:                        0         1564579           1426070
11:                        0          553553            553553
12:                        0         1251067           1252026
13:                        0         1648055           1643073
14:                        0         1443833           1443833
15:                        0         1552980           1393322
16:                        0          553553            553553
17:                        1              NA                NA
18:                        0         1471585           1350470
19:                        0          553553            553553
20:                        0         1443833           1443833
21:                        0         1501799           1567847
22:                        0          553553            553553
23:                        0         1443833           1443833
24:                        0         1552980           1393322
25:                        0          553553            553553
26:                        0         1253499           1252026
27:                        0         1641379           1643073
28:                        0         1443833           1443833
    share_missing_after_rule mean_income_clp median_income_clp

## Enrolled Program FE Coverage

   enrollment_measure n_enrolled_students n_unique_cod_sies n_unique_inst_area
               <char>               <int>             <int>              <int>
1:   first_enrollment              628147              9325               2867
2:    last_enrollment              628147              9320               2879
   n_program_info_found n_fe_level_supported n_fe_estimable share_fe_estimable
                  <int>                <int>          <int>              <num>
1:               628147               440850         440838          0.7018071
2:               628147               437368         437351          0.6962558
   n_area_level_supported n_area_estimable share_area_estimable
                    <int>            <int>                <num>
1:                 587579           587579            0.9354164
2:                 590644           590644            0.9402958
   n_institution_level_supported n_institution_estimable
                           <int>                   <int>
1:                        474332                  474332
2:                        467597                  467597
   share_institution_estimable n_hier_two_way_fe n_hier_area_generica_fe
                         <num>             <int>                   <int>
1:                    0.755129            440838                  146741
2:                    0.744407            437351                  153293
   n_hier_institution_fe n_hier_global_mean n_hier_complete share_hier_complete
                   <int>              <int>           <int>               <num>
1:                 33482               7086          628147                   1
2:                 30229               7274          628147                   1
   mean_fe_income_clp median_fe_income_clp mean_area_income_clp
                <num>                <num>                <num>
1:            1564579              1426070              1482725
2:            1552980              1393322              1471585
   median_area_income_clp mean_institution_income_clp
                    <num>                       <num>
1:                1381582                     1509437
2:                1350470                     1501799
   median_institution_income_clp mean_hier_income_clp median_hier_income_clp
                           <num>                <num>                  <num>
1:                       1595375              1494427                1383139
2:                       1567847              1482885                1368177

## High-Paying Field Coverage

                   high_paying_field_source_m1 n_students n_high_paying_field
                                        <char>      <int>               <int>
1:                       not_matriculated_zero     569835                   0
2:     matriculated_classified_non_high_paying     289544                   0
3:     matriculated_high_paying_existing_field     226628              226628
4: matriculated_high_paying_medicine_plus_area      60120               60120
5:   matriculated_missing_field_classification      51855                   0
   n_non_high_paying_field n_missing_high_paying_field share_high_paying_field
                     <int>                       <int>                   <num>
1:                  569835                           0                       0
2:                  289544                           0                       0
3:                       0                           0                       1
4:                       0                           0                       1
5:                       0                       51855                     NaN

## High-Institution Coverage

                           high_inst_source_m1 n_students n_high_inst
                                        <char>      <int>       <int>
1:                       not_matriculated_zero     569835           0
2: matriculated_institution_fe_at_or_below_0p1     354758           0
3:         matriculated_missing_institution_fe     153815           0
4:       matriculated_institution_fe_above_0p1     119574      119574
   n_non_high_inst n_missing_high_inst share_high_inst
             <int>               <int>           <num>
1:          569835                   0               0
2:          354758                   0               0
3:          153815                   0               0
4:               0                   0               1

## Outputs

- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_person_level_income_outcomes.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_person_level_income_outcomes_stata_va.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_person_level_income_source_summary.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_enrolled_program_income_summary.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_enrolled_income_unsupported_programs.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_high_paying_field_source_summary.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_high_institution_source_summary.csv`
