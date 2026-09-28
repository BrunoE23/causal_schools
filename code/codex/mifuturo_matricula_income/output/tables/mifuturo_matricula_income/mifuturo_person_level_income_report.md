# MiFuturo Person-Level Income Outcomes

Generated: 2026-09-21 18:04:24

## Rule

Canonical working outcome names: `program_income_area`, `program_income_institution`, and `program_income_full`.

- `program_income_area`: matriculated students use the one-way `AREA_CARRERA_GENERICA` FE prediction; non-estimable matriculated cases use the global MiFuturo mean with an explicit source flag.
- `program_income_institution`: matriculated students use the one-way institution FE prediction; non-estimable matriculated cases use the global MiFuturo mean with an explicit source flag.
- `program_income_full`: matriculated students use the selected hierarchy: two-way institution + `AREA_CARRERA_GENERICA` FE, then area FE, then institution FE, then global MiFuturo mean.
- Students with no observed matriculation receive the configured non-matriculation floor.
- The non-matriculation floor is not used for matriculated students.
- Backward-compatible `program_income` columns are retained as aliases to `program_income_full` for existing VA/Stata scripts.
- `high_paying_field_m1`: non-matriculated students are coded 0; matriculated students are coded 1 for Science, Law, Engineering/Manufacturing/Construction, or Medicine+ (`Medicina`, `Quimica y Farmacia`, `Enfermeria`, `Obstetricia y Puericultura`, `Tecnologia Medica`, `Odontologia`). Every other observed enrollment is coded 0, including programs outside the broader retained field taxonomy.
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
                                        income_source n_students
                                               <char>      <int>
 1:         matriculated_fe_institution_area_generica     440838
 2:               not_matriculated_minimum_wage_floor     569835
 3:                    matriculated_missing_fe_income     187309
 4:                     matriculated_area_generica_fe     587579
 5:               not_matriculated_minimum_wage_floor     569835
 6:   matriculated_area_generica_global_mifuturo_mean      40568
 7:                       matriculated_institution_fe     474332
 8:               not_matriculated_minimum_wage_floor     569835
 9:     matriculated_institution_global_mifuturo_mean     153815
10: matriculated_two_way_fe_institution_area_generica     440838
11:               not_matriculated_minimum_wage_floor     569835
12:            matriculated_fallback_area_generica_fe     146741
13:              matriculated_fallback_institution_fe      33482
14:        matriculated_fallback_global_mifuturo_mean       7086
15:         matriculated_fe_institution_area_generica     437351
16:               not_matriculated_minimum_wage_floor     569835
17:                    matriculated_missing_fe_income     190796
18:                     matriculated_area_generica_fe     590644
19:               not_matriculated_minimum_wage_floor     569835
20:   matriculated_area_generica_global_mifuturo_mean      37503
21:                       matriculated_institution_fe     467597
22:               not_matriculated_minimum_wage_floor     569835
23:     matriculated_institution_global_mifuturo_mean     160550
24: matriculated_two_way_fe_institution_area_generica     437351
25:               not_matriculated_minimum_wage_floor     569835
26:            matriculated_fallback_area_generica_fe     153293
27:              matriculated_fallback_institution_fe      30229
28:        matriculated_fallback_global_mifuturo_mean       7274
                                        income_source n_students
    n_matriculated n_not_matriculated
             <int>              <int>
 1:         440838                  0
 2:              0             569835
 3:         187309                  0
 4:         587579                  0
 5:              0             569835
 6:          40568                  0
 7:         474332                  0
 8:              0             569835
 9:         153815                  0
10:         440838                  0
11:              0             569835
12:         146741                  0
13:          33482                  0
14:           7086                  0
15:         437351                  0
16:              0             569835
17:         190796                  0
18:         590644                  0
19:              0             569835
20:          37503                  0
21:         467597                  0
22:              0             569835
23:         160550                  0
24:         437351                  0
25:              0             569835
26:         153293                  0
27:          30229                  0
28:           7274                  0
    n_matriculated n_not_matriculated
    n_strict_two_way_fe_matriculated n_model_estimable_matriculated
                               <int>                          <int>
 1:                           440838                         440838
 2:                                0                              0
 3:                                0                              0
 4:                           440838                         587579
 5:                                0                              0
 6:                                0                              0
 7:                           440838                         474332
 8:                                0                              0
 9:                                0                              0
10:                           440838                         440838
11:                                0                              0
12:                                0                         146741
13:                                0                          33482
14:                                0                              0
15:                           437351                         437351
16:                                0                              0
17:                                0                              0
18:                           437351                         590644
19:                                0                              0
20:                                0                              0
21:                           437351                         467597
22:                                0                              0
23:                                0                              0
24:                           437351                         437351
25:                                0                              0
26:                                0                         153293
27:                                0                          30229
28:                                0                              0
    n_strict_two_way_fe_matriculated n_model_estimable_matriculated
    n_missing_after_rule share_missing_after_rule mean_income_clp
                   <int>                    <num>           <num>
 1:                    0                        0         1564579
 2:                    0                        0          553553
 3:               187309                        1              NA
 4:                    0                        0         1482725
 5:                    0                        0          553553
 6:                    0                        0         1443833
 7:                    0                        0         1509437
 8:                    0                        0          553553
 9:                    0                        0         1443833
10:                    0                        0         1564579
11:                    0                        0          553553
12:                    0                        0         1251067
13:                    0                        0         1648055
14:                    0                        0         1443833
15:                    0                        0         1552980
16:                    0                        0          553553
17:               190796                        1              NA
18:                    0                        0         1471585
19:                    0                        0          553553
20:                    0                        0         1443833
21:                    0                        0         1501799
22:                    0                        0          553553
23:                    0                        0         1443833
24:                    0                        0         1552980
25:                    0                        0          553553
26:                    0                        0         1253499
27:                    0                        0         1641379
28:                    0                        0         1443833
    n_missing_after_rule share_missing_after_rule mean_income_clp
    median_income_clp
                <num>
 1:           1426070
 2:            553553
 3:                NA
 4:           1381582
 5:            553553
 6:           1443833
 7:           1595375
 8:            553553
 9:           1443833
10:           1426070
11:            553553
12:           1252026
13:           1643073
14:           1443833
15:           1393322
16:            553553
17:                NA
18:           1350470
19:            553553
20:           1443833
21:           1567847
22:            553553
23:           1443833
24:           1393322
25:            553553
26:           1252026
27:           1643073
28:           1443833
    median_income_clp

## Enrolled Program FE Coverage

   enrollment_measure n_enrolled_students n_unique_cod_sies
               <char>               <int>             <int>
1:   first_enrollment              628147              9325
2:    last_enrollment              628147              9320
   n_unique_inst_area n_program_info_found n_fe_level_supported
                <int>                <int>                <int>
1:               2867               628147               440850
2:               2879               628147               437368
   n_fe_estimable share_fe_estimable n_area_level_supported
            <int>              <num>                  <int>
1:         440838          0.7018071                 587579
2:         437351          0.6962558                 590644
   n_area_estimable share_area_estimable
              <int>                <num>
1:           587579            0.9354164
2:           590644            0.9402958
   n_institution_level_supported n_institution_estimable
                           <int>                   <int>
1:                        474332                  474332
2:                        467597                  467597
   share_institution_estimable n_hier_two_way_fe
                         <num>             <int>
1:                    0.755129            440838
2:                    0.744407            437351
   n_hier_area_generica_fe n_hier_institution_fe n_hier_global_mean
                     <int>                 <int>              <int>
1:                  146741                 33482               7086
2:                  153293                 30229               7274
   n_hier_complete share_hier_complete mean_fe_income_clp
             <int>               <num>              <num>
1:          628147                   1            1564579
2:          628147                   1            1552980
   median_fe_income_clp mean_area_income_clp median_area_income_clp
                  <num>                <num>                  <num>
1:              1426070              1482725                1381582
2:              1393322              1471585                1350470
   mean_institution_income_clp median_institution_income_clp
                         <num>                         <num>
1:                     1509437                       1595375
2:                     1501799                       1567847
   mean_hier_income_clp median_hier_income_clp
                  <num>                  <num>
1:              1494427                1383139
2:              1482885                1368177

## High-Paying Field Coverage

                   high_paying_field_source_m1 n_students
                                        <char>      <int>
1:                       not_matriculated_zero     569835
2:        matriculated_not_in_high_paying_list     341399
3:     matriculated_high_paying_existing_field     226628
4: matriculated_high_paying_medicine_plus_area      60120
   n_high_paying_field n_non_high_paying_field
                 <int>                   <int>
1:                   0                  569835
2:                   0                  341399
3:              226628                       0
4:               60120                       0
   n_missing_high_paying_field share_high_paying_field
                         <int>                   <num>
1:                           0                       0
2:                           0                       0
3:                           0                       1
4:                           0                       1

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

- `C:/Users/brunem/Box/causal_schools/data/clean/mifuturo_person_level_income_outcomes.csv`
- `C:/Users/brunem/Box/causal_schools/data/clean/mifuturo_person_level_income_outcomes_stata_va.csv`
- `C:/Users/brunem/Research/causal_schools/code/codex/mifuturo_matricula_income/output/tables/mifuturo_matricula_income/mifuturo_person_level_income_source_summary.csv`
- `C:/Users/brunem/Research/causal_schools/code/codex/mifuturo_matricula_income/output/tables/mifuturo_matricula_income/mifuturo_enrolled_program_income_summary.csv`
- `C:/Users/brunem/Research/causal_schools/code/codex/mifuturo_matricula_income/output/tables/mifuturo_matricula_income/mifuturo_enrolled_income_unsupported_programs.csv`
- `C:/Users/brunem/Research/causal_schools/code/codex/mifuturo_matricula_income/output/tables/mifuturo_matricula_income/mifuturo_high_paying_field_source_summary.csv`
- `C:/Users/brunem/Research/causal_schools/code/codex/mifuturo_matricula_income/output/tables/mifuturo_matricula_income/mifuturo_high_institution_source_summary.csv`
