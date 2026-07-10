# MiFuturo Person-Level Income Outcomes

Generated: 2026-07-10 15:42:31

## Rule

Canonical working outcome names: `program_income_area`, `program_income_institution`, and `program_income_full`.

- `program_income_area`: matriculated students use the one-way `AREA_CARRERA_GENERICA` FE prediction; non-estimable matriculated cases use the global MiFuturo mean with an explicit source flag.
- `program_income_institution`: matriculated students use the one-way institution FE prediction; non-estimable matriculated cases use the global MiFuturo mean with an explicit source flag.
- `program_income_full`: matriculated students use the selected hierarchy: two-way institution + `AREA_CARRERA_GENERICA` FE, then area FE, then institution FE, then global MiFuturo mean.
- Students with no observed matriculation receive the configured non-matriculation floor.
- The non-matriculation floor is not used for matriculated students.
- Backward-compatible `program_income` columns are retained as aliases to `program_income_full` for existing VA/Stata scripts.
- `high_paying_field_m1`: non-matriculated students are coded 0; matriculated students are coded 1 for Science, Law, Engineering/Manufacturing/Construction, or Medicine+ (`Medicina`, `Quimica y Farmacia`, `Enfermeria`, `Obstetricia y Puericultura`, `Tecnologia Medica`, `Odontologia`). Matriculated students with insufficient field classification remain missing rather than being silently coded 0.

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
 1:         matriculated_fe_institution_area_generica     439892         439892
 2:               not_matriculated_minimum_wage_floor     317948              0
 3:                    matriculated_missing_fe_income     186520         186520
 4:                     matriculated_area_generica_fe     585910         585910
 5:               not_matriculated_minimum_wage_floor     317948              0
 6:   matriculated_area_generica_global_mifuturo_mean      40502          40502
 7:                       matriculated_institution_fe     473346         473346
 8:               not_matriculated_minimum_wage_floor     317948              0
 9:     matriculated_institution_global_mifuturo_mean     153066         153066
10: matriculated_two_way_fe_institution_area_generica     439892         439892
11:               not_matriculated_minimum_wage_floor     317948              0
12:            matriculated_fallback_area_generica_fe     146018         146018
13:              matriculated_fallback_institution_fe      33442          33442
14:        matriculated_fallback_global_mifuturo_mean       7060           7060
15:         matriculated_fe_institution_area_generica     436414         436414
16:               not_matriculated_minimum_wage_floor     317948              0
17:                    matriculated_missing_fe_income     189998         189998
18:                     matriculated_area_generica_fe     588976         588976
19:               not_matriculated_minimum_wage_floor     317948              0
20:   matriculated_area_generica_global_mifuturo_mean      37436          37436
21:                       matriculated_institution_fe     466618         466618
22:               not_matriculated_minimum_wage_floor     317948              0
23:     matriculated_institution_global_mifuturo_mean     159794         159794
24: matriculated_two_way_fe_institution_area_generica     436414         436414
25:               not_matriculated_minimum_wage_floor     317948              0
26:            matriculated_fallback_area_generica_fe     152562         152562
27:              matriculated_fallback_institution_fe      30187          30187
28:        matriculated_fallback_global_mifuturo_mean       7249           7249
                                        income_source n_students n_matriculated
    n_not_matriculated n_strict_two_way_fe_matriculated
                 <int>                            <int>
 1:                  0                           439892
 2:             317948                                0
 3:                  0                                0
 4:                  0                           439892
 5:             317948                                0
 6:                  0                                0
 7:                  0                           439892
 8:             317948                                0
 9:                  0                                0
10:                  0                           439892
11:             317948                                0
12:                  0                                0
13:                  0                                0
14:                  0                                0
15:                  0                           436414
16:             317948                                0
17:                  0                                0
18:                  0                           436414
19:             317948                                0
20:                  0                                0
21:                  0                           436414
22:             317948                                0
23:                  0                                0
24:                  0                           436414
25:             317948                                0
26:                  0                                0
27:                  0                                0
28:                  0                                0
    n_not_matriculated n_strict_two_way_fe_matriculated
    n_model_estimable_matriculated n_missing_after_rule
                             <int>                <int>
 1:                         439892                    0
 2:                              0                    0
 3:                              0               186520
 4:                         585910                    0
 5:                              0                    0
 6:                              0                    0
 7:                         473346                    0
 8:                              0                    0
 9:                              0                    0
10:                         439892                    0
11:                              0                    0
12:                         146018                    0
13:                          33442                    0
14:                              0                    0
15:                         436414                    0
16:                              0                    0
17:                              0               189998
18:                         588976                    0
19:                              0                    0
20:                              0                    0
21:                         466618                    0
22:                              0                    0
23:                              0                    0
24:                         436414                    0
25:                              0                    0
26:                         152562                    0
27:                          30187                    0
28:                              0                    0
    n_model_estimable_matriculated n_missing_after_rule
    share_missing_after_rule mean_income_clp median_income_clp
                       <num>           <num>             <num>
 1:                        0         1565469           1426070
 2:                        0          553553            553553
 3:                        1              NA                NA
 4:                        0         1483632           1381582
 5:                        0          553553            553553
 6:                        0         1443833           1443833
 7:                        0         1510051           1595375
 8:                        0          553553            553553
 9:                        0         1443833           1443833
10:                        0         1565469           1426070
11:                        0          553553            553553
12:                        0         1251407           1252026
13:                        0         1648396           1643073
14:                        0         1443833           1443833
15:                        0         1553844           1395422
16:                        0          553553            553553
17:                        1              NA                NA
18:                        0         1472463           1350470
19:                        0          553553            553553
20:                        0         1443833           1443833
21:                        0         1502400           1567847
22:                        0          553553            553553
23:                        0         1443833           1443833
24:                        0         1553844           1395422
25:                        0          553553            553553
26:                        0         1253872           1252026
27:                        0         1641774           1643073
28:                        0         1443833           1443833
    share_missing_after_rule mean_income_clp median_income_clp

## Enrolled Program FE Coverage

   enrollment_measure n_enrolled_students n_unique_cod_sies n_unique_inst_area
               <char>               <int>             <int>              <int>
1:   first_enrollment              626412              9325               2867
2:    last_enrollment              626412              9320               2879
   n_program_info_found n_fe_level_supported n_fe_estimable share_fe_estimable
                  <int>                <int>          <int>              <num>
1:               626412               439904         439892          0.7022407
2:               626412               436431         436414          0.6966884
   n_area_level_supported n_area_estimable share_area_estimable
                    <int>            <int>                <num>
1:                 585910           585910            0.9353429
2:                 588976           588976            0.9402374
   n_institution_level_supported n_institution_estimable
                           <int>                   <int>
1:                        473346                  473346
2:                        466618                  466618
   share_institution_estimable n_hier_two_way_fe n_hier_area_generica_fe
                         <num>             <int>                   <int>
1:                   0.7556464            439892                  146018
2:                   0.7449059            436414                  152562
   n_hier_institution_fe n_hier_global_mean n_hier_complete share_hier_complete
                   <int>              <int>           <int>               <num>
1:                 33442               7060          626412                   1
2:                 30187               7249          626412                   1
   mean_fe_income_clp median_fe_income_clp mean_area_income_clp
                <num>                <num>                <num>
1:            1565469              1426070              1483632
2:            1553844              1395422              1472463
   median_area_income_clp mean_institution_income_clp
                    <num>                       <num>
1:                1381582                     1510051
2:                1350470                     1502400
   median_institution_income_clp mean_hier_income_clp median_hier_income_clp
                           <num>                <num>                  <num>
1:                       1595375              1495317                1383139
2:                       1567847              1483751                1368177

## High-Paying Field Coverage

                   high_paying_field_source_m1 n_students n_high_paying_field
                                        <char>      <int>               <int>
1:                       not_matriculated_zero     317948                   0
2:     matriculated_classified_non_high_paying     288771                   0
3:     matriculated_high_paying_existing_field     225945              225945
4: matriculated_high_paying_medicine_plus_area      60095               60095
5:   matriculated_missing_field_classification      51601                   0
   n_non_high_paying_field n_missing_high_paying_field share_high_paying_field
                     <int>                       <int>                   <num>
1:                  317948                           0                       0
2:                  288771                           0                       0
3:                       0                           0                       1
4:                       0                           0                       1
5:                       0                       51601                     NaN

## Outputs

- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_person_level_income_outcomes.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_person_level_income_outcomes_stata_va.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_person_level_income_source_summary.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_enrolled_program_income_summary.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_enrolled_income_unsupported_programs.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_high_paying_field_source_summary.csv`
