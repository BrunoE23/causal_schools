# MiFuturo Generic-Career Log-Income Regression Diagnostic

Generated: 2026-06-30 16:36:25

This revisits the earlier imputation idea in `code/processing_college_apps_outcome.R`:

`log(income) ~ institution + carrera`

The chosen carrera variable is `AREA_CARRERA_GENERICA`. The script treats this as a candidate construction, not an official clean-data modification.

## Model

- `institution_plus_carrera_generica`: institution FE plus MiFuturo generic career FE.

Institution is coded as `Codigo + Tipo de institucion + Nombre de institucion`.
The outcome is the log midpoint of MiFuturo's 4th-year monthly income range in CLP.
Predicted CLP levels use Duan smearing from the fitted log model.
`NOMB_CARRERA_TITULO` is retained only to audit within-generic-career variation; it is not used as a fixed effect in the chosen model.

## In-Sample Model Summary

                          model_name                     outcome n_total_rows
                              <char>                      <char>        <int>
1: institution_plus_carrera_generica log_income_4th_year_mid_clp         1690
   n_observed_income n_missing_income n_institutions_observed
               <int>            <int>                   <int>
1:              1135              555                      92
   n_carreras_observed n_missing_level_supported n_missing_nonestimable
                 <int>                     <int>                  <int>
1:                 156                       454                      5
   n_missing_predicted n_missing_unsupported share_missing_predicted
                 <int>                 <int>                   <num>
1:                 449                   101                0.809009
   r_squared_in_sample adj_r_squared_in_sample rmse_log_in_sample
                 <num>                   <num>              <num>
1:           0.9511396               0.9377442         0.08043757
   mae_log_in_sample rmse_clp_from_log_in_sample mae_clp_from_log_in_sample
               <num>                       <num>                      <num>
1:        0.05890116                    158262.7                   95473.54
   smear_factor n_model_coefficients n_model_rank
          <num>                <int>        <int>
1:     1.003235                  247          245

## Five-Fold Cross-Validation Summary

                          model_name folds mean_share_test_predicted
                              <char> <int>                     <num>
1: institution_plus_carrera_generica     5                 0.9462555
   mean_rmse_log mean_mae_log mean_error_log mean_rmse_clp_from_log
           <num>        <num>          <num>                  <num>
1:     0.1024501   0.07662567   0.0005352689               195405.6
   mean_mae_clp_from_log min_share_test_predicted
                   <num>                    <num>
1:              121750.5                0.9251101

## Carrera Generica / Title Variation Examples

                                  AREA_CARRERA_GENERICA n_mifuturo_rows
                                                 <char>           <int>
 1:          Técnico Asistente del Educador de Párvulos              31
 2:         Administración de Empresas e Ing. Asociadas              30
 3:               Técnico en Administración de Empresas              37
 4: Técnico en Deporte, Recreación y Preparación Física              12
 5:   Técnico en Electricidad y Electricidad Industrial              13
 6:                                    Contador Auditor              33
 7:                               Pedagogía en Ciencias              10
 8:              Pedagogía en Matemáticas y Computación              12
 9:                                Técnico Agropecuario              11
10:                                Técnico en Logística              12
11:                      Técnico en Turismo y Hotelería              11
12:                 Administración Turística y Hotelera              10
    n_titles n_institutions observed_income_rows missing_income_rows
       <int>          <int>                <int>               <int>
 1:       18             31                   22                   9
 2:       14             30                   21                   9
 3:       12             37                   24                  13
 4:       12             12                    7                   5
 5:       12             13                   11                   2
 6:       11             33                   25                   8
 7:       10             10                    5                   5
 8:       10             12                    7                   5
 9:       10             11                    6                   5
10:       10             12                    7                   5
11:       10             11                    6                   5
12:        9             10                    4                   6
    income_min_clp income_median_clp income_max_clp income_sd_clp
             <num>             <num>          <num>         <num>
 1:         650000            650000         750000      42893.20
 2:        1050000           1350000        1750000     193833.51
 3:         850000           1050000        1250000     102150.78
 4:         750000            850000        1050000     106904.50
 5:         950000           1250000        1850000     231595.26
 6:        1050000           1350000        1750000     177951.30
 7:        1150000           1250000        1350000      83666.00
 8:        1150000           1350000        1350000      95118.97
 9:         850000            950000        1050000      75277.27
10:         950000           1150000        1250000      95118.97
11:         750000            850000         850000      51639.78
12:         850000            950000        1050000      81649.66
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    example_titles
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <char>
 1:                                                                                                                                          Asistente de Párvulo | Asistente en Educación de Párvulos | Técnico de Nivel Superior Asistente de Educación de Párvulos | Técnico de Nivel Superior en Asistente de Párvulos | Técnico de Nivel Superior en Educación de Párvulos | Técnico de Nivel Superior en Educación Parvularia | Técnico de Nivel Superior en Educación Parvularia y primer-segundo año de Educación Básica | Técnico de Párvulos y Básica | Técnico en Educación de Párvulos | Técnico en Educación de Pärvulos | Técnico en Educación Parvularia | Técnico en Educación Parvularia y Básica
 2: Administración de Servicios | Ingeniería de Ejecución en Administración | Ingeniería de Ejecución en Administración de Empresas | Ingeniería de Ejecución en Administración de Negocios, de Empresas y de Agronegocios | Ingeniería de Ejecución en Administración e Ingeniería de Administración de Empresas | Ingeniería de Ejedución en Administración de Empresas | Ingeniería en Administración | Ingeniería en Administración de Empresas | Ingeniería en Administración de Empresas Financieras | Ingeniería en Administración de Negocios y Gestión Comercial | Ingeniería en Administración e Ingeniería en Administración de Empresas | Ingeniería en Administración y en Administración de Empresas
 3:      Técnico de Nivel Superior Administración de Empresas, Marketing y Gestión Comercial | Técnico de Nivel Superior en Administración de Empresas | Técnico de Nivel Superior en Administración de Empresas y Administración en Salud | Técnico de Nivel Superior en Administración General y de Empresas | Técnico en Administración | Técnico en Administración de Empresas | Técnico en Administración de Empresas Financieras | Técnico en Administración y Administración de Empresas | Técnico Superior en Administración de Empresas | Técnico Universitario en Administración | Técnico Universitario en Administración de Empresas | Técnico Universitario y Tecnólogo en Administración de Empresas
 4:                                                                                                                                                                                     Personal Trainer | Preparación Física y Técnico en Deportes | Preparador Físico | Preparador Físico y Técnico en Deportes | Técnico de Fútbol y Árbitro de Futbol | Técnico de Nivel Superior en Actividad Física y del Deporte | Técnico de Nivel Superior en Actividad Física y Deportes | Técnico de Nivel Superior en Deportes y Recreación | Técnico Deportivo Universitario | Técnico Deportivo y Personal Trainer | Técnico Universitario en Actividad Física y Salud | Técnico Universitario en Preparación Física
 5:                                                                            Técnico de Nivel Superior en Electricidad | Técnico de Nivel Superior en Proyectos Eléctricos de Distribución | Técnico en Electricidad | Técnico en Electricidad Industrial | Técnico en Electricidad y Automatización Industrial, en Instalaciones y Proyectos Eléctricos | Técnico en Electricidad y Electrónica | Técnico en Electricidad y Electrónica Industrial | Técnico en Electricidad, Eficiencia Energética y Electrónica Industrial | Técnico en Redes Eléctricas | Técnico Universitario en Electricidad | Técnico Universitario en Electricidad y Automatización | Técnico Universitario y Tecnólogo en Electricidad
 6:                                                                                                                                                                                                                                                                       Auditoría | Auditoría e Ingeniería en Control de Gestión, y Contador Público y Auditor | Auditoría y Contador Auditor | Contabilidad y Auditoría, Contador Auditor | Contador Auditor | Contador Auditor - Contador Público | Contador Auditor/Contador Público | Contador Público Auditor | Contador Público Auditor, y Auditoría e Ingeniería en Control de Gestión | Contador Público y Auditor | Contador Público y Contador Auditor
 7:                                                                                                                                                                        Pedagogía en Biología, Física y Química | Pedagogía en Biología, Física, Química y Ciencias | Pedagogía en Biología, Física, Química y Ciencias Naturales | Pedagogía en Biología, Química y Ciencias Naturales | Pedagogía en Ciencias | Pedagogía en Ciencias mención Biología, Física, Química | Pedagogía en Ciencias Naturales | Pedagogía en Educación Media en Biología y Química | Pedagogía en Educación Media en Ciencias Naturales y Biología, Física y Química | Pedagogía en Química y Biología, y en Física y Matemáticas
 8:                                                                                                                                                                                                                                                        Pedagogía Educación Media en Matemática y Física | Pedagogía en Educación Matemática | Pedagogía en Educación Media en Matemática | Pedagogía en Educación Mediaen Matemática | Pedagogía en Matemática | Pedagogía en Matemática y Computación | Pedagogía en Matemática y Computación y en Matemáticas y Educación Tecnológica | Pedagogía en Matemáticas | Pedagogía en Matemáticas e Informática Educativa | Pedagogía en Matemáticas y Computación
 9:                                                                                                                                                                                                                                                                                   Técnico Agrícola | Técnico Agrícola y Ganadero | Técnico de Nivel Superior Agrícola | Técnico de Nivel Superior en Agrícola | Técnico de Nivel Superior en Agronomía | Técnico de Nivel Superior en Gestión Agropecuaria | Técnico en Administración de la Producción Agropecuaria y Agroindustrial | Técnico Experto Agrario | Técnico Universitario en Producción Agropecuaria | Tecnología Agrícola y Producción Ganadera
10:                                                                                                                                                                                                                                                                                          Técnico de Nivel Superior en Control de Gestión y Logística | Técnico de Nivel Superior en Logística | Técnico en Administración Logística | Técnico en Gestión Logística | Técnico en Logística | Técnico en Logística Operativa | Técnico en Logística y en Logística Marítima Portuaria | Técnico en Logística y Operaciones Industriales | Técnico en Operaciones Logísticas | Técnico Universitario en Logística
11:                                                                                                                                     Técnico de Nivel Superior en Turismo y Hotelería | Técnico en Ecoturismo | Técnico en Hotelería y en Turismo Sustentable | Técnico en Hotelería y Turismo | Técnico en Hotelería y Turismo, en Turismo Aventura, y en Servicios Aerocomerciales y Transportes Turísticos | Técnico en Turismo | Técnico en Turismo y en Administración Hotelera y Resorts Internacional | Técnico en Turismo y en Hotelería y Servicios | Técnico Guía de Turismo Aventura y en Turismo | Turismo Técnico, Turismo de Aventura, Administración Hotelera y Tourism & Hospitality Technician
12:                                                                                                                                                                                 Administración de Empresas de Turismo | Administración en Hotelería y Servicios y Administración Turística Internacional | Administración Hotelera y Gastronómica, y Gestión en Turismo y Cultura | Administración Turística Multilingüe | Hotelería y Turismo | Ingeniería en Expediciones y Ecoturismo | Ingeniería en Gestión Turística | Ingeniería en Turismo y Hotelería, Ingeniería en Administración Hotelera Internacional y Administración en Ecoturismo | Turismo y Hotelería, Ecoturismo y Tourism and Hospitality
                                                                                                                                                                                                                                                                                                 example_institutions
                                                                                                                                                                                                                                                                                                               <char>
 1:                                                                                       CFT CEDUC - UCN | CFT CENCO | CFT DE ENAC | CFT DE LA REGION DE ARICA Y PARINACOTA | CFT EDUCAP | CFT INSTITUTO CENTRAL DE CAPACITACION EDUCACIONAL ICCE | CFT INSTITUTO SUPERIOR DE ESTUDIOS JURIDICOS CANON | CFT LAPLACE
 2:                                                                                                                                            IP AIEP | IP DE CHILE | IP DEL VALLE CENTRAL | IP DR. VIRGINIO GOMEZ G. | IP DUOC UC | IP IACC | IP INACAP | IP INSTITUTO DE ESTUDIOS BANCARIOS GUILLERMO SUBERCASEAUX
 3:                                                                                                                                                    CFT CEDUC - UCN | CFT DE ENAC | CFT DE LA REGION DE ARICA Y PARINACOTA | CFT DE LA REGION DE TARAPACA | CFT INACAP | CFT LOTA-ARAUCO | CFT MANPOWER | CFT PUCV
 4:                                                                                                      CFT DE LA REGION DE ARICA Y PARINACOTA | CFT SANTO TOMAS | IP AIEP | IP DE CHILE | IP DUOC UC | IP INSTITUTO NACIONAL DEL FUTBOL | UNIVERSIDAD ARTURO PRAT | UNIVERSIDAD CATOLICA DE LA SANTISIMA CONCEPCION
 5:                                                                                                                                                             CFT CEDUC - UCN | CFT DE LA REGION DE ARICA Y PARINACOTA | CFT INACAP | CFT PUCV | CFT SAN AGUSTIN | IP AIEP | IP DE CHILE | IP DR. VIRGINIO GOMEZ G.
 6:                                                                                  IP AIEP | IP DE CHILE | IP DR. VIRGINIO GOMEZ G. | IP DUOC UC | IP ESCUELA DE COMERCIO DE SANTIAGO | IP ESCUELA DE CONTADORES AUDITORES DE SANTIAGO | IP INSTITUTO DE ESTUDIOS BANCARIOS GUILLERMO SUBERCASEAUX | IP SANTO TOMAS
 7:                     PONTIFICIA UNIVERSIDAD CATOLICA DE CHILE | PONTIFICIA UNIVERSIDAD CATOLICA DE VALPARAISO | UNIVERSIDAD CATOLICA DEL MAULE | UNIVERSIDAD DE CHILE | UNIVERSIDAD DE CONCEPCION | UNIVERSIDAD DE LA FRONTERA | UNIVERSIDAD DE LA SERENA | UNIVERSIDAD DE PLAYA ANCHA DE CIENCIAS DE LA EDUCACION
 8: PONTIFICIA UNIVERSIDAD CATOLICA DE CHILE | PONTIFICIA UNIVERSIDAD CATOLICA DE VALPARAISO | UNIVERSIDAD ALBERTO HURTADO | UNIVERSIDAD CATOLICA CARDENAL RAUL SILVA HENRIQUEZ | UNIVERSIDAD CATOLICA DE LA SANTISIMA CONCEPCION | UNIVERSIDAD CATOLICA DEL MAULE | UNIVERSIDAD DE CHILE | UNIVERSIDAD DE CONCEPCION
 9:                                                                                                                                              CFT DE LA REGION DE ARICA Y PARINACOTA | CFT INACAP | CFT PUCV | CFT SAN AGUSTIN | CFT SANTO TOMAS | CFT TEODORO WICKEL KLUWEN | IP AGRARIO ADOLFO MATTHEI | IP AIEP
10:                                                                                                                                                   CFT DE ENAC | CFT DE LA REGION DE ARICA Y PARINACOTA | CFT ESCUELA DE COMERCIO | CFT INACAP | CFT SANTO TOMAS | IP AIEP | IP DR. VIRGINIO GOMEZ G. | IP DUOC UC
11:                                                                                                                                                                                               CFT DE ENAC | CFT DEL MEDIO AMBIENTE | CFT INACAP | CFT PUCV | CFT SANTO TOMAS | IP AIEP | IP DE CHILE | IP DUOC UC
12:                                                                                                 IP DE CHILE | IP DUOC UC | IP INACAP | UNIVERSIDAD ANDRES BELLO | UNIVERSIDAD AUSTRAL DE CHILE | UNIVERSIDAD DE LAS AMERICAS | UNIVERSIDAD DE PLAYA ANCHA DE CIENCIAS DE LA EDUCACION | UNIVERSIDAD DE VALPARAISO

## Outputs

- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_income_fe_model_summary.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_income_fe_cv_summary.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_income_fe_predictions.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_income_fe_unsupported_missing.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_income_fe_institution_effects.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_income_fe_carrera_effects.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_carrera_generica_title_variation_examples.csv`
- `C:/Users/brunem/Research/causal_schools/output/tables/mifuturo_matricula_income/mifuturo_income_fe_model_artifact.rds`

## Cautions

- `AREA_CARRERA_GENERICA` is the preferred career dimension for this imputation because it has much better support than title-specific career names with similar cross-validated log-income error.
- This model imputes missing MiFuturo income rows from observed MiFuturo rows; it does not solve the separate `COD_SIES` merge problem.
- A row is predicted only if its institution and carrera levels are observed in the income-training sample and the corresponding two-way FE prediction is estimable.
- The old object name `income_imp` was in USD in the earlier script; this diagnostic writes explicit CLP and USD columns.
