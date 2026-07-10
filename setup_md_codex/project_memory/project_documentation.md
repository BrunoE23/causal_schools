# Project Documentation

This file records substantive, project-specific knowledge needed to understand and continue the causal schools research project. It should document data construction, sample definitions, outcome definitions, empirical design choices, interpretation conventions, and links to decision logs.

It should not include Codex workflow rules, collaboration preferences, or general assistant behavior instructions. Those belong in `setup_md_codex/WORKFLOW.md` or other agent-facing setup files.

Empirical regression specifications are tracked separately in `empirical_methods.md`.

## Research Implementation Standard

No se toman atajos in research implementation.

When a task asks for an estimand, standard error, regression object, sample definition, or identification object, implement that object directly.
Do not silently replace it with a proxy, shortcut, or heuristic because the exact object is inconvenient to extract.

If the exact object is not exposed by the current package or workflow, derive the equivalent object carefully or ask before proceeding.
If a proxy is intentionally used for exploratory work or robustness, label it explicitly in code, output column names, and documentation.

For empirical standard errors, the default is the SE implied by the actual regression/specification.
Residual-spread-over-sqrt-n style quantities are diagnostics or explicitly documented approximations, not substitutes for regression-derived SEs.

## `br_code`

`br_code` is a course-level school identifier, not just a school identifier. In practice, it behaves like the combination of school and specific track/course/process information, so multiple `br_code`s can sit inside the same `rbd`.

Operationally:
- `rbd` is the school the student applied to.
- `br_code` is the more granular school-course-process unit inside that school.
- In `sample_students`, students are stacked by application unit, so the same student can appear multiple times for the same `rbd` if that school has multiple relevant `br_code`s.

This matters for construction work. For the school-level first-stage exercise, first-round offers were merged at the `br_code` level and then collapsed back to one `mrun`-`proceso`-`rbd` row.

## Timing Convention in Tracking Databases

In the current tracking databases, relative time is defined as:

`year_rel_sae_change = AGNO - sae_proceso - 1`

So if a student applies in SAE process `2017`, they are assigned for the `2018` school year. Under this convention:

- `2018` is `t_0`
- `2019` is `t_1`
- `2020` is `t_2`
- `2021` is `t_3`

This means:

- "1 year after SAE" corresponds to `t_0`
- "4 years after SAE" corresponds to `t_3`

This convention may change later, but for now it is the convention used in the tracking databases.

## RBD Metadata Handling

`RBD` should generally be treated as the school identifier, but metadata attached to an `RBD` is not always unique or stable in raw files.

Important distinction:
- The fact that `GRUPO_DEPENDENCIA`, `CODIGO_REGION`, `CODIGO_COMUNA`, or similar metadata changes does not by itself prove that `RBD` is unstable.
- It means the metadata associated with an `RBD` may vary within or across years because of administrative changes, coding differences, data errors, or source-file inconsistencies.

When constructing school-year summaries, do not collapse outcomes by `RBD` and then join back `distinct(RBD, metadata...)`. If there is more than one metadata row per `RBD`, this can duplicate already-collapsed school-year rows.

Preferred pattern:
- Summarize outcomes and metadata in the same `group_by(RBD, year)` or equivalent school-year aggregation.
- For metadata fields, use an explicit rule such as first non-missing value, modal value, or a separately cleaned school directory.
- Write diagnostics that flag metadata conflicts rather than treating them as evidence that `RBD` itself is unusable.

See `decisions/2026-04-24-rbd-metadata-handling.md` for the rationale behind the current task-level rule.

## School RBD Observational Values

The current school-level observational-value workflow lives in:

- `code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R`
- `code/codex/school_rbd_observational_values/02_visualize_school_rbd_values.R`

The input file for the constructor is:

- `data/clean/univ_gr8_df.csv`

The main school definition is:

- `school_rbd = most_time_RBD`

This is currently the default school object for the observational-value exercise. If the project later changes to a different school definition, that should be treated as a substantive change in estimand rather than a small coding detail.

Current sample-definition note:

- `def_sample_frame` defines the main universe of students as all students observed in the Chilean school system in grade 8 between 2017 and 2020 with `COD_ENSE == 110`
- each student's grade-8 cohort year, `cohort_gr8`, is defined as the first year in which the student is observed in grade 8
- `def_school` assigns school links by looking at where each student is observed after grade 8
- the resulting school links include an RBD one year after grade 8, an RBD four years after grade 8, and the RBD where the student spends the most time
- `universe_reg_df.R` brings the broad student universe together with SAE indicators, baseline SIMCE controls, post-grade-8 school links, PSU outcomes, STEM outcomes, and higher-education matricula files
- `universe_reg_df.R` writes the merged broad-universe analysis files `data/clean/univ_gr8_df.csv` and `data/clean/univ_gr8_df.dta`
- `sae_binary_prep.RData` contains the student-level SAE grade-9 participation/process marker used in `universe_reg_df.R`
- the object loaded from `sae_binary_prep.RData` is `sae_apps_grade9`, with variables `mrun` and `sae_proceso`
- `sae_proceso` indicates the SAE process in which the student appears for grade-9 admission; if a student appears in multiple processes, the construction keeps the first observed `sae_proceso`
- first-round school assignment/offer information is stored in `data/clean/treatment_1R_v2.RData`, object `all_treatments`
- for current IV planning, the key first-round offer variables are `mrun` and `rbd_treated_1R`
- `rbd_treated_1R` is the RBD offered/admitted in the first round for the application row, and equals `0` when that application row did not generate a first-round offer
- equivalently, `rbd_treated_1R != 0` is the first-round offer indicator, and the nonzero value is the offered RBD
- `treatment_1R_v2.RData` is at the `mrun`-`br_code` level, so estimation code needs an explicit rule to create the student-level first-round offered RBD or offer instrument
- the replicated SAE assignment algorithm has been checked against the original lottery numbers for the 2018-2021 regular grade-9 processes and reproduces more than 99% of observed first-round assignments in each of the four cohorts
- this universe is the broad student universe used to construct observational school values
- the current observational-value constructor additionally restricts to `12 <= EDAD_ALU <= 16`
- this age restriction is currently imposed inside the VA constructor, not upstream in the general universe build

## SIMCE Grade-8 Heterogeneity Inputs

The current grade-8 SIMCE heterogeneity input is limited to the 2019 8th-grade private SIMCE files:

- `data/raw/simce/Simce octavo básico 2019 - Versión privada/Archivos TXT (Planos)/simce8b2019_alu_privada_final-SEG.csv`
- `data/raw/simce/Simce octavo básico 2019 - Versión privada/Archivos TXT (Planos)/simce8b2019_cpad_final_SEG.csv`

The cleaning script is:

- `code/codex/simce8_heterogeneity/01_clean_simce8_2019.R`

The main score variables are:

- `ptje_mate8b_alu`: SIMCE math score
- `ptje_lect8b_alu`: SIMCE reading/verbal score

The parent-survey household-income variable is:

- `cpad_p11`

The 2019 glosses define `cpad_p11` as the same 15-bin household-income question used in the 4th-grade SIMCE cleaner. Codes `0` and `99` are treated as missing. The script maps valid codes `1` through `15` to the same income midpoints used by `clean_simce_survey.R`, then constructs `income_decile_8b`.

The script also constructs:

- `z_sim_mat_8b`
- `z_sim_leng_8b`
- `simce8_math_decile`
- `simce8_math_quintile`
- `simce8_leng_decile`
- `simce8_score_avg_decile`

For a first diagnostic on heterogeneity-bin quality, the script compares grade-4 SIMCE score deciles and grade-4 CPAD income deciles from `simce_4to.Rdata` against the corresponding grade-8 SIMCE and CPAD deciles in 2019.

For the first heterogeneous-effects pass, the intended simple grade-8-achievement split is math-only. The reusable cohort-level file is:

- `data/clean/simce8_heterogeneity/cohort_2019_math_heterogeneity.csv`

It keeps one row per student in the 2019 grade-8 cohort and includes:

- `simce8_math_decile`
- `simce8_math_quintile`
- `simce8_vs_4to_math_decile_change`
- `simce8_math_improved_gt1_decile`
- `simce8_math_within1_decile`
- `simce8_math_worsened_gt1_decile`
- `simce8_math_decile_movement`

The movement categories compare grade-8 math decile to grade-4 math decile: improved by more than one decile, remained within one decile, or worsened by more than one decile. Students in the 2019 cohort without matched grade-8 or grade-4 SIMCE math deciles are retained in the cohort-level file with missing movement fields.

The broad regression dataset construction in `code/universe_reg_df.R` now merges these grade-8 SIMCE math heterogeneity variables by `mrun` when the cohort-level file exists. For an already-built `univ_gr8_df`, `code/codex/simce8_heterogeneity/02_append_to_univ_gr8_df.R` appends the same columns to `data/clean/univ_gr8_df.csv` and `data/clean/univ_gr8_df.dta`. The compact scalar school-value IV construction scripts also carry these fields through to their regression-ready exports.

## Current Scalar-IV Risk-Control Convention

For current main scalar-IV estimates and heterogeneous-effects tables, use the expected-VA risk-control workflow:

- `code/codex/scalar_school_value_iv/08_run_expected_va_scalar_iv.R`
- `data/clean/scalar_school_value_iv/scalar_school_value_iv_expected_va.csv`

This workflow uses one scalar expected-value risk control per school-value metric:

`expected_VA_i = sum_s p_is * V_s`

For example, estimates using `d_math_adj` and `z_math_adj` should control for `expected_math_adj`.

For full-sample grade-4 SIMCE achievement heterogeneity, use `code/codex/scalar_school_value_iv/12_run_expected_va_grade4_quintile_all_sample.R`. It constructs grade-4 math quintiles directly from `z_sim_mat_4to` in `data/clean/scalar_school_value_iv/scalar_school_value_iv_expected_va.csv` and reports adjusted Math, Language, and STEM theta/SE columns.

The older wide probability-control workflow, based on `prob_<rbd>` and `iszero_<rbd>` columns from `01_build_scalar_school_value_iv_df.R` and `02_run_scalar_school_value_iv.do`, is now a legacy/robustness path. It should not be used for new heterogeneity tables unless the task explicitly asks for a wide-probability-control robustness check.

### Main Construction Logic

The observational-value task constructs school-level measures for both score outcomes and higher-education enrollment or field outcomes.

For each school-outcome pair, the constructor currently produces:

- a raw school value
- an adjusted school value based on individual-level regressions with school fixed effects

The main adjusted object is intentionally student-level and does not use school-level regressions on demographic shares.

### Outcomes Currently Built

Score outcomes are built from PSU or PAES-style score variables in `univ_gr8_df.csv`.

The raw score variables currently considered are:

- `math_max`
- `leng_max`
- `leng_math_total`
- `hist_max`
- `scien_max`
- `math2_max`

Because score scales differ across years, the constructor also creates two transformed versions of each score outcome:

- `scale1000_*`: older 850-scale years are multiplied by `1000 / 850`
- `z_year_*`: scores are standardized within `psu_year`

For pooled score comparisons across years, the current preferred object is the `z_year_*` family, especially `z_year_math_max`.

Higher-education outcomes are built from:

- `field_reclassified_m1`
- `field_reclassified_ml`
- existing binary field indicators such as `f_science_m1`, `f_eng_m1`, and related `*_ml` versions
- higher-education matricula accreditation fields from `clean_matriculated_first_time.R`, including `ACREDITADA_CARR_m1`, `ACRE_INST_ANIO_m1`, and `program_certified_years_m1` plus the matching `*_ml` variables

The current main enrollment outcome for STEM-style summaries is:

- `stem_enrollment_m1`

`program_certified_years` is defined as the interaction between program accreditation and institution accreditation years:

`program_certified_years = 1[ACREDITADA_CARR == "ACREDITADA"] * ACRE_INST_ANIO`

For non-accredited programs the value is zero; if the program is accredited but institution accreditation years are missing, the interaction remains missing.

The corresponding institution-years outcome is `inst_certified_years_m1`, defined as:

`inst_certified_years_m1 = 1[ACREDITADA_INST == "ACREDITADA"] * ACRE_INST_ANIO`

For current school-value construction, these accreditation-years outcomes are treated as unconditional first-enrollment outcomes within the admission-exam-taker sample: students without observed higher-ed enrollment are coded as zero, while observed matricula rows with genuinely missing accreditation-years inputs remain missing.

The current paper-facing VA and IV workflow restricts to students with an observed math or language national admission-test score before estimating higher-education outcome value added or validating it in the SAE lottery sample.

During the current estimation-development pass, controlled VA is only estimated for:

- `z_year_math_max`
- `z_year_leng_max`
- `z_year_leng_math_total`
- `stem_enrollment_m1`
- `log_program_income_clp_m1`
- `program_certified_years_m1`
- `inst_certified_years_m1`

As of May 31, 2026, the default school-value construction is All-sample only. Male/Female sample-specific value-added and gender-gap value-added are on hold unless explicitly requested.

When gender-gap VA is explicitly requested, the previous configured outcomes were:

- `gender_gap__z_year_math_max`
- `gender_gap__stem_enrollment_m1`

Raw school means may still be computed for the broader configured outcome set.

### Raw School Values

For a generic outcome `Y`, the raw school value is:

- the simple mean of `Y` among students assigned to that `school_rbd`

This is stored as:

- `raw_mean`

The constructor also creates centered versions:

- `raw_mean_centered`: subtract the national school-level mean of `raw_mean`
- `raw_mean_centered_student`: subtract the national student-weighted mean of `raw_mean`

These are different objects. The first is appropriate when each school should count equally. The second is appropriate when larger schools should count more.

### Adjusted School Values

The adjusted school value is estimated from an individual-level fixed-effects regression of the form:

`outcome ~ controls | school_rbd`

The current control set is:

- `factor(cohort_gr8)`
- `factor(GEN_ALU)`
- `factor(EDAD_ALU)`
- `factor(COD_COM_ALU)`
- `z_sim_mat_4to`
- `I(z_sim_mat_4to^2)`
- `I(z_sim_mat_4to^3)`
- `z_sim_leng_4to`
- `I(z_sim_leng_4to^2)`
- `I(z_sim_leng_4to^3)`
- `income_decile_imputed`
- imputed CPAD parent education, parent indigenous-status, and early-childhood attendance controls
- `z_gpa_middle_mean`
- `z_att_middle_mean`
- `factor(middle_years_observed)`

The controlled regular school-value regressions also absorb:

- `school_rbd`
- `most_time_RBD_middle`

The regular school-value output now includes an `analysis_sample` column. Values are estimated separately for:

- `All`
- `Male`
- `Female`

For the sex-specific regressions, `factor(GEN_ALU)` is removed from the control set because gender is constant within the estimation sample. The existing gender-gap outcomes remain separate objects under `outcome_family == "gender_gap"`.

Interpretation of the controls:

- `cohort_gr8` adjusts for cohort differences
- `GEN_ALU` adjusts for gender differences
- `EDAD_ALU` adjusts flexibly for grade-8 age
- `COD_COM_ALU` is the student's comuna, not the school's comuna
- `z_sim_mat_4to` and `z_sim_leng_4to` adjust for prior achievement through third-order polynomials

Income-control rule:

- `income_decile` comes from the 4th-grade SIMCE parent survey income question
- `simce_rbd_4to` is retained from the 4th-grade SIMCE student file
- `universe_reg_df.R` constructs `income_mid_imputed`, preserving observed `income_mid` where available and imputing missing income only for students with both baseline SIMCE score controls
- the imputation uses median observed `income_mid` with a minimum of `15` donor students per cell
- the hierarchy is `simce_rbd_4to` x `COD_COM_ALU` x `simce_year`, then `simce_rbd_4to` x `COD_COM_ALU`, then `simce_rbd_4to` x `simce_year`, then `simce_rbd_4to`
- remaining missing values are left as missing rather than filled with a broad year or national median
- `income_decile_imputed` is then recomputed from the observed-plus-imputed `income_mid_imputed` distribution among students with both baseline SIMCE score controls
- diagnostic variables include `income_mid_observed`, `income_mid_impute_source`, `income_mid_impute_level`, `income_mid_impute_n`, `income_mid_was_imputed`, `income_decile_was_imputed`, `income_decile_imputation_min_n`, `income_mid_missing_after_impute`, and `income_decile_missing_after_impute`

Additional CPAD baseline controls:

- `clean_simce_survey.R` now extracts parent education, parent indigenous-status indicators, and early-childhood attendance indicators from the 4th-grade SIMCE parent survey
- household size is not used in the current VA control set because it is only available for 2015-2016 in the current 4th-grade CPAD files
- father and mother education are converted from questionnaire categories into approximate years of education as `father_educ_years` and `mother_educ_years`
- `universe_reg_df.R` imputes the CPAD controls with the same `n >= 15` median hierarchy used for income: `simce_rbd_4to` x `COD_COM_ALU` x `simce_year`, then `simce_rbd_4to` x `COD_COM_ALU`, then `simce_rbd_4to` x `simce_year`, then `simce_rbd_4to`
- imputed CPAD controls are named with the `_imputed` suffix and have corresponding `_observed`, `_missing`, `_impute_source`, `_impute_n`, `_was_imputed`, and `_missing_after_impute` diagnostics
- each imputed CPAD control also has an `_impute_level` diagnostic, where `0` means observed, `1` means `simce_rbd_4to` x `COD_COM_ALU` x `simce_year`, `2` means `simce_rbd_4to` x `COD_COM_ALU`, `3` means `simce_rbd_4to` x `simce_year`, `4` means `simce_rbd_4to`, and missing means still missing or not in the baseline-SIMCE imputation population
- binary CPAD controls are imputed by median, so tied donor cells can produce `0.5`; these values are intentionally kept as fractional contextual controls for now

The controlled VA specification includes `income_decile_imputed`, imputed parent education years, imputed parent indigenous indicators, imputed early-childhood attendance indicators, middle-school GPA/attendance summaries, and absorbed middle-school RBD fixed effects as additional controls.

Middle-school controls:

- `code/codex/middle_school_controls/01_construct_middle_school_controls.R` constructs student-level middle-school controls for the grade-8 universe
- the output file is `data/clean/middle_school_controls/middle_school_controls.csv`, with diagnostics in `data/clean/middle_school_controls/middle_school_controls_diagnostics.csv`
- middle-school records are observed grade 5-8 enrollment rows from three years before through the student's first observed grade-8 cohort year
- the construction uses `tracking_univ8gr.RData` for 2017-2020 and reads raw 2014-2016 enrollment files to complete the historical grade 5-8 window for the full grade-8 universe
- `most_time_RBD_middle` is the RBD where the student spent the most observed middle-school years, breaking ties by latest observed year and then RBD
- `z_gpa_middle_mean` and `z_att_middle_mean` average annual GPA and attendance z-scores across observed middle-school records
- for 2017-2020 records, the z-scores reuse the school-grade GPA and attendance means and standard deviations already stored in `tracking_univ8gr.RData`; for 2014-2016 records, the script computes the same school-grade moments from the full raw year file before filtering to the grade-8 universe
- duplicate student-year enrollment records are resolved by preferring nonzero attendance, then passed records, then highest attendance, with any remaining exact ties broken deterministically by RBD

The school fixed effect from this regression is stored as:

- `controlled_value_added`

This fixed effect is not automatically on the same level scale as the raw mean, so the constructor also stores centered or shifted versions:

- `controlled_value_added_centered`: subtract the national school-level mean of the school fixed effect
- `controlled_value_added_centered_student`: subtract the national student-weighted mean of the school fixed effect
- `controlled_adjusted_mean`: raw national outcome mean plus `controlled_value_added_centered`
- `controlled_adjusted_mean_student`: regression-sample outcome mean plus `controlled_value_added_centered_student`

The centered fixed-effect versions are useful for school-to-school comparison. The adjusted-mean versions are more interpretable when raw and adjusted values need to be read on the same outcome scale.

For EB shrinkage, the constructor also exports regression-derived uncertainty for the student-weighted centered school effect:

- `controlled_value_added_se`
- `controlled_value_added_resid_sd`
- `controlled_value_added_se_method`

The default SE method uses `lfe::felm()` and `lfe::getfe(se = TRUE)` with a custom estimable function for `school FE - student-weighted mean(school FE)`.
This is intentionally the SE of `controlled_value_added_centered_student`, not the SE of a raw reference-normalized fixed-effect level.
The current method label is `lfe_getfe_school_rbd_centered_student_iid_bN100`.

### Sample Size Flags

The constructor uses a low-count threshold of `20`.

It currently records:

- `n_students_school`
- `n_students_outcome`
- `n_students_regression`
- `n_students_regression_total`
- `low_outcome_count`
- `missing_controlled_value`

This allows downstream work to separate "true missing" from "estimated with a thin sample."

## Higher-Ed Program Codes and MiFuturo Income

The official higher-ed matricula dictionary defines `CODIGO_UNICO` as the unique career/program code considering institution, sede, career/program, jornada, and version.
Project code usually renames this variable to `COD_SIES`.

Other relevant higher-ed program codes:

- `COD_CARRERA`: career/program code
- `CODIGO_DEMRE`: DEMRE program code, available only for institutions attached to the centralized admissions system
- `COD_INST`: higher-ed institution code

For higher-ed applications, `COD_CARRERA_PREF` is the application/preference program code.
`code/facts_major_choice_prep.R` maps `COD_CARRERA_PREF` to `COD_SIES` using 2024/2025 oferta definitiva files.

The inspected MiFuturo income file does not expose `COD_SIES`.
Its `Código` behaves as an institution code and should not be treated as a program identifier.
MiFuturo income must therefore be audited through institution and career descriptors, not as a direct `COD_SIES` merge.

The first diagnostic workflow for this lives in:

- `code/codex/mifuturo_matricula_income/01_diagnose_mifuturo_matricula_merge.R`
- `code/codex/mifuturo_matricula_income/02_regress_mifuturo_income_on_institution_carrera.R`

It tests joins from matriculated programs to MiFuturo using:

- `COD_INST + TIPO_INST_1 + NOMB_INST + AREA_CARRERA_GENERICA + NOMB_CARRERA`
- `COD_INST + TIPO_INST_1 + NOMB_INST + AREA_CARRERA_GENERICA`

Use `AREA_CARRERA_GENERICA` as the main career dimension for MiFuturo income imputation and program matching diagnostics.
The title/career-name field is too sparse for the main model and should be used only to audit within-generic-career variation and key multiplicity.

The diagnostic writes coverage, unmatched-program, and key-multiplicity reports to `output/tables/mifuturo_matricula_income/`.
Because MiFuturo is not at the full `COD_SIES` level, multiple sede/jornada/version-specific `COD_SIES` values can map to one MiFuturo income row.
Inspect multiplicity before promoting any MiFuturo income variable into the official person-level matricula pipeline.

The second diagnostic revisits the older `log(income) ~ institution + carrera` imputation attempt from `code/processing_college_apps_outcome.R`.
It estimates the chosen additive fixed-effect model, `log(income) ~ institution + AREA_CARRERA_GENERICA`, for observed MiFuturo income rows and predicts missing MiFuturo income rows only when the institution-carrera prediction is in support and estimable.
This imputation addresses missing income within MiFuturo; it does not solve the separate lack of direct `COD_SIES` in the MiFuturo income file.

The person-level candidate outcome is built in `code/codex/mifuturo_matricula_income/03_construct_person_level_income_outcomes.R`.
It assigns values for all students in `univ_gr8_df.csv` using:

- matriculated students: MiFuturo model prediction
- preferred matriculated tier: two-way FE prediction from `log(income) ~ institution + AREA_CARRERA_GENERICA`
- fallback matriculated tiers, when outside two-way support: `AREA_CARRERA_GENERICA` FE, institution FE, then global MiFuturo mean
- non-matriculated students: configurable raw wage-floor proxy, currently `350000` CLP under label `raw_minimum_wage_proxy_2020_2023`

The non-matriculation floor is not used for matriculated students.
Call the outcome `program_income`.
Use `program_income_clp_m1` and `log_program_income_clp_m1` as the first-enrollment candidate outcome, with analogous `_ml` variables for last enrollment.
Always keep the corresponding `program_income_source_*` columns for diagnostics, since they identify whether a value comes from two-way FE, a fallback tier, or the no-matriculation floor.
The longer `mifuturo_income_hier_or_minwage_*` variables remain in the diagnostic output as traceability aliases.

The current program-income value-added, EB shrinkage, and EB expected-VA IV regressions are implemented and run in R because the Stata license is temporarily unavailable.
Treat Stata `.do` versions of these regressions as legacy or future cross-checks until the Stata environment is restored.

Open data-quality issue: 12,568 first-enrollment matriculated students currently have blank/missing `AREA_CARRERA_GENERICA_m1` because `program_info_found_m1 == FALSE`.
Many are high-volume ordinary programs, so this likely can be improved by repairing the `COD_SIES` to generic-area mapping.
See `setup_md_codex/project_memory/issues.md` under "Repair missing `AREA_CARRERA_GENERICA` for `program_income`".

## School Gender-Gap Measures

The constructor now also builds school-level gender-gap objects for:

- `z_year_math_max`
- `stem_enrollment_m1`

These are stored as separate outcomes:

- `gender_gap__z_year_math_max`
- `gender_gap__stem_enrollment_m1`

The corresponding `outcome_family` is:

- `gender_gap`

### Gender Coding

The current coding used in `univ_gr8_df.csv` is treated as:

- `GEN_ALU == 1`: boys
- `GEN_ALU == 2`: girls

The constructor converts this into:

- `female_indicator = 1` for girls
- `female_indicator = 0` for boys

Any other value is treated as missing for the gender-gap exercise.

### Raw Gender Gap

For each school and each supported outcome, the raw gender gap is:

- girls' mean outcome minus boys' mean outcome within the same school

This is stored in the generic raw-value fields for the gender-gap outcomes:

- `raw_mean`
- `raw_mean_centered`
- `raw_mean_centered_student`

For gender-gap rows, these fields should be interpreted as "raw girl-minus-boy gap" rather than as a raw level of the underlying outcome.

### Adjusted Gender Gap

The adjusted gender gap is designed to let controls work flexibly by gender.

The current procedure does not force boys and girls to have the same control slopes. Instead, it estimates a pooled regression with:

- school-by-gender fixed effects
- gender-interacted controls for cohort, age, and prior achievement
- a common, non-interacted comuna control

Operationally, the constructor creates:

- `school_gender_fe = paste0(school_rbd, "__", gender_group)`

where `gender_group` is either `girl` or `boy`.

The adjusted model is then:

`outcome ~ controls + female_indicator:(controls) | school_gender_fe`

where `controls` currently means:

- `factor(cohort_gr8)`
- `factor(EDAD_ALU)`
- `factor(COD_COM_ALU)`
- `z_sim_mat_4to`
- `I(z_sim_mat_4to^2)`
- `I(z_sim_mat_4to^3)`
- `z_sim_leng_4to`
- `I(z_sim_leng_4to^2)`
- `I(z_sim_leng_4to^3)`

Note that `factor(GEN_ALU)` is omitted from the gender-gap model because gender is already built into the school-by-gender fixed effects and the gender-specific control interactions.

The current interaction rule is:

- interact `factor(cohort_gr8)`
- interact `z_sim_mat_4to`
- interact `I(z_sim_mat_4to^2)`
- interact `I(z_sim_mat_4to^3)`
- interact `z_sim_leng_4to`
- interact `I(z_sim_leng_4to^2)`
- interact `I(z_sim_leng_4to^3)`
- interact `factor(EDAD_ALU)`
- do not interact `factor(COD_COM_ALU)`

So comuna is the only shared non-interacted control in the gender-gap VA model, because interacting student comuna by gender would add many degrees of freedom. Cohort, age, and the baseline-score polynomial terms are allowed to work differently by gender.

Interpretation:

- each school has a boys fixed effect
- each school has a girls fixed effect
- the adjusted gender-gap school value is the girls fixed effect minus the boys fixed effect

That difference is stored as:

- `controlled_value_added`

For gender-gap rows, this field should be read as:

- adjusted girl-minus-boy school gap after flexible control adjustment

As with the level outcomes, centered versions are also stored:

- `controlled_value_added_centered`
- `controlled_value_added_centered_student`

For gender-gap rows, the "adjusted mean" fields are currently just the same adjusted gap carried through for convenience:

- `controlled_adjusted_mean`
- `controlled_adjusted_mean_student`

These are not school-level predicted score levels. They are adjusted gender-gap objects.

### Gender-Gap Counts and Flags

For gender-gap outcomes, the constructor also stores:

- `n_girls_outcome`
- `n_boys_outcome`
- `n_girls_regression`
- `n_boys_regression`
- `low_gender_count`

`low_gender_count` is triggered when the smaller of the boys or girls outcome counts is below the current threshold of `20`.

This matters especially for sparse outcomes such as STEM enrollment, where a school can easily have a usable overall sample but a noisy gender-specific gap.

## Current Research Priority

The current project priority is to measure causal effects of schools on higher-education field choice and causal effects of schools on academic scores as two central outcome objects.

Separating those effects into achievement and non-achievement channels is now a secondary goal rather than the main framing of the project. That decomposition may still matter later, but it should not be treated as the primary summary of the research agenda in project descriptions.

The current version of the project is reduced-form rather than structural. Reusable summaries should not describe the project as containing an active structural component unless that changes again later.

## Writing Narrative From Overleaf Drafts

The current writing drafts in `writing/overleaf/` emphasize a few narrative points that should carry over into future project summaries:

- The project is about school effects on higher-education outcomes at unusually fine granularity, not just on college attendance in the abstract.
- The distinctive contribution of the setting is the coexistence of a centralized school assignment system and a centralized higher-education application system in Chile.
- This combination makes it possible to observe program-institution application portfolios, exam-taking choices, field choice, enrollment, and later program switches.
- A recurring motivation in the drafts is that broad outcomes such as college enrollment or number of semesters are too coarse to capture how schools shape later choices.
- The paper frames major choice, especially STEM and related field composition, as a substantively important outcome in its own right.

The older December 2025 draft still uses a "beyond achievement" headline. The more recent materials keep that mechanism question alive, but the current project priority is better summarized as estimating causal school effects on scores and higher-education field choice first, with mechanism decomposition treated as a later extension.

## Scale and Design Facts Used in the Narrative

- The paper narrative focuses on oversubscribed high schools identified through the Chilean centralized assignment system.
- The assignment-probability exercise is treated as a practical cornerstone of identification, not just a technical appendix item.
- Avoid relying on provisional result counts, coverage numbers, or draft-specific estimates in reusable project summaries unless those details are explicitly needed for the task at hand.

## SAE Support and Particular Pagado Schools

When using the school directory dependency codes, `Particular Pagado` or non-subsidized private schools are identified as:

- `COD_DEPE == 4` in the detailed dependency code
- `COD_DEPE2 == 3` in the grouped dependency code

A diagnostic run on the current `k100_timely_risk` SAE probability-support file found no `Particular Pagado` schools in the supported SAE set. Using the latest available school directory at the time of the check, `2025/20250926_Directorio_Oficial_EE_2025_20250430_WEB.csv`, the supported set contained:

- `1,012` supported RBDs
- `1,005` supported RBDs with more than `100` students with positive assignment probability
- `0` supported RBDs with `COD_DEPE == 4`
- `0` supported RBDs with `COD_DEPE2 == 3`

This matches the substantive expectation that `Particular Pagado` schools are outside the SAE system. This does not mean that all private-sector schools are absent from SAE: `Particular Subvencionado` schools are present in the SAE support and should be distinguished from `Particular Pagado` in plots, summaries, and binary-treatment definitions.

## School Public Funding Per Student

A first-pass school resource measure is constructed from the MINEDUC `Subvenciones-a-EE` files for 2017-2021.

The current construction lives in:

- `code/codex/school_expenditure_values/01_construct_public_funding_per_student.R`

The outputs are:

- `data/clean/school_expenditure_values/school_public_funding_per_student_year.csv`
- `data/clean/school_expenditure_values/school_public_funding_per_student_2017_2021.csv`
- `data/clean/school_expenditure_values/school_public_funding_per_student_diagnostics.csv`

The school-year measure is:

`annual public funding per student = annual sum of selected monthly subsidy/payment components / average monthly enrollment`

Monetary values are converted to 2021 pesos using annual CPI index values for Chile from World Bank indicator `FP.CPI.TOTL`, sourced from IMF International Financial Statistics. Nominal variables are retained, but the preferred first-pass 2017-2021 school-level scalar is:

- `enrollment_weighted_public_funding_per_student_2021_pesos_2017_2021`

This is public funding received through the subsidy files, not total school expenditure. `Reliquidacion-FICOM` files are excluded because FICOM is not the relevant resource margin for the current lottery-supported schools; the SAE support contains no `Particular Pagado` schools.

The construction also creates a growth/change version of the real funding measure. It compares average real public funding per student in the early period, 2017-2018, to average real public funding per student in the later period, 2019-2021. The candidate variables are:

- `change_public_funding_per_student_2021_pesos_2019_2021_vs_2017_2018`
- `pct_change_public_funding_per_student_2021_pesos_2019_2021_vs_2017_2018`
- `log_change_public_funding_per_student_2021_pesos_2019_2021_vs_2017_2018`

The level and growth measures should be treated as different school characteristics. The level measure asks whether students benefit from being induced into higher-funded schools. The growth measure asks whether students benefit from being induced into schools whose real public funding per student increased more over the period.
