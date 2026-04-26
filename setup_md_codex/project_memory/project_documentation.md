# Project Documentation

This file records substantive, project-specific knowledge needed to understand and continue the causal schools research project. It should document data construction, sample definitions, outcome definitions, empirical design choices, interpretation conventions, and links to decision logs.

It should not include Codex workflow rules, collaboration preferences, or general assistant behavior instructions. Those belong in `setup_md_codex/WORKFLOW.md` or other agent-facing setup files.

Empirical regression specifications are tracked separately in `empirical_methods.md`.

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
- this universe is the broad student universe used to construct observational school values
- the current observational-value constructor additionally restricts to `12 <= EDAD_ALU <= 16`
- this age restriction is currently imposed inside the VA constructor, not upstream in the general universe build

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

The current main enrollment outcome for STEM-style summaries is:

- `stem_enrollment_m1`

During the current estimation-development pass, controlled VA is only estimated for:

- `z_year_math_max`
- `z_year_leng_max`
- `z_year_leng_math_total`
- `stem_enrollment_m1`

The gender-gap VA is only estimated for:

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

Interpretation of the controls:

- `cohort_gr8` adjusts for cohort differences
- `GEN_ALU` adjusts for gender differences
- `EDAD_ALU` adjusts flexibly for grade-8 age
- `COD_COM_ALU` is the student's comuna, not the school's comuna
- `z_sim_mat_4to` and `z_sim_leng_4to` adjust for prior achievement through third-order polynomials

Important current restriction:

- `income_decile` is intentionally excluded because it is currently messy and not yet clean enough for use as a control

The school fixed effect from this regression is stored as:

- `controlled_value_added`

This fixed effect is not automatically on the same level scale as the raw mean, so the constructor also stores centered or shifted versions:

- `controlled_value_added_centered`: subtract the national school-level mean of the school fixed effect
- `controlled_value_added_centered_student`: subtract the national student-weighted mean of the school fixed effect
- `controlled_adjusted_mean`: raw national outcome mean plus `controlled_value_added_centered`
- `controlled_adjusted_mean_student`: regression-sample outcome mean plus `controlled_value_added_centered_student`

The centered fixed-effect versions are useful for school-to-school comparison. The adjusted-mean versions are more interpretable when raw and adjusted values need to be read on the same outcome scale.

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
