# Session Log

## 2026-04-26: Scalar School-Value IV Setup and VA Rebuild

### Main Thread

This session moved from constructing broad-universe school observational values toward the estimation setup.

The main conceptual framing is now a scalar school-value IV:

- construct broad-universe school values `V_s`
- merge those values onto the lottery/SAE estimation sample
- define the endogenous scalar treatment as `D_i = V_{S_i}`
- instrument that scalar treatment with lottery-generated first-round offer variation

This is documented in `empirical_methods.md`.

### Student Universe and School Links

The broad universe is defined in `def_sample_frame.R` as students observed in the Chilean school system in grade 8 between 2017 and 2020 with `COD_ENSE == 110`.

`cohort_gr8` is the first year in which the student is observed in grade 8.

`def_school.R` creates post-grade-8 school links by tracking students after grade 8:

- `RBD_rel1`: RBD one year after grade 8
- `RBD_rel4`: RBD four years after grade 8
- `most_time_RBD`: RBD where the student spends the most time in the post-grade-8 tracking window

Current observational school values use:

- `school_rbd = most_time_RBD`

### Merged Broad-Universe File

`universe_reg_df.R` assembles the broad-universe analysis file `univ_gr8_df`.

It starts from `universe_controls.csv` and merges in:

- SAE participation/process information
- first-round offer information
- baseline SIMCE controls
- post-grade-8 school links
- PSU/PAES outcomes
- STEM/higher-education outcomes
- higher-education matricula files

The file writes:

- `data/clean/univ_gr8_df.csv`
- `data/clean/univ_gr8_df.dta`

### SAE and Offer Data

`sae_binary_prep.RData` loads an object called `sae_apps_grade9` with `mrun` and `sae_proceso`.

`sae_proceso` identifies the SAE grade-9 admission process in which the student appears. If the object has multiple rows per student, the intended student-level object is the first observed SAE process.

First-round offer data originally comes from `treatment_1R_v2.RData`, object `all_treatments`. The important offer variable is:

- `rbd_treated_1R`

Rule:

- `rbd_treated_1R != 0` implies a first-round offer
- the nonzero value is the offered RBD

Important diagnostic from this session:

- `all_treatments` is application-level, not student-level
- raw join by `mrun` expanded the broad universe from 944,360 rows to 1,778,389 rows
- after collapsing offers to one row per student, the join sequence stayed at 944,360 rows

Current working design:

1. Collapse offers to `mrun` x `sae_proceso`.
2. Within each student-process, define the nonzero first-round offered RBD if one exists.
3. Then keep the first `sae_proceso` per `mrun`.
4. Merge that student-level first offer into `universe_reg_df`.
5. Define `timely_sae = cohort_gr8 == sae_proceso`.

As of this session, `universe_reg_df.R` references `offers_1R_p_proceso.RData` and constructs `offers_1R_first` by keeping the minimum `sae_proceso` per `mrun`. This should be rechecked after the user's latest dataframe rebuild.

### VA Specification Changes

The controlled observational value-added script is:

- `code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R`

The main controlled VA now uses third-order polynomials in baseline SIMCE scores:

- `z_sim_mat_4to`
- `I(z_sim_mat_4to^2)`
- `I(z_sim_mat_4to^3)`
- `z_sim_leng_4to`
- `I(z_sim_leng_4to^2)`
- `I(z_sim_leng_4to^3)`

The working reason is that the value-added literature typically uses flexible controls for baseline achievement. Citations still need to be added.

The rest of the main VA controls are:

- `factor(cohort_gr8)`
- `factor(GEN_ALU)`
- `factor(EDAD_ALU)`
- `factor(COD_COM_ALU)`

The constructor still restricts the VA sample to:

- `12 <= EDAD_ALU <= 16`

This restriction is inside the VA constructor, not the upstream universe definition.

### Narrowed Controlled VA Outcomes

To speed iteration, controlled VA is currently estimated only for:

- `z_year_math_max`
- `z_year_leng_max`
- `z_year_leng_math_total`
- `stem_enrollment_m1`

Raw means are still computed for the broader configured outcome set.

Gender-gap VA is currently estimated only for:

- `gender_gap__z_year_math_max`
- `gender_gap__stem_enrollment_m1`

### Gender-Gap VA

Gender coding:

- `GEN_ALU == 1`: boys
- `GEN_ALU == 2`: girls
- gender gap is girls minus boys

The adjusted gender-gap model uses school-by-gender fixed effects.

The gender-gap controls are:

- `factor(cohort_gr8)`
- `factor(EDAD_ALU)`
- `factor(COD_COM_ALU)`
- cubic baseline-score terms for math and language SIMCE

Interaction rule:

- interact all gender-gap controls with gender except `factor(COD_COM_ALU)`
- do not interact student comuna because it has many categories

This means cohort, grade-8 age, and baseline-score polynomial controls are allowed to vary by gender.

### Bug Fixed During VA Run

The gender-gap VA initially exported missing controlled values because the fixed-effect vector name collided inside a `tibble()` call:

- the code created a column named `school_gender_fe`
- then `as.numeric(school_gender_fe)` read that just-created character column rather than the numeric fixed-effect vector

Fix:

- store the extracted fixed effects in `school_gender_effects`
- use `names(school_gender_effects)` and `as.numeric(school_gender_effects)`

Also, fixed effects are now extracted with explicit bracket indexing:

- `fixef(model)[["school_rbd"]]`
- `fixef(model)[["school_gender_fe"]]`

This avoids ambiguity with `$` on `fixest` fixed-effect objects.

### VA Output Status

The VA constructor was rerun successfully after the fixes.

Output:

- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/school_rbd_observational_values/school_rbd_observational_values.csv`

Final output check:

- rows: 170,856
- outcomes: 42
- schools: 4,068

Controlled VA nonmissing counts:

- `z_year_math_max`: 3,366 schools
- `z_year_leng_max`: 3,375 schools
- `z_year_leng_math_total`: 3,376 schools
- `stem_enrollment_m1`: 3,703 schools
- `gender_gap__z_year_math_max`: 3,057 schools
- `gender_gap__stem_enrollment_m1`: 3,297 schools

### Next Checks

Before estimation, check:

- `univ_gr8_df` is one row per student after the new `offers_1R_p_proceso.RData` merge
- `offers_1R_first` is one row per `mrun`
- no student has multiple distinct nonzero `rbd_treated_1R` within the same `mrun` x `sae_proceso`
- `timely_sae` is missing only for students without SAE participation
- the Stata estimation sample is restricted to valid lottery/offer/probability variation, not simply everyone in the broad universe

The probability/risk-control layer is still the major missing estimation input.
