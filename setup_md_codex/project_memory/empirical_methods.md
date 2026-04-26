# Empirical Methods

This file records the empirical specifications used in the causal schools project. The goal is to keep each family of regressions explicit enough that the estimand, treatment definition, controls, and interpretation are clear before implementation.

## Scalar School-Value IV

### Student Universe

The broad student universe is defined upstream in `def_sample_frame`. It contains all students observed in the Chilean school system in grade 8 between 2017 and 2020 with `COD_ENSE == 110`.

Each student's grade-8 cohort year, `cohort_gr8`, is defined as the first year in which the student is observed in grade 8.

Student-school links are defined in `def_school` by looking at where each student is observed after grade 8. The resulting links include an RBD one year after grade 8, an RBD four years after grade 8, and the RBD where the student spends the most time.

The merged broad-universe analysis file is assembled in `universe_reg_df.R`. It starts from `universe_controls.csv` and merges in SAE indicators, baseline SIMCE controls, post-grade-8 school links, PSU outcomes, STEM outcomes, and higher-education matricula files. The resulting files are `data/clean/univ_gr8_df.csv` and `data/clean/univ_gr8_df.dta`.

The SAE participation/process marker comes from `sae_binary_prep.RData`. The loaded object is `sae_apps_grade9`, with variables `mrun` and `sae_proceso`. In this file, `sae_proceso` indicates the SAE grade-9 admission process in which the student appears; if a student appears in multiple SAE processes, the construction keeps the first observed process.

The first-round school assignment/offer file is `data/clean/treatment_1R_v2.RData`. It contains the object `all_treatments`. For the scalar school-value IV, the key variables are:

- `mrun`
- `rbd_treated_1R`

`rbd_treated_1R` records the RBD offered/admitted in the first round for the application row, and is `0` when that row did not generate a first-round offer. Equivalently, `rbd_treated_1R != 0` is the first-round offer indicator, while the nonzero value gives the offered RBD. The file is at the `mrun`-`br_code` level, so the estimation build needs an explicit rule for collapsing or selecting the student-level first-round offered RBD used as the instrument.

This broad universe is used to construct observational school values `V_s`. The IV estimation sample is narrower: it should include only students and school choices for which lottery-generated variation is available.

### Motivation

The main estimation target is not a fully flexible vector of causal effects for every school. A model with one treatment indicator for each school would treat school attendance as:

`D_i_full = (1{S_i = 1}, 1{S_i = 2}, ..., 1{S_i = J})`

and would require estimating a separate causal effect for every school:

`Y_i = alpha + sum_s tau_s 1{S_i = s} + gamma'P_i + delta'X_i + epsilon_i`

The lottery instruments generally do not identify a separate `tau_s` for every school. The design can shift students across schools, but it usually does not provide enough independent variation to recover a full vector of school-specific causal effects.

The current approach therefore reduces the high-dimensional school treatment into a scalar school-value index. The empirical question becomes:

When lottery-induced school attendance changes move students toward schools with higher pre-specified observational value, do student outcomes improve?

### School-Value Index

For each school `s`, define an observational school value `V_s`. The current construction creates two main versions:

- raw school mean
- individual-level controlled observational value-added

For student `i`, the scalar treatment is the value of the school attended or otherwise linked to that student:

`D_i = V_{S_i}`

Equivalently:

`D_i = sum_s V_s 1{S_i = s}`

Here `S_i` must be defined carefully. Using the attended school, assigned school, offered school, or most-time school changes the estimand. The current observational-value construction uses:

- `school_rbd = most_time_RBD`

For IV estimation, the school object used to create `D_i` should be stated explicitly in each regression table or script.

### Constructed School Values

The raw school value for outcome `Y` is:

`V_s_raw = mean(Y_i | S_i = s)`

The controlled observational value-added is estimated from a student-level regression with school fixed effects:

`Y_i = alpha_s + X_i'lambda + kappa_c + eta_i`

The estimated school fixed effect is:

`V_s_VA = alpha_hat_s`

In the current implementation, the controlled value-added is estimated at the student level using student-level controls. The baseline SIMCE controls, `z_sim_mat_4to` and `z_sim_leng_4to`, enter as third-order polynomials. The controlled value-added is not estimated by regressing school mean outcomes on school-level demographic shares.

The current construction details are documented in `project_documentation.md` under "School RBD Observational Values." The output file is:

- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/school_rbd_observational_values/school_rbd_observational_values.csv`

### IV System

For a given school-value index `V_s`, merge the school values back to the student-level estimation data and define:

`D_i_raw = V_{S_i}^{raw}`

or:

`D_i_VA = V_{S_i}^{VA}`

Then estimate the 2SLS system:

`D_i = pi_0 + Pi'Z_i + gamma'P_i + delta'X_i + u_i`

`Y_i = alpha + beta D_hat_i + gamma'P_i + delta'X_i + epsilon_i`

where:

- `Y_i` is the student outcome.
- `S_i` is the relevant school linked to student `i`.
- `V_s` is the pre-specified observational value of school `s`.
- `D_i = V_{S_i}` is the scalar endogenous treatment.
- `Z_i` is the lottery offer instrument or vector of lottery instruments.
- `P_i` are lottery risk/probability controls.
- `X_i` are other student-level controls.
- `beta` is the causal return to a lottery-induced increase in the scalar school-value index.

The regression should be run separately for each version of `V_s`, for example raw means and controlled value-added.

### Interpretation

The IV coefficient `beta` is not the causal effect of attending any particular school. It is the causal effect of moving along the chosen school-value index among students whose school attendance is shifted by the lottery instruments.

The estimand is therefore specific to:

- the school-value index used to construct `V_s`
- the school link used to define `S_i`
- the instruments `Z_i`
- the lottery risk controls `P_i`
- the population of lottery compliers

If `V_s` is based on math value-added, `beta` answers whether lottery-induced moves toward higher math-value schools affect `Y_i`. If `V_s` is based on STEM enrollment, `beta` answers whether lottery-induced moves toward higher STEM-enrollment schools affect `Y_i`.

### Current Guardrails and Open Decisions

Avoid interpreting `V_s` as a causal school effect. The raw and controlled values are observational indices. The IV design estimates the causal return to moving along those indices.

Be explicit about whether the value index and the causal outcome are the same variable. If `V_s` is constructed from the same outcome as `Y_i`, own-observation or same-sample mechanical correlation may matter. Candidate safeguards include leave-one-out school values, constructing `V_s` on a separate sample, or cross-fitting.

Define `S_i` before estimation. `most_time_RBD`, assigned RBD, offered RBD, or actual attended RBD can each be defensible, but they answer different questions.

Define `Z_i` and `P_i` in the estimation code. The methods section should state whether the instrument is a single offer, a vector of offer indicators, or another lottery-generated object, and how admission-risk controls are included.

Distinguish SAE participation from identifying lottery variation. `sae_proceso` marks that the student appears in the SAE grade-9 admission process, but the IV sample still needs to be restricted to observations with valid lottery-generated instruments and the corresponding risk/probability controls.

Keep the school-value construction and IV estimation separated. First construct `V_s`; then merge it into the estimation sample and run 2SLS. This makes the scalar treatment explicit and prevents the estimation code from silently changing the definition of school value.
