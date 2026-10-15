# Empirical Methods

## Submitted school rankings and joint VA preferences (2026-10-14)

The subsequent2026-10-15 PAES extension adds school-average current regular
M1 for admission process2025(2024 graduates) and both type interactions.
Construct from raw MATE1_REG_ACTUAL among current-graduate valid takers,
not MATE1_MAX; no GPA/SAE-sample restriction or zero scores. Keep all prior
VA terms/type interactions and distance:18coefficients. Preserve existing
VA scaling; normalize M1 equally across unique retained destinations.
Exclude incomplete rankings intact. Refit the15-term baseline on the same
sample for comparison, pooled and four groups with both distances. These
later scores are a school-performance measure, not necessarily information
available at application. See decisions/2026-10-15-paes-m1-school-preferences.md.

Estimate the exact Plackett-Luce ordering likelihood conditional on each
student's submitted RBD set. The initial two specifications include four
current pooled EB VAs simultaneously (math, exam taking, high-premium field
and high-premium institution) and log(1+Haversine km), using published
student points or home-comuna centers. The latest2026-10-15 user request adds
all eight interactions of the four VAs with the mixed HC/TP and TP-only
indicators, retaining all main effects and distance in every pooled and
subgroup model (15 coefficients). Non-TP VA slopes are beta; mixed and TP-only
slopes are beta plus the corresponding interaction. Report total slopes and
their SEs using the full student-clustered covariance. The type intercepts
refer only to all four standardized VAs equal to zero; type premiums vary
with VA. Preserve additive estimates in the dated clean-data archive.
Use regular HC code310 and TP codes410/510/610/710/810
from the fixed2024 Directorio. Mixed offers both; TP-only offers TP withoutHC.
Indicators are raw0/1, not standardized; the single any-TP flag is metadata.
The omitted category is non-TP:1557HC-only,1artistic and16other/no-regular-
secondary schools on the2501-school support. Preserve these alternatives and
the exact sample; do not impose2024 activity restrictions on historical lists.
Both distance versions retain actual campuses.
Use the Table 2 applicant roster, retain genuine priority choices, exclude
auto-continuity alternatives, collapse RBD to its first submitted program,
and exclude an entire ranking if any regressor is missing. Single-school
lists provide no identifying information. Normalize VAs equally across unique
destination schools in the final shared estimation support and reuse those
moments in both models. Cox event time equals school rank, stratum equals
ranking, and cluster equals student; unique ranks make Breslow exact here.
Inference conditions on existing EB estimates. No school fixed effects,
unlisted alternatives or peer controls enter. On 2026-10-15 the user requested
separate fits by low-income prioritario status crossed with origin-school
continuation. Use the same four VAs, both distances, whole-list restrictions
and unchanged pooled normalization in every group; exclude unresolved origins.
Both assignment-risk statuses enter. Separate-logit coefficient differences
may reflect utility-error scale as well as preferences.
See `decisions/2026-10-14-school-preference-va.md` for sources and validation.

## Main IV by origin-school continuation (2026-10-12)

Re-estimate the paper's seven first-stage and pass-through specifications
separately for school-ending and voluntary applicants, using the existing
Table 2 continuation classification. Both regression samples require assignment
risk and retain the original outcome-specific controls/missingness restrictions.
Keep EB school values and expected-VA construction fixed across groups, with
the paper's heteroskedasticity-robust SEs. Assignment-risk rates and primary
expected-VA means instead use all Table 2 applicants within each group;
additional expected-VA means use the exact IV samples. See
`decisions/2026-10-12-main-iv-by-continuation.md` for complete definitions.

## Averaged academic histories for voluntary SAE (2026-10-10)

At Bruno's request, replace each four-grade academic history with the arithmetic
mean of its observed values in grades 5-8. Each of six summaries enters with its
square, separately or jointly, retaining the context and annual survey-PC1
controls. Standardize the newly constructed averages with original-training
observed moments; use the training median when wholly missing and categorical
observed-grade counts for every included average. Sample and clustered inference
remain unchanged. A two-period sensitivity (5-6 and 7-8) checks whether averaging
hides timing. For the focused GPA/math-gap comparison, use identical observed
grades and pupils to preserve the gap identity. See
`decisions/2026-10-10-average-grade-histories.md` for exact specifications.

## Separate predictors and survey PCA (2026-10-10)

For the voluntary-SAE sample, compare 24 separate grade-specific academic
measure-plus-square OLS regressions with a model entering them all jointly.
Use the same context and annual survey-PC1 controls, clustered by origin school.
Focused GPA/math-gap comparisons hold the observed sample fixed within grade.
Academic units/imputation remain unchanged from the prior prepared design.
Survey PCA uses each year's applicable standardized items among complete
respondents, without outcome information or answer imputation. Keep incomplete
respondents in the full regressions with zero-reference scores and completeness
controls. PC coefficients vary by survey year; a sensitivity includes PCs 1-3.
The exact specifications, PCA coverage, and algebra re-expressing gaps in current
GPA and baseline achievement are documented in
`decisions/2026-10-10-separate-predictors-survey-pca.md`.

## SAE participation prediction (2026-10-05)

Fixed nested binomial logits predict any explicit regular/complementary grade-9
SAE application in the grade-8 cohort year. School offerings, own/relative grades
and attendance, and GPA-minus-SIMCE peer z-score gaps enter the agreed sequence;
the administrative numeric measures include squares. Separate questionnaire
availability from answer content. A fixed 80/20 origin-school split keeps schools
together across cohorts, with training-only imputation/scaling and identical
samples across specifications. Evaluate AUC, Brier loss, log loss and calibration
overall, within continuation groups and by cohort; descriptive coefficient SEs
cluster at origin school. Full-year grade-8 inputs make this retrospective. A
sensitivity drops those annual grade-8 measures. No geography is estimated.
The primary classification comparison is now Bruno's deterministic continuation
rule: no application if grade 9 is offered, application if the school ends at
grade 8. Use identical held-out pupils with resolved continuation status, a fixed
0.5 cutoff for logits, equal error costs, and separate results in each school
group. Report accuracy and mistakes corrected/introduced, with paired gain CIs
from a pupil-level intercept-only regression clustered by origin school. CIs
condition on the fitted models. A separate probability benchmark assigns each
group its training application rate. No test-set threshold tuning is performed.
Actual continuation is distinct from this application outcome.
The voluntary-margin follow-up refits six nested logits only among origins
offering grade 9, preserving the original school holdout and using subgroup
training preprocessing. Two CART probability trees use inner school validation
to choose complexity. The primary benchmark predicts no application for everyone;
also report AUC, average precision and top-decile applicant concentration. Treat
this follow-up as exploratory because the holdout was already examined. See
`decisions/2026-10-05-voluntary-sae-margin.md` for exact specifications.
The subsequent requested simple regression is a single joint OLS linear
probability model on all 313,813 voluntary-margin pupils, with all constructed
full-model regressors and school-clustered SEs. Existing standardization,
imputation, quadratic and missingness terms are preserved. The full coefficient
table in `output/reports/sae_participation/voluntary_ols.html` reports percentage
points and SEs; R2 is .0343078. This is descriptive full-sample OLS, not held-out
prediction performance.
See `decisions/2026-10-05-sae-participation-prediction.md` for sample, normalization,
questionnaire harmonization, zero-attendance and timing decisions.

This file records the empirical specifications used in the causal schools project. The goal is to keep each family of regressions explicit enough that the estimand, treatment definition, controls, and interpretation are clear before implementation.

## Scalar School-Value IV

### Student Universe

The broad student universe is defined upstream in `def_sample_frame`. It contains all students observed in the Chilean school system in grade 8 between 2017 and 2020 with `COD_ENSE == 110`.

Each student's grade-8 cohort year, `cohort_gr8`, is defined as the first year in which the student is observed in grade 8.

Student-school links are defined in `def_school` by looking at where each student is observed after grade 8. The resulting links include an RBD one year after grade 8, an RBD four years after grade 8, and the RBD where the student spends the most time.

The merged broad-universe analysis file is assembled in `universe_reg_df.R`. It starts from `universe_controls.csv` and merges in SAE indicators, baseline SIMCE controls, post-grade-8 school links, PSU outcomes, STEM outcomes, and higher-education matricula files. The resulting files are `data/clean/univ_gr8_df.csv` and `data/clean/univ_gr8_df.dta`.

The higher-education matricula cleaner now carries program and institution accreditation information from the raw matricula files. It keeps `ACREDITADA_CARR`, `ACREDITADA_INST`, and `ACRE_INST_ANIO` for first and last ingreso, and defines `program_certified_years = 1[ACREDITADA_CARR == "ACREDITADA"] * ACRE_INST_ANIO`.

The annual higher-education persistence panel uses full SIES matricula files
for 2022--2025. Expected entry is grade-8 cohort plus five years. Grade-8
cohort 2017 has three follow-up years, 2018 has two, 2019 has one, and 2020 has
entry-year status only through full SIES 2025. Outcomes
distinguish conditional persistence among entry-year enrollees from
unconditional entry-and-persistence, and use set overlap when students have
multiple valid enrollments in a year.

As of May 31, 2026, current school-value and scalar-IV development uses All-sample school values by default. Male/Female sample-specific value-added and gender-gap value-added are on hold unless explicitly requested.

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

Current controlled VA controls include cohort, gender, grade-8 age, student grade-8 comuna, imputed SIMCE parent-survey income decile, imputed parent education years, imputed parent indigenous indicators, imputed early-childhood attendance indicators, and cubic baseline SIMCE math/language controls. The income control is `income_decile_imputed`, which is recomputed from `income_mid_imputed` after filling missing income midpoints with baseline-context medians using minimum donor-cell size `15`. The other CPAD controls use the same median-imputation hierarchy and donor-cell threshold. Remaining missing values are left missing, so the VA regression still uses complete cases for the listed controls.

Current paper-facing school VA estimates restrict the broad panel to admission-exam takers, defined as students with an observed math or language national admission-test score. Higher-education outcomes such as STEM enrollment and institutional accreditation years are therefore zero-coded only within this exam-taker sample.

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

### Probability-Control Support

The first-pass probability-control matrix uses a practical support restriction to avoid carrying probability controls for every school that appears anywhere in the assignment simulation.

The current support is defined among students in the IV construction sample:

- timely SAE students
- with matching simulated assignment probabilities
- with assignment risk, `any_risk == 1`

A school is retained in the probability-control set if at least `25` timely at-risk students have positive simulated assignment probability for that school.

This produces the k=25 supported school set used to construct the `prob_<rbd>` and `iszero_<rbd>` controls. The same supported set is used when coding the scalar offer instrument normalization: supported offers can generate nonzero `Z_i`; no-offer and outside-support offers are coded as the omitted `Z_i = 0` category.

The k=25 threshold is a practical support rule. It reduces the dimensionality of the probability-control matrix while retaining schools with meaningful lottery support in the estimation sample. The construction writes diagnostics such as the number of supported schools and the retained probability mass, which should be checked when the support rule changes.

For the first-pass scalar IV dataframe, score outcomes such as `z_year_math_max` and `z_year_leng_max` are constructed on the broad `univ_gr8_df` by PSU year before merging the reduced probability controls. This keeps the score scale from being redefined by the narrower IV estimation sample.

### Expected-VA Risk Control Variant

The expected-VA scalar-IV variant is the current preferred path for new main estimates and heterogeneous-effects tables. It replaces the high-dimensional `prob_<rbd>` and `iszero_<rbd>` controls with one scalar risk control for each school-value index:

`expected_VA_i = sum_s p_is * V_s`

where `p_is` is the simulated assignment probability that student `i` is assigned to school `s`, and `V_s` is the All-sample school value for the relevant value metric.

This variant is implemented in `code/codex/scalar_school_value_iv/08_run_expected_va_scalar_iv.R`. It uses the long DA probability files directly (`DA_probs_2018.csv` through `DA_probs_2021.csv`) rather than the wide probability-control matrices. It keeps the IV sample restricted to timely at-risk students with probability rows and an observed math or language national admission-test score.

For this variant:

- `D_i = V_{most_time_RBD}` is the value of the student's attended/linked school.
- `Z_i = V_{rbd_treated_1R}` is the value of the first-round offered school.
- `expected_VA_i` is included as the scalar assignment-risk control.
- no-offer cases are coded with `Z_i = 0`; offered schools without an available All-sample value are left missing for that value metric.

The first table from this variant is written to:

- `output/tables/scalar_school_value_iv/scalar_school_value_iv_main_results_expected_va.csv`
- `output/tables/scalar_school_value_iv/scalar_school_value_iv_main_results_expected_va.tex`

Current workflow rule:

- Use the expected-VA regression dataframe for new estimates.
- For each scalar treatment/instrument pair, include its matching scalar risk control, for example `expected_math_adj` with `d_math_adj` and `z_math_adj`.
- For heterogeneous-effects tables, split or interact this expected-VA dataframe by the heterogeneity variable. Do not default back to the wide `prob_*` / `iszero_*` controls unless the task is explicitly a legacy robustness comparison.
- For full-sample grade-4 achievement heterogeneity, use `code/codex/scalar_school_value_iv/12_run_expected_va_grade4_quintile_all_sample.R`. It constructs grade-4 math quintiles directly from `z_sim_mat_4to` and reports adjusted Math, Language, and STEM expected-VA IV estimates.

The older k-support probability-control matrix path is retained only as a legacy or robustness path. It constructs wide `prob_<rbd>` and `iszero_<rbd>` controls and uses the restricted supported-school set. That path should not be described as the current default.

### EB-Shrunken School-Value Robustness

An Empirical-Bayes robustness path now shrinks the stage-1 adjusted observational school value-added estimates before constructing the scalar IV variables.

This path is separate from the current main expected-VA workflow.
It should not be described as hybrid causal EB or IV VAM.
It validates an EB-shrunken observational school-value index along the same SAE lottery margin.

The workflow is:

- rerun `code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R` so the school-value file includes `controlled_value_added_se`
- run `code/codex/empirical_bayes_school_va/01_construct_eb_school_values.R`
- run `code/codex/empirical_bayes_school_va/02_run_expected_va_scalar_iv_eb.R`

For outcome `m`, the EB school value is constructed from the adjusted All-sample value `Vhat_s^m = controlled_value_added_centered_student` and its stage-1 regression SE.
The current SE is:

`controlled_value_added_se = SE(controlled_value_added_centered_student)`

Because `fixef()` returns the fixed-effect point estimates but not their SEs, the constructor runs an auxiliary regression with `school_rbd` entered as explicit dummies and the remaining fixed effects still absorbed.
It uses `lfe::felm()` and `lfe::getfe(se = TRUE)` with a custom estimable function for the student-weighted centered school effect.
The relevant SE is not the SE of a raw reference-normalized fixed-effect level.
It is the regression-derived uncertainty of `school FE - student-weighted mean(school FE)`, matching `controlled_value_added_centered_student`.
The current stage-1 SE method label is `lfe_getfe_school_rbd_centered_student_iid_bN100`.
`controlled_value_added_resid_sd` is only a diagnostic column.

For each outcome, the EB constructor estimates:

`tau_m^2 = max(weighted Var(Vhat_s^m) - weighted mean(se_s^2), 0)`

using `n_students_regression` as the weight, and then sets:

`lambda_s^m = tau_m^2 / (tau_m^2 + se_s^2)`

`V_EB_s^m = mean_m + lambda_s^m * (Vhat_s^m - mean_m)`

The EB IV defines:

- `A_i^EB = V_EB_{most_time_RBD}^m`
- `O_i^EB = V_EB_{rbd_treated_1R}^m`
- `E_i^EB = sum_s p_is V_EB_s^m`

and instruments `A_i^EB` with `O_i^EB`, controlling for `E_i^EB`.

Because EB shrinkage compresses the school-value scale, EB and non-EB pass-through coefficients should be compared together with the SD of the corresponding school-value index.

The EB path includes `log_program_income_clp_m1` as the program-income outcome.
The current program-income EB-IV implementation is fully in R because the Stata license is temporarily unavailable.
Do not rely on `.do` scripts for current program-income EB regressions unless the Stata environment is restored and the R results are intentionally cross-checked.

### Current Scalar Offer-Instrument Normalization

The current first-pass scalar IV uses the value of the attended school as the endogenous treatment:

`D_i = V_{most_time_RBD}`

and the value of the first-round offered school as the scalar offer instrument:

`Z_i = V_{rbd_treated_1R}`

Because the probability-control set is restricted to schools in the practical k=25 lottery support, the scalar offer instrument is normalized so that unsupported offers and no-offer cases are the omitted baseline. For each school-value measure, compute:

`mu_out = mean(V_{rbd_treated_1R} | rbd_treated_1R outside supported set, rbd_treated_1R != 0)`

among timely at-risk students.

Then code:

`D_i = V_{most_time_RBD} - mu_out`

and:

`Z_i = 1{rbd_treated_1R in supported set} * (V_{rbd_treated_1R} - mu_out)`

So:

- no first-round offer has `Z_i = 0`
- an offer to a school outside the supported probability-control set has `Z_i = 0`
- an offer to a supported school has positive or negative `Z_i` depending on whether that school's value is above or below the outside-support offered-school baseline

This normalization makes zero correspond to the omitted outside-support/no-supported-offer category, not the national mean school value. The same baseline is subtracted from `D_i` so the endogenous treatment and scalar instrument are measured on the same shifted value scale.

This is an implementation choice for the first-pass scalar IV with restricted probability support. If the support definition changes, the outside-support baseline should be recomputed.

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

## Staffing characteristics aligned with VA (2026-09-07)

The agreed final target is two school-level staff-characteristic measures over
2018-2024, corresponding to the four post-grade-8 years for VA cohorts 2017-2020.
The extended cleaner reads Box staff records from 2013-2025; earlier records
provide observed career history and 2025 is excluded from VA-window feature rows.
Prior experience uses information strictly before each staff year; current spells
and historical role-persistence indicators use information through that year only.
All-appointment role experience counts each person-year once, and role-at-school
experience counts each person-school-year once. Main-appointment primary-function
histories are retained as a distinct definition, not silently substituted for all
appointments. Full-window EVER flags are retrospective and excluded from the
VA-feature tables. No prior observed history is not zero lifetime experience.

Orientador spells and cumulative experience are distinct. The existing
`ORIENTADOR_ANY_CONSECUTIVE_YEARS_TO_DATE` tracks consecutive observed years
in either primary or secondary orientador function, across appointments.
`ORIENTADOR_AT_SCHOOL_CONSECUTIVE_YEARS_TO_DATE` tracks the analogous
person-RBD spell. Both include the current year. Moving schools alone does
not interrupt the person-level spell, but starts a new school-specific spell.

The added `ORIENTADOR_PRIMARY_CUMULATIVE_YEARS_OBSERVED` counts distinct
observed years from 2013 through t with `ID_IFP=9` at any appointment.
Here primary/main function does not require the `PERSONAS=1` designation.
The separate `ORIENTADOR_MAIN_CUMULATIVE_YEARS_OBSERVED` applies both
restrictions. All cumulative counts retain experience across nonconsecutive
spells and school moves, without filling missing years or using future data.
The `*_PRIOR_YEARS_OBSERVED` versions remain strictly prior to t; cumulative
through-t counts must not silently replace them in predetermined specifications.
An observed cumulative count is not lifetime experience; its support is
reported in `*_KNOWN_YEARS_TO_DATE` and the broader history-coverage fields.

School tenure uses the current record's reported `ANO_SERVICIO_EE`, exposed as
the preferred `YEARS_AT_SCHOOL` input. It is not reconstructed from panel records
or capped at 2013; missing reported tenure is not filled with record counts.
Prior role experience counts observed role years from 2013 through t-1. Reconstructed
role-at-school history remains supplemental and distinct from reported tenure.
Credential indicators distinguish reported title status, tertiary qualification,
institution type, title specialty, and applicable mentions; they are not ranks of
training institutions.

Teacher characteristics use only classroom teachers assigned to regular
educacion media (HS) at that appointment's RBD. The assignment restriction is
`COD_ENS_1` or `COD_ENS_2` in `310, 410, 510, 610, 710, 810, 910`, covering
youth H-C, T-P and artistic media (MINEDUC annual staff codebooks 2013-2025,
Annex V, pp. 19-20). Adult-only media is excluded for the regular VA cohorts.
Either classroom function (primary or secondary) and either teaching slot can
qualify; mixed basic/HS assignments and non-main appointments are included.
The teacher's credentials, school-level offerings and appointments at other
schools cannot substitute for their own teaching-assignment code.
`NIVEL1/2` is only an audited consistency check, not an imputation source.
Missing/unmapped teaching codes stay unknown and do not qualify without a
confirmed HS code in the other slot.

`TEACHER_HS_*` is the preferred HS-specific role-history family, counting
observed HS teaching years from 2013 through t-1. All-level teacher histories
are supplemental and remain available to distinguish prior basic teaching from
missing career records. `VA_TEACHER_ELIGIBLE` and `VA_ORIENTADOR_ELIGIBLE` keep
the two rosters separate inside the combined appointment-level feature file.
Orientador eligibility/history remains function-based and is not subject to the
classroom-assignment filter. School support, aggregation and PCA were settled
in the completed 2026-09-10 construction below. Orientador allocation to HS
versus other levels remains unknown. School VA is never an input to the indices.

For students per orientador, the user authorized a VA-sample student numerator
on 2026-09-07. The proposed common baseline is the broad estimation sample,
not an outcome-specific score-observed subset. A read-only check of
`output/tables/empirical_bayes_school_va/stata_va_eb_input_exam.csv` verified
3,682 unique school RBDs and a sum of `n_students` equal to 757,999.
School-level counts match the saved STEM and full program-income VA inputs
exactly. The saved math VA input uses 561,977 students instead. School assignment
is `most_time_RBD` and the pooled grade-8 cohorts are 2017-2020; this does not
establish actual annual attendance at that RBD. Label any resulting ratio as
VA-sample students per orientador, not total HS students per orientador.
Bruno subsequently approved the period-average denominator on 2026-09-07.
For scope r (primary or any function), define H_st^r as the number of distinct
valid MRUNs with that function on any appointment at school s in year t.
Primary is ID_IFP=9; any is ID_IFP=9 or ID_IFS=9. Neither requires PERSONAS=1.
The final measure is N_VA_s / [(1/7) sum_{t=2018}^{2024} H_st^r].
The pooled numerator is not divided by four cohorts or converted to student-years.
The denominator is a mean of annual headcounts, not the number of ever-observed
orientadores, nor an average of student/staff ratios. Use equal calendar-year
weights, including confirmed zero years. This normalization is for this staffing
ratio only; it does not choose weights for the two proposed staff-quality scores.

The full-period denominator requires seven known annual headcounts. A school-year
absent from the staff directory stays missing. Unknown person roles or invalid
identifiers on potentially relevant appointments also leave the affected headcount
unknown. A positive role on another appointment at the same person-school-year
resolves that person's inclusion. Keep partial-year means only as labeled
diagnostics, not a replacement denominator. A known zero full-period mean yields
an undefined ratio and a separate no-orientador indicator, not zero workload.
Count all school-specific appointments without restricting orientadores to HS
classroom assignments; their coverage of HS versus other school levels is unknown.
See `code/codex/docentes_educacion/README.md` for outputs and reproducible checks.

### Completed staff indices and VA associations (2026-09-10)

The implemented specification, normalization, missingness gates and inference
are recorded in `decisions/2026-09-10-staff-characteristics-va.md`. The common
3,682-school support uses 2018-2024 current staff and 2013-onward histories.
Equal-person annual means require 80% eligible-member coverage. Equal-active-year
period means require all seven annual role counts and 80% valid eligible years.
No staff is not zero qualifications. Indices require three active years and all
four components: prior role years, current school-role spell, university share,
and teaching-title share (HS-specific for teachers). Z-standardize components,
form/standardize two equal-component blocks, then equally weight/standardize
the blocks. Fit PCA to the same staff-only components as a sensitivity.
Reported period-average school tenure is excluded after the 2019 coding-break
audit; the preserved raw-reported field and 2018 baseline remain separately
available. This supersedes using period-average reported tenure as a core input.

The output contains 876 measure-outcome associations using saved All-sample EB
VA, unshrunk counterparts, and 288 alternative-support index associations.
Adjusted regressions standardize the staff variable and VA within complete cases
and include log size and its square plus 2024 categorical school context.
Use exact HC1 regression covariance, t inference and role-wide BH adjustments;
do not infer causal staff effects or wage percentages from standardized betas.
The decision record supplies all controls, support definitions, ranks/weights,
PCA sign conventions and the separately fitted teacher-history-since-2016 check.

### Leadership-only extension (2026-09-10)

The same school-window construction is now implemented separately for primary
or secondary leadership codes 3/4/10/15, with no HS-teaching requirement.
The balanced score uses prior primary leadership years, current any-leadership
school spell, university qualification and teaching qualification. It covers
3,461 schools; the sensitivity truncating BOTH history components to 2015 covers
3,447. Capacity is separate, not included in the qualifications/experience index.
There are 504 main measure-outcome associations and 144 robustness rows; BH is
within the leadership family (or robustness subsample), with the same HC1 model
and contemporary 2024 controls. These are descriptive, not causal quality scores.
Full coding-break treatment, normalization and checks are in
`decisions/2026-09-10-leadership-characteristics-va.md`.
