# PAES math score and high-premium field enrollment

Run `01_plot_paes_math_field.R` from the repository root with R, data.table and
ggplot2. Uses selected original columns of the saved current main-IV frame
`data/clean/peer_lottery_variation/main_regression_df_with_peer_shocks.csv`.

The unit is an individual student, not a school VA estimate. Sample: timely
at-risk SAE applicants in processes 2018-2020 with observed
`z_year_math_max` and `high_paying_field_m1`. No IV-control or baseline-score
completeness restriction is imposed. Math is the existing project score
standardized within test year, pooling PAES/PDT score vintages. The first
recorded 2022-2025 high-premium field enrollment indicator includes zeros for
non-enrollees, consistent with the main outcome. It is not conditional on
college enrollment or restricted to college applicants.

The main plot shows mean math and the enrollment share in 50 rank bins,
keeping equal scores together. OLS is fitted to all individual observations
with equal student weights and no controls. Displayed R-squared and slope refer
to the individual-level model. No inferential band is added. The individual
binary-outcome scatter is also saved; no jitter is used. The relationship is
descriptive, with the existing unequal first-enrollment observation window.

PNG/PDF outputs: `output/figures/paes_field_scatter/`. Aggregate bins and fit
summary: `data/clean/paes_field_scatter/`. Checks verify unique students,
sample flags, valid binary outcomes, non-enrollee zero coding, exactly 50
nonempty bins, and preservation of sample size and the overall outcome mean.

## Broad-panel version

```powershell
$env:SCATTER_SAMPLE = 'broad_panel'
& 'C:/Program Files/R/R-4.4.1/bin/Rscript.exe' code/codex/paes_field_scatter/01_plot_paes_math_field.R
```

This uses the established 931,547-student broad panel: Grade 8 cohorts
2017-2020, Grade 8 age 12-16, positive observed most-time high-school RBD,
unique mrun, including private schools. No SAE or baseline-completeness
restriction is imposed. Score/outcome missingness is the only subsequent
filter. It reads selected universe columns and joins the existing small
`program_info_22-25.rds` by the unique program code to recover generic career
area. The exact `derive_high_paying_field` helper and dependencies are imported
from the existing person-outcome constructor without running its pipeline.
The reconstructed binary is checked against all 162,633 students in the
previous at-risk input; every value and missingness pattern must agree.

PAES/PDT math is standardized using the **broad panel's** within-test-year
mean/SD, saved in a separate audit. The earlier saved main-IV frame implicitly
uses an SAE-only score-standardization population; this reference-population
difference is explicit here. No raw scores are pooled across scales.
Outputs go under `broad_panel/` subdirectories, preserving the at-risk figures.

## Broad panel by gender, field and institution

Run `02_plot_broad_panel_by_gender.R` after the broad-panel version above.
It uses the same broad panel and saved pooled test-year score means/SDs;
scores are not separately standardized within gender. `GEN_ALU == 1` is male
and `GEN_ALU == 2` female. Of 645,903 score-observed students, one has an unusable
gender code, leaving 304,610 male and 341,292 female students.

Both outcomes use the canonical person-outcome helpers. High-premium institution
means the institution's centered MiFuturo FE exceeds 0.1 log points, keyed by
institution code/type/normalized name as in the original constructor. Observed
non-enrollees and enrolled institutions without an estimated FE are zero,
matching the current main outcome. Each reconstructed outcome must match all
162,633 students in the saved main-IV frame exactly, including missingness.

Points are gender-specific means within 50 common pooled-score rank bins.
Separate OLS lines use every individual's binary outcome within gender,
with no controls. Displayed axes are common across outcomes and genders;
the bin display does not change the individual-level regression. The combined
PNG/PDF and separate field/institution PNGs are saved under figure
`broad_panel/by_gender/`. Fits, bins, gender coverage, score normalization,
and main-outcome/previous-pooled-fit validation CSVs are saved in the analogous
clean-data folder. Checks verify that the preceding pooled field slope/N/R2
are reproduced before excluding the unusable gender code, and that every
group's bin counts and weighted means reproduce its fitted sample.

## Logit versus linear fit by gender

Run `03_compare_logit_by_gender.R`. It reuses the compact validated individual
input `gender_scatter_input.rds` now exported by script 02; if absent, it runs
02 to reconstruct that input using all its preceding checks. Rerun 02 first
if the upstream data or definitions change. The sample, outcomes, pooled
within-test-year score scale and 50 bins are identical to the preceding gender
figure (645,902 students, each with both binary outcomes).

For each gender and outcome it fits individual-level `glm(enrollment ~
math_score, family = binomial(link = "logit"))` alongside the original OLS
specification, without controls or additional sample restrictions. The figure
overlays full-sample logit (solid) and OLS (dashed) curves on the existing bin
means. No causal interpretation or inferential bands are attached.

Prediction validation separately fits both models on 80% of students and
evaluates squared error on the remaining 20%. Seed 20261106 assigns balanced
five-way folds within gender after sorting student IDs; fold 1 is held out.
The same student split applies to both outcomes. Loss uses the binary outcome
in units 0/1 and unmodified predictions; linear predictions outside [0,1] are
audited rather than clipped. Thus the OLS metric is raw squared prediction
error, while logit's is also a valid Brier score. This is a single random
student holdout, not out-of-school or out-of-cohort validation. No AIC or
pseudo-R2 comparison between different likelihoods is used.

Institution held-out MSE falls from 0.123829 to 0.120169 for males (2.96%) and
from 0.115953 to 0.113499 for females (2.12%). Field MSE is essentially unchanged:
0.239632 versus 0.239633 for males, 0.186161 versus 0.186244 for females. Logit
therefore improves the institution approximation; there is no held-out squared
error gain for field in this split. Institution linear predictions are outside
[0,1] for 9.84% of male and 8.83% of female test students.

Outputs are under `broad_panel/by_gender/logit_comparison/` in the figure and
clean-data folders: combined PNG/PDF, full-sample coefficient and range audit,
fitted curves, held-out losses, fold counts and reproducible student assignments.
Coefficients are on probability units for OLS and log-odds units for logit;
they should not be compared directly as marginal effects. Assertions reproduce
the preceding OLS sample sizes, coefficients and bin totals and verify logit
convergence and valid probabilities.

## Logit curves by school VA group and gender

Run `04_plot_logit_by_va_group_gender.R`. This retains the preceding broad-panel
score scale, outcomes and gender samples, including private schools. It links
the compact individual input to `most_time_RBD` via a four-column read of the
universe, reproducing the broad-panel filter and first-mrun deduplication.
The preceding sample contains 645,902 students. School VA is missing for 20
field observations (15 male, 5 female) and 15 institution observations (11 male,
4 female), leaving 645,882 and 645,887 respectively. No score reference or bin
boundaries are recomputed after this exclusion.

School quintiles use the outcome-specific All-sample `va_raw` from saved
`stata_va_eb_input_highpay.csv` and `stata_va_eb_input_highinst.csv`, before
centering or EB. This follows the user's earlier authorization of that separate
Stata run; these are not the current unified R/EB estimates. Raw values and
VA-regression student counts are verified against the corresponding Stata EB
outputs, and the raw-minus-centered location shift must be constant. The raw
normalization changes levels, not ranks; shrinkage could change ranks.

Define quintiles once across all schools with finite All-sample VA, separately
by outcome, with equal school weights and `ceiling(5*average_rank/N)`, keeping
ties together. The reference is 3,680 field schools and 3,682 institution
schools, including private schools. Do not re-form quintiles within gender,
score-observed students, or a reduced set of matched schools. Collapse Q2-Q4
into a single middle group. Student Ns therefore need not be 20/60/20 or equal
across gender/outcome. Each outcome uses its own school ranking.

The figure contains separate male/female panels and three independent
individual-level binomial-logit curves for each outcome. There are twelve
models, all `enrollment ~ math_score`, without controls. Points are group/gender
means in the same preceding 50 pooled-score bins, and curves span each cell's
bin-mean score range. Colored N annotations count students in that panel/group;
school counts are also saved in the CSV. The relationship is descriptive:
school VA is estimated using these cohorts' outcomes, so separation of curves
is not an independent causal validation or an out-of-sample exercise.

Combined PNG/PDF and separate field/institution PNGs go to figure
`broad_panel/by_gender/raw_stata_va_groups/`. The analogous clean-data folder
contains school quintile/group assignments and definitions, matching coverage,
student/school group Ns, partition checks, classified individual RDS, observed
bins, model coefficients and fitted curves. Assertions verify unique school
and student/outcome keys, full school-ID linkage, conserved group and bin Ns,
preserved outcome means, logit convergence and bounded probabilities.

### All five VA quintiles separately

```powershell
$env:VA_GROUPING = 'quintiles'
& 'C:/Program Files/R/R-4.4.1/bin/Rscript.exe' code/codex/paes_field_scatter/04_plot_logit_by_va_group_gender.R
```

This fits twenty models: Q1, Q2, Q3, Q4 and Q5 separately within each outcome
and gender. It uses the identical school quintile definitions, student samples,
score reference and common bins. Every panel labels all five student Ns.
Outputs go to `raw_stata_va_quintiles/` rather than `raw_stata_va_groups/`,
preserving the preceding three-group figures and diagnostics. The default
`VA_GROUPING=three_groups` retains that earlier version. All partition, bin,
outcome-mean and model checks adapt to the requested number of groups.

### School VA terciles and score-observation diagnostic

```powershell
$env:VA_GROUPING = 'terciles'
& 'C:/Program Files/R/R-4.4.1/bin/Rscript.exe' code/codex/paes_field_scatter/04_plot_logit_by_va_group_gender.R
& 'C:/Program Files/R/R-4.4.1/bin/Rscript.exe' code/codex/paes_field_scatter/05_diagnose_score_selection.R
```

T1-T3 are genuine school terciles: `ceiling(3*average_rank/N)` across the same
finite-VA school reference, separately by outcome, before gender or score
filtering. They are not a relabeling of the preceding Q1/Q2-Q4/Q5 groups.
The individual samples, pooled score scale, 50 bins and no-control logit
specification are retained. Twelve models and their usual checks are saved
under `raw_stata_va_terciles/`, including `school_va_tercile_definitions.csv`.

Script 05 reads ten selected universe columns and reconstructs the full
931,547-student broad panel, keeping students without an admission score in the
denominator. Math observation exactly reproduces the preceding valid-score
and test-year-reference rule, with student ID/gender/score parity checked
against the cached plot input. The diagnostic retains known male/female
genders, uses the same school terciles, and checks each curve's N against the
corresponding count of score-observed students in the full denominator.

Diagnostic CSVs report observation rates overall, by Grade 8 cohort, school
VA tercile, Grade 4 baseline-math quintile, and their relevant combinations.
The five baseline groups use average ranks over all finite Grade 4 math values
in the full broad panel, with common cutoffs across genders and a separate
missing-baseline category. Saved tables also report positive math-or-language
score coverage, baseline math means among score-observed/unobserved students,
and recorded first higher-education enrollment rates by score-observation
status. These enrollment summaries use the program-code presence indicator,
not a claim about persistence or completion. All missing-baseline students are
included in overall rates; the diagnostic figure displays the five observed
baseline groups only.

These checks describe selection into an observed score. Missing scores do not
identify school dropout, college attrition, or why a test record is absent.
Because the missing students' PAES scores are unobserved, different coverage
rates cannot establish either the size or direction of slope selection bias.
No imputation, inverse-probability weighting, or causal interpretation is added.
Outputs are under `raw_stata_va_terciles/score_selection/` in clean data and
figures. The main curves still use the score-observed sample.

Observed math coverage is 64.35% among 473,379 males and 74.49% among 458,167
females; one broad-panel student has an unusable gender code. Coverage is
47.00% versus 60.76% in baseline-math Q1, and 87.51% versus 91.00% in Q5.
The male gap persists in each Grade 8 cohort. Within field-VA T1/T2/T3,
male coverage is 51.03/67.10/69.62%, versus 62.94/75.38/82.34% for females.
Among students with observed baseline math, scored males average +0.265 SD
and unscored males -0.411 SD; female counterparts are +0.151 and -0.461 SD.
This supports differential selection as a plausible contributor, without
identifying its effect on the conditional field-score slope.

Interpretation: logit nonlinearity itself can produce nonparallel probability
curves even with a common score coefficient and additive VA-group shifts:
the score derivative is beta*p*(1-p). These figures fit separate group slopes,
but do not establish causal VA-by-ability interactions. PAES is measured after
high-school exposure, and school VA also uses these cohorts' outcomes.

## Simple pooled logit for DGP calibration

Run `06_fit_simple_pooled_logit.R` to estimate each choice outcome on math
score alone, pooling genders with no gender controls, school effects, VA groups
or other covariates. It reuses the preceding 645,902-student broad-panel plot
sample and pooled within-test-year score scale. Non-enrollees retain zero
outcomes. This is a descriptive association for rough DGP calibration.

For `Pr(H=1|Y) = logistic(alpha_H + beta_Y*Y)`, field estimates are
alpha_H=-0.5073755 and beta_Y=0.5930070; institution estimates are
alpha_H=-1.8535453 and beta_Y=1.097863. Beta is a log-odds coefficient per one
score SD, not a probability-point slope. Average marginal effects are about
12.93 and 12.97 percentage points per SD, respectively. Coefficients, odds
ratios, marginal effects and Ns are saved to clean-data
`broad_panel/by_gender/simple_pooled_logit/pooled_logit_coefficients.csv`.
Checks verify sample uniqueness, domains, model convergence, bounded fitted
probabilities and agreement of fitted and observed outcome means.
