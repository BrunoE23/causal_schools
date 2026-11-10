# Premium enrollment on scores and family income

The user chose four OLS linear probability models, with both math and verbal
entrance-exam scores and income-decile indicators (decile 1 reference).
Outcomes are premium institution, premium field, either (inclusive union), and both.

Run from the repository root:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/01_regress.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/02_table.R
```

Requires data.table and fixest. `CAUSAL_SCHOOLS_REPO_WD` overrides the repository.
Reuse the 23 MB broad-panel cache from `all_students_by_attended_va`, before
its school-VA merge. No master reread or plot generation is needed.

## Specification and sample

For each binary outcome, estimate OLS on an intercept, linear standardized math
and verbal scores, income-decile indicators 2-10, and admission-year indicators.
Use original within-admission-year broad-panel score normalization and the
existing imputed baseline household-income deciles; do not re-rank or normalize
in the regression sample. No score-income interactions or school fixed effects.

All four models use a common complete sample from the paper's broad panel
(grade-8 cohorts 2017-2020, ages 12-16, attended school observed). Require both
positive observed test scores and a valid income decile. There is no high-math
threshold, HE-enrollment restriction, or requirement for estimated school VA.
Preserve existing premium flags; non-enrollees are zero for all four outcomes.

Retain admission 2026, whose recorded premium outcomes are uniformly zero.
The earlier logit analysis omitted this cell because of separation; OLS can
include it. Document this inclusion explicitly rather than silently treating
the recorded zeros as evidence about future enrollment.

Standard errors cluster by attended high school (`most_time_RBD`), allowing
arbitrary within-school residual correlation. Use the CR1 correction
`G/(G-1) * (N-1)/(N-K)` with K including intercept and all year/decile indicators;
confidence intervals and p-values use t(G-1). Tables show coefficients and SEs
multiplied by 100, in percentage points.

The fitted sample has 565,040 students in 3,372 attended high schools, including
97,717 non-enrollees and 5,105 admission-2026 records. Each equation has 17
parameters. The input cache is unchanged, and all four regressions pass the
normal-equation, covariance, and common-design coefficient-identity checks.

## Checks and outputs

Check binary flags, union/intersection identities, non-enrollee zeros, common
sample, matrix rank, and OLS normal equations. Independently reconstruct each
clustered sandwich covariance and compare with fixest. The common-X OLS
coefficients must obey `either = institution + field - both`. Save outside-[0,1]
fitted-value rates without clipping predictions.

Models, analysis sample, full coefficients (including year effects), covariance
matrices, confidence intervals, joint tests and sample diagnostics are local in
`data/clean/premium_enrollment_scores_income/`. Rendered table:
`output/reports/premium_enrollment_scores_income/regression_table.html`.
Paper table fragment: `output/tables/premium_enrollment_scores_income/regression_table.tex`.

## Four premium-field groups

The follow-up runs the same specification and covariance on Science, Law,
Engineering/Manufacturing/Construction, and Medicine+. Run:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/03_field_groups.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/04_field_table.R
```

Reuse the exact prior 565,040 student IDs and all covariates. Read only five
master columns to recover the earlier field definitions. Science, Law and
Engineering use their existing first-enrollment flags. Medicine+ is the
intersection of `high_paying_field_m1` and `field_reclassified_m1 == "Medicine"`,
preserving the original medicine, pharmacy, nursing, midwifery, medical
technology and dentistry list. Each indicator is zero for non-enrollees and
students enrolling outside that group. Do not condition on premium-field
enrollment or change the sample or score normalization.

Verify individual-level exclusivity and exact summation to the old field flag.
The four estimated coefficient vectors must sum to the previous premium-field
coefficient vector, including intercept and year effects. Each regression has
its own school-clustered covariance; component SEs must not be added together.

Save the new models, estimates, sample and audits under
`data/clean/premium_field_groups_scores_income/`, and the HTML/LaTeX tables under
the corresponding `output/reports/` and `output/tables/` task folders. Earlier
four-outcome regression outputs remain unchanged.

## Cubic score controls and interactions

The user replaced the proposed score-decile specification with polynomials up
to degree three and math-verbal interactions. Scripts `05_cubic_field_groups.R`
and `06_cubic_field_table.R` fit and present the four field-group OLS models with
the full bivariate cubic basis:
`M, V, M^2, V^2, M*V, M^3, V^3, M^2*V, M*V^2`.
M and V retain the existing standardized scores. This is total degree at most
three, not a tensor-product polynomial through degree six. There are no score
deciles or score-income interactions. Keep all income indicators, admission-year
effects, the exact student sample, and school-clustered CR1 inference.

Run `05_cubic_field_groups.R` followed by `06_cubic_field_table.R` using the same
Rscript command as above. Input is the small saved field-group analysis cache;
no external master reread is needed. Reuse one 24-column design matrix for the
four fits and covariance checks. Verify normal equations, clustered covariance,
nested R-squared improvement, unchanged means/sample, and component additivity
against the aggregate cubic OLS normal-equation solution. Report clustered Wald
F tests for all seven added terms, the three interaction terms, and the four
degree-three terms. Save income-coefficient comparisons to the linear-score
models. Raw polynomial coefficients are not constant one-SD marginal effects.

Outputs use task name `premium_field_groups_cubic_scores_income` under
`data/clean/`, `output/reports/` and `output/tables/`; previous results are retained.

## Logit replacement and fit comparison

The user requested logit after seeing the low OLS R-squared. Run
`07_logit_cubic_field_groups.R` and then `08_logit_cubic_field_table.R`.
Reuse the field-group cache and the full cubic score basis, decile indicators,
and admission-year effects. Restrict all four models to a common set of year
cells with outcome variation in every field. The all-zero admission-2026 cell
has no finite logit year effect and is excluded. Refit the four cubic OLS
comparators on exactly the same remaining sample; preserve prior OLS outputs.

Estimate four independent binary logits, with attended-school-clustered CR1
covariance. Recompute the Hessian/clustered-score sandwich at the converged
coefficients and compare with fixest covariance. Store this reported covariance
separately from the fitted objects, whose package covariance may use nearly
identical weights from the preceding IRLS step. Delta-method intervals use
normal quantiles. Check convergence, score equations and analytic effect
gradients by directional finite differences.

Main tables report probability effects: mean discrete income-decile contrasts
versus D1 over every student's observed scores/year, and average partial score
derivatives incorporating all polynomial terms and the logit link. Score AMEs
are local derivatives per SD, not finite one-SD changes. Raw log-odds coefficients,
odds ratios, full covariance and effect gradients are saved separately.

Fit diagnostics distinguish McFadden pseudo-R2 (likelihood ratio versus an
intercept-only fit), Tjur R2, AUC, and squared-error/Brier R2. The last uses
`1 - mean((Y-p)^2)/mean((Y-mean(Y))^2)` and is directly comparable with same-sample
OLS R2. All metrics are in-sample; no claim about held-out prediction is made.
Independent logits need not have additive fitted probabilities, so save a sum
diagnostic and do not apply the earlier OLS coefficient-additivity check.

Outputs use task name `premium_field_groups_logit_cubic_scores_income` under
`data/clean/`, `output/reports/` and `output/tables/`. No plots are generated.

## Return to cubic OLS: income's contribution to eight outcomes

Run `09_cubic_ols_income_contribution.R` and then
`10_cubic_ols_income_tables.R`. Both use the same Rscript command and working
directory as above. Reuse the two small original OLS analysis caches, matching
unique student IDs and checking every shared variable. Restore the original
565,040 students and 3,372 school clusters, including the 5,105 admission-2026
records. No master reread, new normalization, or plot is needed.

Estimate the full cubic-score OLS model for institution, field, either, both,
Science, Law, Engineering/Manufacturing/Construction, and Medicine+. For each,
also estimate a reduced model dropping only the nine income-decile indicators.
Both retain the same score terms and year indicators on the exact same sample.
Income's reported fraction is `(R2_full - R2_without_income) / R2_full`, expressed
as a percentage. It measures incremental explanatory power after scores/year;
it does not allocate their shared variation and is not a causal contribution.
Use ordinary unadjusted R2. Also save, separately, income's partial R2 with
denominator `1 - R2_without_income`; do not confuse this with the reported share.

Keep actual school-clustered CR1 inference and t(G-1), with K=24 for full models
and K=15 for reduced models. Tables show all income-decile coefficients plus
average local math/verbal derivatives from the cubic polynomial, with their
exact linear-combination clustered SEs. Raw basis coefficients remain in the
HTML and coefficient CSV. Verify normal equations and covariance for both
models, nested-model projection identities, union and component coefficient
identities, and exact reproduction of the previous cubic OLS field estimates.

Outputs use task name `premium_cubic_income_contribution` under `data/clean/`,
`output/reports/`, and `output/tables/`. The HTML report combines the R2
contribution summary and both four-column regression tables. Three LaTeX table
fragments provide the contribution summary, main outcomes, and field groups.
Model objects, covariance matrices, all coefficients, clustered income Wald
tests, source checksums, and diagnostics stay in the clean-data task folder.

## HE enrollment and log projected income

Run `11_income_enrollment_cubic_ols.R` and then
`12_income_enrollment_cubic_tables.R` using the same Rscript command. Reuse the
original OLS sample cache plus the 23 MB broad-panel cache to obtain existing
`log_program_income_full_clp_m1`; no master reread is needed. Verify every shared
covariate and the natural-log income identity after matching unique student IDs.

HE enrollment uses all original 565,040 scored students, including non-enrollees.
Log projected monthly CLP income uses only their HE enrollees, with coverage
audited explicitly. Keep the original score scaling and income deciles. Rebuild
admission-year factor levels and school-cluster counts for each outcome sample.
The full and no-income fits share the exact same students within each outcome.

The specification, school-clustered CR1/t inference, income-share definition,
and verification follow the eight-outcome analysis above. Report log-income
coefficients in log points and HE-enrollment coefficients in percentage points.
Score summaries remain average local derivatives per SD. Income assigned to
non-enrollees is excluded from the log-income model; projected income is not
observed individual earnings. The output task name is
`income_enrollment_cubic_income_contribution` under clean data, reports, and tables.
All previous results are preserved.

The completed income model includes all 467,323 enrollees (3,327 schools), with
no missing projected-income exclusions. Its full R2 is 0.3048 and income's
incremental share is 1.37%. Enrollment retains 565,040 students (3,372 schools),
with full R2 0.1681 and income's share 0.11%. The 5,105 admission-2026 students
are all non-enrollees and therefore appear only in the enrollment model.

### Restrict both outcomes to high math scores

Pass `--high-math` to both scripts:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/11_income_enrollment_cubic_ols.R --high-math
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/12_income_enrollment_cubic_tables.R --high-math
```

This applies the earlier `z_math >= 1` definition to the original complete-score
regression sample, using the original broad-panel within-admission-year scale.
No restandardization or income-decile re-ranking occurs. The cubic controls,
year effects, income-share definition, and actual clustered inference remain
unchanged. Save exact selection diagnostics and sample IDs for both outcomes.

The selected enrollment sample has 93,423 students in 2,735 schools, including
8,522 non-enrollees and 840 admission-2026 records. Its 84,901 HE enrollees all
have projected income and span 2,689 schools. Income's incremental share is
0.42% for projected income (full R2 0.1272) and 0.38% for enrollment (full R2
0.1968). Full/reduced covariance and nested-model checks pass. Outputs are
separate under `high_math_income_enrollment_cubic_income_contribution`; prior
unrestricted results remain unchanged. Omitting the flag retains the original
unrestricted analysis.

## Four main premium outcomes in the high-math sample

Pass `--high-math-main` to scripts 09 and 10:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/09_cubic_ols_income_contribution.R --high-math-main
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/10_cubic_ols_income_tables.R --high-math-main
```

Flagged runs fit only institution, field, either (inclusive union), and both,
using `z_math >= 1` and exactly the same 93,423 IDs as the preceding high-math
HE-enrollment regression. All 2,735 schools and 8,522 non-enrollees remain;
non-enrollees have zero for all four outcomes. Keep the 840 admission-2026
records, original score scaling and income ranks, full cubic score controls,
year indicators, and actual school-clustered CR1/t inference. Full and reduced
models differ only by the nine income indicators. Verify both models' normal
equations/covariance, nested-model projection identities, and the coefficient
identity `either = institution + field - both`.

Outputs are separate under `high_math_main_cubic_income_contribution`, including
the regression table and income-contribution summary. Full R2 is
0.1667/0.0756/0.1308/0.0992 for institution/field/either/both, respectively.
Income's incremental share of explained variation is
33.54%/24.11%/1.62%/6.53%. The field-component regressions are not rerun in this
mode. Omitting the flag preserves scripts 09/10's original eight-outcome mode.

### Add home-region controls and use income decile 5 as reference

Prepare the region linkage, then run the separate region-adjusted mode:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/13_build_paes_origin_region.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/09_cubic_ols_income_contribution.R --high-math-main-region-d5
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/10_cubic_ols_income_tables.R --high-math-main-region-d5
```

Read only MRUN, ANYO_PROCESO, and CODIGO_REGION_DOMICILIO from the six public
2021-2026 PAES/PDT socioeconomic-domicile files under the Box data root.
Match by student and the existing admission year. All 93,423 selected students
match with a valid region code 1-16; no sample exclusion or imputation occurs.
Origin means reported home region at admission, not birthplace or necessarily
grade-8 residence. Preserve the existing baseline family-income classification.

Both full and no-income models include region indicators, the cubic score
basis, and admission-year effects. Full-model income coefficients compare each
decile with D5; region code 1 is the omitted nuisance category. Full K=39 and
reduced K=30. Retain the exact same 2,735 school clusters and CR1/t inference.
Income's incremental share is now conditional on scores, year, and home region.
Independent D1-reference fits verify the D5 coefficient/covariance transformation
and equality of fitted values and R2. The other existing verification checks pass.

Outputs are separate under `high_math_main_region_cubic_income_contribution`.
Full R2 is 0.3297/0.0870/0.1411/0.1741 for institution/field/either/both;
without income it is 0.3101/0.0746/0.1404/0.1736. Income's incremental share
is 5.94%/14.26%/0.48%/0.30%. Save region source/linkage/coverage audits,
reference-category checks, and comparisons with the previous no-region fits
alongside the model objects and existing diagnostics. Prior output modes remain
available and their results are preserved.

## Mother and father education: HE enrollment

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/14_build_parent_education.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/15_parent_education_he_ols.R
```

Keep the latest high-math (`z_math >= 1`), region-linked sample and estimate HE
enrollment on both parents' education years jointly. Controls are the same
nine cubic score terms, income-decile indicators (D5 reference), admission-year
indicators, and home-region indicators. Use attended-school-clustered CR1
inference and t(G-1) confidence intervals. These are conditional associations.

Parent education is from the grade-4 SIMCE parent survey, separately recorded
as `father_educ_years` and `mother_educ_years`, with existing `_imputed` versions.
The project maps response categories to approximate completed years; tertiary
categories are coarse (completed university 16, master's 18, doctorate 21).
Preserve the existing year-specific mapping and baseline-context imputations.
Script 14 preflights the 1.23 GB universe CSV, reads only nine linkage/education
columns, immediately filters to the 93,423 requested IDs, and caches the join.
All IDs match. No new imputation, reranking, or score standardization is done.

The main model uses reported plus existing imputed years, excluding 19 remaining
incomplete records: N=93,404 and 2,734 school clusters. Sensitivity checks use
both parents reported (N=80,483) and exclude the admission-2026 all-zero
enrollment cell (N=92,565, with reported/imputed years). Parent coefficients in
pp per year are father -0.0009 / -0.0197 / -0.0006 and mother -0.1084 / -0.0900 /
-0.1104, respectively. Main-model clustered SEs are 0.0466 and 0.0461 pp.

All raw coefficients, covariance matrices, models, sample IDs, parent coverage,
joint/equality tests, R2 contribution and covariance/normal-equation checks are
under `data/clean/parent_education_he_enrollment/`. The HTML regression table is
in `output/reports/parent_education_he_enrollment/regression_table.html`; its
LaTeX fragment is in `output/tables/parent_education_he_enrollment/`.

### Full sample, continuous parents only

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/premium_enrollment_scores_income/17_parent_education_only_ols.R
```

The user's final specification is HE enrollment on continuous father and mother
schooling years jointly, with an intercept and no other regressors. Use the
full 931,547-person broad panel, before school-VA matching; impose no high-math,
exam-taking, income, or region restriction. Existing reported-plus-imputed
parent years are retained, with remaining missing values excluded. The final
sample is 775,268 students in 3,701 schools; 156,279 lack one or both measures.

Coefficients are father 1.4390 pp/year (clustered SE 0.02885) and mother 1.9187
pp/year (SE 0.03403). Both enter continuously and jointly. Enrollment is 70.72%
and R2 is 0.04330. Of the estimation sample, 663,163 report both education
measures; father values are imputed for 103,243 and mother values for 74,011.
No new imputation is performed. These are descriptive associations.

Use the existing school-clustered CR1/t inference; independently check the
three-column design, normal equations, and sandwich covariance. Code reads
only six columns of the preflighted universe CSV and preserves original sources.
Models, sample, coefficients, diagnostics and provenance are saved under
`data/clean/parent_education_only_he_enrollment/`. HTML and LaTeX tables use the
same task name under `output/reports/` and `output/tables/`. Earlier controlled
high-math regressions remain available in their original directories.

### Add linear entrance math and verbal

Run `18_parent_education_linear_scores.R` using the same Rscript command.
Reuse the small parent-only sample and broad-panel score caches. Add only
`z_math` and `z_verbal`, linearly, to father and mother years plus the intercept.
Keep the existing within-admission-year score standardization; do not add
year effects, income, region, interactions, polynomials, or a high-math filter.

Of the previous 775,268 students, 569,681 have both scores; 205,587 are excluded.
Refit the parent-only comparison on these same 569,681 students and 3,373 school
clusters. Main coefficients in pp are father 0.2062 (SE 0.02345), mother 0.0942
(0.02557), math 2.7516 per SD (0.07429), and verbal 3.5290 per SD (0.08030).
R2 is 0.02619 versus 0.00734 for the same-sample parent-only model. Both retain
school-clustered CR1 inference; normal equations and covariance checks pass.
Save models/audits under `data/clean/parent_education_linear_scores_he_enrollment/`
and HTML/LaTeX comparison tables in the matching reports/tables directories.

### Add baseline math and verbal

Run `19_add_baseline_scores.R` using the same Rscript command. Add the existing
grade-4 SIMCE `z_sim_mat_4to` and `z_sim_leng_4to` linearly to both parent years
and both linear entrance scores. Read only four columns from the preflighted
universe source, matching student IDs and cohorts. Preserve each baseline
score's original SIMCE-year normalization, without imputing or restandardizing.
No other controls or high-math restriction are added.

Requiring both baseline scores retains 564,542 of the previous 569,681 students
(5,139 exclusions), with 3,372 school clusters. Refit the preceding four-regressor
model on the exact same students. R2 rises from 0.0261595 to 0.0265245. Baseline
math is 0.849157 pp per SD (SE 0.088949), and baseline verbal 0.258643 (0.081727).
All seven-parameter/full and five-parameter/reduced OLS and covariance checks
pass. The output task is `parent_education_baseline_scores_he_enrollment` under
clean data, reports, and tables; source inputs and earlier results are preserved.

### Complete VA baseline controls

Run `20_add_va_baseline_controls.R`. Retain linear PAES math/verbal and add the
baseline specification from `school_rbd_observational_values/01_construct_school_rbd_values.R`:
separate cubic grade-4 math/verbal terms; linear income decile; both parent
education years, indigenous indicators and four early-childhood controls;
standardized middle-school GPA and attendance; cohort, sex, age, student comuna,
middle-years-observed and middle-school indicators. Parent education enters once.
Destination high-school effects are the VA object, not baseline controls, and
are not included. There is no high-math or SAE-applicant restriction.

Use four-column/selected-column reads of the universe and middle-school sources
after size preflight. Complete baseline availability retains 559,914 of 569,681
students; refit the preceding model on these exact IDs. School-clustered CR1
inference counts all absorbed baseline effects with `K.fixef="full"` and
`K.exact=TRUE`. Independently reconstruct covariance from demeaned regressors
and actual residuals. Average baseline-score derivatives account for the cubic
terms; raw polynomial coefficients are not marginal effects. Outputs use
`he_enrollment_paes_va_baseline` under clean data and reports.

The final model has 3,371 school clusters and overall R2 0.15695, versus 0.02657
for the previous model on the same sample. Father/mother coefficients are
0.02927/0.15042 pp per year; linear PAES math/verbal coefficients are 2.45565/
3.10338 pp per SD. Average baseline math/verbal derivatives, conditional on all
controls, are -0.31531/-0.86119 pp per original SD. Verification passes.

### Premium field/institution: parents + PAES versus full baseline

Run `21_premium_outcomes_va_baseline_comparison.R`. Reuse the 559,914-person
complete-baseline cache and attach the original `high_paying_field_m1` and
`high_inst_m1` outcomes from the small broad-panel cache. Both outcomes and both
models share exactly these students and 3,371 attended-school clusters, including
non-enrollees coded zero. There is no high-math or HE-enrollment restriction.

The simple model contains only both continuous parent education measures and
linear PAES math/verbal, plus an intercept. It does not include baseline scores.
The full model uses precisely the preceding full VA baseline specification,
including middle-school indicators and separately cubic grade-4 scores. Existing
outcome definitions, score normalizations, and baseline imputations are unchanged.
Verify all source IDs/covariates, outcome support and non-enrollee zeros, common
sample, OLS normal equations and school-clustered CR1 covariance for all four
fits. Use exact full FE degrees of freedom and overall R2 for the full models.

Output task: `premium_parent_paes_va_baseline_comparison` in clean data/reports;
the combined HTML comparison includes key coefficients and all raw coefficients.

Completed R2 comparisons: premium field 0.07833 to 0.14485; premium institution
0.20846 to 0.34570. Outcome means are 38.99% and 18.68%, respectively. All four
sample, normal-equation and covariance checks pass; source caches are unchanged.

### Parent education interacted with attended-school VA

Run `22_parent_education_va_interactions.R` with the same Rscript command.
Extend both previous specifications with the corresponding attended-school VA
main effect and its interactions with father and mother years: field VA for
field enrollment, institution VA for institution enrollment. Match the existing
2017-2020 All-sample EB observational VA by attended RBD, using the complete VA
source rather than the restricted SAE-offering-school plotting sample.

Common support retains 559,819 students and 3,276 school clusters. Exclude 95
students from 95 schools lacking both VAs. Center parents at sample means and
each VA at its student-weighted mean, scaling VA by 0.10 probability units.
Reported interaction coefficients are pp per parent schooling year per +10 pp
VA. Include all lower-order terms; retain linear PAES and the exact previous
full-baseline controls. Preserve non-enrollee zeros and baseline imputations.

Outputs under `premium_parent_education_va_interactions` in clean data/reports
contain four fitted models, all coefficients, common sample, scaling constants,
coverage/exclusion audit, source hashes, exact cluster-covariance checks, and
parent gradients at VA quartiles with linear-combination SEs. The HTML table is
`output/reports/premium_parent_education_va_interactions/interactions.html`.
Inference treats estimated VA as fixed; overlapping estimation cohorts/outcomes
mean these are descriptive associations, not cross-fitted causal effects.

Full-baseline father/mother interaction estimates are -0.01894/-0.01277 pp for
field (joint p=0.82461) and 0.13081/0.10472 pp for institution (joint p=4.65e-16).
All four independent covariance and normal-equation checks pass. Full-model R2
is 0.15859 for field and 0.35925 for institution.
