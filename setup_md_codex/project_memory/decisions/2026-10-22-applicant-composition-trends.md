# SAE applicant composition trends

## Request and sample

Construct school-cohort baseline SIMCE, female share and parental schooling means
and test time trends in the 100 schools with most applications. Bruno explicitly
requested no multiple-testing adjustment and then requested counts of schools
with at least one, exactly two, and all three significant trends.

Use the existing 294,198-person paper applicant roster for SAE 2018-2020 (timely
grade-9 applicants, ages 12-16 at grade 8, observed attended HS). Rank genuine
submitted school choices from raw C1, excluding automatic continuity entries,
retaining priorities and deduplicating applicant-process-RBD. Fix the pooled top
100 across years. This corrects the application definition for this task only:
RBD 4977 replaces RBD 52 in the earlier top 100. There are 212,608 pairs, 86 schools
with three cohorts and 14 with two. Existing upstream files remain unchanged.

## Measures and inference

Primary SIMCE is the average of both grade-4 subject z-scores already standardized
within test year, without rescaling the composite. Female is GEN_ALU = 2 among
codes 1/2. Parent education averages available reported mother/father years per
student and then across students; report coverage explicitly. Component measures,
both-parent reports, existing parent imputations and raw SIMCE points are checks.

Primary estimation uses equal-cohort OLS of each school mean on SAE year. Ordinary
lm standard errors use G - 2 degrees of freedom: one for three cohorts, zero for
two. Do not invent p-values for two-cohort or constant-mean cases. Export all slopes.
Supplementary student WLS weights each observation by 1/n in its school/cohort/
outcome. Its slope equals equal-cohort OLS exactly; use regression-derived HC2
covariance and t(N - 2) p-values under independent-applicant sampling. This is a
different uncertainty model, without cohort shocks or origin-school dependence,
and must remain separately labeled. No adjustment of p-values is performed.

Use raw p < .05 on the three primary measures for school counts. Distinguish
exactly two from at least two. Schools without any available test are not put in
the zero-significant group. Constant gender outcomes receive no artificial p-value.

## Results and interpretation

Cohort-mean OLS: 8/86 schools have at least one trend, all exactly one; none have
two or three. Fourteen schools cannot be tested. Independent-applicant inference:
81/100 have at least one, 25 at least two, 2 all three; exact 1/2/3 counts 56/23/2.
The large discrepancy reflects G=3 versus independent individual sampling; it is
not a difference in estimated annual slopes. Cohort OLS has very low power.

Cross-school 2018-2020 correlations are .899/.978/.968 for SIMCE/female/parents;
median absolute changes .0468 average subject-z units/2.062 pp/.437 education
years. Parent schooling mainly falls 2018-2019, then partly recovers. Missingness
and baseline questionnaire years need auditing before interpreting the full
parental-education shift as applicant-population change. Stable relative ranks
do not establish stable levels, and these descriptive results identify no peer effect.

## Reproducibility

Code: `code/codex/applicant_composition_trends/01_build_panel.R` through
`04_verify.R`. Local clean data: `data/clean/applicant_composition_trends/`.
Report: `output/reports/applicant_composition_trends/report.html`.
