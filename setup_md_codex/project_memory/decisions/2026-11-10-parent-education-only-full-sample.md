# Full-sample enrollment on both continuous parent education variables

The user explicitly chose the full student sample, removed other controls, and
finally requested both parents' schooling years as continuous regressors.
The current model is HE enrollment = intercept + father years + mother years.
There are exactly three parameters. Do not include test scores, income deciles,
region effects, admission-year effects, decile indicators, or a high-math filter.
The earlier controlled high-math results remain saved separately and are not
the current requested specification.

Start from the full broad-panel cache before the school-VA merge: 931,547
students, grade-8 cohorts 2017-2020, ages 12-16, attended school observed. Match
all IDs to six selected universe columns. Use the existing reported-plus-imputed
father and mother schooling years, retaining the documented approximate SIMCE
category conversion. Exclude 156,279 with remaining missing education, leaving
775,268 students in 3,701 schools. There is no exam-taking or income availability
restriction. Both reported years are observed for 663,163; 103,243 father and
74,011 mother values use existing imputations. Do not impute again.

Use OLS linear probability estimation and actual school-clustered CR1 covariance
with G/(G-1)*(N-1)/(N-K), K=3, and t(G-1) inference. Report pp per additional
schooling year; each parent's coefficient is conditional on the other parent's
schooling, and neither is an identified causal effect. No other model is needed
for this request. Independently verify normal equations and the full covariance.

Final estimates: father 1.439020 pp/year (SE 0.028850; 95% CI 1.382457 to
1.495584); mother 1.918718 (SE 0.034026; CI 1.852006 to 1.985430). Both p<0.001.
Enrollment is 70.7159%, and R2 is 0.0432951. The model has 7,993 admission-2026
records and one enrolled student among them; the old scored/high-math sample's
all-zero-2026 statement must not be generalized to this broader sample. Keep
the outcome and year records unchanged; do not add year controls or exclusions.

The selected-column source read and original inputs are preserved; small-source
checksums and large-source size/mtime checks pass. Regression verification also
passes. Output task: `parent_education_only_he_enrollment` under clean data,
reports, and tables. The run script is
`code/codex/premium_enrollment_scores_income/17_parent_education_only_ols.R`.
