# Institution IV with parent-education interactions

User explicitly requested the IV specification, using premium-institution VA
and premium-institution enrollment as outcome, interacted with each parent's
education. Extend the paper's September scalar-IV design, jointly including
both parent interactions rather than carrying over the descriptive OLS controls.

First reproduce highinst_adj_eb from the paper cache for VA 2017-2020 / SAE
2018-2020: outcome high_inst_m1, attended VA d_highinst_adj_eb, assigned VA
z_highinst_adj_eb, expected-risk control expected_highinst_adj_eb. Keep timely
SAE, nondegenerate assignment risk, entrance-exam taking and complete paper
variables, as in the saved specification. No HE-enrollment restriction or
PAES-score control. Retain paper zeros for non-enrollees and main controls:
linear grade-4 math/verbal and cohort, sex, age indicators.

Attach both existing imputed baseline parent-schooling variables from the
universe by mrun, checking cohort. Read six columns from the preflighted 1.23 GB
source and immediately filter to the paper sample. Audit missing parent values
and existing imputations; do not impute anew. Center parent schooling at the
included students' means. Center attended, assigned and expected VA using the
same attended-VA mean, and divide all by 0.10 probability units.

Endogenous regressors: D, D*father, D*mother. Excluded instruments: Z, Z*father,
Z*mother. Exogenous controls additionally include both parent main effects and
E*father, E*mother alongside E (expected VA), so each interacted instrument has
its matching assignment-risk control. Do not absorb attended-school effects.
Report coefficients in outcome pp per +10 pp VA and, for interactions, per
additional parent schooling year. Both interactions are estimated jointly.

Follow paper HC1 inference and normal/chi-square tests, treating estimated VA
as fixed. Independently reconstruct 2SLS and HC1 using structural residuals and
projected regressors; check sample, rank and instrument moments. Report joint
excluded-instrument robust Wald F and partial R2 for each first stage, clearly
distinguishing these from conditional weak-identification statistics. IV
interpretation requires the instrument assumptions to extend to interacted
instruments; parent main effects are not causal schooling returns. Preserve
the paper's exam-taking restriction and flag that scope in the report.

Script: code/codex/premium_enrollment_scores_income/24_institution_iv_parent_interactions.R.
Output task: institution_iv_parent_education_interactions in clean data/reports.

Completed: paper IV reproduced at 0.66689517 (SE 0.11981742), N=94,487 and
first-stage F=821.068. Parent availability excludes 243 students; final N=94,244,
with 2,987 attended schools and outcome mean 9.6176%. Existing imputation
counts are 12,254 fathers and 8,133 mothers. Mean parent years are 11.33037 and
11.55520; attended institution VA mean is -0.0308953 in probability units.

At mean parent education, +10 pp VA has an IV coefficient of 6.45662 pp
(HC1 SE 1.20190). Father interaction +1.30744 pp (0.55271), p=0.01801;
mother interaction -1.48555 pp (0.55394), p=0.007323. Joint chi-square(2)=8.39875,
p=0.015005. Robust joint excluded-instrument first-stage F values for D,
D*father and D*mother are 283.410, 57.958 and 49.548, respectively; partial R2
values are 0.03627, 0.02893 and 0.03271. These do not measure conditional
strength separately for the three endogenous regressors.

Independent 2SLS coefficient, structural residual, HC1 covariance and instrument
moment checks pass (maximum errors below 7e-13); projected design has full rank
18. Source checks and paper replication pass. Parent effects and heterogeneity
are conditional on the other parent's education, including their interaction.
