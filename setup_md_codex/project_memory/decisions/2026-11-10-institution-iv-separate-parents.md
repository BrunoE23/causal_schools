# Institution IV: one parent at a time

User requested one parent at a time after the joint-parent interaction IV.
Estimate two models: father only and mother only. Each includes the selected
parent's main effect, attended institution VA, VA x parent, expected VA and
expected VA x parent, plus the paper's cohort/sex/age indicators and linear
grade-4 math/verbal. Omit the other parent's main effect and all its interactions.
Instrument VA and VA x parent with assigned VA and assigned VA x parent.

Keep the preceding 94,244-person common sample and centering/scaling exactly,
so comparisons with the joint specification are not sample-composition changes.
Outcome is high_inst_m1, retaining non-enrollee zeros, the paper's lottery-risk
and exam-taking restrictions, existing parent imputations and fixed estimated VA.
HC1 inference with normal/chi-square tests follows the preceding IV and paper.
Independently reconstruct each 2SLS coefficient vector, structural residuals,
HC1 covariance and instrument moments; verify design rank and source hashes.
Report standard first-stage joint Wald F statistics, not mislabeled as
conditional weak-identification statistics. These are separate specifications,
not causal effects of parental schooling or formal tests comparing model coefficients.

Script: code/codex/premium_enrollment_scores_income/25_institution_iv_separate_parents.R.
Output task: institution_iv_separate_parent_interactions in clean data/reports.

Completed on all 94,244 prior observations. Father-only VA effect at mean father
education: 6.40267 pp (HC1 SE 1.19314) per +10 pp VA. Father interaction:
0.57467 pp (0.45742), p=0.20899. Mother-only VA effect: 6.84326 pp (1.19396).
Mother interaction: -0.78364 pp (0.46847), p=0.09438. Both are smaller in absolute
value than the joint-parent estimates and neither rejects zero at 5%.
Both projected designs have full rank 15; independent 2SLS, structural residual,
HC1 covariance, moment and source hash checks pass. First-stage robust joint
F values: father model 419.75 (VA), 86.39 (interaction); mother model 422.30
(VA), 74.38 (interaction). No new sample or data exclusions.
