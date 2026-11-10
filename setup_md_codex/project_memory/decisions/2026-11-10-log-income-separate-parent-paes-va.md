# Parent-specific log income: interact schooling with institution VA

User requested adding parent education x high-premium-institution VA to the
latest PAES-adjusted log projected-income models. Continue separate father and
mother OLS specifications, omitting the other parent. Add attended institution
VA and selected parent x VA, retaining selected parent main effect, intercept,
and linear PAES math/verbal. No IV or extra baseline controls.

Use the identical prior 463,033 enrollees and 3,262 school clusters. Reuse the
existing All-sample 2017-2020 EB premium-institution VA (high_inst_m1_va).
Center parent years and VA at their estimation-sample means and divide VA by
0.10 probability units. Parent main effect is at mean VA; VA main effect is
at mean selected-parent education. Report natural-log points, with interactions
per schooling year per +10 pp VA. Preserve source imputations and score scales.

School-clustered CR1 and t(G-1) inference; estimated VA treated as fixed.
Verify exact prior IDs, checksums, finite regressors, six-parameter single-parent
designs, normal equations, independent cluster covariance and nested R2.
Report parental slopes at VA quartiles using full linear-combination covariance.
These are descriptive conditional associations, using projected rather than
realized income and non-cross-fitted VA. The inherited complete-baseline and
VA-coverage sample restrictions remain in place, with no new exclusions.

Script: code/codex/premium_enrollment_scores_income/28_log_income_separate_parent_paes_va.R.
Output task: log_income_separate_parent_paes_va in clean data/reports.

Completed on 463,033 enrollees/3,262 school clusters. Father interaction
-0.00409881 (SE 0.00025651), mother -0.00463663 (0.00029949), both p<0.001.
Parent main effects at mean VA: 0.00453912/0.00407830. VA main effects at mean
parent schooling: 0.02471332/0.02525300 log points per +10 pp VA. R2 is
0.304292/0.304029 versus 0.298604/0.297916 previously. Parent slopes at the
25th/75th VA percentile are 0.008208/0.002892 (father) and 0.008229/0.002216
(mother). All exact sample, source, design, normal-equation and CR1 checks pass.
