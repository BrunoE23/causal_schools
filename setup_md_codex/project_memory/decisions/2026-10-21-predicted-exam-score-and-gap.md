# Predict HE outcomes from baseline-predicted exam scores and realized gaps

The user requested an exercise on the two cohorts with grade-8 SIMCE,
constructing predicted PAES performance b_i and realized-minus-predicted
performance g_i, and regressing HE application and premium-field outcomes
jointly on b_i and g_i. The user clarified that the existing paper master
database should supply the outcomes.

Use grade-8 cohorts 2017 and 2019 from univ_gr8_df.csv, with timely
entrance exams in admission years 2022 and 2024. These are the broader
administrative cohorts; only 2019 belongs to the current 2018-2020 SAE
applicant sample. Retain both entrance-exam components, both grade-8
SIMCE scores and complete baseline controls including middle-school ID.

The score is a math-verbal composite, with components standardized
within admission year and the composite standardized to SD one within
year. This allows comparison of PDT 2022 and PAES 2024 without mixing
their raw scales. This follows the earlier composite-score framing,
stated to the user before fitting. It uses the master's best-available
scores, not strictly regular-session-only PAES.

The prediction equation includes cubic grade-4/8 math and reading scores,
the existing imputed parent/background controls, middle-school GPA and
attendance, and fixed effects for cohort, gender, age, home comuna,
middle-school observed years and most-attended middle-school RBD.
There are no high-school identifiers or HE outcomes in this equation.
Five deterministic student folds generate out-of-fold predictions; each
student's score is excluded from its own prediction training sample.
Unseen fixed-effect levels are counted explicitly as missing predictions.
Keep b_i and g_i in actual-score SD units; do not normalize them separately.

HE application is !is.na(year_1st_app), the existing master flag for
any observed regular centralized university application in 2021-2026.
It is neither financial-aid application nor every direct/IP/CFT
application. Its constructor uses the first observed regular application
and the master left-joins it to the full student universe. Premium field
copies high_paying_field_m1 at first observed enrollment, with existing
zeros for non-enrollees and genuine missing values preserved. First
enrollment is observed in 2022-2025. Follow-up differs between cohorts.

The two requested second-stage linear-probability regressions contain
an intercept, b_i and g_i only, on a shared outcome-complete sample.
Use middle-school-clustered HC1 covariance, G/(G-1)*(N-1)/(N-3), with
t(G-1) inference. These are regression-derived conditional SEs given the
generated predictors, rather than full first-stage bootstrap inference.
The gap is a prediction residual, not a causal estimate of school gains.

Code 07/08, a README and the report are under the existing
simce4_exam_predictiveness task. Preserve all earlier scripts and outputs,
the preference report, and the paper master database unchanged.

## Results and validation

The complete predictor sample contains 234,140 students. Exclude 353 with
fixed-effect levels unseen in their fold's training set, leaving 233,787:
109,764 in 2017 and 124,023 in 2019. Both outcomes are observed for the
entire retained sample, spanning 5,647 middle-school clusters.

Cross-fitted prediction R-squared is 0.7203 pooled, 0.6922 in 2017 and
0.7449 in 2019. Application slopes on b_i/g_i are 0.18953/0.18610
(clustered SEs 0.00185/0.00208); premium-field slopes are 0.14230/0.12490
(0.00231/0.00210). Equality of the two slopes has p=0.105 for application
and p<0.001 for premium field. Outcome means are 0.7468 and 0.4305.
Construction passes 22 checks and independent verification passes 52,
including exact decomposition, held-out prediction reproduction and
independent reconstruction of both clustered covariance matrices.

## Logistic replacement requested by the user

The user subsequently requested logistic regression instead of the LPM.
Reuse the same cached 233,787 students, b_i/g_i values, score units and
master outcomes. Fit binomial-logit models with an intercept and b_i/g_i
only; preserve the prediction stage and historical LPM models unchanged.

Use the logit score sandwich with middle-school clusters and the same
G/(G-1)*(N-1)/(N-3) correction and t(G-1) inference. Average marginal
effects are beta_k times mean[p_i(1-p_i)]. Compute their standard errors
by the delta method using the entire clustered coefficient covariance,
including the derivative of the average logistic weight. These are local
average derivatives, not finite changes from raising a component by one SD.
The coefficient-equality test is primary; equality of the two AMEs is the
same null because both share a common positive multiplier.

Script 09 fits the two logits from the saved scores and records input
preservation hashes. Script 10 verifies the likelihood, score sandwich,
AME derivatives and covariance independently and updates the current
predicted_score_and_gap.html report. Its main table shows AMEs in
percentage points and a collapsed table retains raw logit coefficients
and odds ratios. The prior LPM report and table are archived as `_lpm`;
script 08 writes those archive paths on reruns.

The two logits converge in five and four iterations. Average marginal
effects on b_i/g_i are 0.21708/0.19401 for HE application (clustered
delta-method SEs 0.00127/0.00160) and 0.13864/0.12355 for premium-field
enrollment (0.00220/0.00207). The b_i coefficient is larger for both
outcomes; coefficient-equality p-values are 3.75e-29 and 1.11e-7.
This changes the application's comparison relative to the preceding LPM,
where equality was not rejected. Fit validation passes 14 checks and
independent logit/AME verification passes 28, including numerical
finite-difference verification of the marginal-effect gradients.
