# Add linear entrance scores to the parent-only enrollment model

The user requested math and verbal PAES scores after choosing continuous father
and mother schooling years, the full student sample, and no other controls.
Add only linear standardized math and verbal to that model. There are five
parameters including the intercept; no year effects, income, region, cubic
terms, interactions, or high-math restriction. Score normalization remains the
existing broad-panel within-admission-year PSU/PAES scale, not a new sample scale.

Reuse the two small analysis caches. All original parent-sample IDs, cohorts,
schools and outcomes match. Requiring both observed scores retains 569,681 of
775,268 students, excluding 205,587, and gives 3,373 school clusters. Parent
years retain the existing SIMCE coding and imputations. Refit a parent-only
model on exactly the same scored sample to separate adding regressors from
the change in sample. Enrollment is 82.6829%; 5,167 admission-2026 records are
retained without adding a year effect or changing their recorded outcomes.

Main coefficients in pp: father +0.206175 per year (SE 0.023447), mother
+0.094201 (0.025574), math +2.751644 per SD (0.074286), verbal +3.528961 per SD
(0.080300). Same-sample parent-only coefficients are +0.648314 and +0.538356.
R2 rises from 0.0073393 to 0.0261886. These are descriptive conditional
associations. CR1 school-clustered covariance and t(G-1) inference are unchanged;
independent normal-equation and sandwich checks pass for both fits. Source
checksums are unchanged. Output task: parent_education_linear_scores_he_enrollment.
