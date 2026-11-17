# Parent education interactions with attended-school VA

User requested interacting both parents' continuous education with attended HS
VA, following the two-outcome parents+PAES versus full-baseline comparison.
Fit four OLS models: field and institution outcomes, each with both prior control
specifications plus the corresponding VA main effect and both parent-by-VA terms.
Use field VA for high_paying_field_m1 and institution VA for high_inst_m1.
Use existing 2017-2020 All-sample observational EB VA, column
controlled_value_added_eb_centered_student, matched by most_time_RBD to school_rbd.
Do not restrict to SAE offering schools or reestimate VA.

Start from the previous 559,914-student complete-baseline sample, retain common
finite support for both VA measures, and audit any exclusions. Non-enrollees
remain zero; no high-math or HE-enrollment restriction. Preserve baseline
imputations and original score standardizations. Both PAES scores remain linear.
The full model uses the exact previous baseline specification, including cubic
grade-4 scores and middle-school effects; no destination-HS fixed effects.

Center parents at their common-student-sample means and each VA at its own
student-weighted sample mean. Scale VA by 0.10 probability units, so interaction
coefficients reported in pp describe the change in the parent-year gradient
per +10 pp VA. This rescaling/recentering preserves the fitted model. The parent
main effects apply at average VA, and the VA main effect at mean parent schooling.
Save original VA and centering constants. Report parent gradients at empirical
student-weighted VA quartiles using the full linear-combination covariance.

Use attended-HS clustered CR1 inference with exact full baseline-FE degrees of
freedom; t(G-1) for individual tests and F(2,G-1) for the joint interactions.
Independently verify sandwich covariance and OLS normal equations. Inference
treats estimated VA as fixed. VA uses overlapping cohorts/outcomes rather than
cross-fitting; these are descriptive conditional associations, not causal effects.
Script: code/codex/premium_enrollment_scores_income/22_parent_education_va_interactions.R.
Output task: premium_parent_education_va_interactions in clean data and reports.

Completed: both VAs retain the same 559,819 students and 3,276 schools. The 95
excluded schools each contribute one student in the preceding sample. Father
and mother means are 12.40105 and 12.47400 years. Student-weighted VA means in
probability units are 0.00004039 (field) and 0.00037879 (institution).

Interactions (pp per year per +10 pp VA), father/mother respectively:
field parents+PAES -0.49569/-0.43961; field full -0.01894/-0.01277;
institution parents+PAES 0.15577/0.14501; institution full 0.13081/0.10472.
Joint interaction p-values: field simple 2.02e-18, full 0.82461; institution
simple 5.45e-17, full 4.65e-16. Full-baseline institution parental slopes at
the 25th/75th VA percentiles are 0.04999/0.21301 pp per father year and
0.01465/0.14516 pp per mother year. All four OLS/covariance audits pass,
input hashes remain unchanged, and common sample/outcome support checks pass.
