# Joint parent education and HE enrollment regression

The user returned to the regressions and requested father and mother education
years as predictors of higher-education enrollment. Continue the latest
specification, as stated in the task commentary: original standardized entrance
math >= 1 SD, nine full bivariate cubic math/verbal terms, income indicators
with decile 5 omitted, admission-year indicators, and PAES home-region controls.
Use the existing region-linked 93,423-person sample, with parents entering
jointly and linearly. This is a descriptive conditional association, not an
identified parental-education causal effect.

Use the separate baseline grade-4 SIMCE `father_educ_years` and
`mother_educ_years` variables. The year-specific mapping in
`code/clean_simce_survey.R` was inspected: years of basic and secondary schooling
are harmonized; vocational/incomplete tertiary categories use approximations,
and completed university, master and doctorate are assigned 16, 18 and 21.
Retain this existing approximate-year measure and explicitly label its units.
Do not interpret the categorical conversion as exact individual schooling
duration or introduce a different coding silently.

The main model uses the existing reported-plus-imputed baseline variables,
preserving observed values. Imputation follows the earlier baseline-context
median hierarchy; no new imputation is performed. All 93,423 IDs match the
universe; 19 still lack at least one parental measure. This leaves N=93,404
in 2,734 schools. Both reported values are available for 80,483, used as a
sensitivity check. A second sensitivity check excludes the admission-2026
recorded-zero enrollment cell, retaining N=92,565. The main model preserves
that cell to match the prior OLS convention, with its limitation documented.

Report coefficients and school-clustered SEs in percentage points per additional
approximate schooling year. CR1 uses G/(G-1)*(N-1)/(N-K), including all parameters,
and t(G-1) inference. Independently verify the clustered sandwich and normal
equations. Refit a no-parent model on each identical sample to measure the
parent block's incremental R2, without attributing shared explanatory power.

Results: main father -0.000879 pp (SE 0.046566; p=0.98494), mother -0.108436 pp
(SE 0.046100; p=0.01873). Enrollment is 90.8773%, and R2 is 0.197607 versus
0.197554 without parents; the parent block adds 0.0270% of explained variation.
Joint parent p=0.0410; mother-minus-father p=0.1594. On reported-only data,
father is -0.019747 pp (p=0.67758) and mother -0.090013 pp (p=0.05272).
Excluding 2026 gives -0.000562 and -0.110413 pp, respectively. The sign of the
conditional mother coefficient should not be read as a total causal effect.

The nine-column selected read of the 1.23 GB universe is cached under
`data/clean/parent_education_he_enrollment/`; source size/mtime and checksums of
small source files remain unchanged. All three covariance/normal-equation and
nested-projection checks pass. Preserve previous regressions and publish the
new HTML and LaTeX table under the matching reports/tables task directories.
