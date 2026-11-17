# Reuse grade-4-SIMCE income decile in school-preference models

## Decision

For the school-preference extension, add the previously constructed
grade-4-SIMCE parent-survey income-decile composition measure. Do not build an
income measure from the 2022 2nd-year-high-school SIMCE survey.

## Construction and sample

Reuse `income_decile_mean` from
`data/clean/staff_lasso_va/school_student_composition.csv`. That school-level
measure averages the established observed/imputed `income_decile_imputed`
variable for 2017-2020 grade-8 students attending each RBD. Standardize its
destination-school values with equal school weights in the final pooled
sample. Exclude an entire ranking if a required listed-school measure is
missing; do not shorten submitted lists. In the current run it is observed
for every ranking in the 273,184-applicant parent-education sample.

## Model

Include income decile alongside the four EB VAs, PAES M1, parent education,
distance, two school-type indicators and all seven attribute-by-type
interactions. Compare the seven-attribute model with the matched six-attribute
parent-education model using the same applicants. Estimate pooled and four
priority-by-continuation groups under published student coordinates and
comuna-centroid distances, with student-clustered standard errors.

## Interpretation

The school income-decile mean is constructed from grade-4 SIMCE reports of
students later attending the school. It is a retrospective, descriptive
composition measure for the 2018-2020 SAE application cohorts, not necessarily
information families had at choice time and not a causal peer effect.

The pooled published-coordinate total utility slopes per school-level SD are
-0.0796 (SE0.0076) for non-TP, 0.0176 (SE0.0117) for mixed HC/TP and -0.0262
(SE0.0153) for TP-only schools, conditioning on parent education, PAES M1,
the four VAs, school type and distance. Results and reproducible scripts are
documented in `code/codex/school_preference_va/README.md`.
