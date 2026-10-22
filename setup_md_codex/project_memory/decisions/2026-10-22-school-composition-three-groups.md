# Three-year school composition for applicants, offers and attendees

## Request and explicit attendee choice

Bruno requested average parent education years, baseline SIMCE, female share and
income decile across the three years for probability-weighted applicants,
lottery-offered students, and actual attendees, saved for reuse. When asked about
attendance, he explicitly selected all students grouped by the high school they
attended most often, rather than SAE applicants only or first-year attendance.

## Definitions

Keep the same top 100 destination schools. The weighted applicant pool is the
existing genuine-choice panel from the 294,198-person timely SAE paper roster.
The main realized-offer group uses that full roster and first-round
`rbd_treated_1R`, regardless of later attendance or deterministic versus stochastic
admission. A separate `offered_at_risk` group restricts realized offers to school-
specific simulated 0 < p < 1. This preserves both readings of lottery-offered.

Actual attendees are all master-universe students with grade-8 cohort 2018-2020
whose `most_time_RBD` equals the target school. Do not restrict to SAE, lottery
risk, offers, ages 12-16, exam takers or complete covariates. These are cohort
composition measures by modal high school, not calendar-year enrollment counts.

Keep reported available-parent schooling years, the arithmetic average of both
grade-4 subject z-scores and female shares as in prior work. Add the existing
`income_decile_imputed` without recomputing decile cutoffs or doing new imputation;
also save observed-only income. Store female means in 0-1 share units.

## Averaging

Save equal-weight averages of available cohort means with explicit n_cohorts;
save strict three-cohort means only where all three exist; and save pooled
student-weighted means over the three years. Weighted applicants use p_school,
actual offers and attendees weight every student by one. Each characteristic has
its own observed denominator. Do not replace empty cohorts with zero or silently
describe two-cohort means as three-cohort means.

## Data and checks

Main offers: 52,019 students, including 403 offers to schools absent from the
genuine-choice panel and 53 offers with zero recorded simulated probability.
Keep those realized offers in the main group and audit the discrepancies.
Strictly stochastic offered subset: 37,454. All attendees: 72,502, of whom 21,087
are outside the existing paper SAE roster; 49 are outside ages 12-16 or have
missing age. All 100 schools have all three attendee cohorts for all four
primary characteristics. Applicant and offered support is less complete.

Scripts 09 and 10 under `code/codex/applicant_composition_trends/` build and
verify the output. Main file:
`data/clean/applicant_composition_trends/group_averages/school_group_averages_2018_2020.csv`
(300 rows). A 100-row school-wide CSV and an RDS with definitions and annual/long
tables accompany it. Raw inputs and earlier outputs are unchanged. Verification
independently checks pooled and equal-cohort averages, missing-year handling,
population uniqueness, and reproduction of earlier weighted composition.
