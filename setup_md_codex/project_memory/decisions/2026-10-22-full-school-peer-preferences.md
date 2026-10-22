# Extend peer composition to the whole observed school population

The user explicitly replaced the top-100 restriction with a request to
extend the peer measures and proceed with the four-column preference
comparison. Preserve all population definitions and existing VA estimates.

## Peer construction

Reuse full genuine school-level rankings (1,007,253 pairs, 294,198 timely
grade-9 applicants in 2018-2020), not the VA-complete estimation subset.
Recover simulated school probabilities by summing program probabilities;
check all applicants' probability distributions including unmatched mass
before assigning zero to absent destination rows. Exclude automatic
continuity alternatives exactly as in the existing clean rankings.

The destination roster is the union of genuine SAE destinations, positive
recorded first-round offer RBDs and positive most-attended high-school RBDs
in the master universe, giving 3,964 schools. The three primary populations
are unchanged: probability-weighted genuine applicants; all first-round
offer recipients within the paper roster; all master-universe attendees
using most_time_RBD. The latter has no SAE, age, risk or exam restriction.
Keep the stochastic-offer supplement separate from the preference models.

Use available-parent education years, both-subject average baseline SIMCE
z-score, female share, and the existing imputed income decile. Equal-weight
available cohort means over 2018-2020, retaining cohort counts, numerators,
denominators and missing-data coverage. Do not impute an empty school-group
cell. The original 100-school probabilities and all 300 primary table rows
reproduce to numerical precision. Independent weighted-mean and cohort-mean
checks cover every expanded group/outcome.

## Common preference sample and units

All 2,481 original D schools have attendee measures, but only 2,456 and
2,469 have complete applicant and offer measures. Exclude entire rankings
containing any missing peer regressor across any of the three populations:
267,937 rankings, 940,605 pairs, 2,454 schools remain (98.079% of original D).
Never shorten rankings or use a different sample in the first column.
Re-estimate D on this common sample; all ten original fee indicators remain.

The four main models are D, D+applicant peers, D+offered peers, and
D+attendee peers. D includes published-coordinate log distance, fee bands,
mixed HC/TP and TP-only indicators and four standardized EB VAs, without
PAES mean or interactions. Preserve original VA units. Use a common
attendee-school normalization across 2,454 equally weighted destinations
for every peer population; SDs are 1.46926 parent-education years,
0.364408 baseline SIMCE units, 0.149969 female-share units and 1.45038
income deciles. This replaces the old 99-school peer normalization only
in the new full-school report.

Use exact submitted-list Plackett-Luce via stratified Cox with student-
clustered covariance. Check direct likelihood, scores, full-rank information,
independently reconstructed sandwich covariance and preserved sources.
Three auxiliary controls+peers fits omitting VA support incremental
four-variable-block comparisons. These remain in-sample associations;
same-period composition is endogenous and conditional coefficient sizes
alone do not establish which school attribute families value more.

## Reproduction and outputs

Composition: applicant_composition_trends scripts 11 then 10 --all-schools;
derived tables and bundle use all_school_group_averages. Preferences: script
38, then 36 and 37 with --all-schools; new data/models/tables use
peer_populations_all_schools. Report:
output/reports/school_preference_va/with_peer_populations_all_schools.html.
Original top-100 inputs/reports and original full-sample D remain unchanged.

## Results and verification

All seven main/auxiliary fits converge in three iterations. Adding peers
conditional on VA raises log likelihood by 1,740.71 (applicants), 951.11
(offers) and 2,722.56 (attendees). Adding VA conditional on peers raises it
by 785.38, 863.90 and 655.87 respectively. Peer blocks improve ranking fit
more in every version, reversing the selective top-100 comparison. Both
blocks remain jointly predictive with student-clustered Wald tests.

Peer baseline-SIMCE coefficients are 0.13190, 0.09096 and 0.23163. Attendee
peers reduce math VA from 0.04854 to -0.01806 and exam-taking VA from
0.07868 to -0.00203 (SE 0.00275); premium-field VA remains positive at
0.05425. Conditional income coefficients are negative but are not a
standalone dislike-of-income interpretation because the peer measures
enter jointly and are correlated. The original 100-school subset was
highly selected; do not generalize its earlier opposite fit comparison.

The source reproduction checks, 72 independent aggregation checks,
preflight checks, 46 model checks and report checks all pass. Cohort
retained counts are 69,338 / 100,045 / 98,554 for 2018 / 2019 / 2020;
excluded counts are 642 / 2,157 / 2,448. Missingness reasons overlap.
