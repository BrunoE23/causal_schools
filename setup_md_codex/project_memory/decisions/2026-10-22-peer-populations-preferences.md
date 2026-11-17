# D with applicant, offered and attendee peer composition

The sample restriction in this record is superseded by the user's subsequent
request to extend peer measures to the whole school population. See
`2026-10-22-full-school-peer-preferences.md`. The top-100 outputs remain
as a historical comparison.

The user requested a new four-column preference report: D, then D plus
the four peer characteristics measured in each of three populations.
Use the supplied 300-row school-group averages for the same 100 schools:
probability-weighted applicants, all first-round offer recipients, and
all master-universe attendees assigned to their most-attended HS.
Use parent education, baseline SIMCE, female share and imputed income decile.
The strictly stochastic offer supplement is not used.

## Explicit sample choice

Only 13,037 of the 273,184 original D rankings list exclusively schools
in this file (4.77%). The user explicitly selected these complete rankings
for all four columns rather than extending peer measures to all original
destinations. There are 36,276 applicant-school pairs and 99 used schools.
Never shorten lists. Re-estimate D on this sample; the first column is a
replication of its specification, not the original full-sample estimates.
Five fee indicators are identically zero and omitted from all columns;
all estimable D controls remain, including the four existing standardized
EB VAs, school-type indicators, distance and the five supported fee bands.

## Peer construction and units

Read mean_equal_cohort columns directly from the supplied CSV. Average
available 2018-2020 cohort means equally; retain cohort counts and coverage
metadata. All four peer measures are observed for all 100 schools/groups.
No new composition estimates or imputation are constructed.

Use the mean/SD across the 99 attendee-population school observations as
a common normalization for each peer measure in all three columns.
One peer unit is 1.151 parent-education years, 0.368 student baseline
SIMCE z units, 0.195 female-share units, or 1.127 income deciles.
Original VA normalization is unchanged. Bare coefficient magnitudes
therefore do not establish relative peer-versus-VA importance.

## Estimation and comparison

Use the published-coordinate distance and exact submitted-set Plackett-Luce
likelihood represented by stratified Cox, clustering covariance by student.
No PAES mean, interactions, school fixed effects or extra controls are added.
The four main models converge in four iterations. Check complete-list
preservation, all terms, exact likelihood, score equations, positive
information and independent reconstruction of student-clustered covariance.
Original D, source data and the supplied composition file are hash-preserved.

For a unit-invariant diagnostic, compare incremental log likelihood of
adding the four peer variables conditional on VAs and of adding the four
VAs conditional on peers. Three auxiliary peer-plus-controls/no-VA models
support this comparison and are not additional coefficient-table columns.
Peer/VA gains are 115.2/137.5 for applicants, 107.5/156.1 for offers and
77.0/176.4 for attendees. Both blocks remain jointly predictive; in-sample
fit gains are larger for VA in all three versions.

Peer baseline SIMCE is positive in every column (0.340/0.378/0.329 in the
common peer units). Parent education and income are correlated 0.84-0.92;
individual conditional peer signs should not be interpreted as structural
preferences. Composition is measured in the same years as applications
and is potentially endogenous. Restriction to complete top-100 lists is
highly selective; no causal claim or general full-sample conclusion follows.

Code: school_preference_va/35_peer_composition_preflight.R,
36_fit_peer_populations.R and 37_report_peer_populations.R.
Derived data/models use data/clean/school_preference_va/peer_populations/;
tables use output/tables/school_preference_va/peer_populations/.
The new report is output/reports/school_preference_va/with_peer_populations.html.
Existing preference reports remain unchanged.
