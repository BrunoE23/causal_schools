# Simple additive school-preference specifications

## Request and specification

The user replaced the increasingly complicated report with an explicit
four-model comparison:

- A: log(1 + distance) and Directorio fee bands.
- A + averages: A plus the four observed school outcome averages, without
  school type, inserted before B after the subsequent user request.
- B: A plus mixed HC/TP, TP-only and four observed school outcome averages,
  including PAES M1 and the three other raw outcome means after the later expansion.
- C: A plus math, exam-taking, high-premium-field and high-premium-institution VA.
- D: C plus mixed HC/TP and TP-only, excluding PAES M1 after the user's revision.

Interpret the models additively, with common attribute slopes across school
types. Parent education, parent income and all attribute-by-type interactions
are excluded. Separate mixed HC/TP and TP-only indicators retain the previous
classification, with Non-TP as reference. No school fixed effects or intercept
are introduced.

## Common sample and units

Reuse the preceding complete fee-model data directly, retaining 273,184
applicants, 969,377 school alternatives and 2,481 schools for every model.
Do not broaden the smaller models' samples when their regressor set shrinks.
Existing VA and PAES normalization remains unchanged across columns.
Each fee field uses free as reference; retain the nine observed paid-band
indicators and the shared unknown-fee indicator. The absent lowest monthly
paid band is not estimated. Distances retain log1p kilometers.

Estimate four pooled models under published student coordinates and another
four under comuna-centroid distances. Use the exact submitted-set
Plackett-Luce/Cox representation and covariance clustered by student.
Check full-list/sample preservation, complete term inclusion, numerical
convergence and nested likelihood ordering A <= B, A <= C <= D within distance.
B and D are not nested because only B contains PAES M1.

B's other observed school means are the unadjusted exam-taking, high-premium
field and high-premium-institution enrollment averages; D contains their
VA counterparts instead. Thus the intended B/D comparison is observed
school outcomes versus the controlled EB VA dimensions, with school type
included in both. PAES M1 is retained as the requested observed math proxy.

## Reporting and reproducibility

Replace the current fee report in place with one visible four-column table.
Place the four municipality-distance columns in one collapsed robustness
section. Do not add applicant-group or school-type-slope panels to this view.
Preserve the preceding comprehensive report at
`output/reports/school_preference_va/with_directory_prices_2024_full.html`;
script 29 now renders that file. The current simplified report keeps the
original `with_directory_prices_2024.html` path.

Scripts 31-33 and `four_specifications_common.R` use separate
`directory_prices_2024/four_specifications` clean-data/table subfolders.
Previous datasets, normalizations and model objects remain unchanged.
The directory and PAES variables remain later school snapshots; coefficients
are conditional ranking associations. Differences across logit specifications
can also reflect utility-error-scale changes.

## Prior D specification with PAES M1

All eight fits converge in three iterations. Fitting checks pass 58/58;
independent verification passes 101/101, including a student-clustered
covariance reconstruction for D under published coordinates. Existing input
and model hashes remain unchanged.

In the main-distance specification, adding school type and PAES M1 from C
to D changes math VA from 0.0180 (SE 0.0019) to -0.0506 (0.0026) and exam
taking VA from -0.0184 (0.0020) to 0.0431 (0.0024). High-premium-field VA
changes from 0.0578 (0.0014) to 0.0445 (0.0014); high-premium-institution VA
from -0.0046 (0.0030) to -0.0093 (0.0030). These are joint conditional slopes,
not causal preference parameters or comparable utility scales by construction.

## Revision: remove PAES M1 from D only

The user then explicitly excluded the PAES M1 mean from D. The current D
contains distance, fees, school type and the four VAs. B retains PAES M1;
A, B and C fitted objects and exports are unchanged. Script 31 now supports
`--specifications=D` for a partial refit with preserved-object hashes; its
default continues to fit all models reproducibly.

Both revised D distance models converge in three iterations on the same
273,184 applicants. For published student coordinates, the revised D's
math VA coefficient is 0.0491 (SE 0.0019), exam-taking VA is 0.0778 (0.0024),
high-premium-field VA is 0.0569 (0.0014) and high-premium-institution VA is
-0.0181 (0.0030). All 26 partial-refit checks pass, including preservation
of all six A-C model files and unchanged A-C coefficient/summary exports.
Independent verification passes 102 checks, and the updated compact report
passes 10 checks with PAES M1 displayed only in column B.

## Revision: observed school outcome means in B

The user asked to treat PAES M1 as an unadjusted math outcome average and
include analogous averages for each other VA dimension in B. Reuse the
existing uncentered `raw_mean` from the All-sample observational school-value
file for grade-8 cohorts 2017-2020. Its controlled school effects and
regression counts match the precursor used by the current EB source exactly.
Do not use controlled adjusted means or reconstruct the VA.

The raw exam-taking mean uses all eligible age-12-16 school attendees with
valid attended high-school RBD; field and institution means use exam takers,
including non-enrollees as zero, following the paper's outcome-specific
denominators. Raw means precede complete-control restrictions; retain their
observed-student counts. All 2,481 current destinations have these means.
Standardize each new share with equal weight per unique current destination
school, retaining the current PAES scaling and common applicant sample.

Script 34 creates a separate augmented ranking dataset in the
`four_specifications/school_outcome_means` clean-data subfolder. Script 31's
`--specifications=B` option updates the two B distance models and preserves
A, C and D. The compact report gains three B-only mean rows; its four VA
rows, existing simple layout and current file path remain in place.

The expanded B uses the same 273,184 applicants and converges in three
iterations under both distance measures. Main-distance coefficients per
school SD are PAES M1 0.0517 (SE 0.0031), exam-taking share 0.1233 (0.0033),
high-premium-field share 0.0699 (0.0018) and high-premium-institution share
0.0402 (0.0036). All four averages have positive conditional associations.
Construction passes 22 checks, partial fitting 26, independent verification
124 and the compact report 10. All A, C and D fitted objects and prior input
columns are preserved exactly.

## Additional averages column between A and B

The user requested an intermediate column with the four school outcome
averages. Retain A's distance and fee controls, add the four averages, and
omit school-type indicators; this interpretation was stated before fitting.
The new internal key is `A_means` and its visible heading is `A + averages`.
The existing A, B, C and D columns keep their labels and results.

Fit only the two new distance models using the same complete rankings,
school normalizations and student-clustered covariance. Both the new column
and B include all four means, so check A <= A_means <= B within each
distance definition; A <= C <= D remains valid. The report now has one
visible five-column comparison plus the collapsed municipality version.
There are ten model objects, retaining the historical four-specification
file/directory names for reproducibility and stable output paths.

Both new fits converge in three iterations. Published-coordinate slopes
per school SD are PAES M1 0.0306 (SE 0.0030), exam-taking share -0.0302
(0.0028), high-premium-field share 0.0743 (0.0018) and institution share
0.0506 (0.0035). The existing eight models remain unchanged. Expanded
independent verification passes 142 checks.
