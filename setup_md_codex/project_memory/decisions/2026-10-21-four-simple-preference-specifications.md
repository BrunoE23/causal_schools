# Simplify the school-preference report to four additive specifications

## Request and specification

The user replaced the increasingly complicated report with an explicit
four-model comparison:

- A: log(1 + distance) and Directorio fee bands.
- B: A plus mixed HC/TP, TP-only and the PAES M1 school mean.
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
