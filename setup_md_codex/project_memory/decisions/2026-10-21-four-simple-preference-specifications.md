# Simplify the school-preference report to four additive specifications

## Request and specification

The user replaced the increasingly complicated report with an explicit
four-model comparison:

- A: log(1 + distance) and Directorio fee bands.
- B: A plus mixed HC/TP, TP-only and the PAES M1 school mean.
- C: A plus math, exam-taking, high-premium-field and high-premium-institution VA.
- D: B plus those four VAs.

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
convergence and nested likelihood ordering A <= B,C <= D within distance.

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

## Current results and validation

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
