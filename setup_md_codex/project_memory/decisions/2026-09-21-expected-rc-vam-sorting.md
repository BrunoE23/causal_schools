# Decision: Repeat expected-VA sorting with test1 RC-VAM

**Date:** 2026-09-21
**Status:** active

Bruno requested the same baseline sorting exercise using the new unregularized
RC-VAM values while preserving the earlier EB analysis. The RC-VAM file provides
matched values for math, verbal, and log projected program income, so the new
table contains those three dimensions.

The design matches the EB exercise: construct application-specific expected VA
from the DA probabilities; standardize each expected-VA dependent variable and
the five continuous baseline covariates on one common sample; regress on baseline
math, verbal, income decile, father education, mother education, female, and
assignment-cohort fixed effects; use heteroskedasticity-robust SEs. The sample
contains timely 2018-2020 SAE applicants with probability mass one, complete
baseline covariates, and complete RC-VAM coverage across all three dimensions;
the explicit unmatched state receives centered VA zero. N is 239,190.

This is descriptive sorting into ex ante assignment opportunities, not causal.
Outputs are isolated under `expected_va_sorting/rc_vam_test1`; no EB or binary
tables are overwritten. R2 is 0.046 for math, 0.048 for verbal, and 0.015 for
projected income. The comparison with EB is suggestive rather than an exact
same-sample decomposition because the common VA-coverage samples differ by 817
students.
