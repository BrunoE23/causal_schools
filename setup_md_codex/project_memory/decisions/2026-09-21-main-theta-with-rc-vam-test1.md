# Decision: Rerun the main theta table with test1 RC-VAM values

**Date:** 2026-09-21
**Status:** active

Bruno requested the continuous scalar-IV main table, not an update to the
binary high-VA complier tables. The new table uses the supplied unregularized
R implementation at `data/clean/rc_vam_school_values/test1/` for three matched
outcome/value dimensions: standardized math, standardized verbal, and log
projected program income. The legacy income name in the RC-VAM file is the
documented alias for the full hierarchical program-income outcome.

For each dimension, construct attended-school RC-VAM, first-round offered-
school RC-VAM, and expected RC-VAM `sum_j p_ij V_j`. Estimate the corresponding
student outcome on attended RC-VAM, instrumenting it with offered RC-VAM and
controlling for expected RC-VAM, cohort, grade-4 math and verbal scores, gender,
and age. Use timely SAE cohorts 2018-2020 to match the RC-VAM input and report
heteroskedasticity-robust SEs. This is theta/pass-through; no VA cutoff enters.

The expected-value construction retains the established centered-zero rule for
the simulated unmatched state. RC-VAM school coverage plus unmatched states
accounts for about 99.9% of mean probability mass in each regression sample;
more than 99.2% of observations have fully accounted probability mass. Outputs
are isolated under `rc_vam_scalar_iv/test1`; existing EB and binary tables are
unchanged.

Results: math theta 0.648 (SE 0.046), verbal 0.744 (0.059), and log projected
income 0.653 (0.068). First-stage F statistics exceed 2,300 in all columns.
