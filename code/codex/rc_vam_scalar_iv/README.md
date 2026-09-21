# RC-VAM scalar IV main table

`01_run_main_theta_table.R` reruns the continuous scalar-value IV for math,
verbal, and log projected program income using the supplied unregularized
RC-VAM school values in `rc_vam_school_values/test1`. It does not modify or
reuse the binary high-VA/complier tables.

For each outcome m it constructs attended value A, first-round offered value O,
and expected assignment value E=sum_j p_ij V_j. It estimates outcome m on A,
instrumenting A with O and controlling for E, cohort, baseline grade-4 math and
verbal scores, gender, and age. Standard errors are heteroskedasticity robust.
The sample uses timely 2018--2020 SAE applicants, matching the RC-VAM input.

The program-income outcome `log_program_income_clp_m1` is the backward-
compatible alias for `log_program_income_full_clp_m1`. Missing school values in
the expected-value calculation follow the established scalar-IV implementation:
their contribution is zero and probability coverage is saved diagnostically.
The explicit simulated unmatched state also contributes zero because school VA
is student-centered. In the completed run, RC-VAM schools plus unmatched states
account for 99.88--99.89% of mean assignment probability mass, and 99.24--99.34%
of regression observations have fully accounted probability mass.
No cutoff or binary classification enters this analysis.

Outputs are isolated under `data/clean/rc_vam_scalar_iv/test1` and
`output/tables/rc_vam_scalar_iv/test1`.
