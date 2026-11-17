# Pre-SAE attended-school VA accounting

Bruno requested an accounting comparison of the cohorts preceding SAE against
the studied SAE cohorts. The implemented comparison uses grade-8 cohorts
2014-2017 as pre-SAE and 2018-2020 as SAE-era cohorts.

The cleaning script is `code/clean_pre_sae_va_accounting.R`. It constructs the
first observed grade-8 origin RBD and the four-year post-grade-8 high-school link
using the same core rules as `def_sample_frame.R` and `def_school.R`: ordinary
basic grade 8 defines the cohort, and `most_time_RBD` is chosen from the four
years after grade 8 after resolving duplicate student-year records by dropping
zero-attendance duplicate rows, preferring passed records when possible, then
highest attendance, then deterministic RBD tie-breaking.

Because SAE offerings are not defined before SAE, the pre-SAE continuation split
uses a registry analogue. A grade-8 origin is treated as continuing if the same
RBD has grade-9 enrollment in the following academic year. The school-ending
group is origins without that grade-9 evidence. The voluntary-switch group is
students at origins with grade-9 evidence who nevertheless switch to a different
most-time high school and whose destination is not private paid (`COD_DEPE2 != 4`).

The accounting script is
`code/codex/pre_sae_va_accounting/01_build_pre_post_va_accounting.R`. It joins
the cleaned high-school links to the existing EB school VA values measured on
the 2017-2020 VA sample, using attended `most_time_RBD`. Outputs are in
`data/clean/pre_sae_va_accounting/`,
`output/reports/pre_sae_va_accounting/`, and
`output/figures/pre_sae_va_accounting/`.
