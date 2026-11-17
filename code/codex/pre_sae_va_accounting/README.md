# Pre-SAE VA accounting

This task compares attended high-school value added before and after SAE for two
grade-8 switch margins:

- schools that do not show grade-9 continuation in the next year
- students whose grade-8 origin shows grade-9 continuation but who switch to a
  non-private high school

Run from the repository root:

1. `Rscript code/clean_pre_sae_va_accounting.R`
2. `Rscript code/codex/pre_sae_va_accounting/01_build_pre_post_va_accounting.R`

The cleaner creates reusable student links in
`data/clean/pre_sae_va_accounting/`, including:

- `pre_post_grade8_universe.rds`
- `post_grade8_tracking_window_clean.rds`
- `pre_post_student_high_school_links.rds`

The accounting script joins those links to the existing EB school VA file
`data/clean/empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv`.
It writes cohort and period summaries to `data/clean/pre_sae_va_accounting/`,
a short report to `output/reports/pre_sae_va_accounting/`, and a selected-outcome
figure to `output/figures/pre_sae_va_accounting/`.

The pre-SAE continuation split uses a registry analogue because SAE offerings are
not defined before SAE. A grade-8 origin is classified as continuing if the same
RBD has grade-9 enrollment in the following academic year. The high-school link
is `most_time_RBD` over the four years after grade 8, with duplicate student-year
records resolved by dropping zero attendance among duplicates, preferring passed
records when any passed record exists, then highest attendance, then deterministic
RBD tie-breaking.
