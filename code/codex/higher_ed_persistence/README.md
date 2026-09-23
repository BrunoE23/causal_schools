# Higher-education persistence outcomes

`01_construct_higher_ed_persistence.py` builds annual higher-education
persistence outcomes from the full SIES matricula files for 2022--2025.

The grade-8 cohorts and horizons are:

- 2017: expected entry in 2022; observed at years 1--3 in 2023--2025.
- 2018: expected entry in 2023; observed at years 1--2 in 2024--2025.
- 2019: expected entry in 2024; observed at year 1 in 2025.
- 2020: expected entry in 2025; no full-SIES follow-up is available yet.

The output includes enrollment at each horizon, persistence conditional on
entry-year enrollment, unconditional entry-and-persistence indicators, and
retention in the same `CODIGO_UNICO`, `AREA_CARRERA_GENERICA`, and institution.
It also records high-premium field and institution enrollment at each horizon
and persistence among students who began in a high-premium option.

Annual matching is set-based. A student with multiple valid enrollments is
treated as remaining in the same category if any follow-up enrollment matches
any entry-year enrollment. This prevents row ordering from turning dual
enrollment into an artificial switch.

High-premium field follows the existing project classification: Science, Law,
Engineering/Manufacturing/Construction, and Medicine+. High-premium institution
uses the existing centered MiFuturo institution fixed effect above 0.1;
unsupported matriculated institutions are coded zero, matching `high_inst_m1`.
High-premium field is a positive-list binary: every observed enrollment outside
the named premium categories is zero, including Agriculture, Services, and CINE
combinations outside the broader nine-category taxonomy.

The PAES 2026 admissions-matricula file is not used as a year-1 persistence
measure for the 2020 cohort because it is not the full SIES enrollment census.

Run from the repository root with the bundled Python runtime or another Python
installation containing pandas. Set `CAUSAL_SCHOOLS_DATA_WD` to override the
default Box data root.

Outputs:

- `data/clean/higher_ed_persistence/higher_ed_persistence_outcomes.csv`
- `data/clean/higher_ed_persistence/higher_ed_persistence_scan_diagnostics.csv`
- `output/tables/higher_ed_persistence/higher_ed_persistence_summary.csv`

`02_run_persistence_scalar_iv_eb.R` replicates the main expected-VA scalar IV
specification with persistence outcomes. It uses SAE cohorts 2018--2019 and
measures entry followed by continued participation in the second academic year.
Each column pairs the outcome with its corresponding EB VA: higher-ed
enrollment, high-premium field, or high-premium institution.

The same script also writes a separate version requiring persistence in both
subsequent academic years. This longer horizon uses only the SAE 2018 cohort;
the redundant cohort fixed effect is omitted while all other elements of the
main specification remain unchanged.

It also writes a cross-outcome second-academic-year table with persistence
outcomes in columns and four EB VA measures in rows: higher-ed enrollment,
high-premium field, high-premium institution, and math. Every cell uses the
expected value of its row VA as the assignment-risk control.

Persistence regressions do not condition on admission-exam taking. The exam
indicator is tied to a later observed exam year and is not part of the sample
definition for these higher-education persistence outcomes.
