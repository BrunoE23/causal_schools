# Compact staff/school characteristics vs school EB VA

VA source: current paper EB VAs, cohorts 2017-2020 (copy of Box
`data/clean/empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv`
in `data/clean/staff_va_compact_inputs/`).

Goal: a readable 8-row descriptive table of All-sample EB school VA on staff,
peer and resource characteristics (1-2 per staff role). Not causal.

Estimator: identical to `code/codex/staff_ols_va/01_fit.R` (unweighted school
OLS; Y and X standardized by sample SD within each outcome sample; median
placeholders + missingness indicators; role-absence/roster nuisance; HC1).
`02_verify_against_codex_r.py` reproduces the R 14-variable results to 1e-14.

Differences from the 14-variable table:
- Focal rows: staff per 100 students enrolled (mean 2018-2024 headcount /
  universe students with most_time_RBD = school, grade-8 cohorts 2017-2020;
  from `00_hs_enrollment_from_universe.R`) for teachers, orientadores, leaders; teacher and leadership balanced qualification indices
  (`data/clean/staff_quality_va`, `data/clean/leadership_quality_va`);
  orientation-specific qualification; peer grade-4 math mean; log public funding.
- Caveat: the universe denominator still undercounts some schools (62 schools
  have >30 teachers per 100; median 22 universe students). Ratios remain skewed
  (teachers skew 33); full-sample capacity rows are driven by these schools.
- Track rows (0/1, Y-SD units): TP (ENS 410-810) and artistic (910) enter
  separately; artistic = RBD 320, 8511 only (2024 directory; see
  `data/clean/staff_va_compact_inputs/artistic_rbd_2024.csv`).
- Unreported controls: log VA students, dependency (ref municipal)
  and region (ref RM) dummies.
- R2 rows: controls only; + 6 staff rows; + peers and funding. In-sample.

Main sample: schools with >=100 pooled VA-sample students (2,334 schools;
Bruno's choice 2026-09-23). `MIN_VA_STUDENTS=0` writes the all-schools
robustness version to `*_allschools/`.

Orientador rows (2026-09-23): headcount and counseling-trained headcount per
100 students, split by birth cohort (born 1977+ vs before 1977), built by
`03_build_orientador_tiers.py`. Counseling-trained = counseling-specific
postitulo/postgraduate SIES award as of the staff year. Replaces the any-level
orientation-specific qualification share.

Run from repo root (Python 3 with numpy/pandas; R unavailable in this session):
    python code/claude/staff_va_compact/03_build_orientador_tiers.py
    python code/claude/staff_va_compact/01_fit_compact_table.py
    MIN_VA_STUDENTS=0 python code/claude/staff_va_compact/01_fit_compact_table.py
    python code/claude/staff_va_compact/02_verify_against_codex_r.py

Outputs: `data/clean/staff_va_compact[_allschools]/` (coefficients, model summary,
coverage, source md5) and `output/tables/staff_va_compact[_allschools]/`
(`staff_va_compact.tex`, `.csv`).
