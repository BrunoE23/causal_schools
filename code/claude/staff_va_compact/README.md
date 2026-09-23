# Compact staff/school characteristics vs school EB VA

Goal: a readable 8-row descriptive table of All-sample EB school VA on staff,
peer and resource characteristics (1-2 per staff role). Not causal.

Estimator: identical to `code/codex/staff_ols_va/01_fit.R` (unweighted school
OLS; Y and X standardized by sample SD within each outcome sample; median
placeholders + missingness indicators; role-absence/roster nuisance; HC1).
`02_verify_against_codex_r.py` reproduces the R 14-variable results to 1e-14.

Differences from the 14-variable table:
- Focal rows: log(1 + staff per 1,000 VA students) for teachers, orientadores,
  leaders; teacher and leadership balanced qualification indices
  (`data/clean/staff_quality_va`, `data/clean/leadership_quality_va`);
  orientation-specific qualification; peer grade-4 math mean; log public funding.
- Staffing ratios are log(1+x) because raw ratios have skew 6-24.
- Track rows (0/1, Y-SD units): TP (ENS 410-810) and artistic (910) enter
  separately; artistic = RBD 320, 8511 only (2024 directory; see
  `data/clean/staff_va_compact_inputs/artistic_rbd_2024.csv`).
- Unreported controls: log VA students, dependency (ref municipal)
  and region (ref RM) dummies.
- R2 rows: controls only; + 6 staff rows; + peers and funding. In-sample.

Sensitivity: `MIN_VA_STUDENTS=100` keeps schools with >=100 pooled VA-sample
students (2,334 schools). Capacity rows change sharply; see decision log.

Run from repo root (Python 3 with numpy/pandas; R unavailable in this session):
    python code/claude/staff_va_compact/01_fit_compact_table.py
    MIN_VA_STUDENTS=100 python code/claude/staff_va_compact/01_fit_compact_table.py
    python code/claude/staff_va_compact/02_verify_against_codex_r.py

Outputs: `data/clean/staff_va_compact[_minva100]/` (coefficients, model summary,
coverage, source md5) and `output/tables/staff_va_compact[_minva100]/`
(`staff_va_compact.tex`, `.csv`).
