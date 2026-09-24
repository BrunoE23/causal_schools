# Staff value-added table by measurement coverage

`01_fit_age_coverage_table.py` builds a separate robustness version of the
staff-correlates table. It separates three types of measures:

1. staff counts by age from the administrative roster;
2. university and teaching qualifications reported in the staff directory for
   staff of all ages; and
3. detailed linked-degree measures for staff born in 1989 or later.

The 1989 cutoff is deliberately strict: the linked SIES degree register begins
in 2007, when this cohort was at most 18 years old. The regression specification,
school sample, outcome definitions, standardization, missing-value treatment,
and HC1 standard errors match the existing no-peer staff table.

Run from the repository root:

```powershell
python code/codex/staff_va_age_coverage/01_fit_age_coverage_table.py
```

The LaTeX table is written to
`output/tables/staff_va_age_coverage/staff_va_age_coverage.tex`.
