# Decision: Compact 8-row staff/school vs EB VA table

**Date:** 2026-09-23
**Status:** active (descriptive; sample choice pending Bruno)

## Decision

Descriptive table of 7 All-sample EB VAs (math, language, exam taking, HE
enrollment, STEM, high-premium institution, projected income) on 8 rows:
log(1 + teachers/orientadores/leaders per 1,000 VA students), teacher and
leadership balanced qualification indices, orientation-specific qualification,
peer grade-4 math mean, log public funding per student. Unreported controls:
log VA students, TP/artistic, dependency and region dummies, plus the existing
missingness and role-absence/roster nuisance indicators. Estimator is the
codex fixed-OLS estimator (HC1, SD-standardized); verified to 1e-14.

- 2026-09-23 update: TP and artistic split into separate reported 0/1 rows.
  Only 2 schools (RBD 320, 8511) have artistic code 910 in the 2024
  directory; neither has TP. The artistic coefficient is not interpretable.

## Rationale

- Staffing ratios enter as log(1+x): raw ratios have skew 6-24, driven by
  schools with very few VA-sample students.
- Dependency and region added because credential-to-VA associations (e.g.
  teacher high-premium degree -> high-premium institution VA, 0.49 in the
  14-variable table) likely reflect geography and sector.

## Risk

Capacity rows are not robust to dropping small schools. With >=100 pooled
VA-sample students (2,334 of ~3,680 schools), the teacher-ratio coefficient on
exam-taking VA moves from -1.03 to -0.13 and the leader-ratio coefficients fall
from 0.11-0.30 to 0.01-0.06. Treat small-denominator staffing ratios with
caution. Peer baseline math is the only row stable and large across samples.

Code: `code/claude/staff_va_compact/`. Run in Python (R unavailable in that
session); estimator matches the R implementation exactly.
