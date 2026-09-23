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

- 2026-09-23 update 2: staffing ratios now staff per 100 students enrolled,
  denominator = universe students (grade-8 cohorts 2017-2020) whose
  most_time_RBD is the school. Still undercounts schools students attend only
  partially: ratios remain skewed (teacher skew 33) and full-sample capacity
  rows are outlier-driven. A true annual grades 9-12 enrollment count from
  tracking files is the proper denominator (open).

- 2026-09-23 update 3: Bruno chose the main sample = schools with >=100
  pooled VA-sample students (2,334 schools). All-schools version kept as
  robustness (`*_allschools`). This restricts the population described to
  larger schools; small schools' EB VA is heavily shrunk anyway.

- 2026-09-23 update 4 (orientador tiers): SIES undergrad match reaches >90%
  only for staff born 1985+, but counseling specialization is almost entirely
  post-UG (45 UG vs 3,473 postitulo/postgrad counseling awards) and happens
  mostly after age 30 (median ~40). Split orientadores at birth year 1977 (age
  30 in 2007): training essentially fully observed for 1977+, partly hidden
  before. Rows: orientadores per 100 students and counseling-trained (post-UG
  counseling award as of staff year) orientadores per 100, by tier. Counts, not
  within-tier shares, so schools without a tier have true zeros. Rebuilt
  headcounts reproduce counselor__mean_headcount exactly. Rolling award windows
  and same-age residualization were considered and rejected (Bruno wants an
  accurate school stock, not a fair person comparison).

- 2026-09-23 update 5: switched outcomes to the current paper EB VAs
  (Box data/clean/empirical_bayes_school_va/cohorts_2017_2020, copied to
  data/clean/staff_va_compact_inputs/). Columns: math, language, exam taking,
  FUAS application (any_postulacion), high-premium field, high-premium inst.,
  projected income. HE enrollment and STEM dropped (not in current paper VA).
  The earlier codex staff tables used the older *_for_iv.csv VA file.

- 2026-09-23 update 6: Particular Pagado schools (COD_DEPE == 4) excluded
  (Bruno). Main sample 2,058 schools (276 pagado dropped from the >=100 set).
  Teachers-per-100 FUAS coefficient flips from -0.13 to +0.04: the earlier
  negative sign was the private-school contrast.

- 2026-09-23 update 7: paper version adds significance stars (two-sided
  normal p from HC1 t; * .10, ** .05, *** .01) at Bruno's request. Unlike the
  codex 14-variable table, this specification was fixed by design rather than
  selected by Lasso, but stars still do not adjust for multiple outcomes or
  for estimation error in the EB VA dependent variables.

- 2026-09-23 update 8: dropped log public funding per student (Bruno):
  public funding is targeted by student vulnerability (SEP) and private
  contributions are unobserved, so it is not a clean resource measure.
  Paper write-up (sept_draft.tex, subsection School Inputs and Value Added)
  updated to match.

- 2026-09-23 update 9: Bruno considered dropping peer grade-4 math (mixes
  peer effects and sorting). Dropping it loaded composition onto staff, size
  and track (teacher index -> math VA 0.01 -> 0.13; TP -> math -0.08 -> -0.66),
  so peers stay as a reported row. Log students enrolled (universe count)
  replaces log VA-sample size and is now reported. R2 rows: controls+track;
  +staff; +peers and size. Update 10: R2 rows reordered to controls only ->
  + peers, size and track -> + staff (full model), so the last row is the
  reported model; staff add <= ~1 pp in every column. Update 11 (Bruno):
  order is controls -> + size and track -> + staff -> + peers (full model).

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
