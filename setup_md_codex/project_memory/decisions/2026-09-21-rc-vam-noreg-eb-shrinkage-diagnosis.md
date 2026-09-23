# Diagnosis (open, needs review): RC VAM noreg EB shrinkage collapses to zero for every outcome

**Date:** 2026-09-21
**Status:** superseded 2026-09-22 -- RC-VAM abandoned (see `2026-09-22-abandon-rc-vam.md`). Previously: open question -- diagnosed, NOT fixed. Written up for a second opinion (Bruno is sharing this with codex).

## Symptom

`code/claude/rc_vam/02_construct_eb_rc_vam_values.R`, run against
`rc_vam_school_values_r_noreg.csv` (output of
`01_construct_rc_vam_school_values_noreg.R`), produces `tau2 = 0` for
**every one of the 13 outcomes**, with `p10/p50/p90 reliability` all
exactly 0. Under the script's normal-normal shrinkage formula
(`tau2 <- max(observed_variance - mean_noise_variance, 0)`; `reliability
<- tau2 / (tau2 + se2)`), `tau2 == 0` forces every school's EB value to
the (weighted) grand mean, which after centering means **every single
school gets exactly 0** for every outcome. See
`data/clean/rc_vam_school_values/eb_rc_vam_school_values_diagnostics.csv`.

This is not a bug in the EB script -- it is doing exactly what the
formula says given its inputs. The inputs (the noreg regression's point
estimates and SEs) are the actual problem.

## Evidence the noreg regression's outputs are themselves broken

From `rc_vam_school_values_r_noreg.csv`:

- `z_year_math_max` (a within-year z-score, should be roughly in
  [-4, 4]): point estimates of **-30.02, -30.91, -29.98, -29.73** across
  different schools, with SEs of **8.66, 8.69, 8.66, 8.66** -- nearly
  identical across schools.
- `admission_exam_taker` (a literal 0/1 outcome): point estimates of
  **2.97, 3.64, 3.23, 3.18, 3.11** (impossible for a probability), SEs
  of **~4.08-4.11**.
- The clearest tell: for `admission_exam_taker`, the school with **1**
  student has SE = 4.10579; the school with **621** students has SE =
  4.08514 -- essentially identical despite a 600x difference in sample
  size. A school's own sampling variance should shrink sharply with
  more students. SE that barely moves across a 600x range in `n` is not
  actually measuring that school's own precision.

This is the textbook signature of severe multicollinearity in the
design matrix, not a real, precisely-estimated signal.

## Why: the regression design

`01_construct_rc_vam_school_values_noreg.R` fits, in one OLS:

```
y ~ 0 + factor(school_rbd) + x_terms + gp_mat
```

- `factor(school_rbd)`: ~3,000+ literal school dummies (no reference
  level -- this is the `ibn.school_rbd` / risk-controlled point
  estimate + exact analytic SE design, chosen specifically so each
  school's SE comes straight off the coefficient table rather than a
  bootstrap/naive approximation -- see that script's own header
  comment).
- `gp_mat`: ~2,656 `prob_*`/`iszero_*` columns (g(p_i)), passed as one
  matrix-valued term to avoid an R formula stack overflow (see same
  script's header).

The problem: g(p_i) is *by construction* a function of which schools a
given student could possibly be assigned to. In particular, `iszero_j`
("probability of assignment to school j is exactly 0") is, for most
(student, school) pairs, very close to "this student was never a real
candidate for school j" -- which is closely entangled with "which
school this student actually ended up at," i.e. with the `factor(
school_rbd)` dummy itself. With ~3,000 schools nationwide (not a single
city's centralized round with a few dozen-to-low-hundreds of schools),
most students' real "choice set" overlaps only a small subset of
schools -- meaning most of the ~2,656 `iszero_*` columns are near-
constant (or perfectly collinear with combinations of the school
dummies) for any given student. That is a plausible mechanism for
exactly this symptom: wildly inflated, out-of-range point estimates and
SEs that don't scale with `n`.

## Cross-check against the source paper

Read directly from `writing/methods/papers/rc_vam.pdf` -- Angrist,
Hull, Pathak, Walters (2024), "Credible School Value-Added with
Undersubscribed School Lotteries," *ReStat* 106(1):

- **Main estimating equation (p. 5, eq. 4):**
  `Yi = alpha0 + sum_j alpha_j D_ij + Xi'gamma + g(pi) + eta_i`, with
  g(pi) described as "linear terms in the elements of pi and a set of
  dummy variables indicating when each p_ij equals 0. The resulting
  parsimonious specification of g(pi) has **2J terms** in a district
  with J schools." -- **This matches what's implemented here.** The
  full per-school 2J-term risk control is the paper's actual main
  design, not a deviation from it. The paper justifies this via a
  propensity-score-theorem argument (control for a linear function of
  p_i is equivalent to dummies for every point of support of the
  assignment-score distribution).
- **Separately**, footnote 13 (p. 10) describes a *different*, low-
  dimensional risk control -- "expected value-added" (a score-weighted
  average of school VA under assignment probabilities) plus
  "probability of receiving any offer" -- used ONLY for an illustrative
  balance-check (Table A1), not as an alternative main specification.
  "Expected VA" there is built from the paper's *"conventional" VAM*
  (their second of four benchmark specifications -- OLS + demographics
  + baseline-score controls, no risk adjustment at all), **not** from
  the RC VAM estimate itself. So there is no circularity in the paper:
  the low-dim summary (which does need a VA input) is never used to
  build the main estimator, and the main estimator (which has no VA
  input at all, just raw p_i / zero-dummies) has nothing to be circular
  about.

**This directly answers Bruno's circularity concern**: "we can't
include expected VA under assignment probabilities if we don't have
the VA estimated for each school" is correct, but it's moot for the
*main* regression, because the main regression's g(p_i) never uses a
VA estimate as an input in the first place -- only the paper's
footnote-13 illustrative balance check does, and that check plugs in
an *independent, non-risk-adjusted* VA estimate (their "conventional"
VAM) specifically to avoid this exact problem. If a low-dimensional
risk summary were ever wanted here, the same trick is available: this
repo already has an independent, non-RC-VAM estimate sitting in
`data/clean/school_rbd_observational_values/school_rbd_observational_values.csv`
(built by `code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R`,
no g(p_i) at all) that could serve the same role `school_rbd`-level.

## Open question -- not yet resolved

The paper's own main design uses the same structural approach (literal
per-school dummies + 2J-term linear/zero-dummy risk control in one
regression) and apparently doesn't hit this problem in their own
setting. Candidate explanations, none yet confirmed:

1. **Scale mismatch.** The paper's empirical settings (single-city
   centralized lotteries -- Boston/Denver/NYC-style) likely have far
   fewer schools (J in the tens/low hundreds) and/or a smaller, more
   overlapping choice-set structure than Chile's SAE, which appears to
   span ~3,000+ schools nationwide. More schools -> a much higher-
   dimensional, sparser g(p_i) relative to sample size, and much more
   scope for near-perfect collinearity between `iszero_j` terms and the
   school-identity dummies.
2. **Choice-set scoping.** It's possible g(p_i) here is being built
   over a broader set of "supported schools" than each student's own
   realistic choice set -- worth checking exactly how "supported
   schools" is scoped in the (now-archived) precursor scripts
   `code/claude/rc_vam/archive/00_export_probability_controls_all_timely_sae.R`
   and `00b_build_gp_wide_dta.R`, and whether `RC_VAM_MIN_SUPPORT`
   (`support_k`, default 100 in `00_build_rc_vam_analysis_sample.R`)
   is doing what's intended.
3. **A real implementation issue** in how `gp_mat`'s `prob_*`/`iszero_*`
   columns are constructed, distinct from (1)/(2) -- not yet checked
   line-by-line against the paper's exact g(p_i) construction for this
   pass.

None of these has been verified yet. This doc is written up for a
second opinion (codex) precisely because the next step -- lowering
`support_k`, restricting g(p_i) to a genuinely local choice set,
switching to absorbed school FE with a different SE strategy, or
something else -- depends on which of these is actually true, and that
hasn't been checked yet.

## What this does NOT mean

It does not mean the EB script (`02_construct_eb_rc_vam_values.R`) is
broken -- it is behaving correctly given its inputs. Any fix belongs
upstream, in `01_construct_rc_vam_school_values_noreg.R`'s regression
design (or its inputs from `00_build_rc_vam_analysis_sample.R`), not in
the EB shrinkage formula itself.
