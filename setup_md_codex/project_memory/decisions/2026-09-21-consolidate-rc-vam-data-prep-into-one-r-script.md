# Decision: Move ALL RC VAM Data Prep into One R Script; Stata Only Regresses

**Date:** 2026-09-21
**Status:** active, untested (no Stata/R execution available in the environment this was written in)

## Context

Even after the 3-cohort restriction and `compress` fixes (see
`2026-09-21-rc-vam-restrict-to-3-cohorts-2018-2020.md`), Bruno reported the
Stata script was still stuck overnight, apparently in the `greshape`/
`reshape` step (the stale pre-built `.dta` from before the cohort fix meant
Stata was redoing that reshape from scratch). Stata's `reshape`/`greshape`
and its wide `merge` have been the recurring slow point in this pipeline
across several previous fixes. Bruno asked directly: make Step 0 a single R
script, and make sure the Stata step never has to reshape anything.

## Decision

All data construction -- DA_probs stacking, roster/support-threshold
construction, the wide g(p_i) pivot, the rich X_i controls and outcomes
construction, and the final merge -- now happens in ONE new R script:
`code/claude/rc_vam/00_build_rc_vam_analysis_sample.R`. It writes ONE file,
`data/clean/rc_vam_stata_analysis_sample.dta`, directly via
`haven::write_dta()`. Stata's job is now purely: load that file, define
`x_terms`/`outcomes`, run the regularization-variant regression, export
results. No `reshape`, no `greshape`, no wide `merge` anywhere in any of the
three `.do` scripts anymore.

This consolidates what used to be three separate places:
- `00_export_probability_controls_all_timely_sae.R` (probs/roster/support)
- `00b_build_gp_wide_dta.R` (wide g(p_i) pivot via `data.table::dcast`)
- Steps 1-3 of `01_construct_rc_vam_school_values.do` (reshape/greshape +
  rich X/outcomes construction + the wide merge)

Both superseded R scripts are left in place, untouched, for provenance --
not deleted, just no longer the recommended path.

## Why R instead of Stata for this

`data.table::dcast` (the wide pivot) and a `data.table` key-merge (joining
the ~220-column rich-X/outcomes table against the ~3,000+ column g(p_i)
table) are both dramatically faster than Stata's equivalents for exactly
this "many columns" case -- this was already established for the pivot
specifically (that's why `00b_build_gp_wide_dta.R` existed at all), and the
same reasoning extends to the final merge, which was the OTHER slow/
appears-stuck point Stata hit.

A secondary benefit: reading `univ_gr8_df.csv` ONCE in R (for both the
roster columns and the rich-X columns) instead of twice, independently, as
the old Stata Step 1 + Step 2 did, removes the exact bug class that
produced the `rbd_treated_1r is str5 in master but long in using data`
error -- there's only one copy of any given column now, so it can't
disagree with itself.

## What changed in the three `.do` scripts

- `01_construct_rc_vam_school_values.do` (ridge, primary spec): Steps 1-3
  deleted entirely. Now starts by loading `rc_vam_stata_analysis_sample.dta`
  (erroring with a clear message, pointing at the R script, if it isn't
  there), `compress`es it, and goes straight into defining `x_terms`/
  `outcomes` and the ridge regression loop (renumbered Steps 1-4).
- `01_construct_rc_vam_school_values_lasso.do` / `_noreg.do`: unchanged in
  substance (they were already loading the cached sample rather than
  rebuilding it) -- only their `DEPENDENCY` header comment was updated to
  point at the new R script instead of the old `.do` file's Steps 1-3.

## What this does NOT change

The estimation sample definition itself (timely SAE, 2018-2020, k=100
support threshold, the X controls and outcome constructions) is unchanged
-- this is a pure implementation/performance rewrite of HOW the sample gets
built, not a change to WHAT the sample is. The R script's transformations
were written to mirror the Stata Step 2 logic line-for-line (same NA/rescale
handling for `math_max`/`leng_max`, same `std()`-by-`psu_year` for
`z_year_math_max`/`z_year_leng_max`, same `higher_ed_enrolled_m1`/
`stem_enrollment_m1`/`program_certified_years_m1`/`inst_certified_years_m1`
construction, same `school_rbd > 0` / `edad_alu` 12-16 filters).

## Untested risk

This is a substantial rewrite with no way to run it here. The most likely
failure points on a real run, in rough order of likelihood: (1) exact raw
column casing in `univ_gr8_df.csv`/the income and middle-school CSVs --
handled by lowercasing every column name immediately after `fread()`,
mirroring Stata's own default-lowercase-on-import behavior, but untested;
(2) `fread`'s NA handling (`na.strings = c("NA", "")`) matching Stata's
`destring ..., force` behavior closely enough; (3) `haven::write_dta()`
producing a `.dta` that Stata 16 reads back cleanly given ~3,000+ columns.
If any of these are off, the fix is almost certainly local to that one
step, not a sign the overall design is wrong -- same posture as every other
untested piece of this pipeline so far.
