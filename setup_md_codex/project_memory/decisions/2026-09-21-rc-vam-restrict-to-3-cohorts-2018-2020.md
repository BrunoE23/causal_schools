# Decision: Restrict RC VAM (Stata) to sae_proceso 2018-2020, and Speed Up the Wide Merge

**Date:** 2026-09-21
**Status:** active, untested (no Stata license available to run it)

## Context

Bruno reported `01_construct_rc_vam_school_values.do` "getting stuck at
merging." Separately, and for an unrelated reason, he asked to restrict RC
VAM to the "3 good cohorts."

A subagent fact-check (chunked read of `univ_gr8_df.csv`, columns
`sae_proceso`, `z_sim_mat_4to`, `timely_sae`) confirmed why: among
`timely_sae==1` rows, the share missing `z_sim_mat_4to` (baseline 4th-grade
SIMCE, required in `x_vars_needed` for every RC VAM outcome) is 15.6-22.8%
for 2017-2020 but **93.9%** for 2021. 2021 was already contributing almost
nothing to the estimation sample after the missing-X drop in Step 5 -- this
is the same underlying data gap Bruno flagged earlier ("I don't actually
have 4 cohorts").

## Decision

1. **Restrict `sae_proceso` to 2018-2020** (`rc_vam_min_year`/`rc_vam_max_year`
   locals near the top of the script, currently 2018/2020). Applied in three
   places, all as early as possible so the row-count reduction cascades
   through every downstream step, not just the final merge:
   - The `DA_probs_{year}.csv` stacking loop (Step 1) now loops
     `numlist `rc_vam_min_year'/`rc_vam_max_year'` instead of a hardcoded
     `2018 2019 2020 2021`.
   - The Step 1 universe/roster read adds `keep if sae_proceso >=
     `rc_vam_min_year' & sae_proceso <= `rc_vam_max_year'` right after the
     `timely_sae==1` filter.
   - The Step 2 rich-control read adds the same filter immediately after its
     own destring loop, before any of the outcome/control construction that
     follows.
   - The `output_stub` used to look for a pre-built wide `.dta` (from
     `00b_build_gp_wide_dta.R`) now bakes the year range into its filename,
     so a stale 4-cohort pre-built file is never silently picked up -- it
     just won't match, and the script falls back to rebuilding via
     `greshape`/`reshape` over the restricted population instead.
2. **2017 deliberately NOT included**, even though it exists in
   `univ_gr8_df.csv` (Bruno noted "2017 would come in to the sample" -- true,
   but only for a plain VAM). `DA_probs_2017.csv` doesn't exist (SAE
   probability files start in 2018), so g(p_i) cannot be constructed for
   2017 -- it's only usable for the separate g(p_i)-free "plain VAM"
   validation exercise (see the 2026-09-21 conversation about dropping
   g(p_i) entirely), not this script.
3. **`compress`** added right after building `gp_wide` (both the pre-built-
   `.dta` fast path and the Stata-built branch) and right after building
   `analytic` (Step 2's end), before the wide 1:1 merge between them. This
   is aimed squarely at the "stuck at merging" report: `analytic` meeting
   `gp_wide` is a merge of ~220 columns against ~3,000+ prob_/iszero_
   columns, and Stata holds the whole thing in RAM -- `compress` downcasts
   every column to its smallest safe type first, which can meaningfully
   shrink the in-memory footprint of a dataset this wide before the merge.
   A row/column-count `display` was also added immediately before the merge
   line, so it's possible to tell "still running, just slow" apart from
   "actually hung" on a rerun.

## What this does NOT fix

If the merge is still slow/stuck after this, the likely remaining cause is
genuine memory pressure from a dataset this wide (same root cause as the
earlier reshape/greshape slowness -- column count, not row count, is what
hurts). The fix in that case is almost certainly lowering `support_k` (fewer
supported schools -> fewer prob_/iszero_ columns) rather than anything
further at the merge step itself.

## Consequence for the sibling scripts

`01_construct_rc_vam_school_values_lasso.do` and `_noreg.do` both load the
cached `rc_vam_stata_analysis_sample.dta` this script builds in Step 3 --
they did not need direct edits, since they'll automatically inherit the
2018-2020 restriction once this script is rerun and its cache rebuilt.

## Update 2026-09-21 (same day): real bug found at that exact merge line

Bruno ran the script after the fixes above and hit a genuine error at the
Step 3 merge: `variable rbd_treated_1r is str5 in master but long in using
data`. Root cause: `rbd_treated_1r` is read from `univ_gr8_df.csv` in BOTH
Step 1 (roster -> carried into `gp_wide`, destringed there) and Step 2
(`analytic`, NOT destringed there -- it was never in Step 2's per-variable
destring list, since nothing in `x_terms`/`outcomes` actually uses it).
Same source column, two different places, only one of them typed it.

Fix: added `rbd_treated_1r` to Step 2's `step2_destring_vars` list. Chose
this over dropping/renaming it out of `gp_wide` because it's the same
underlying column in both places -- once both sides agree on type, the
values should already match for a given (student_id, sae_proceso), so there
was no real reason to discard it, just to type it consistently.
