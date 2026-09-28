# Decision: Ridge-Regularize the g(p_i) Block Only, in the Stata RC VAM Regression

**Date:** 2026-09-21 (supersedes `2026-09-20-lasso-regularize-gp-block-stata-rc-vam.md`)
**Status:** active, untested (no Stata license available to run it -- see script header)

## Context

Same underlying problem as the superseded Lasso doc: `code/claude/rc_vam/
01_construct_rc_vam_school_values.do` fits, per outcome, a single OLS
regression with a large g(p_i) nuisance block (~3,000 prob_/iszero_ terms at
k=100) alongside the school effects of interest. A first pass regularized
that block with Lasso. Bruno then said "lets go w ridge then," switching the
choice.

## Decision

Regularize **only** the g(p_i) block, via Ridge (L2). School dummies and
X_i controls (`x_terms`) are never penalized -- same overall design goal as
before, different penalty.

## Why Ridge over Lasso (the actual reason for the switch)

The Lasso doc's own caveat: AHPW's theory keeps every g(p_i) term
unconditionally because each term's job is satisfying conditional
independence for its own school, not predicting the outcome well. Lasso
selects on outcome-predictive power and can legitimately zero out a term
that's weak for the outcome but still CIA-relevant for some school. Ridge
shrinks every term toward zero but never drops any of them, so every school
keeps its own risk-control term, just down-weighted. This was the original
"Recommended" option before Lasso was tried; Bruno came back to it after
seeing the tradeoff spelled out.

## Mechanism actually implemented (and why it looks different from the Lasso version)

Ridge has no discrete selection event, so the Lasso version's trick --
lasso-select, then refit plain OLS on the survivors -- has nothing to hang
onto: every g(p_i) term always survives under Ridge, so "refit on the
survivors" would just be the original, unregularized regression again. The
coefficients therefore have to come directly from a penalized fit.

Two ways to get that were considered and rejected:

- **Stata's `elasticnet` (alpha near/at 0)** -- its postestimation isn't
  built to hand back a plain coefficient vector + classical SE for the
  always-included variables; that gap is exactly what `dsregress`/
  `poregress` exist to fill, and those are ruled out below on scaling
  grounds.
- **Hand-derived closed-form ridge via `matrix accum`** -- mathematically
  the cleanest option, but `matrix accum`/`vecaccum` do not understand
  `i.factor` syntax, which would force every `x_terms` factor variable
  (`cohort_gr8`, `gen_alu`, `edad_alu`, `cod_com_alu`, `middle_years_observed`)
  to be manually re-expanded into dummies and manually re-based (one level
  dropped per factor, replicating what `i.` does automatically) with no way
  to check the result was right. Rejected as too much hand-rolled numerical
  code with no ability to test it before Bruno runs it.

**What's implemented instead**: the classic "ridge regression = OLS on
augmented data" identity. One pseudo-observation is added per g(p_i) term,
with outcome 0 and `sqrt(ridge_lambda)` in that term's own column and
exactly 0 in every other column -- every school dummy, every X control,
every other g(p_i) term. Fitting that augmented dataset with plain `regress`
gives exactly the ridge solution, and because the pseudo-rows are always 0
on the school dummies and X controls, those stay completely unpenalized.
This uses nothing beyond `regress`/`tabulate ..., generate()`/`replace`/
`set obs` -- the same primitives already used everywhere else in this file
-- and `i.cohort_gr8` etc. can stay as ordinary factor syntax in the final
regression, because `regress` (unlike `matrix accum`) handles factor
variables correctly on its own; pseudo-rows just need to sit at each
factor's own base (minimum) level to zero out its non-base dummies, which
`regress`'s normal factor-variable machinery does automatically.

**Consequence for school-coefficient extraction**: because a pseudo-row
needs to represent "no school," which `ibn.school_rbd` cannot encode for a
real observation, school_rbd itself is expanded into literal dummies
(`sch_dum_1, sch_dum_2, ...` in sorted-level order) for this fit rather than
using `ibn.`. This means the coefficients come back named `sch_dum_#`
(position in sorted order) instead of Stata's own `#.school_rbd` labels, so
a small position -> school_rbd lookup table is built (in a scratch Stata
frame, so it doesn't disturb the analysis sample) and merged back in after
`parmest` to recover the real school id.

Also deliberately not `dsregress`/`poregress` for the underlying design, for
the same reason as before: those run a separate penalized fit per "variable
of interest," fine for a handful of policy variables, not for ~1,600+ school
dummies.

## Caveat (fixed lambda, not cross-validated)

`ridge_lambda` (set near the top of Step 4/5) is a single hand-set value.
Cross-validating it would mean rebuilding the augmented dataset and refitting
once per candidate lambda per fold -- more compute and more new code for a
nuisance-shrinkage knob that isn't the parameter of interest. If the
school-level estimates look sensitive to it, try a couple of values by hand
and compare `n_gp_terms_used` (always equals `n_gp_terms_candidate` under
Ridge -- nothing is ever dropped) and `sd_rc_vam` across runs in
`rc_vam_school_values_diagnostics_stata.csv`.

## Untested implementation risk

Everything here reduces to `regress`/`tabulate ..., generate()`/`set obs`/
`replace`/`frame create`/`parmest` -- all well-established Stata commands,
none of the newer, less-certain lasso-suite postestimation that the
superseded Lasso version depended on. The main things to watch for on a real
run: (1) `set obs` growing the dataset correctly mid-loop without disturbing
already-built columns, (2) the `_n <= n_real` boundary used to separate real
rows from pseudo-rows staying valid (breaks if anything sorts the data
between building the pseudo-rows and the extraction step -- nothing should,
but flagging it), (3) `frame create`/`frame drop` behaving as expected across
repeated per-outcome loop iterations. If any of these error, the fix should
be local to that one line, not a sign the overall design is wrong.

## Diagnostics

`rc_vam_school_values_diagnostics_stata_ridge.csv` still reports
`n_gp_terms_candidate` and `n_gp_terms_used` (always equal under Ridge) plus
a new `ridge_lambda_used` column, and `controlled_value_added_se_method` is
now `"stata_regress_dummy_simple_se_ridge_gp_augmented"`.

## Update 2026-09-21 (later same day): added as a 3-way robustness check, not the sole spec

Bruno asked for a Lasso version and a no-regularization version as separate
scripts with separate outputs, specifically to check how much the
regularization choice for g(p_i) actually matters empirically. Added:

- `code/claude/rc_vam/01_construct_rc_vam_school_values.do` (this one, Ridge)
  -- now the primary spec, and also the one that builds/caches
  `rc_vam_stata_analysis_sample.dta` in its Step 3. Output filenames
  suffixed `_ridge`.
- `code/claude/rc_vam/01_construct_rc_vam_school_values_lasso.do` -- revives
  the superseded Lasso mechanism from `2026-09-20-lasso-regularize-gp-block-
  stata-rc-vam.md` verbatim, as its own script. Output filenames suffixed
  `_lasso`.
- `code/claude/rc_vam/01_construct_rc_vam_school_values_noreg.do` -- the
  AHPW baseline, no regularization at all on g(p_i). Output filenames
  suffixed `_noreg`.

All three reuse the SAME cached `rc_vam_stata_analysis_sample.dta` (the
lasso/noreg scripts error out with a clear message if it isn't there yet,
rather than each re-running the ~300-line data construction / reshape step)
so the comparison across regularization choices holds the estimation sample
completely fixed. Run the main (ridge) script first at least once.

Worth checking once all three have actually run: whether `sd_rc_vam` (in
each `_diagnostics_stata_{ridge,lasso,noreg}.csv`) and individual school-
level `controlled_value_added` values are close across the three, or whether
they diverge meaningfully -- especially for schools where Lasso's
`n_gp_terms_used` is far below `n_gp_terms_candidate`, which is exactly
where the Ridge-vs-Lasso caveat above predicts trouble.
