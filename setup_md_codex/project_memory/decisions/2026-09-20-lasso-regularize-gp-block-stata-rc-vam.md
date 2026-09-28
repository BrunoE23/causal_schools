# Decision: Lasso-Regularize the g(p_i) Block Only, in the Stata RC VAM Regression

**Date:** 2026-09-20
**Status:** SUPERSEDED same day -- see
`2026-09-21-ridge-regularize-gp-block-stata-rc-vam.md`. Bruno switched the
choice from Lasso to Ridge shortly after this was implemented, precisely
because of the "Caveat (open, not resolved)" below -- Ridge can't drop a
CIA-relevant g(p_i) term the way Lasso can. Kept here for the record of what
was tried and why it was replaced; the .do file no longer matches this doc.

## Context

`code/claude/rc_vam/01_construct_rc_vam_school_values.do` fits, per outcome, a
single OLS regression `y ~ ibn.school_rbd + x_terms + gp_used, noconstant`,
where `gp_used` is the g(p_i) risk-control block (prob_/iszero_ terms for
every supported school, ~3,000 terms at k=100). Bruno flagged this as needing
regularization given the size of that nuisance block ("I think we should add
regularization in this step, given the 3 k extra paarameters").

## Decision

Regularize **only** the g(p_i) block, via Lasso (L1). The school dummies
(`ibn.school_rbd` -- the parameters of interest) and the X_i controls
(`x_terms`) are never penalized. Considered and rejected:

- **Ridge on g(p_i) only** -- was the initial recommendation (shrinks every
  term but never zeroes any out, so it can't drop a CIA-relevant-but-
  outcome-weak term for some school -- see Caveat below). Bruno chose Lasso
  instead.
- **Regularize everything, including school effects** -- rejected outright:
  shrinking the school coefficients themselves defeats the point of
  estimating them.
- **Stata's `dsregress`/`poregress`** (native double-selection / partialling-
  out lasso commands) -- these are built for a *few* policy variables of
  interest, running a separate lasso for each one against the control pool.
  With ~1,600+ school dummies as the "variables of interest," that would mean
  ~1,600 extra lasso fits per outcome -- not tractable. Rejected on
  computational-design grounds, not statistical ones.

## Mechanism actually implemented

One `lasso linear` fit per outcome, with school dummies (expanded to literal
dummy variables via `tabulate ..., generate()`, not `ibn.` factor notation,
since that combination is untested inside lasso's always-included list) and
`x_terms` forced in as "always included" (unpenalized), and `gp_used` as the
only candidate pool subject to the L1 penalty/selection. Then a plain
post-lasso OLS refit -- `regress y ibn.school_rbd x_terms gp_selected,
noconstant`, same design as before, just with `gp_used` narrowed to whatever
survived selection -- produces the actually-reported point estimates and
analytical SEs. The lasso step's only job is deciding which g(p_i) terms are
worth keeping; it does not itself supply the reported coefficients.

## Caveat (open, not resolved)

AHPW's own specification keeps every g(p_i) term unconditionally: each
term's job is satisfying conditional independence for its own school, not
predicting the outcome well. Lasso selects on outcome-predictive power, so it
can legitimately drop a term that is weak for the outcome but still
CIA-relevant for some school -- this is exactly the failure mode Ridge would
have avoided (shrink, never zero out). Bruno chose Lasso anyway. If a
specific school's RC VAM estimate looks unstable or an outcome's diagnostics
show a lot of dropped terms (`n_gp_terms_used` << `n_gp_terms_candidate` in
`rc_vam_school_values_diagnostics_stata.csv`), check whether that school's own
prob_/iszero_ term was among the terms lasso dropped for that outcome.

## Untested implementation risk

Two Stata mechanics used here could not be verified without a Stata license:

1. Whether `e(allvars_sel)` is the right stored result to pull the
   lasso-selected variable list (the script tries this first, falls back to
   `lassocoef, matrix(B)` / `rownames`, and if both fail, degrades to keeping
   *all* g(p_i) terms for that outcome -- i.e. no regularization applied,
   with a loud error message -- rather than crashing).
2. Whether `lasso linear`'s always-included-parentheses syntax accepts
   `noconstant` cleanly alongside a full (non-omitted) dummy set this wide.

If Bruno runs this and either mechanic errors, the fix is almost certainly
local to that one line (see the inline comments in Step 5 of the .do file),
not a sign the overall design is wrong.

## Diagnostics added

`rc_vam_school_values_diagnostics_stata.csv` now reports both
`n_gp_terms_candidate` (pre-lasso, zero-variance-filtered pool) and
`n_gp_terms_used` (post-lasso, actually included in the final refit) per
outcome, so the amount of regularization applied is visible per outcome.
