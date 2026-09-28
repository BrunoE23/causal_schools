# Decision: Clarify Actual Probability-Control Support Threshold (k=100, not k=25)

**Date:** 2026-09-20
**Status:** active

## Context

`setup_md_codex/project_memory/decisions/2026-04-26-use-k25-practical-probability-support.md`
documents a decision to use `k=25` as the practical-support threshold for the
wide `prob_*`/`iszero_*` probability-control matrix. However, the
implementation committed in the same session
(`code/codex/admission_probability_controls/01_export_probability_controls_k25.R`,
commit `9fc9f76`) shipped with `support_k <- 100L` from its first version.
There is no later commit that changes the threshold from 25 to 100 -- the
code and the doc simply disagreed from the start, and all downstream outputs
(`probs_columns_wide_k100_timely_risk.csv`, the scalar IV construction, the
current paper's estimates) use k=100.

## Clarification

The actual threshold used everywhere in the pipeline is **k=100**, not k=25.
Per Bruno (recalled 2026-09-20): the higher threshold was chosen because
k=25 produced a probability-control matrix that was too slow to run and/or
too large on disk. Raising k reduces the number of schools that qualify as
"supported" (fewer schools clear a higher positive-probability-count bar),
which shrinks the column count of the wide matrix and was enough to make the
regression tractable.

The file name and the 2026-04-26 decision doc still say "k25" for historical/
naming reasons; treat `k100_timely_risk` as the operative artifact.

## Forward-looking implication for RC VAM

`code/claude/rc_vam/00_export_probability_controls_all_timely_sae.R` builds
the same style of matrix over ALL timely SAE applicants (~319k) rather than
just the at-risk subset (~163k) -- see
`2026-09-20-rc-vam-estimation-sample-all-timely-sae.md` (or the RC VAM script
header) for why. At the same k=100 threshold, this roughly doubles the row
count, and a larger population also likely pushes *more* schools over the
100-student support bar, growing the column count too. Both dimensions
growing at once risks reintroducing the exact size/runtime problem that
motivated raising k from 25 to 100 in the first place.

If `00_export_probability_controls_all_timely_sae.R` is slow or produces an
unworkably large file at the default `RC_VAM_MIN_SUPPORT=100`, raise it (e.g.
to 150-250) via the `RC_VAM_MIN_SUPPORT` environment variable -- both that
script and `code/claude/rc_vam/01_construct_rc_vam_school_values.R` read the
same variable, so no code change is needed, just a higher threshold to
roughly offset the larger population.

**Update 2026-09-20 (later same day):** the "unworkably large file" risk
above was real but the fix was the storage format, not (only) a higher k. A
dry-run diagnostic against the all-timely-SAE population (~410k students,
1,617 supported schools at k=100) showed the dense wide CSV (~3,200
prob_*/iszero_* columns) was projected to take on the order of 45+ minutes
just to write, because it stores ~1.3 billion mostly-zero cells to hold a few
million real numbers. `00_export_probability_controls_all_timely_sae.R` was
rewritten to store the g(p_i) matrix long/sparse instead (nonzero
student-school probability triples + a small per-student roster);
`01_construct_rc_vam_school_values.R` reconstructs the wide matrix in R
memory immediately before the regression, so the regression logic itself did
not change. This should make k=100 (or lower) tractable again without
needing to raise the threshold, though it hasn't been run end-to-end yet.

## Risks and Follow-Up

The original 2026-04-26 doc's call for sensitivity checks across thresholds
(k=10, k=25, k=50, no threshold) was never done and is still open. Now that
there are two populations in play (at-risk-only vs. all-timely-SAE), a
sensitivity check should ideally cover both, since the "right" k plausibly
differs between them.
