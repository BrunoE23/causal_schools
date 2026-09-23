# Decision: Abandon RC-VAM

**Date:** 2026-09-22
**Status:** active

## Context

RC-VAM (Angrist, Hull, Pathak, Walters 2024, ReStat) was explored as a way to
obtain credible school effects for undersubscribed schools. Several
implementations were tried (Stata ridge/lasso/noreg, R noreg, codex test1).

## Decision

Bruno dropped RC-VAM on 2026-09-22. Do not revive, extend, or debug the
RC-VAM pipeline, and do not propose it as the path for undersubscribed-school
effects, unless Bruno explicitly asks.

## Implementation

- Existing RC-VAM code (`code/claude/rc_vam/`, `code/codex/rc_vam*`,
  `acropolis_transfer/`) and outputs (`*/rc_vam_test1/`, `rc_vam_scalar_iv/`)
  are left in place for the record. They are not current paper inputs.
- The EB observational-VA scalar-IV workflow remains the main path.
- Supersedes the open issue and diagnosis in
  `2026-09-21-rc-vam-noreg-eb-shrinkage-diagnosis.md`; the ridge, lasso,
  3-cohort, test1 theta, and RC-VAM sorting decisions are inactive.
