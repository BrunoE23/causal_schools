# Decision: Normalize Scalar Offer Instrument to Outside-Support Baseline

**Date:** 2026-04-26
**Status:** active

## Context

The first-pass scalar school-value IV reduces a high-dimensional school assignment problem to a scalar treatment:

`D_i = V_{most_time_RBD}`

and a scalar offer instrument:

`Z_i = V_{rbd_treated_1R}`.

The probability-control matrix is restricted to a practical k=25 support: schools are retained if at least 25 timely at-risk SAE students have positive simulated assignment probability for that school. This creates a supported set of lottery schools and an outside-support category.

## Options Considered

- Center school values using the broad national/student-weighted mean.
- Center school values using the timely at-risk lottery-support mean.
- Treat no-offer and outside-support offers as the omitted category and normalize supported offer values relative to the outside-support offered-school baseline.

## Decision

Use the outside-support offered-school baseline for the current first-pass scalar IV.

For each school-value measure, compute:

`mu_out = mean(V_{rbd_treated_1R} | rbd_treated_1R outside supported set, rbd_treated_1R != 0)`

among timely at-risk students.

Then construct:

`D_i = V_{most_time_RBD} - mu_out`

and:

`Z_i = 1{rbd_treated_1R in supported set} * (V_{rbd_treated_1R} - mu_out)`.

Thus:

- no first-round offer has `Z_i = 0`
- an offer to an outside-support school has `Z_i = 0`
- an offer to a supported school has nonzero `Z_i` only to the extent that the supported school's value differs from the outside-support offered-school baseline

## Rationale

The scalar instrument is nonzero only for lottery-supported offers whose school value differs from the omitted outside-support category. This is more interpretable than setting no-offer or unsupported offers equal to the broad national mean, because the restricted probability-control matrix already defines a practical lottery support.

Subtracting the same baseline from `D_i` keeps the endogenous treatment and the scalar offer instrument on the same shifted value scale.

## Implementation

The construction is implemented in:

- `code/codex/scalar_school_value_iv/01_build_scalar_school_value_iv_df.R`

The R construction script starts from `univ_gr8_df.csv`, merges the k=25 probability-control file and school values, constructs the `d_*` and `z_*` variables, and writes:

- `data/clean/scalar_school_value_iv/scalar_school_value_iv_k25_timely_risk.csv`
- `data/clean/scalar_school_value_iv/scalar_school_value_iv_k25_timely_risk_baselines.csv`
- `data/clean/scalar_school_value_iv/scalar_school_value_iv_k25_timely_risk_diagnostics.csv`

## Risks and Follow-Up

This normalization depends on the definition of the supported set. If the k threshold, probability support, or estimation sample changes, the outside-support baseline should be recomputed.

The outside-support baseline is an implementation normalization, not a separate causal estimand. Results should be interpreted as the causal return to lottery-induced movement along the chosen school-value index among the supported lottery-offer margin.
