# Decision: Use k=25 Practical Probability-Control Support

**Date:** 2026-04-26
**Status:** active

## Context

The simulated Deferred Acceptance assignment probabilities can create a very wide matrix of school-specific probability controls. If there are `J` schools, the wide control matrix contains roughly `2J` columns: one `prob_<rbd>` column and one `iszero_<rbd>` column for each retained school.

Including every school that appears anywhere in the simulation is computationally expensive and may add fragile controls for schools with almost no support in the actual estimation sample.

## Options Considered

- Keep all schools appearing in the simulated probabilities.
- Keep a fixed number of large schools, as in the older export script.
- Define support from the actual IV construction sample and retain only schools with enough positive-probability students.

## Decision

Define the practical probability-control support among timely at-risk SAE students with matching simulated assignment probabilities.

A school enters the supported set if at least `25` timely at-risk students have positive simulated assignment probability for that school.

The retained schools generate the wide probability controls:

- `prob_<rbd>`
- `iszero_<rbd>`

## Rationale

The k=25 rule reduces the dimensionality of the probability-control matrix while keeping schools that have meaningful support in the actual lottery-risk sample. This is preferable to selecting schools by size alone because the relevant support is assignment-risk support, not simply school enrollment or capacity.

The threshold also avoids creating many columns for schools that appear with positive probability for only a handful of students.

## Implementation

The support rule is implemented in:

- `code/codex/admission_probability_controls/01_export_probability_controls_k25.R`
- `code/codex/scalar_school_value_iv/01_build_scalar_school_value_iv_df.R`

Outputs are written under:

- `data/clean/DA_probs/probs_columns_wide_k25_timely_risk.csv`
- `data/clean/DA_probs/probability_support_k25_timely_risk.csv`
- `data/clean/DA_probs/probability_export_diagnostics_k25_timely_risk.csv`

The scalar IV construction later uses the same supported set to code supported offers versus outside-support offers.

## Risks and Follow-Up

The threshold is practical rather than theoretically fixed. Sensitivity checks with alternative thresholds, such as k=10, k=50, or no support threshold, may be useful later.

The retained probability mass diagnostic should be checked whenever the support rule or estimation sample changes. If the retained mass falls substantially below 1, the support restriction may be dropping too much assignment-probability support.
