# Decision: Document that Particular Pagado schools are absent from the SAE support

Date: 2026-04-26

## Context

While exploring possible binary high-value school definitions, we compared the distribution of adjusted STEM enrollment school values across SAE-supported schools, `Particular Pagado` schools, and other schools.

This raised the question of whether `Particular Pagado` schools appear in the SAE system at all.

## Diagnostic

Using the current `k100_timely_risk` probability-support file and the latest available school directory:

- `data/clean/DA_probs/probability_support_k100_timely_risk.csv`
- `data/raw/school_directory/2025/20250926_Directorio_Oficial_EE_2025_20250430_WEB.csv`

we checked dependency codes among supported RBDs.

The diagnostic found:

- `1,012` supported RBDs
- `1,005` supported RBDs with more than `100` students with positive assignment probability
- `0` supported RBDs with detailed dependency code `COD_DEPE == 4`
- `0` supported RBDs with grouped dependency code `COD_DEPE2 == 3`

## Decision

Record as a project fact that current SAE-supported school sets contain no `Particular Pagado` schools, as expected.

## Implication

Plots or binary-treatment designs that compare SAE schools to `Particular Pagado` schools should treat these as distinct categories. `Particular Subvencionado` schools are different: they are private subsidized schools and do appear in SAE support.
