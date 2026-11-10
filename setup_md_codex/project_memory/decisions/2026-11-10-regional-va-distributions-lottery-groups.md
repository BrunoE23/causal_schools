# Regional VA distributions by lottery persistence

The user requested 16 regional VA distribution plots with colors corresponding
to the preceding always / sometimes / never lottery-variation classification.
An optional clarification offered math, premium-field, or premium-institution
VA, with the stated default of EB-adjusted math VA. Proceed with math absent a
different preference. No new school classifications or lottery thresholds.

Use the same 2,529 classified grade-9 SAE schools and fixed school region map.
Join RBD to the current 2017-2020 EB source, analysis_sample All, outcome
z_year_math_max, column controlled_value_added_eb_centered_student. Preserve
its original centering and test-score SD units. These are adjusted observational
school values, not separately estimated school lottery effects. Do not restrict
to high-math students, re-estimate VA, weight by students, or standardize within
regions. Omit only schools without the requested VA and audit exclusions.

Give each school equal weight. Plot three colors consistently: blue always,
orange sometimes, red never. Kernel densities normalize within region-group
and therefore compare shapes rather than the groups' population shares. Use one
Gaussian-kernel bandwidth, bw.nrd0 of pooled plotted VA, for every curve and
common x/y scales. Draw a curve only for groups with at least five schools.
Show every observed school as a colored tick, in separate group rows below zero,
and display group counts including zero groups. This preserves small never-group
observations without inventing a smooth distribution from a single school.

Deliver a 16-panel overview, 16 individual regional PNGs, a vector overview,
and an HTML gallery. Save density ordinates/integrals, coverage, excluded schools,
plot specification, input checksums, and verification in clean data. Inspect the
rendered overview and individual plots before delivery. The inherited unequal
SAE observation windows and one-year schools remain documented.

Math implementation matches 2,504 schools: 2,194 always, 296 sometimes, and 14
never. Missing VA accounts for 25 schools (17 / 3 / 5, respectively). All 16
regions remain represented. Use 30 density curves; the other nonempty groups
have fewer than five schools and are represented by ticks. The shared bandwidth
is approximately 0.020876 test-score SD. Densities integrate to one within 0.005
tolerance over a shared range extending four bandwidths beyond observed VA.
Input hashes are unchanged. Save the individual plots, overview, and gallery
under the lottery_variation_by_region task with the math subdirectory/suffix.

The completed overview and individual plots for La Araucania (all three curves)
and Atacama (one always curve, one sometimes tick, no never observations) were
visually checked. Sparse groups remain explicit and labels are not clipped.
