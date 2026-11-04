# Intervention magnitude plot

Run `Rscript code/codex/nov_pres_analysis/benchmark_plot.R` from the repository root.
Exports the PNG to `output/figures/nov_pres_analysis/intervention_benchmarks.png`
and the underlying source/value audit to `data/clean/nov_pres_analysis/intervention_benchmarks.csv`.
The presentation source is not modified.

The two own-paper effects use full-precision September EB P90-P10 gaps times
same-outcome lottery pass-through. The field probability effect is multiplied
by 100 to express percentage points. This is an implied school contrast, not
an evaluated intervention or a direct STEM effect.

## Published benchmarks (verified November 4, 2026)

- Muralidharan, Singh and Ganimian (2019), *Disrupting Education? Experimental
  Evidence on Technology-Aided Instruction in India*: Table 2, math ITT 0.37 SD,
  4.5-month intervention in Delhi, grades 6-9.
  https://alejandroganimian.com/files/20190216_Disrupting_education.pdf
- Guryan et al. (2023), *Not Too Late*: Table 5, pooled first-year CPS math-test
  TOT 0.282 SD (SE 0.059), Chicago grades 9-10. Corresponding ITT is 0.119 SD;
  the plot explicitly labels the participation/TOT estimate, not the offer effect.
  https://educationlab.uchicago.edu/wp-content/uploads/sites/3/2023/10/UChicago-Education-Lab-Not-Too-Late-Paper_03.23.pdf
- Breda, Grenet, Monnet and Van Effenterre (2023), *How Effective are Female Role
  Models in Steering Girls towards STEM?*: Section 4.3, 3.4 pp ITT increase in
  male-dominated STEM undergraduate enrollment among grade-12 girls. Selective
  STEM is a separate 3.1 pp outcome, not an independent intervention.
  https://www.parisschoolofeconomics.com/grenet-julien/wp/Breda_Grenet_Monnet_Van_Effenterre_2023.pdf
- Brodnax (2025, online 2024), *The Effects of Information Provision on the Gender
  Gap in Technology*: Table 6 female-imagery arm, year-2 technology-major choice
  among women, 2.2 pp = 4.7 - 2.5 pp. The article calls this arm analysis exploratory.
  It is not an effect on total STEM enrollment: substitution across STEM fields
  is possible. The combined-effect SE cannot be recovered from the two marginal
  SEs without their covariance; no significance claim is made for this benchmark.
  https://journals.sagepub.com/doi/10.3102/01623737241228278

These are selected contextual benchmarks, not a systematic review or a ranking
of cost effectiveness. Different SD normalizations, ages, exposure periods,
outcome definitions and ITT/TOT estimands prevent an apples-to-apples comparison.
No uncertainty intervals are plotted for any point, and no cross-study tests
or effect-size ratios are asserted. The right panel explicitly distinguishes
the paper's high-premium-field outcome from STEM/technology outcomes.
