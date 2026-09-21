# Baseline characteristics of high-VA compliers

From the repository root:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/high_va_compliers/test_estimators.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/high_va_compliers/01_run.R
```

Requires data.table and fixest. Input data root is configurable with
`CAUSAL_SCHOOLS_DATA_WD`; default discovery checks the existing Box and Dropbox
project roots. `COMPLIER_YEARS` defaults to `2018,2019,2020` and accepts a
comma-separated list. This follows the current three-cohort analysis window;
2021 has particularly poor baseline-score coverage. This is a descriptive
complier analysis, not an RC-VAM regression, and uses the observational EB
top-quartile classifications selected by the user.

## Median-cutoff version (kept separately)

Set `HIGH_VA_PERCENTILE=50` to define high VA as strictly above the equally
weighted school median (inverse empirical CDF, type 1). Run all three steps:

```powershell
$env:HIGH_VA_PERCENTILE = '50'
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/high_va_cutoffs/02_build_indicators.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/high_va_compliers/01_run.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/high_va_compliers/02_center_on_applicants.R
Remove-Item Env:HIGH_VA_PERCENTILE
```

The supported cutoffs are 25, 50, and 75; omitted defaults to P75.
For P25, use the same three commands with `HIGH_VA_PERCENTILE='25'`.
This defines high VA as strictly above P25 (approximately the top 75% of
schools), and saves all outputs in separate `p25/` subfolders. P25, median,
and P75 results are all retained.
For P50, indicators, clean results, and tables go in `median/` subfolders of
their corresponding original directories, preserving all P75 outputs. Both
versions use the same estimator, years, covariates, and exclusion rules.
The script recomputes D, Z, q, eligibility, and comparison-group means for
the selected threshold; it does not reuse P75 first stages or samples. Thus
the two tables can describe different applicants as well as different compliers.

## Inputs and definitions

- `data/clean/high_va_cutoffs/school_high_va.csv` in this repository, generated
  by `code/codex/high_va_cutoffs/02_build_indicators.R`.
- External data root: `data/clean/univ_gr8_df.csv` (only 13 columns read), and
  `data/clean/DA_probs/DA_probs_YEAR.csv`.
- Five outcomes: math, language, highinst, highpay, program_income_full.
- D: high VA of `most_time_RBD`, the existing most-attended-school definition.
- Z: high VA of first-round offered school, `rbd_treated_1R`.
- q: sum of the *unrounded* saved probabilities over high-VA assignment options.
  Different admission tracks at the same school inherit the same school label.
- Baseline characteristics: female (`GEN_ALU==2`, with codes outside 1/2 missing),
  `z_sim_mat_4to`, `z_sim_leng_4to`, `income_decile_imputed`,
  `father_educ_years_imputed`, `mother_educ_years_imputed`.

No new imputation, score standardization, or post-school outcome filter is added.
Existing income/parent-education imputations are baseline-context imputations;
the reported means therefore describe those completed measures.

## Sample and classification coverage

Start with timely applicants in the configured assignment years. Require
probability mass to sum to one within 1e-6, all positive-probability school
options to have an available VA category, and 0 < q < 1 (numerical tolerance
1e-10). No probabilities are renormalized over classified schools. Missing VA
never becomes low VA. Each VA may therefore have a different sample.

The explicit simulated `unmatched` option is a low *offer* state. Observed RBD
0 is coded Z=0 following the existing project no-first-round-offer convention;
actual missing RBD remains unknown. This does not label the school eventually
attended by unmatched students as low. The project's unresolved interpretation
of no-first-round-offer records remains relevant.

Then require known observed offer and attendance categories, and nonmissing
values of the characteristic being described. Samples are characteristic-specific;
main, untreated, and spline estimates for a given characteristic use identical
students. Selection on missing attendance can affect IV validity; the script
reports it rather than asserting it is innocuous. Covariate missingness changes
the complier population being described.

## Estimators and uncertainty

Let w=Z-q. The main treated-state mean is sum(w*D*X)/sum(w*D).
The untreated-state counterpart is sum(w*(1-D)*X)/sum(w*(1-D)). Under valid
assignment risk and binary-treatment monotonicity, both identify baseline
complier means weighted by q(1-q). They need not coincide in finite samples.
We do not pool them or claim to identify individual compliers.

For each ratio A/B, heteroskedastic-robust variance is
`n/(n-1) * sum((a_i-estimate*b_i)^2) / sum(b_i)^2`.
This is the moment sandwich, validated against an equivalent no-intercept IV
regression. Observations must be unique by person; the script checks this.
The spline robustness uses fixest 2SLS with a natural cubic spline of q (df=4)
in both stages and heteroskedastic-robust SEs. The baseline characteristic being
profiled is not added as its own regression control. Failed or unidentified
estimates are recorded with a status rather than silently substituted.

The saved `weighted_first_stage` is sum((Z-q)*D)/sum(q*(1-q)), an
assignment-variance-weighted complier share under the maintained assumptions,
not an F statistic. Negative/near-zero values undermine this interpretation.
The saved offer balance moment is mean((Z-q)*X). Neither positive first stages
nor balance establish individual monotonicity. Confidence intervals are normal
Wald intervals and may be unreliable with weak first stages; estimates are not
clipped to logical bounds.

SEs condition on estimated VA cutoffs and simulated q. They do not include
first-step estimation uncertainty or account for dependence across applicants
induced by the full assignment mechanism. Heteroskedastic SEs follow the current
scalar-IV convention; these are not randomization-inference SEs.

## Outputs

**Current reporting benchmark: common SAE population.** Run
`Rscript code/codex/high_va_compliers/03_center_on_sae_population.R` after all
three cutoff runs. This centers all 90 main estimates on a single mean per
characteristic among all timely SAE applicants in the saved cohort roster,
including applicants excluded from estimation. It validates that the full
rosters and baseline values agree across every VA/cutoff. Missing baseline
values are excluded characteristic by characteristic; no new imputation is
performed. The benchmark is fixed across VA definitions and cutoffs.

Use `complier_minus_sae_population.md` / `.csv` in the original, `median/`,
and `p25/` table folders. Clean numeric results have the same CSV filename;
`all_cutoffs_minus_sae_population.csv` and `sae_population_benchmarks.csv`
in the base clean folder contain combined results and reference means.
Joint SEs use individual contributions over the full observed population:
`I(IV sample)*b*(X-complier_mean)/sum_IV(b) - (X-population_mean)/N_population`,
with HC1 correction N_population/(N_population-1). This accounts for the
overlap of the IV sample with the population-mean sample. VA/probability
uncertainty remains conditioned out. Older eligible-sample-centered tables
remain available with their distinct `complier_minus_applicant_means` names.

To report differences from eligible applicants, run
`Rscript code/codex/high_va_compliers/02_center_on_applicants.R` after the main
script. This uses the saved student analysis inputs without rereading the
universe or assignment files. It saves `complier_minus_applicant_means.csv` in
the clean output folder and a compact CSV/Markdown table in the table folder.
The reference mean is the equally weighted mean among the same eligible
applicants with that characteristic observed, separately for each VA. It is
not a national mean or a mean across all SAE applicants. The 30 main
treated-state differences describe baseline composition, not treatment effects.
Their joint robust SEs use influence function
`b*(X-complier_mean)/mean(b) - (X-applicant_mean)`, where `b=(Z-q)*D`,
including the covariance between the two estimated means.

Repository `data/clean/high_va_compliers/` contains all 120 estimates (5 VA x
6 characteristics x 2 treatment states x 2 methods), sample exclusions,
student-level analysis inputs, and R session information. Each output is separate
from existing analyses. `output/tables/high_va_compliers/` contains a compact
CSV and Markdown table of the 30 main estimates.

References: local `writing/methods/papers/hoee_methods (1).pdf`, Section 3.2.1
for baseline complier means and Section 4.3 for recentered assignment instruments.

## Validation run (2026-09-21)

The default three-year run completed all 120 estimates with finite coefficients
and positive SEs. Eligible samples before characteristic missingness: math
36,676; language 47,464; highinst 41,577; highpay 90,103; full income 68,568.
Main weighted first-stage moments range from 0.346 to 0.450. Spline treated-state
means differ from main estimates by at most 0.0122 in the characteristic's units.
The constructed-population test recovers exact known complier means, matches
the robust IV variance, and checks treatment of unmatched and unknown schools.
