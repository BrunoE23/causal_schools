# PAES test-form score differences

User request: test whether receiving a different PAES form gives a score advantage.
Clarification: test variation across forms within each year; do not test whether
one form is consistently better across years. All regressions already implement
separate within-year, within-sitting, within-subject comparisons.

## Run

From `C:/Users/brunem/Research/causal_schools` in PowerShell:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/paes_test_forms/01_preflight.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/paes_test_forms/02_analyze.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/paes_test_forms/04_verify.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/paes_test_forms/05_report.R
```

Packages: data.table, fixest, ggplot2. Optional scripts `00_inspect.R` and
`03_inspect_results.R` print diagnostics without changing inputs.

## Inputs and efficiency

`CAUSAL_SCHOOLS_DATA_WD` defaults to `C:/Users/brunem/Box/causal_schools`.
Read one `A_INSCRITOS_PUNTAJES*.csv` per year under `data/raw/2023` through
`data/raw/2026`. Header/size inspection precedes reading each 202-239 MB file.
Read selected columns with data.table, excluding item-response strings and names.
Cache each year locally (approximately 95-111 MB); the estimator reads one year at
a time and builds one subject-specific sample at a time. Do not modify raw data.

The accompanying 2023 and 2026 PDF dictionaries confirm that FORMA fields identify
test forms. LOCAL_EDUCACIONAL is part of educational-establishment metadata;
it is not used as a test-center identifier.

## Estimand and sample

Primary: mean reported-score differences across forms within year, regular
sitting, and subject/module. Eight tests/modules in each admissions process
2023-2026. Winter PAES 2024-2026 is supplementary; winter 2023 was PDT and is
excluded. Admissions year t regular examinations occurred late in t-1.
Form codes are not comparable across years or subjects.

Match each FORMA_REG/INV field with its same-sitting ACTUAL score, never MAX.
Analyze biology, physics, chemistry and technical sciences separately, using
module-specific form indicators and the same-sitting CIEN score. Verify that a
student has at most one positive science-module form within a sitting.

Keep form > 0, score 100-1000, RBD > 0, GPA 4-7, ranking 100-1000, age 14-90,
recorded sex 1/2, and graduation year 1930 through admissions year minus one.
Use complete cases and schools with at least two observed forms. No SAE,
college-application, or university-enrollment restrictions. Age is evaluated at
December; for winter this is a uniform six-month shift, absorbed by the cubic.
Three positive-form records have invalid scores and are excluded.

Regress score on form indicators, cubic GPA, cubic ranking, cubic age, sex,
graduation-year and educational-track indicators, and attended-school fixed
effects. Centering/scaling of controls is solely numerical. Raw and school-only
models use identical observations. Current-graduate robustness adds the
restriction graduation year = admissions year - 1 and reapplies form support.

Secondary outcomes are correct-answer counts. Do not condition score on correct
answers, which is downstream of form and differs in interpretation across forms.
Baseline balance tests regress GPA, sex, age, or recent-graduate status on form
and attended-school effects; the tested outcome is not also a control.

## Inference

Regression sandwich SEs cluster by attended school. `fixest::ssc` uses K.adj=TRUE,
K.fixef="nonnested", G.adj=TRUE and t.df="min". Form-only Wald tests use
F(q, G-1); pairwise contrasts use the full coefficient covariance and t(G-1).
No residual-spread or other approximate SEs are substituted.

Disable covariance regularization (`vcov_fix=FALSE`). Some full covariances are
numerically singular in nuisance directions: negative eigenvalues are less than
2.4e-16 of the largest in absolute magnitude. All tested form blocks are positive
definite. Independent QR/FWL estimation and manual clustered sandwich reproduce
the headline Reading 2026 form SEs to 2.7e-14.

Holm corrections are applied separately by sitting, specification and outcome:
32 regular or 24 winter joint tests. For balance, all characteristics form one
family (128 regular and 72 winter tests). Pairwise Holm tests use all pairs in
the family; regular score contrasts number 102. Bonferroni simultaneous 95%
intervals protect against selection of the highest-minus-lowest fitted form.
The selected point gap remains upward-biased as an estimate of the true maximum.

## Results and interpretation

13/32 primary adjusted regular tests survive Holm correction; 12/32 survive
in the current-graduate robustness. Reading 2026: form 103 minus 106 = 7.4405
points (SE 0.5006; simultaneous 95% CI 5.6936-9.1874; N=247,204, 3,443 schools).
The estimate is 0.0595 score SD. For current graduates it is 8.4479 points.
No baseline balance joint test survives the family correction. One of 24 winter
tests survives (2024 biology); winter support coverage is weaker.

These are conditional associations consistent with small form advantages.
Observed balance does not verify random assignment. Test-center/room identifiers
and the assignment rule are unavailable. School clustering may not capture all
test-room dependence. Results concern mean score differences, not all quantiles,
individual treatment effects or university-admission effects.

## Outputs

- `data/clean/paes_test_forms/`: selected input caches, source metadata, all form
  counts, sample accounting, descriptive statistics, joint tests, all pairwise
  contrasts, largest fitted gaps, regression objects, validation and covariance
  diagnostics. Individual records stay in ignored clean data.
- `output/reports/paes_test_forms/report.html`: complete readable report.
- `output/figures/paes_test_forms/reading_math_form_gaps.png`: static research figure.

The design and interpretation are also recorded in project memory:
`setup_md_codex/project_memory/decisions/2026-10-24-paes-test-form-advantages.md`.
