# Orthogonal School VA

This standalone workflow constructs a math VA and a STEM-enrollment VA component
that is orthogonal to math VA. It does not modify the existing
`school_rbd_observational_values` or scalar-IV scripts.

## Method

The script adapts the EB-free variance/covariance construction from Rose,
Schellenberg, and Shem-Tov (2025). For each outcome, it estimates the current
adjusted school-VA model with:

- school fixed effects for `most_time_RBD`
- middle-school fixed effects for `most_time_RBD_middle`
- the baseline demographic, SIMCE, parent-survey, and middle-school controls
  used in the existing adjusted VA workflow

It then builds school-by-cohort residual means:

```text
Ybar_sc = school FE_s + mean residual among students in school s, cohort c
```

The math/STEM covariance uses cross-cohort products only, excluding products from
the same cohort within school. The projection coefficient is:

```text
beta_stem_on_math =
  Cov_RSS(STEM school effect, math school effect) /
  Var_RSS(math school effect)
```

The main orthogonal metric is:

```text
stem_va_orth_math =
  stem_va - beta_stem_on_math * math_va
```

The output also includes the reverse residualized object,
`math_va_orth_stem`, for diagnostics.

For the final pooled school scores, the script reuses the existing All-sample
adjusted VA file from `school_rbd_observational_values` when available. The new
regressions are used for the school-by-cohort residual means and the
noise-corrected projection coefficient.

## Run

```powershell
& 'C:\Program Files\R\R-4.4.1\bin\Rscript.exe' .\code\codex\orthogonal_school_va\01_construct_orthogonal_school_va.R
& 'C:\Program Files\R\R-4.4.1\bin\Rscript.exe' .\code\codex\orthogonal_school_va\02_run_orthogonal_va_iv.R
```

## Outputs

Outputs are written to:

```text
C:/Users/xd-br/Dropbox/causal_schools/data/clean/orthogonal_school_va/
```

- `school_orthogonal_va.csv`: one row per school with pooled math VA, pooled STEM
  VA, `stem_va_orth_math`, and diagnostics flags.
- `school_cohort_residual_means.csv`: school-by-cohort residual means for math
  and STEM.
- `orthogonal_va_covariance_inputs.csv`: school-level cross-cohort signal inputs.
- `orthogonal_va_diagnostics.csv`: covariance estimates, projection coefficient,
  and naive before/after correlations.
- `orthogonal_va_iv_regression_df.csv`: lottery-IV sample with attended, offered,
  and expected math/orthogonal-STEM VA metrics.

The IV script also writes paper-table files to:

```text
output/tables/orthogonal_school_va/
```

- `orthogonal_va_iv_results.csv`: full long-form coefficient output.
- `orthogonal_va_iv_2x2.csv`: compact 2-by-2 table.
- `orthogonal_va_iv_2x2.tex`: LaTeX version of the 2-by-2 table.
