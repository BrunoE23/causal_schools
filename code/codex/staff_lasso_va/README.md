# Joint staff Lasso and school VA

Approved 2026-09-20: separate Gaussian Lasso for each of 12 saved All-sample EB
VA outcomes, jointly entering teacher/orientador/leadership characteristics and
school context. These are predictions of saved estimates, not causal staff effects.

## Run from repository root

Use R 4.5.1 and bundled Python (numpy/pandas). The dependency helper installs
glmnet and missing dependencies to this task's Git-ignored local library, not
the system library. Installation requires CRAN network access.

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla code/codex/staff_lasso_va/00_dependencies.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla code/codex/staff_lasso_va/01_build_inputs.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla code/codex/staff_lasso_va/test_lasso.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla code/codex/staff_lasso_va/02_fit_models.R
& 'C:/Users/brunem/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/python.exe' code/codex/staff_lasso_va/03_verify_and_report.py
```

For an intentional refresh, add `--overwrite` to steps 01/02. Never alter the
source VA, staff caches or raw data. Input hashes are checked before and after.

## Specification

- Existing 3,682-school universe, staff window 2018-2024, observed histories
  beginning 2013. Each outcome uses all schools with a finite saved EB VA.
- 163 raw candidates: 27 school context, 43 HS-teacher, 46 orientador and
  47 leadership measures. All eight dated credential shares enter for each role.
- Role absence and incomplete rosters are separate from qualifications; undefined
  predictors get train-only median placeholders and missingness indicators.
  No outcome is imputed. Training-constant columns are excluded.
- No composite indices alongside their components. No broken post-2018 tenure
  averages, funding, 2024-only age or invented class-size measure. Existing
  staffing ratios use VA-sample counts, not annual HS enrollment.
- Joint 70-lambda grid 1 to 0.0005; school-only adds 20 weaker penalties and zero
  after a boundary audit (91 points). Alpha=1; X/Y scaled inside each training
  fold. Intercept unpenalized, every predictor penalized. Five inner tuning
  folds, five outer evaluation folds, stored deterministic common school splits.
- Main one-SE penalty, minimum-error sensitivity. Full-sample coefficients are
  separately tuned/refitted; prediction R2 is from outer held-out schools.
- Separately tuned school-only baseline uses identical outcome samples/folds.
  No post-selection OLS or conventional significance stars. Selection frequency
  is descriptive stability over the five outer training fits.

Detailed rationale and caveats are in
`setup_md_codex/project_memory/decisions/2026-09-20-staff-lasso-va.md`.
Estimator: https://glmnet.stanford.edu/articles/glmnet.html

## Outputs and checks

- `data/clean/staff_lasso_va/`: source manifest, predictor dictionary/exclusions,
  school matrix and credential shares, folds, outcomes, tuning curves, full
  coefficients, outer coefficients, predictions, performance, selection
  frequency, KKT audit, saved final models/settings and independent verification.
- `output/reports/staff_lasso_va.html`: readable coefficient tables for all
  12 outcomes with separate staff/context and missingness/coverage tables.
- `output/reports/staff_lasso_va.md`: Markdown version, main coefficients split
  into four-outcome panels; both penalties' predictive performance.
- `output/tables/staff_lasso_va/staff_lasso_coefficients.tex`: LaTeX table fragment
  requiring booktabs/longtable/array; coefficients split into four-outcome panels.

Synthetic R checks cover train-only preprocessing, absent/all-missing/constant
fields, scaling, tuning rule, optimizer KKT and coefficient/prediction agreement.
Independent Python reconstructs credential aggregation for all roles, uses the
original VA source, reconstructs all held-out predictions from independently
computed train-fold medians/scales and coefficients, checks all final/outer KKT
conditions and selection frequencies, and recomputes all R2/RMSE statistics.
The source VA is treated as fixed, not reestimated inside folds. Shared staff
and school-market relationships are not explicitly clustered in these folds.
