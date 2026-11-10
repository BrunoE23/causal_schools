# Continuous institution-premium school value added

Replace the high-premium institution indicator with the underlying centered
institution FE from `log(income) ~ institution + AREA_CARRERA_GENERICA`.
This is the two-way, field-adjusted premium used to construct `high_inst_m1`,
not the one-way institution predicted-income outcome.

Run from the repository root with R 4.5 and the existing project packages:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/continuous_institution_va/01_prepare.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/continuous_institution_va/02_estimate.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/continuous_institution_va/03_report.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/continuous_institution_va/04_verify.R
```

Inputs are the consolidated Box universe and the repository's existing
MiFuturo institution FE and person-level outcome files. `CAUSAL_SCHOOLS_DATA_WD`
and `CAUSAL_SCHOOLS_REPO_WD` can override the roots. Large CSVs are read with
column selection. No raw or existing clean source is changed.

Keep the current 2017-2020 cohorts, ages 12-16, observed destination high school,
exam-taking restriction, complete baseline controls, and default estimator drops.
Use the exact current VA control terms and high-school/middle-school FEs;
do not control for downstream PAES scores. All sexes and school sectors are pooled.

The outcome `institution_fe_enrolled_m1` is restricted to enrollees with an
observed FE. Non-enrollees are NA, as explicitly requested by the user.
Unsupported enrolled institutions are also missing. The result conditions on
a downstream enrollment decision and is not an unconditional causal effect.
Match FEs by official institution code, which is unique in the FE table.
Validate every legacy matched FE against this join. The older three-part
code/type/name join misses professional institutes because of a type typo
(`INSITUTOS` versus `INSTITUTOS`); the code join recovers their actual FEs.
Save the full join audit and preserve all existing binary outputs.

Point estimates and student-count centering match the current VA constructor.
Regression-derived SEs reuse its `lfe::getfe` student-centered estimable function
and IID bootstrap with bN=100, with deterministic seeds. With lfe 3.1.1,
4 threads and 500 MB bootstrap memory, this rounds to 128 actual draws on this
sample; the configuration is recorded. EB shrinkage matches
the existing student-weighted variance-minus-noise rule and recenters the result.
Uncertainty treats the estimated MiFuturo institution premia as fixed, as does
the existing binary pipeline. These are observational school VA estimates;
this task does not estimate lottery IV pass-through.

Derived student data, actual regression models, school VA, summaries, coverage,
source manifests and checks are saved under `data/clean/continuous_institution_va/`.
The IDs and student data are local, ignored research artifacts.
The HTML report is `output/reports/continuous_institution_va/report.html`;
the static distribution/comparison figure is in `output/figures/continuous_institution_va/`.
The verifier checks the exact connected components of the high-school/middle-school
graph and independently reconstructs the saved EB results. It avoids rereading
the large fitted model because the estimation script already checks its actual
normal equations. The final sample is 482,583 students and 3,256 schools in one
connected component, following removal of 340 singleton/perfect-fit observations.
Student-weighted VA SDs are 0.03391518 unshrunk and 0.03100715 EB in log-premium
units. The EB correlation with the existing binary VA is 0.8848986 across
3,234 common schools; their outcome definitions and estimation samples differ.

If an interruption occurs after complete per-outcome SE/EB checkpoints have
been written, `02_estimate.R --resume-completed` validates those checkpoints
and restores the aggregate export without repeating estimation. Then run
scripts 03 and 04. This option never accepts point-estimate-only checkpoints.
