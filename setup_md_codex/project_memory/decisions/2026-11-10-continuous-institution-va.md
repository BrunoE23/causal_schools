# Continuous institution-premium school VA

The user requested school VA using the continuous HE institution FE underlying
the existing high-premium-institution binary. Use the centered institution FE
from the existing MiFuturo `institution + AREA_CARRERA_GENERICA` model, in log
income units. Do not substitute the existing one-way institution income
prediction. First HE enrollment and the FE table's unweighted-institution
centering are retained. Validate the join against the original person-level
FE export and reconstruct `high_inst_m1 = 1[enrolled and FE > 0.1]` exactly.

The user explicitly instructed: "leave as NA for now" for non-enrollees.
Estimate only the enrolled-with-observed-FE outcome. Enrollees at institutions
without a MiFuturo FE also remain missing. Report excluded students and
institutions. The estimate conditions on a downstream enrollment decision.

The existing three-part institution key misses many supported professional
institutes because the FE table spells the type `INSITUTOS PROFESIONALES`
versus `INSTITUTOS PROFESIONALES` in enrollment. For this continuous outcome,
join on official institution code after verifying uniqueness in the 92-row
FE table. Preserve and verify every legacy matched FE and save a full join
audit. Do not change the existing binary files or its published school VA.
The code join recovers 91,458 students at 21 institutions in the complete-control
exam-taking sample. Of those, 1,535 have premium >0.1; the existing binary join
therefore has a small but nonzero classification error, logged in issues.md.
Eligibility before estimator drops is 482,923 enrolled students at 86 supported
institutions. Non-enrollees number 129,639; unsupported enrollees number 10,247.

Preserve the current 2017-2020 observational VA design: grade-8 ages 12-16,
observed positive high-school RBD, observed math or verbal entrance score,
complete baseline controls, cohort/sex/age/comuna/middle-years indicators,
cubic grade-4 math/verbal, linear imputed income, parent/early-childhood controls,
middle-school GPA/attendance and middle-school FE. Destination high-school FE
is the VA object. No downstream PAES score control, SAE restriction, or gender
split. Default fixest singleton/perfect-fit removal follows the original code.

Student-weight center school FE and use the existing lfe centered-school
IID SE function with bN=100. Fixed seed 20261110. With lfe 3.1.1, 4 threads,
500 MB bootstrap memory and tolerance 1e-8, the package rounds this to 128
actual draws (two blocks of 64); these match the run's original option values.
Apply the existing
student-weighted normal-normal EB variance-minus-noise and recentering formulas.
Treat estimated MiFuturo premia as fixed. Save actual fitted models and audit
normal equations, source preservation, count consistency and EB identities.
This request computes observational VA; it does not itself run lottery IV.

Code: `code/codex/continuous_institution_va/`.
Local data/results: `data/clean/continuous_institution_va/`.

Completed: 482,583 enrollees in 3,256 high schools after 340 estimator removals;
all belong to one connected high-school/middle-school component. Outcome mean
0.01447028, SD 0.09203520; overall regression R2 0.428016. Student-weighted
unshrunk VA SD 0.03391518, EB VA SD 0.03100715, inferred signal SD 0.03275547,
and mean reliability 0.9366469. School-unweighted EB p10/p90 are
-0.03252755/0.03621445 (spread 0.06874199 log-premium units).

Across 3,234 common schools, the existing binary EB VA correlation is
0.8848986 unweighted and 0.9151831 weighted by continuous-VA enrollment counts;
Spearman correlation 0.8444925. The existing binary has 570,772 observations
and retains its earlier matching and non-enrollee zeros, so this comparison
does not hold the estimation sample fixed. No inference about realized wage
effects is implied by the log-premium units.

All sample, source-integrity, exact FE-join, connected-component, normal-equation,
student-centering, count and independently reconstructed EB checks pass.
Report: `output/reports/continuous_institution_va/report.html`.
