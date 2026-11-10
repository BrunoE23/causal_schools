# Enrollment regression with the VA baseline control set

The user expanded the current HE-enrollment regression to all baseline variables
used for VA estimation. Use the current observational-VA baseline specification
in `code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R`,
while retaining linear standardized PAES math and verbal.

Baseline controls: grade-4 SIMCE math and verbal separately through degree 3,
linear imputed income decile, both parent education years, both parent indigenous
indicators, sala cuna/jardin/prekinder/kinder indicators, standardized middle-school
GPA and attendance, and cohort/sex/age/student-comuna/middle-years-observed and
middle-school indicators. Retain existing source coding and imputations. Income
is linear as in the VA code, not decile dummies. Include parent years once.
Destination high-school fixed effects are the VA object rather than baseline
controls and are omitted. Do not restrict to SAE applicants or high math scores.

Start from the latest 569,681-person scored sample. Select only required columns
from the preflighted universe and middle-school files; match unique student IDs
and verify cohort and parent-variable consistency. Complete baseline availability
retains 559,914; exclude 9,767. Fit the previous linear-baseline model on exactly
the same sample for comparison. No new imputation or normalization is performed.

Inference clusters by attended high school. Absorb categorical baseline effects
without removing singleton groups, using CR1 with all baseline fixed effects
counted and fixest exact degrees of freedom. Independently verify the covariance
using within-transformed regressors, actual residuals, and the reported CR1 K.
Report average baseline-score derivatives incorporating quadratic/cubic terms
with exact linear-combination clustered SEs; preserve raw coefficients separately.
All estimates are descriptive conditional associations. Output task:
`he_enrollment_paes_va_baseline`, with code `20_add_va_baseline_controls.R`.

Completed results: N=559,914, G=3,371 schools, full K=6,749 including baseline
fixed effects. Overall R2 is 0.15694705 versus 0.02656683 for the previous model
on the identical sample; enrollment is 82.7082%. Full coefficients in pp:
father years 0.029274 (SE 0.023268, p=0.2084), mother years 0.150422 (0.025071),
PAES math 2.455654 (0.079522), PAES verbal 3.103380 (0.082299).
Average local baseline-score derivatives are math -0.315305 pp/SD (SE 0.078330)
and verbal -0.861191 (0.075889), conditional on all controls and PAES scores.
Do not interpret these as total causal effects of baseline achievement.
Both model covariance/normal-equation checks and source-integrity checks pass.
