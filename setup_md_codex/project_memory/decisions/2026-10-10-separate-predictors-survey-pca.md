# Separate academic predictors and parent-survey PCA

Bruno requested understanding peer-relative GPA and the GPA-minus-SIMCE-math gap,
redoing the table one at a time, and including PCA of the parent survey. The
working interpretation, stated to Bruno with an optional clarification, is one
grade-specific academic measure plus its square per regression. No new geographic
analysis, prediction tuning or causal claim is introduced.

## Population and specifications

Keep all 313,813 students whose grade-8 origin offers grade 9, with explicit
regular/complementary grade-9 SAE application as the outcome. Each of six academic
constructs in each of grades 5-8 enters separately, yielding 24 OLS regressions.
Also fit all 24 academic measures jointly. All retain the original voluntary
training medians, means and SDs, squares and relevant missingness indicators.

Common controls are cohort-by-region, sector, gender, age/square/missingness,
same grade-4 and grade-8 school/missingness, parent-record indicator, survey year,
fraction of applicable questions answered, year-specific questionnaire-complete
indicators and year-specific PC1 scores. Individual parent-answer coefficients
are replaced by PCA. Thus this is a new common-control comparison; differences
from the old table cannot automatically be attributed only to dropping one term.
All SEs cluster by origin school. PCA-score uncertainty is conditioned on.

For the focal interpretation, compare GPA alone, math gap alone and both together
on the identical observed-GPA/observed-gap sample within each grade. This prevents
missing score availability from explaining those coefficient changes. The focused
samples are 274,971, 274,573, 274,905 and 274,425 pupils for grades 5-8.

## PCA construction

Use each year's full available item set from the existing 39-construct crosswalk:
19 questions in 2013, 29 in 2014, 20 in 2015 and 16 in 2016. Separate annual PCAs
avoid interpreting not-asked items or questionnaire changes as attitudes.
Fit exact correlation PCA among complete respondents in the voluntary sample;
center/scale items within those respondents. Do not impute answers or use Y.
Counts are 18,294, 49,701, 76,778 and 73,468, totaling 218,241 (69.5% of pupils).
Noncomplete pupils remain in regressions with zero PC scores and explicit
completeness/response controls. Their zero is a reference, not an observed rating.

Orient PC1 toward favorable ratings; all its loadings are positive. Scale scores
to within-year mean zero and SD one. Do not impose a common coefficient across
different yearly questionnaires. Main models use PC1; a prespecified sensitivity
uses PCs 1-3 separately by year. PC2/3 signs have the largest absolute loading
positive, but their substantive meaning is year-specific. Save all PCA objects,
loadings, explained variance, coverage and the analysis frame.

## Meaning of GPA and gap coefficients

Before extra regression standardization, define G = peer-standardized GPA,
M = peer-standardized baseline SIMCE math and D = G-M. Then aG+bD=(a+b)G-bM.
Holding G fixed, a larger gap is lower baseline math. Holding D fixed, a higher
G moves both current GPA and baseline math. The original model also has the
language gap, whose coefficient contributes to the current-GPA re-expression.
Undo each regressor's SD before combining coefficients; use the full clustered
covariance to derive transformed SEs. Quadratic gap terms expand into squared
levels and interactions, so re-expressed slopes are local at centered references.
Absolute GPA, attendance and other grades also change what a conditional slope
means. A gap is a relative-position difference, not a learning-gain estimate.

## Results

Grade-8 peer GPA is +.3126 pp (SE .0948) alone versus +5.8969 (.4290) in the new
joint model; the math gap is -.6792 (.0735) alone versus -2.9502 (.4676) jointly.
On the common observed grade-8 pair sample, GPA is +.276 (.099) alone and +1.036
(.114) with the gap; the gap is -.685 (.074) alone and -1.290 (.083) with GPA.
Thus GPA-gap overlap explains part, but not all, of the full-model conditioning
difference. Original grade-8 slopes, re-expressed in the underlying peer z-score
units at the reference values, are +2.413 (.367) for current GPA holding both
baselines fixed, +2.663 (.420) for math baseline holding GPA/language fixed, and
+.892 (.437) for language baseline holding GPA/math fixed.

PC1 explains 47.0%, 51.7%, 53.5% and 48.9% of response variation. With all academic
controls, its coefficients are -1.129 (.335), -1.244 (.194), -1.072 (.159) and
-.675 (.135) pp per SD for 2013-2016. The four PC1 coefficients jointly have
F(4,1999)=26.397, p=2.32e-21. These conditional descriptive associations refer to
grade-4 parent reports and may concern a different school from grade 8.

## Artifacts and verification

Scripts 12-15 in `code/codex/sae_participation/` construct PCA, fit comparisons,
verify the rebuilt design, and render the report. Outputs are under
`data/clean/sae_participation/interpretation_pca/` and
`output/reports/sae_participation/voluntary_separate_pca.html` (Markdown alongside).
Verify PCA eigenvectors/orthogonality/variance, score scaling, complete regression
samples, single-measure formulas, fixed pair samples, exact polynomial algebra,
and independently reconstructed clustered SEs for the grade-8 pair. Compare the
rebuilt training and test academic/context columns to the prior cached designs.
All 51 regression/design checks passed, alongside the annual PCA checks.
The coefficient-comparison figure was visually inspected. Full-year grade-8
inputs remain potentially post-application; none of these results are causal.
