# Add reported home-region effects and change income reference to decile 5

The user requested region of origin (possibly using PAES data) and income
decile 5 as the omitted category in the four main high-math OLS models.
Operationalize origin as the student's reported domicile region in the same
admission process as the existing `psu_year`. Use `CODIGO_REGION_DOMICILIO`
from each year's public PAES/PDT socioeconomic-domicile file, joining by MRUN
and admission year. This is residence reported in the admissions data, not
birthplace or necessarily grade-8 residence. Do not substitute school or HE
institution region and do not replace the existing baseline income decile.

The 2026 dictionary (page 2 and territorial Annex I) identifies the domicile
region field and its regional codes. The identical field is present in all
six source files for 2021-2026. All 93,423 prior high-math regression students
match in their admission year and have a valid code 1-16; all regions are
represented. There is no missing-region exclusion or imputation. Preserve
the exact sample, original z_math >= 1 selection/normalization, non-enrollee
zeros, 840 admission-2026 records, and 2,735 attended-school clusters.

Include additive region indicators in both full and no-income models, alongside
the existing full total-degree-three math/verbal basis and admission-year
indicators. Income decile 5 is the reference; all other decile coefficients
and their actual clustered SEs are contrasts to D5. Region code 1 is the
omitted region (a nuisance parameterization only). Full K=39 and reduced K=30.
Use school-clustered CR1 with t(G-1) inference, and report average local score
derivatives in percentage points per original SD.

Income's contribution remains (R2_full - R2_without_income)/R2_full, now
conditional on scores, admission year, and reported home region. Use ordinary
unadjusted in-sample R2 and identical students in both models. A reference-
category change alone cannot change fitted values or R2. Verify this by fitting
the region model with D1 omitted and checking the coefficient/covariance
transformation to D5 as well as fitted-value equality. Compare region-model
R2 with the previous no-region model on the same sample. Preserve earlier results.

Preparation: `13_build_paes_origin_region.R`. Estimation/reporting: scripts 09
and 10 with `--high-math-main-region-d5`, all under
`code/codex/premium_enrollment_scores_income/`. Outputs use
`high_math_main_region_cubic_income_contribution` under clean data, reports, and
tables. Read only three columns per 15-47 MB raw file and cache the matched
sample; never modify raw/master inputs. Save linkage audits, source manifests,
region coverage/counts, nested-model/covariance checks, reference verification,
all coefficients, model objects, and R2 contribution measures. No plots.

Completed results (institution / field / either / both): full R2 is
0.3297 / 0.0870 / 0.1411 / 0.1741; no-income R2 is
0.3101 / 0.0746 / 0.1404 / 0.1736. Income's incremental share is
5.94% / 14.26% / 0.48% / 0.30%, compared with
33.54% / 24.11% / 1.62% / 6.53% before adding region on the same sample.
The D10-versus-D5 coefficients are 17.33 / -12.72 / 2.00 / 2.61 percentage
points, with school-clustered SEs 1.01 / 0.95 / 0.66 / 0.98. All nine income
indicators are jointly significant at p < 0.001 in each model. These are
conditional descriptive associations; the income-share change is attributable
to the regional adjustment, not the reference-category change.

All normal-equation, clustered-covariance, nested-projection, and outcome-union
checks pass. Independent D1-to-D5 rebasing checks have maximum coefficient
error below 2.2e-11, covariance error below 4.2e-13, fitted-value difference
below 3.7e-11, and exactly zero reported R2 difference. Source checksums are
unchanged. The HTML report and LaTeX fragments display every income decile,
with D5 marked as the reference.
