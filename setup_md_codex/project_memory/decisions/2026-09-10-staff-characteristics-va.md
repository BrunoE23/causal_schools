# Staff-characteristic indices and VA associations

Date: 2026-09-10. Status: implemented and verified.

## Objective and scope

Complete two observable school staff-characteristic measures, one for orientadores
and one for regular youth-HS teachers, and examine their association with saved
higher-education and test-score VA. These are descriptive school-level measures,
not estimated individual contributions or causal effects of staff hiring.

The implementation is `code/codex/staff_quality_va/`. It reuses the existing annual
cleaner/history definitions but streams 2013-2024 staff files directly into a lean
VA-school feature cache. The old full cleaner exports need not be produced first.
The common school universe is the saved broad VA count file: 3,682 schools and
757,999 students, grade-8 cohorts 2017-2020, assigned by most_time_RBD. Current
staff features span 2018-2024; 2025 contributes neither features nor histories.
Outcome-specific VA supports remain intact within this common school universe.

## Measurement choices, made before inspecting associations

Keep all school-specific appointments, deduplicated to person-school-year.
Orientadores include primary or secondary function 9. Teachers require classroom
function and their own regular youth-HS assignment codes. Current composition
uses current roles, never full-window EVER labels. Later promotions do not alter
earlier history. Primary-function orientador experience uses ID_IFP=9 at any
appointment, not only PERSONAS=1. Prior years exclude the current year. Current
role and school-role spells include it. All are observed histories, not lifetime
experience. School-role spells can reset on a school move without resetting
person-level role experience.

Within school-year, average equally across eligible people. A valid attribute
needs at least 80% member coverage. Average equally across active eligible years,
requiring at least 80% valid years and all seven annual role counts known. Record
counts and partial means separately. No role is not zero credentials/experience;
unknown role counts are not absence. Subject-conditional metrics have their own
eligible denominators. Indices require at least three active-role years and all
four core components observed. Do not impute missing components.

The four core components are prior role years, current school-role spell,
university tertiary qualification share, and teaching-title share (HS-specific
title for teachers). Initially prior role-years at the school was a candidate;
only 1,000 teacher schools had a valid period value because new arrivals lack
prior within-school history. Before looking at VA, replace that core candidate
with the current school-role spell, which includes new arrivals at one year.
Keep the original variable unchanged in the full output.

Reported ANO_SERVICIO_EE remains unchanged, but the full audit found an abrupt
zero-coding break in 2019, often for staff observed previously at the same RBD.
Exclude its 2018-2024 mean from the core and association family. Keep that field
as an audit and analyze reported 2018 tenure separately. This supersedes use of
the unqualified period-average reported tenure as the preferred core measure;
it does not relabel observed spells as reported school tenure.

Credential evidence is harmonized across appointments. Positive binary evidence
is retained; conflicting numeric reports become missing and are counted. Actual
institution names/ranks are unavailable. An orientation mention is not a census
of counselor training: mentions are applicable to basic-education titles. Report
both the all-counselor recorded-evidence share (known non-applicability means no
recorded evidence) and the conditional applicable-title rate. Unknown eligible
mentions remain unknown. Math/Spanish specialty matching requires the same HS
slot's subsector, 32001/31001; foreign languages are excluded. Explicit absence
of a teaching title means zero recorded teaching specialty, not exclusion from
the assigned-teacher denominator. This does not mean no subject knowledge.

## Scaling and PCA

Fit on each role's complete-case, three-active-year sample, without any VA input.
Z-standardize the four components. Average the two experience component Z scores
and standardize this block. Do the same for the two credential components.
Average the standardized blocks with weights 1/2 and standardize the balanced
index to SD 1. Save all centers/scales and retain both blocks separately.
Availability: 1,642 orientador and 2,706 teacher schools.

PCA uses the same four standardized components on the same schools. PC1's sign
is aligned with the balanced staff index, never with VA; normalize PC1 to SD 1.
PC1 explains 42.9%/49.8% of component variance for orientadores/teachers. Its
orientador weights mostly load on experience. Block correlations are 0.062/0.099,
so neither PCA nor the balanced score establishes a unidimensional quality factor.
Teacher sensitivity refits the balanced index using prior HS years since 2016
(2,666 schools), because early assignment slots have poorer coverage.

## Association specifications and inference

Use saved All-sample centered EB VA for 12 outcomes. Cross 37 orientador and
36 teacher measures: 876 pairs. Report Pearson r, usual Pearson confidence
interval/p-value, Spearman r, common broad-student-weighted r and unshrunk-VA r.
Preserve paired school Ns. Spearman canonicalizes values to 10 significant digits
via numeric sprintf formatting to prevent floating-point artificial tie breaks;
other estimation remains unrounded. Associations read the exported school doubles
to allow independent verification on identical inputs.

For each measure/outcome, use complete cases and regress standardized EB VA on
the standardized staff variable plus log broad-VA student count, its square, and
categorical 2024 dependency, region, rural status, TP/artistic offerings and basic
offerings. Omit constant categorical controls. Use regression-derived HC1
covariance and residual-df t inference. Save coefficients, SEs, CIs, p-values,
partial correlations and adjusted Ns. This is conditional on estimated VA and
does not propagate VA uncertainty or resolve unobserved sorting. Contemporary
2024 controls must not be described as predetermined.

BH corrections apply to all main tests separately within staff role, separately
for Pearson and adjusted p-values. For four indices/blocks, repeat on schools
excluding COD_DEPE=4, schools with >=100 VA students, and schools with >=5 active
staff years (288 rows). Robustness adjusted BH families are role by subsample.
Do not pick index weights by significance. Do not overwrite/re-estimate source VA.

## Results and verification

Orientador balanced r with full income/math/language VA is 0.051/0.042/0.050;
no adjusted balanced-index link survives BH. Four exploratory capacity or
switching links survive in the full counselor family, not a broad quality signal.
Teacher balanced r is 0.342/0.377/0.420; teacher credential-block r is
0.446/0.406/0.512. Adjusted credential coefficients are 0.104/0.084/0.136 SD.
These are descriptive associations, not income percent effects.

Construction tests: 34. Independent checks reconstruct 1,391,796 annual cells,
198,828 period aggregates and 7,014 balanced-index scores including sensitivity;
verify all 876 correlations and unshrunk counterparts, 288 robustness supports/
correlations, all BH adjustments, and 12 exact HC1 regressions. Source-size/mtime
checks and exact reconciliation of annual counselor counts pass. No analysis
warnings remain. The eight-page PDF is rendered and visually checked.
Data and individual records stay local under data/clean/staff_quality_va/.
