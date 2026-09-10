# School staff characteristics and school value added

## Objective and analysis plan (2026-09-09)

Review and complete observable counselor and HS-teacher characteristics, construct
outcome-independent summary indices, and report their associations with all saved
higher-education and test-score VA outcomes. These are school-level correlates,
not estimates of individual staff quality or causal effects of hiring staff.

The school universe is the existing 3,682-school broad VA count file, grade-8
cohorts 2017-2020. Staff characteristics refer to 2018-2024, with observed role
histories beginning in 2013 and no future information assigned to earlier staff.
Counselors include primary or secondary orientadores. Teachers must teach regular
youth HS at their own appointment, following the existing cleaner's COD_ENS rules.
Keep one person per school-year, including non-main appointments. Staff at mixed
schools are not assumed to serve HS exclusively. Raw files remain unchanged.

## Measurement decisions made before inspecting VA correlations

- Separate staffing availability and capacity from conditional staff attributes.
  No counselors is not zero counselor qualifications or zero experience.
- Inspect primary-function experience, role-at-school experience, observed spells,
  role persistence, role changes, primary versus secondary duties, reported school
  tenure, institution type, teaching/HS titles, and subject/orientation credentials.
- Reported ANO_SERVICIO_EE is preserved and audited against repeated school
  appearances. Do not substitute reconstructed tenure under the same label.
  A field dominated by implausible/default zeros will not enter the core index.
- Orientation mentions are collected for basic-education qualifications. Report
  both recorded evidence and the applicable-subgroup rate, and do not interpret
  no recorded mention as no counselor training. Do not rank actual institutions:
  only institution type is observed.
- Math/language teacher credential matching uses the SAME HS teaching slot:
  SUBSECTOR=32001 for math and 31001 for language, excluding foreign languages.
  Source: 2024 staff codebook Annex VI, p. 22. Report these conditional rates
  separately, not as general teacher subject-composition measures.
- Within a school-year, give each current staff member equal weight. Across years,
  give each active-role year equal weight. Require full seven-year role-count
  coverage; a characteristic's annual mean needs at least 80% of its eligible
  members observed, and its period mean needs at least 80% of active eligible
  years. Record every numerator, denominator, and missingness gate.
- Build a transparent balanced index from experience and credential blocks only,
  with equal block weights and published component standardizations. Start with
  prior primary-orientador years / current counselor-at-school spell for counselors,
  prior HS-teacher years / current HS-teacher-at-school spell for teachers, and
  university-tertiary qualification / teaching title (HS title for teachers).
  Confirm adequate coverage and nonzero variation before fitting; document any
  change without looking at VA correlations. Require at least three active years.
- Fit PCA to the same complete-case components as a descriptive sensitivity,
  publishing loadings and explained variance. Align its arbitrary sign with the
  balanced index, not with VA. A mixed-loading component is not automatically
  a higher-quality score. Keep all original measures available.
- Role switches, low turnover, exclusive-role histories and rare recorded mentions
  are not automatically assigned positive/negative quality weights. A promotion
  can be a good outcome and cannot rewrite earlier role histories.

The initial school-experience candidate was prior observed role-years at that RBD.
The full pre-outcome audit found only 1,000 teacher schools with a valid period
value, versus 2,983 with credentials, because first school observations have no
prior within-school history. The core instead uses the current observed school-role
spell, including new arrivals at one year, without relabeling or filling the old
measure. This choice was made before estimating any VA correlations. Reported
tenure is informative in 2018 but heavily zero-coded from 2019; retain 2018 tenure
separately and keep the broken period-average field as an audit, outside the index.

Subject matching refers to a recorded teaching specialty, not all subject knowledge.
An explicit absence of a reported teaching qualification contributes zero recorded
teaching specialty, not a missing observation excluded from the assigned-teacher
denominator. Unknown teaching credentials remain unknown.

## Association design

Primary descriptive results use equally weighted schools and saved EB VA. Report
Pearson and Spearman correlations, school counts and coverage. Show the unshrunk
centered VA counterpart, student-weighted correlations, and sensitivity to school
size, administrative dependency, region, rural status and HS type where observed.
Use heteroskedasticity-robust school-level regression inference for adjusted
associations; it is conditional on estimated VA, not a full propagation of VA
estimation uncertainty. Report exploratory multiple-testing adjustments rather
than choosing measures by significance. Do not change the VA estimates, impute
outcomes, learn staff-index weights from VA, or describe these links as causal.

Spearman ranks use values formatted to 10 significant digits to prevent floating-point
or CSV serialization from spuriously breaking ties in equal staffing ratios.
Pearson correlations, index fitting and regressions retain their original precision.
The independent Python verifier reconstructs index scores and checks every
Pearson, rank and weighted correlation, every BH adjustment, and selected HC1
regressions from the exported inputs. No approximate residual-spread SE is used.

## Deliverables and completion checks

1. Reusable school-year and school-period counselor and teacher measures, plus
   input/coverage/conflict/tenure audits and a variable dictionary.
2. Separate transparent indices and PCA diagnostics for both staff groups.
3. A complete association table spanning higher-education and score VA, with
   sample sizes, missingness, robustness checks and multiple-testing context.
4. A readable PDF report and figures, visually checked after rendering, describing
   the substantive findings, weak or null associations, and limitations.
5. Synthetic tests, independent calculation checks, research-method documentation
   and a scoped local commit. Individual staff data stay Git-ignored.

## Run and refresh

Use R 4.5 with data.table, sandwich and ggplot2. Python verification/reporting uses
the bundled Python with pandas, numpy, reportlab and pypdf; it does not need scipy
or statsmodels. From the repository root:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla code/codex/staff_quality_va/01_build_school_staff_measures.R --preflight
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla code/codex/staff_quality_va/01_build_school_staff_measures.R --overwrite
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla code/codex/staff_quality_va/02_analyze_staff_va.R --overwrite
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla code/codex/staff_quality_va/test_staff_quality.R
& 'C:/Users/brunem/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/python.exe' code/codex/staff_quality_va/verify_staff_va.py
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla code/codex/staff_quality_va/03_plot_staff_va.R
& 'C:/Users/brunem/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/python.exe' code/codex/staff_quality_va/04_render_staff_va_report.py
```

First-time builds stream one of 12 annual raw files at a time, retain thin national
history rows, and restrict current staff features to the VA schools. They do not
load the large student microdata cache. The build reconciles every annual
orientador count with the existing students-per-orientador output.
For changes only to aggregation, `01 ... --aggregate-cache --overwrite` reuses
the person-school-year RDS after checking source paths, sizes and timestamps.
Do not use that cache mode after changing cleaning or history definitions.
For staff-only index construction, `02 ... --indices-only --overwrite` does not
read any VA outcome. Review definitions before running the association mode.

The input dependencies are the existing broad exam-taking VA counts, saved All
sample EB/observational VA table, existing orientador-ratio files, Box staff
directories (2013-2024) and the official 2024 school directory. Exact staff paths
are retained in `staff_source_manifest.csv`; the other paths are explicit in the
build scripts. Raw and source-VA files are read-only.

Tables and RDS features are under `data/clean/staff_quality_va/` and remain
Git-ignored. The wide school-period CSV is the primary merge-ready output (key:
RBD, ROLE). Long annual and period tables preserve eligibility and coverage
counts. The analysis dictionary marks each tested measure and each excluded
diagnostic. PCA loadings, standardization means/scales, core correlations,
source manifests and numerical/conflict/tenure audits are separate small files.
Figures are in `output/figures/staff_quality_va/`; the stable report is
`output/pdf/staff_quality_and_school_va_report.pdf`.

Render the final PDF with Poppler and inspect all eight pages before delivery.
The report requires a successful numerical-verification file at least as recent
as the main association CSV. Data checks cover all 1,391,796 annual metric cells,
198,828 period aggregates, 7,014 balanced-index scores including the teacher
history sensitivity, 876 main correlations, 288 robustness samples/correlations,
all BH adjustments and 12 independent exact-HC1 regressions.

## Completed findings (2026-09-10)

Balanced indices are available for 1,642 orientador and 2,706 teacher schools.
The orientador balanced index has Pearson r=0.051 with full projected-income VA,
0.042 with math VA and 0.050 with language VA. Its adjusted associations do not
survive the role-wide BH correction. Four exploratory counselor capacity/role
trajectory associations do survive; the PDF and full table identify them.
The teacher balanced index has r=0.342, 0.377 and 0.420, respectively. Teacher
credentials alone have r=0.446, 0.406 and 0.512. Adjusted teacher credential
coefficients are 0.104, 0.084 and 0.136 outcome SD per credential-index SD.
These associations attenuate substantially with school controls; they are not
causal staff effects or wage percentages. PCA mainly weights experience,
especially for orientadores; retain experience and credentials as separate blocks.

Durable design and unresolved data issues are recorded in
`setup_md_codex/project_memory/decisions/2026-09-10-staff-characteristics-va.md`,
`empirical_methods.md` and `issues.md`.

## 2024 age distribution

`05_plot_staff_age_distribution.R` reads the existing teacher/orientador and
leadership person-school-year caches and three columns from the 2024 raw file.
It changes no data or scores and writes only
`output/figures/staff_quality_va/staff_age_distribution_2024.png`.
Run with `Rscript --vanilla code/codex/staff_quality_va/05_plot_staff_age_distribution.R`.
Each person counts once within each role at VA schools; roles may overlap.
Birth field DOC_FEC_NAC is YYYYMM in 2024. Age is 2024 minus birth year (age
attained during the year), not exact age on the survey date. Exclude placeholder
190001, malformed/month-invalid records, implied ages outside 18-100 and
conflicting valid birth reports. Do not impute from other years in this plot.
Five-year bins use percentages within role, common axes and median lines.
Valid N / median: HS teachers 72,818 / 38; orientadores 1,986 / 51;
leadership 7,680 / 53. Excluded ages: 36 / 0 / 12; no conflicting dates.
Age is not a validated quality measure. All previous analysis outputs remain
unchanged; the new figure is additional, not a replacement.
