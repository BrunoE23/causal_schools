# Leadership characteristics and school VA

Approved scope (2026-09-10): leadership only, codes 3 (Planta Directiva), 4
(Director), 10 (Directiva), 15 (Subdirector), in primary or secondary functions.
No classroom or HS teaching-assignment restriction. Current features use the
existing 3,682 VA schools and 2018-2024 window. Retain all appointments, count
each leader once per school-year, and keep raw function codes and code-specific
composition shares. Code 16 and technical-pedagogical/inspection functions do
not qualify unless the same person has an eligible leadership role.

## Measurement plan fixed before VA analysis

Use strictly prior primary-function leadership years from 2013, current any-
leadership spell at the school, university tertiary-qualification share, and
teaching-qualification share. Teaching titles are observable qualifications, not
management-specific training. No university identity/ranking is available.
Make equal-component standardized experience and credential blocks, standardize
each block, and form an equally weighted standardized balanced index. PCA uses
the same four components, sign-aligned to the staff index rather than VA.
Keep all component variables; do not select weights or variables by significance.

A move among codes 3, 4, 10, 15 preserves leadership experience. Cumulative years
survive gaps/moves; consecutive spells reset at a missing/nonleadership year or
school move as appropriate. Later observations do not revise earlier histories.
Primary function means ID_IFP at any appointment; PERSONAS=1 is used only for
the separate detailed main-function switching and leadership entry/exit rates.
Those rates use only prior adjacent-year comparisons, not current transitions.

The director code included profesores encargados through 2014 (MINEDUC Annex
III, p. 16, footnote 27). Keep pre-2015 code-4 history flagged. Exclude an observed
2014 code-4 to 2015 code-16 change from switching-rate comparisons rather than
calling it a true transition. A separate balanced-index sensitivity truncates
BOTH experience histories to 2015; do not add current code-16 staff to leadership.

Use the previous 80% annual-member/80% active-year coverage gates, all seven
annual role counts known, and >=3 active years for indices. Absence and missing
staffing remain distinct. Credentials are conditional on current leaders, not
zero for schools with none. Reported school tenure remains unchanged, with its
period average excluded from the core/association family because of the prior
coding-break discovery; analyze 2018 tenure separately and audit leadership
tenure directly. Staffing ratios use broad pooled VA counts / mean annual leader
headcount, not annual enrollment or actual caseload. Codes can overlap within a
person-year, so composition shares need not sum to one.

## Analysis plan

Use the same 12 saved All-sample VA outcomes and the previous Pearson, Spearman,
student-weighted and unshrunk-VA checks. Adjust separate standardized school-level
regressions for log broad VA sample size and its square, 2024 dependency, region,
rural status, TP/artistic offerings and basic-school offerings. Use HC1 regression
inference, conditional on saved VA; these are not causal leadership effects.
BH families cover all leadership metric-outcome tests, separately for Pearson
and adjusted p-values. Repeat the four main indices/blocks on non-private-paid
schools, >=100 VA students, and >=5 active leadership years; robustness BH is
within subsample. Preserve sparse/constant results as unavailable with status.

## Outputs and preservation

All new data go to `data/clean/leadership_quality_va/`, figures to
`output/figures/leadership_quality_va/`, and the new leadership report to
`output/pdf/leadership_characteristics_and_school_va_report.pdf`.
The old teacher/orientador tables, figures and PDF are never overwritten.
The build checks hashes of those outputs and source VA; all raw files are read-
only. A thin source-appointment cache for current leaders supports independent
history verification and is Git-ignored along with all person-level data.

The only shared-helper changes are optional extra role fields/keep-all behavior
for appointment deduplication and optional explicit four-component index inputs.
Default teacher/orientador behavior remains unchanged and is regression-tested.

## Reproduction

Run from the repository root with R 4.5.1 (data.table, sandwich, ggplot2) and
Python (pandas, numpy, reportlab, pypdf). The bundled Python runtime works.

```powershell
Rscript --vanilla code/codex/leadership_quality_va/01_build_leadership_measures.R --preflight
Rscript --vanilla code/codex/leadership_quality_va/01_build_leadership_measures.R
Rscript --vanilla code/codex/leadership_quality_va/02_analyze_leadership_va.R --indices-only
Rscript --vanilla code/codex/leadership_quality_va/02_analyze_leadership_va.R --overwrite
python code/codex/leadership_quality_va/verify_leadership_history.py
python code/codex/leadership_quality_va/verify_leadership_va.py
Rscript --vanilla code/codex/leadership_quality_va/03_plot_leadership_va.R
python code/codex/leadership_quality_va/04_render_leadership_report.py
Rscript --vanilla code/codex/leadership_quality_va/test_leadership.R
```

Build/analysis refuse existing owned outputs unless `--overwrite` is explicit.
For aggregate-only changes, use the first build with `--aggregate-cache
--overwrite`; source manifest must still match. This does not rebuild histories.
After changing source/history rules, rerun the full build. Report generation
checks verified input hashes and expects six pages; render and inspect before
delivery. Existing `staff_quality_va` tables provide the source manifest,
school context and outcome dictionary; existing VA is read-only.

## Completed results and checks (2026-09-10)

- 13,104 people; 51,397 current person-school-years; 3,461 main index schools.
- 34 base metrics, plus capacity/baseline/index variables. Excluding pure audits
  leaves 42 measures x 12 outcomes = 504 main pairs and 144 robustness rows.
- Balanced r with income/math/language VA: 0.075/0.068/0.116. Adjusted SD betas:
  0.004/-0.002/0.022; none survives BH. Experience and credential blocks also
  have weak adjusted links. PCA is dominated by experience (45.0% variance).
- Mean leader headcount r: 0.257/0.236/0.193; adjusted SD betas:
  0.066/0.156/0.099. Capacity associations are not individual quality effects.
- Both-histories-since-2015 index covers 3,447 schools and gives similar results.
  2,340 current leaders have earlier pre-2015 director-code exposure; 40 have
  the specific possible 2014-to-2015 director/encargado code break.
- All 28 leadership, 34 staff-quality, 83 staff-cleaning and 44 ratio tests pass.
  Independent checks rebuild 513,970 history values, 876,316 annual cells,
  125,188 period aggregates, 6,908 balanced scores, blocks and PCA; verify all
  correlations, BH adjustments, robustness supports and 15 exact HC1 regressions.
  No analysis warnings. Prior output/source-VA MD5 checks pass.

Primary reusable files: `leadership_indices_and_measures.csv`,
`leadership_va_correlations.csv`, `leadership_va_index_robustness.csv`,
`leadership_analysis_metric_dictionary.csv`, PCA/scaling tables, and source,
tenure, coverage, conflict and independent-verification audits. All live in
`data/clean/leadership_quality_va/`; person-level exports must remain Git-ignored.
Scientific rationale is in
`setup_md_codex/project_memory/decisions/2026-09-10-leadership-characteristics-va.md`.
