# Leadership characteristics and school VA

## Authorization and scope

Bruno approved extending the completed staff analysis to leadership only.
Eligible primary/secondary functions are 3 Planta Directiva, 4 Director,
10 Directiva and 15 Subdirector. Keep every appointment, deduplicate MRUN-RBD-
year, and do not require teaching or an HS teaching assignment. Code 16,
technical-pedagogical and inspection roles do not qualify on their own.
Preserve original function codes and overlapping composition shares. This is
an additional role analysis, not a replacement for teachers or orientadores.

Use current staff in 2018-2024 at the existing 3,682 broad VA schools, with
757,999 pooled grade-8 cohort 2017-2020 students. Preserve all-position national
histories of those current leaders from 2013. Histories are filtered for storage
efficiency by eventual membership but individual dated features never use future
observations. No raw or saved VA changes and no new VA estimation.

## Measurement fixed before examining outcomes

Core components are prior primary-function leadership years (2013 through t-1),
current primary-or-secondary leadership spell at the school (including t),
university tertiary-qualification share, and teaching-qualification share.
Primary experience means ID_IFP at any appointment; PERSONAS=1 is used only for
separate detailed main-function switching and leadership entry/exit measures.
Switching rates use strictly prior adjacent-year comparisons. Moving among the
four leadership codes is a detailed change, not leadership entry/exit. Gaps
break consecutive spells but not cumulative observed experience. Unobserved
earlier histories remain missing, not zero lifetime experience.

Average equally across current people within school-year, then across active
years. Require known role counts in all seven years, 80% observed members for
each annual attribute and 80% valid active years for the period mean. An index
requires all four components and at least three active years. Known no leaders
is zero capacity, not zero attributes; missing staff coverage remains unknown.
Standardize four components on complete schools; average and standardize each
two-component block; equally average the two standardized blocks and standardize
the balanced score. PCA uses the same standardized components and is sign-aligned
to the balanced index, never VA. Keep blocks and all underlying measures.

No university identity or rank, and no leadership-specific qualification, is
observed. Teaching qualifications/specialties are observable credentials, not
proof of management skills. Preserve reported tenure but exclude period-average
school/system tenure from the main family and indices; analyze 2018 separately.
School-tenure zeros jump from 388/6,963 in 2018 to 4,878/7,093 in 2019, including
4,233 people already observed at the same school.

The distributed MINEDUC ER_Cargos Docentes codebook, Annex III p.16 footnote 27,
says director code 4 included profesores encargados through 2014. Flag earlier
code-4 history; exclude the specific 2014 code-4 to 2015 code-16 comparison from
switching-rate denominators. This does not resolve who was a true director.
A separate balanced sensitivity truncates BOTH primary-role experience and
school-leadership spells to 2015. Do not include current code-16 staff.

Staffing measures use mean annual leader count and pooled VA-sample students /
mean annual count, including a logged version and inverse per-1,000 version.
These are staffing proxies, not annual enrollment, HS-only caseload or FTE.

## Association design and findings

Use all 12 saved All-sample EB VA outcomes with outcome-specific support, equal-
school Pearson and canonical 10-significant-digit Spearman correlations,
broad-student-weighted correlations and unshrunk-VA comparisons. Regress within-
sample standardized VA on one standardized characteristic, log broad-VA size and
its square, and categorical 2024 dependency, region, rural status, TP/artistic
offerings and basic-school offerings. These controls are contemporaneous, not
predetermined. Use exact HC1 regression covariance and residual-df t inference,
conditional on estimated VA; this is not a causal leadership effect.

The 42 measures x 12 outcomes yield 504 main pairs. BH adjustment covers the
entire leadership family separately for Pearson and adjusted p-values. Repeat
four indices/blocks under three restrictions (no private-paid schools, >=100 VA
students, >=5 active leadership years): 144 rows, with BH within subsample.
Retain unavailable statuses instead of silently dropping nonestimated pairs.

Main balanced index N=3,461; 2015 sensitivity N=3,447. Income/math/language r is
0.075/0.068/0.116, with adjusted SD betas 0.004/-0.002/0.022; no adjusted balanced
association survives BH. Experience and credential blocks have weak adjusted
links. PC1 explains 45.0% and mostly reflects experience; blocks correlate -0.023.
Headcount r is 0.257/0.236/0.193 and adjusted betas 0.066/0.156/0.099. These
capacity/organizational associations must not be recast as individual quality.
52 adjusted pairs survive BH: 41 staffing, 10 composition and one detailed
main-function switching. The history sensitivity leaves the main pattern intact.

## Verification and artifacts

Current panel: 13,104 people / 51,397 person-school-years. 2,340 have pre-2015
director-code history; 40 have the specific possible coding-break transition.
28 leadership construction tests and all 34 existing staff-quality, 83 cleaning
and 44 ratio tests pass. Independent Python checks reconstruct 513,970 history
values, 876,316 annual metric cells, 125,188 period aggregates, 6,908 main/alternate
balanced scores, blocks and PCA. Check all 504 correlations, 144 robustness
supports/correlations, BH adjustments and 15 exact HC1 regressions. No analysis
warnings. Existing staff outputs and source VA remain byte-identical.

Code: code/codex/leadership_quality_va/. Derived private data:
data/clean/leadership_quality_va/. Six-page visually checked report:
output/pdf/leadership_characteristics_and_school_va_report.pdf. Figures:
output/figures/leadership_quality_va/. Shared helper extensions are optional
and preserve teacher/orientador defaults. Person-level data stay Git-ignored.
