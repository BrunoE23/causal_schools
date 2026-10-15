# Add SIMCE parent education to school preference models

## Decision

Use the 2022 second-year-high-school SIMCE parent questionnaire to construct
one school-level family-composition attribute for the SAE submitted-list
preference models. The source gives each respondent's school RBD and asks about
both parents' education. This grade provides the most direct school-level link
to high-school destinations in the current ranking data.

## Construction

- `cpad_p04` is education of the mother or father's partner; `cpad_p05` is
  education of the father or mother's partner.
- Valid response codes 1-20 are mapped to approximate years from the 2022
  questionnaire. Blank, 0, 21 (don't know), 99 (double mark) and other codes
  are missing. The conversion assigns no schooling=0; basic grades 1-8=1-8;
  secondary grades I-IV=9-12; IV/V TP=12; incomplete/complete CFT-IP=13/14;
  incomplete university=14; complete university=16; master's=18; doctorate=21.
- For each survey student, average the available mother/father years. Average
  these family means among respondents at the same RBD. Families with one
  valid parent report contribute one value.
- Standardize using the unweighted mean and N-1 SD across unique destination
  RBDs in the final pooled complete-ranking sample. The current reference has
  2,481 schools, mean 11.6998 years and SD 1.4187 years.
- If any school in a submitted list lacks a school mean, exclude the full list;
  never shorten a ranking. This currently removes 332 of 273,516 PAES-M1-eligible
  applicants and leaves 273,184 applicants/969,377 school pairs.

## Model

Add parent education to the existing four-VA plus PAES-M1 specification, with
two raw school-type indicators and all six attribute-by-type interactions.
Estimate the extended and matched five-attribute baseline on identical lists
for pooled and four priority-by-continuation groups, under published student
coordinates and home-comuna centroids. Use exact submitted-set Plackett-Luce
likelihood via stratified Cox and student-clustered covariance.

## Interpretation

The 2022 survey postdates the 2018-2020 SAE applications. Treat its coefficient
as a descriptive association with school composition observed in 2022; it does
not establish information families had when applying or a causal effect of
parent composition. The pooled main-coordinate total slopes per SD are
-0.0390 (SE 0.0046) for non-TP, -0.1075 (0.0072) for mixed HC/TP and -0.0679
(0.0117) for TP-only schools. Parent education correlates 0.710 with PAES M1
across destination schools, so interpret its coefficient conditional on M1 and
the four VAs.

## Reproducibility

Scripts 19-22 under `code/codex/school_preference_va/` build, fit, independently
verify and report the extension. Derived data and model objects are in
`data/clean/school_preference_va/parent_education_2022/`; the report is
`output/reports/school_preference_va/parent_education_2022.html`.
