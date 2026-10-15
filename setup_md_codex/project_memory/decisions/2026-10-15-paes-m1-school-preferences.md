# PAES M1 school means in the preference model

The user requested adding a school-average PAES2024 or2025 M1 measure to
the existing type-interacted preference model, and explicitly anticipated
constructing it. Reuse the four existing EB VAs and constructed rankings;
build the additional school measure directly from raw individual scores.

## Measure and timing

Use the2025 admission-process Inscritos-Puntajes file as the primary source.
Keep current graduates(ANYO_DE_EGRESO=2024), positive graduation-school RBD,
and valid100-1000 MATE1_REG_ACTUAL. Each student contributes once to the
arithmetic school mean. Do not require nonzero GPA or membership in the
paper's SAE/VA universe. Save school-level taker counts and score SDs.
Registered nonscorers contribute no zero scores. This is a mean among M1
takers, not all enrolled pupils, and it is unadjusted performance rather
than school VA. Do not substitute MATE1_MAX(best score across valid sittings).

The admission process2025 refers here to2024 graduates and the current
regular sitting in late2024. Construct process2024(2023 graduates) by the
same rule for coverage auditing only. Do not mix years through fallback,
pool them, or silently label process2025 as an exam taken in calendar2025.
Codebooks have some stale year labels; actual file headers, ANYO_PROCESO
and graduation year govern extraction. Read selected columns after the
230/234MB file-size preflight; never read the full wide files unnecessarily.

The existing Box clean/Enzo/Adm_combined_na_21_25.csv summary is unsuitable
for this definition: avg_MATE_ACTUAL actually uses MATE1_MAX with
RINDIO_PROCESO_ANTERIOR==0 rather than a current-graduate restriction.
The general college-applications cleaner keeps current graduates but
retains only MATE1_MAX, so it also cannot supply the current-regular mean.
Preserve those separate source products unchanged. The2024 raw file has
309352 rows versus309350 stated in its manual; record actual data counts
rather than asserting the stale manual count.

The score measure postdates the2018-2020 SAE applications. It describes
association between submitted rankings and later measured school performance;
it does not establish parents' information about these exact future means.

## Specification, support and normalization

Add M1 mean as a fifth school metric and interact it with both mixed-HC/TP
and TP-only indicators, consistent with the current request to allow metric
slopes by school type. Keep all four VA terms, both type main effects, their
existing eight interactions and logged distance. The extended model has
18 coefficients:5metrics+2types+10interactions+1distance. No other controls
or demand-model extensions are introduced.

Preserve every submitted ranking and its flags in the constructed inputs.
For estimation, start from the existing four-VA informative sample and
exclude an entire list if any destination lacks the primary school M1 mean.
Never shorten lists or impute scores. Do not introduce a minimum-taker cutoff.
Report coverage by school type, application year and applicant group.

Keep the original four VA means/SDs across2501 equally weighted destination
schools unchanged. Normalize the additional M1 mean using the unweighted
mean and N-1 SD across unique schools in the final pooled retained sample.
Save those moments and use them unchanged in every applicant group and
both geographic specifications. Do not standardize within type or group.

Fit the extended model for the pooled and four prioritario-by-continuation
groups, with both existing distance definitions. Refit the previous15-term
specification on exactly those same lists as matched_baseline, so coefficient
changes are not conflated with sample loss. Both variants condition on the
submitted set and use student-clustered covariance. Total type-specific
metric slopes are main beta plus the applicable interaction; their SEs
use the full covariance, including the main-interaction covariance term.
Type intercepts apply only at all five standardized metrics=0. Treat school
means and existing EB VAs as fixed for inference. Separate group logits
retain their separate error-scale normalization.

## Reproducibility and preservation

paes_common.R and scripts13(build),14(fit),16(verify),15(report) implement
the extension under code/codex/school_preference_va. Separate paes_m1
clean-data/table subfolders and with_paes_m1.html preserve prior15-term
data, fits, tables and reports. A manifest stores pre-extension hashes of
26 prior artifacts. No raw/existing source clean data or manuscript is
modified. Validation independently checks score means, ranks, whole-list
selection, normalization, products, contrast covariance, within-list design
rank, Hessian conditioning and exact PL likelihood; expanded conditional
logit checks both specifications on complete validation rankings.

## Constructed sample and pooled estimates

The primary school means use177466 valid current-graduate regular-M1 takers
at3405 RBDs. Process2024 has173917 takers at3396 RBDs. The strict2025
whole-list rule retains273516 applicants,970924 school pairs and2483
schools, excluding2677 of276193 baseline rankings(0.969%). The four
applicant-group sizes are86353/153798/15153/13646, plus4566 unresolved
origins retained only in the pooled fit. Separate2024 coverage would retain
274019 baseline rankings;774 lists with2024 but missing2025 coverage are
correctly excluded, with no fallback. All30 build checks pass.

New M1 normalization is mean585.7118083 points, school SD60.02694668 across
2483 equally weighted destinations. Existing VA normalization remains byte-
identical. In the pooled main extended model the total M1 slopes are
non-TP .154104(.003831), mixed .206558(.005976), TP-only .121331(.009898).
Math-VA totals become-.039460/-.032174/-.056996, compared with
.050234/.057476/-.006234 in the matched-sample baseline. These changes
hold the underlying applicants, lists, VA units and distances fixed.
Interpret the VA coefficients conditional on the school-average M1 measure
and other metrics; a negative partial association does not establish that
families dislike school effectiveness. The strong positive M1 association
among TP-only schools also limits the earlier broad interpretation of weak
within-TP sensitivity to school performance.

All20 fits converge in three iterations with no warning or dropped term;
236 fit checks pass. The extended pooled within-ranking design has rank18
and likelihood-information condition number about129. The equally weighted
school correlation of M1 with math VA is.695809; M1 VIF is2.391865. All
1187 independent checks pass, including fresh narrow raw extracts,330
coefficient rows,270 total slopes/full-clustered-covariance SEs and expanded
conditional-logit equivalence for every group/variant/distance combination.
Maximum expanded-fit coefficient/covariance discrepancies are below6e-15;
maximum full-sample PL discrepancy is2.991874e-7. All26 prior artifact hashes
and byte counts remain unchanged. The information CSVs carry the actual
coefficient names on both axes; no raw data, previous estimates or manuscript
are modified.
The saved report and14 table/CSV companions pass162 additional reporting
checks, retaining all18 coefficients and exact total-slope standard errors.
