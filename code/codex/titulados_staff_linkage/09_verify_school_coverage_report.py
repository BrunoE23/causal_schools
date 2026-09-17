"""Independently reconstruct school linkage rates and verify VA correlations."""
from pathlib import Path
import json
import hashlib
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[3]
BASE = ROOT / "data/clean/titulados_staff_linkage"
OUT = BASE / "school_coverage"
ROLES = ["HS teachers", "Orientadores", "Leadership"]
METRICS = ["ANY_DB", "UG_DB", "ANY_ASOF", "UG_ASOF"]

def close(a, b):
    np.testing.assert_allclose(np.asarray(a, dtype=float), np.asarray(b, dtype=float), atol=1e-11, rtol=1e-10, equal_nan=True)

members = pd.read_csv(BASE / "staff_membership_person_school_year.csv.gz", dtype={"MRUN": str})
members = members[members.ROLE.isin(ROLES)]
flags = pd.read_csv(BASE / "staff_match_person_role_year.csv.gz", usecols=["MRUN", "ROLE", "AGNO",
    "MATCH_ANY_2007_2025", "FIRST_UNDERGRAD_YEAR", "MATCH_ANY_BY_STAFF_YEAR", "MATCH_UNDERGRAD_BY_STAFF_YEAR"], dtype={"MRUN": str})
flags["ANY_DB"] = flags.MATCH_ANY_2007_2025.astype(int)
flags["UG_DB"] = flags.FIRST_UNDERGRAD_YEAR.notna().astype(int)
flags["ANY_ASOF"] = flags.MATCH_ANY_BY_STAFF_YEAR.astype(int)
flags["UG_ASOF"] = flags.MATCH_UNDERGRAD_BY_STAFF_YEAR.astype(int)
j = members.merge(flags[["MRUN", "ROLE", "AGNO"] + METRICS], on=["MRUN", "ROLE", "AGNO"], validate="many_to_one", how="left")
assert len(j) == len(members) and not j[METRICS].isna().any().any()
assert not j.duplicated(["MRUN", "ROLE", "AGNO", "RBD"]).any()
keys = ["RBD", "ROLE", "AGNO"]
agg = j.groupby(keys)[METRICS].sum().reset_index()
agg["N_REBUILT"] = j.groupby(keys).size().values
long = agg.melt(id_vars=keys + ["N_REBUILT"], value_vars=METRICS, var_name="METRIC", value_name="N_MATCHED_REBUILT")
annual = pd.read_csv(OUT / "school_staff_match_annual.csv.gz")
assert len(annual) == 3682 * 3 * 7 * 4
v = annual.merge(long, on=keys + ["METRIC"], how="left", validate="one_to_one")
active = v.N_ROLE.gt(0)
close(v.loc[active, "N_ROLE"], v.loc[active, "N_REBUILT"])
close(v.loc[active, "N_MATCHED"], v.loc[active, "N_MATCHED_REBUILT"])
close(v.loc[active, "RATE"], v.loc[active, "N_MATCHED_REBUILT"] / v.loc[active, "N_REBUILT"])
assert v.loc[~active, "RATE"].isna().all()
assert v.loc[v.N_ROLE.eq(0), "N_MATCHED"].eq(0).all()
# Check exported roster counts directly against original role-specific rosters.
t = pd.read_csv(ROOT / "data/clean/staff_quality_va/staff_school_year_roster.csv")
l = pd.read_csv(ROOT / "data/clean/leadership_quality_va/leadership_school_year_roster.csv")
rosters = []
for role, source, col in [(ROLES[0], t, "N_TEACHER"), (ROLES[1], t, "N_COUNSELOR"), (ROLES[2], l, "N_ROLE")]:
    x = source[["RBD", "AGNO", col]].rename(columns={col: "N_SOURCE"}).copy()
    x["ROLE"] = role
    rosters.append(x)
r = annual[annual.METRIC.eq("ANY_DB")].merge(pd.concat(rosters), on=keys, how="left", validate="one_to_one")
close(r.N_ROLE, r.N_SOURCE)
period = pd.read_csv(OUT / "school_staff_match_period_long.csv")
pk = ["RBD", "ROLE", "METRIC"]
rebuilt = annual.groupby(pk).agg(N_KNOWN=("N_ROLE", "count"), N_ACTIVE=("N_ROLE", lambda x: x.gt(0).sum()),
                                MEAN=("RATE", "mean"), STAFF=("N_ROLE", "sum")).reset_index()
rebuilt["EXPECTED"] = rebuilt.MEAN.where(rebuilt.N_KNOWN.eq(7) & rebuilt.N_ACTIVE.gt(0))
pv = period.merge(rebuilt, on=pk, validate="one_to_one")
close(pv.RATE, pv.EXPECTED)
close(pv.N_KNOWN_YEARS, pv.N_KNOWN)
close(pv.N_ACTIVE_YEARS, pv.N_ACTIVE)
unique_staff = j.drop_duplicates(["RBD", "ROLE", "MRUN"])
u = unique_staff.groupby(["RBD", "ROLE"])[["ANY_DB", "UG_DB"]].mean().reset_index().melt(
    id_vars=["RBD", "ROLE"], var_name="METRIC", value_name="EXPECTED_UNIQUE")
uv = period[period.METRIC.isin(["ANY_DB", "UG_DB"])].merge(u, on=pk, how="left", validate="one_to_one")
close(uv.RATE_UNIQUE_PEOPLE, uv.EXPECTED_UNIQUE.where(uv.STATUS.eq("observed")))
distribution = pd.read_csv(OUT / "school_staff_match_distribution.csv")
for row in distribution.itertuples():
    x = period.loc[period.ROLE.eq(row.ROLE) & period.METRIC.eq(row.METRIC), "RATE"].dropna()
    assert len(x) == row.N_SCHOOLS
    close([x.mean(), x.std(), *x.quantile([.1, .25, .5, .75, .9])],
          [row.MEAN, row.SD, row.P10, row.P25, row.MEDIAN, row.P75, row.P90])

analysis = pd.read_csv(OUT / "school_staff_match_va_analysis.csv.gz")
results = pd.read_csv(OUT / "school_staff_match_va_correlations.csv")
robust = pd.read_csv(OUT / "school_staff_match_va_robustness.csv")
# Verify source VA was selected without re-estimation or alteration.
va = pd.read_csv(ROOT / "output/tables/empirical_bayes_school_va/stata_eb_school_rbd_observational_values_for_iv.csv",
    usecols=["school_rbd", "analysis_sample", "outcome", "controlled_value_added_eb_centered_student", "controlled_value_added_centered_student"])
va = va[va.analysis_sample.eq("All")].rename(columns={"school_rbd": "RBD", "outcome": "OUTCOME",
    "controlled_value_added_eb_centered_student": "SOURCE_Y", "controlled_value_added_centered_student": "SOURCE_Y_RAW"})
av = analysis.merge(va, on=["RBD", "OUTCOME"], how="left", validate="many_to_one")
close(av.Y, av.SOURCE_Y)
close(av.Y_RAW, av.SOURCE_Y_RAW)

def correlation_checks(d, row, xcol="RATE"):
    ok = np.isfinite(d[xcol]) & np.isfinite(d.Y)
    x, y = d.loc[ok, xcol], d.loc[ok, "Y"]
    assert len(x) == row.N
    if row.STATUS != "estimated":
        assert len(x) < 30 or x.std() == 0 or y.std() == 0
        return
    close(np.corrcoef(x, y)[0, 1], row.PEARSON_R)
    xr = x.map(lambda z: float(format(z, ".10g"))).rank()
    yr = y.map(lambda z: float(format(z, ".10g"))).rank()
    close(np.corrcoef(xr, yr)[0, 1], row.SPEARMAN_R)
    w = d.loc[ok, "N_VA_STUDENTS"].to_numpy(dtype=float)
    w /= w.sum()
    dx, dy = x.to_numpy() - np.sum(w*x), y.to_numpy() - np.sum(w*y)
    close(np.sum(w*dx*dy) / np.sqrt(np.sum(w*dx*dx)*np.sum(w*dy*dy)), row.STUDENT_WEIGHTED_R)
    raw = d[[xcol, "Y_RAW"]].dropna()
    assert len(raw) == row.N_UNSHRUNK
    close(raw.corr().iloc[0, 1], row.UNSHRUNK_VA_R)

for row in results.itertuples():
    d = analysis[analysis.ROLE.eq(row.ROLE) & analysis.METRIC.eq(row.METRIC) & analysis.OUTCOME.eq(row.OUTCOME)]
    correlation_checks(d, row)
for row in robust.itertuples():
    d = analysis[analysis.ROLE.eq(row.ROLE) & analysis.METRIC.eq("ANY_DB") & analysis.OUTCOME.eq(row.OUTCOME)].copy()
    d["X"] = {"pooled_person_years": d.RATE_POOLED_PERSON_YEARS, "unique_people": d.RATE_UNIQUE_PEOPLE,
              "three_active_years": d.RATE.where(d.N_ACTIVE_YEARS.ge(3))}[row.SPEC]
    correlation_checks(d, row, "X")
# Check BH separately within each prespecified role/coverage-definition family.
for _, d in results.groupby(["ROLE", "METRIC"]):
    finite = d.PEARSON_P.notna()
    p = d.loc[finite, "PEARSON_P"]
    order = np.argsort(p.to_numpy())
    adjusted = np.minimum.accumulate((p.to_numpy()[order]*len(d)/np.arange(1, len(p)+1))[::-1])[::-1].clip(0, 1)
    close(adjusted, d.loc[finite, "PEARSON_Q_BH"].to_numpy()[order])

def md(df):
    return "\n".join(["| " + " | ".join(df.columns) + " |", "| " + " | ".join(["---"]*len(df.columns)) + " |"] +
        ["| " + " | ".join(str(x) for x in row) + " |" for row in df.itertuples(index=False, name=None)])

main = distribution[distribution.METRIC.eq("ANY_DB")].set_index("ROLE").loc[ROLES].reset_index()
display = main[["ROLE", "N_SCHOOLS", "MEAN", "P10", "MEDIAN", "P90", "N_ZERO", "N_ONE"]].copy()
for c in ["MEAN", "P10", "MEDIAN", "P90"]:
    display[c] = display[c].map(lambda x: f"{100*x:.1f}%")
display.columns = ["Role", "Schools", "Mean", "P10", "Median", "P90", "Zero coverage", "Complete coverage"]
outcomes = pd.read_csv(ROOT / "data/clean/staff_quality_va/staff_va_outcome_dictionary.csv")
corrtable = results[results.METRIC.eq("ANY_DB")].pivot(index="LABEL", columns="ROLE", values="PEARSON_R").reindex(outcomes.LABEL)[ROLES]
corrtable = corrtable.map(lambda x: f"{x:.3f}").reset_index().rename(columns={"LABEL": "VA outcome"})
coverage = pd.read_csv(OUT / "school_staff_match_availability.csv")
comparison = distribution.pivot(index="ROLE", columns="METRIC", values="MEAN").reindex(ROLES)[METRICS]
comparison = comparison.map(lambda x: f"{100*x:.1f}%").reset_index()
comparison.columns = ["Role", "Any, entire DB", "UG, entire DB", "Any, by staff year", "UG, by staff year"]
report = ["# School staff coverage in titulados and correlations with VA", "",
    "## Definition", "",
    "Main measure: for each school, role and year, the fraction of identified role members with at least one exact-MRUN qualification match anywhere in the 2007-2025 reporting-cohort database. "
    "Average those fractions equally over active-role years in 2018-2024. A person counts once per school-role-year, even with several appointments or awards. "
    "The same person can legitimately appear at different schools or in different roles. Teachers means regular youth-HS classroom teachers under the established definition; orientadores and leadership include primary or secondary functions.", "",
    "This is database linkage coverage, not historical qualification possession or staff quality. A later award can establish a full-database match, but cannot establish an as-of credential. "
    "Separate ANY_ASOF and UG_ASOF measures enforce reporting and award years no later than the staff year. UG_DB requires an undergraduate match anywhere in the database.", "",
    "Require known role counts in all seven staff years for the main period measure, consistent with the existing staff-VA analysis. "
    "At least one active-role year is sufficient for this coverage diagnostic; a three-active-year sensitivity is also reported. "
    "Zero staff means undefined coverage, never zero matching. Unknown role counts remain missing. "
    "All 3,682 schools are retained in the merge-ready file for each role, including missing rates and their status. "
    "The observed-years average is retained separately for partial rosters, but does not enter the main plots/correlations.", "",
    "Alternative aggregation: pooled matched person-years divided by staff person-years, and matched unique people divided by all unique people ever at that school in the role. "
    "These are saved and used in sensitivity correlations; neither silently replaces the equal-year primary measure.", "",
    "## Distribution across schools", "", md(display), "",
    "Each school receives equal weight. The orientador distribution is highly discrete: small role headcounts often produce zero or complete matching. "
    "A school with no observed orientador is excluded from this distribution rather than added to its zero bin.", "",
    f"![Distribution]({(ROOT / 'output/figures/titulados_school_coverage/school_staff_match_distribution.png').as_posix()})", "",
    "### Coverage definitions", "", md(comparison), "",
    "All four definitions use the same eligible schools within role. The differences reflect award type and temporal coverage, not a changed school sample.", "",
    "### School availability", "", md(coverage), "",
    "## Correlations with school VA", "",
    "Unweighted Pearson correlations with the existing All-sample empirical-Bayes school VA estimates. "
    "Each outcome uses its own pairwise complete school sample; no imputation or VA re-estimation. "
    "These are unadjusted descriptive correlations, not causal effects, and do not establish that missing qualifications are random.", "",
    md(corrtable), "",
    f"![VA correlations]({(ROOT / 'output/figures/titulados_school_coverage/school_staff_match_va_correlations.png').as_posix()})", "",
    "Most correlations are small. Teacher coverage has modest negative Pearson associations with high-premium-institution and math VA. "
    "For math the teacher Spearman correlation is near zero, so the negative Pearson association is not a strong monotonic pattern throughout the distribution. "
    "Orientador and leadership coverage have weak associations with the main income and achievement VA measures.", "",
    "Small does not mean statistically indistinguishable from zero: the teacher links to higher-ed enrollment and high-premium-institution VA, "
    "and the orientador links to high-premium-field and projected-income VA, survive the stated within-role/definition BH correction. "
    "Undergraduate-only teacher coverage has a stronger negative link to high-premium-institution VA: "
    f"r={results.loc[results.ROLE.eq('HS teachers') & results.METRIC.eq('UG_DB') & results.OUTCOME.eq('high_inst_m1'), 'PEARSON_R'].iloc[0]:.3f} "
    "for the entire DB, and "
    f"r={results.loc[results.ROLE.eq('HS teachers') & results.METRIC.eq('UG_ASOF') & results.OUTCOME.eq('high_inst_m1'), 'PEARSON_R'].iloc[0]:.3f} "
    "for as-of undergraduate coverage. These results concern matching coverage, not the eight credential characteristics; "
    "their associations with VA have not been computed in this analysis.", "",
    "The full correlation CSV includes N, Pearson r and conventional 95% confidence intervals/p-values, Spearman r, student-weighted r, and unshrunk-VA r. "
    "Student weights are the fixed broad VA-school student counts, not annual enrollment. Spearman ranks use 10 significant digits to preserve intended ties. "
    "BH corrections are within role and coverage definition across the 12 outcomes. Inference is conditional on estimated VA and does not propagate VA estimation uncertainty. "
    "No adjusted regressions are claimed in this diagnostic.", "",
    "## Files and verification", "",
    f"- [School-level rates]({(OUT / 'school_staff_match_rates.csv').as_posix()})",
    f"- [Detailed period rates and alternative denominators]({(OUT / 'school_staff_match_period_long.csv').as_posix()})",
    f"- [All correlations and sample sizes]({(OUT / 'school_staff_match_va_correlations.csv').as_posix()})",
    f"- [Alternative aggregation correlations]({(OUT / 'school_staff_match_va_robustness.csv').as_posix()})", "",
    f"Independent Python verification reconstructs active annual numerators/denominators from person-school memberships and match flags; "
    f"checks all {len(annual):,} annual and {len(period):,} period rows against roster/aggregation rules; "
    f"checks all distribution summaries, {len(results)} main and {len(robust)} sensitivity correlation rows, BH adjustments and source-VA equality. "
    "Source input hashes are unchanged. Records start in 2007, so missing matches can reflect older or foreign degrees rather than no qualifications.", ""]
report_path = ROOT / "output/reports/school_staff_titulados_coverage_va.md"
report_path.write_text("\n".join(report), encoding="utf-8")
manifest = pd.read_csv(OUT / "school_staff_match_input_manifest.csv")
for row in manifest.itertuples():
    assert hashlib.md5(Path(row.PATH).read_bytes()).hexdigest() == row.MD5
verified = {"annual_rows": len(annual), "period_rows": len(period), "correlation_rows": len(results),
            "robustness_rows": len(robust), "source_hashes_unchanged": True,
            "source_va_unchanged": True, "all_checks_passed": True}
(OUT / "school_staff_match_verification.json").write_text(json.dumps(verified, indent=2), encoding="utf-8")
print(json.dumps(verified, indent=2))
print(comparison.to_string(index=False))
print(coverage.to_string(index=False))
print(report_path)
