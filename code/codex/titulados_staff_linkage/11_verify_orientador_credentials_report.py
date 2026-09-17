"""Check aggregation/correlations independently and report counselor credentials."""
from pathlib import Path
import json
import hashlib
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[3]
BASE = ROOT / "data/clean/titulados_staff_linkage"
OUT = BASE / "orientador_credentials_va"
dictionary = pd.read_csv(OUT / "orientador_credentials_dictionary.csv")
metrics = dictionary.METRIC.tolist()

def close(a, b):
    np.testing.assert_allclose(np.asarray(a, dtype=float), np.asarray(b, dtype=float),
                               rtol=1e-10, atol=1e-11, equal_nan=True)

m = pd.read_csv(BASE / "staff_membership_person_school_year.csv.gz", dtype={"MRUN": str})
m = m[m.ROLE.eq("Orientadores")]
cols = ["MRUN", "ROLE", "AGNO"] + metrics + ["MATCH_ANY_BY_STAFF_YEAR", "MATCH_UNDERGRAD_BY_STAFF_YEAR"]
f = pd.read_csv(BASE / "credentials/staff_credentials_person_role_year.csv.gz", usecols=cols, dtype={"MRUN": str})
f = f[f.ROLE.eq("Orientadores")]
j = m.merge(f, on=["MRUN", "ROLE", "AGNO"], validate="many_to_one", how="left")
assert len(j) == len(m) and not j[metrics].isna().any().any()
assert not j.duplicated(["RBD", "MRUN", "AGNO"]).any()
keys = ["RBD", "AGNO"]
sums = j.groupby(keys)[metrics + ["MATCH_ANY_BY_STAFF_YEAR", "MATCH_UNDERGRAD_BY_STAFF_YEAR"]].sum()
sums["HEADCOUNT"] = j.groupby(keys).size()
sums = sums.reset_index().melt(id_vars=keys + ["HEADCOUNT", "MATCH_ANY_BY_STAFF_YEAR", "MATCH_UNDERGRAD_BY_STAFF_YEAR"],
                              value_vars=metrics, var_name="METRIC", value_name="QUALIFIED")
annual = pd.read_csv(OUT / "orientador_credentials_annual.csv.gz")
v = annual.merge(sums, on=keys + ["METRIC"], how="left", validate="one_to_one")
active = v.N_ROLE.gt(0)
close(v.loc[active, "N_ROLE"], v.loc[active, "HEADCOUNT"])
close(v.loc[active, "N_QUALIFIED"], v.loc[active, "QUALIFIED"])
close(v.loc[active, "RATE"], v.loc[active, "QUALIFIED"] / v.loc[active, "HEADCOUNT"])
assert v.loc[~active, "RATE"].isna().all()
denom = np.where(v.METRIC.eq("UG_HIGH_PREMIUM"), v.MATCH_UNDERGRAD_BY_STAFF_YEAR, v.MATCH_ANY_BY_STAFF_YEAR)
expected_matched = np.divide(v.QUALIFIED, denom, out=np.full(len(v), np.nan), where=denom > 0)
expected_matched[~active] = np.nan
close(v.RATE_MATCHED, expected_matched)
roster = pd.read_csv(ROOT / "data/clean/staff_quality_va/staff_school_year_roster.csv", usecols=keys + ["N_COUNSELOR"])
rv = annual[annual.METRIC.eq(metrics[0])].merge(roster, on=keys, how="left", validate="one_to_one")
close(rv.N_ROLE, rv.N_COUNSELOR)

period = pd.read_csv(OUT / "orientador_credentials_school_period.csv")
expected = annual.groupby(["RBD", "METRIC"]).agg(
    KNOWN=("N_ROLE", "count"), ACTIVE=("N_ROLE", lambda x: x.gt(0).sum()),
    RATE=("RATE", "mean"), MATCHED=("RATE_MATCHED", "mean"), MATCHED_YEARS=("RATE_MATCHED", "count"),
    ANY_COVERAGE=("ANY_MATCH_RATE", "mean"), UG_COVERAGE=("UG_MATCH_RATE", "mean")).reset_index()
valid = expected.KNOWN.eq(7) & expected.ACTIVE.gt(0)
for c in ["RATE", "MATCHED", "ANY_COVERAGE", "UG_COVERAGE"]:
    expected.loc[~valid, c] = np.nan
expected.loc[expected.MATCHED_YEARS.lt(np.ceil(.8*expected.ACTIVE)), "MATCHED"] = np.nan
pv = period.merge(expected, on=["RBD", "METRIC"], validate="one_to_one", suffixes=["", "_EXPECTED"])
close(pv.RATE, pv.RATE_EXPECTED)
close(pv.RATE_MATCHED, pv.MATCHED)
close(pv.ANY_MATCH_RATE, pv.ANY_COVERAGE)
close(pv.UG_MATCH_RATE, pv.UG_COVERAGE)

a = pd.read_csv(OUT / "orientador_credentials_va_analysis.csv.gz")
result = pd.read_csv(OUT / "orientador_credentials_va_correlations.csv")
assert len(result) == 8*10*3
assert not result.OUTCOME.isin(["z_year_math_max", "z_year_leng_max"]).any()
rawva = pd.read_csv(ROOT / "output/tables/empirical_bayes_school_va/stata_eb_school_rbd_observational_values_for_iv.csv",
    usecols=["school_rbd", "analysis_sample", "outcome", "controlled_value_added_eb_centered_student", "controlled_value_added_centered_student"])
rawva = rawva[rawva.analysis_sample.eq("All")].rename(columns={"school_rbd": "RBD", "outcome": "OUTCOME"})
av = a.merge(rawva, on=["RBD", "OUTCOME"], how="left", validate="many_to_one")
close(av.Y, av.controlled_value_added_eb_centered_student)
close(av.Y_RAW, av.controlled_value_added_centered_student)
for row in result.itertuples():
    d = a[a.METRIC.eq(row.METRIC) & a.OUTCOME.eq(row.OUTCOME)].copy()
    d["X"] = {"all_orientadores": d.RATE, "matched_only": d.RATE_MATCHED,
              "three_active_years": d.RATE.where(d.N_ACTIVE_YEARS.ge(3))}[row.SPEC]
    ok = d.X.notna() & d.Y.notna()
    x, y = d.loc[ok, "X"], d.loc[ok, "Y"]
    assert len(x) == row.N
    assert row.STATUS == "estimated"
    close(np.corrcoef(x, y)[0, 1], row.PEARSON_R)
    xr, yr = [z.map(lambda q: float(format(q, ".10g"))).rank() for z in [x, y]]
    close(np.corrcoef(xr, yr)[0, 1], row.SPEARMAN_R)
    w = d.loc[ok, "N_VA_STUDENTS"].to_numpy(dtype=float)
    w /= w.sum()
    dx, dy = x.to_numpy()-np.sum(w*x), y.to_numpy()-np.sum(w*y)
    close(np.sum(w*dx*dy)/np.sqrt(np.sum(w*dx*dx)*np.sum(w*dy*dy)), row.STUDENT_WEIGHTED_R)
    raw = d[["X", "Y_RAW"]].dropna()
    assert len(raw) == row.N_UNSHRUNK
    close(raw.corr().iloc[0, 1], row.UNSHRUNK_VA_R)
    if row.SPEC == "all_orientadores":
        p = d[["RATE", "Y", "ANY_MATCH_RATE", "UG_MATCH_RATE"]].dropna()
        design = np.column_stack([np.ones(len(p)), p.ANY_MATCH_RATE, p.UG_MATCH_RATE])
        residual = p[["RATE", "Y"]].to_numpy() - design @ np.linalg.lstsq(design, p[["RATE", "Y"]].to_numpy(), rcond=None)[0]
        assert len(p) == row.N_PARTIAL
        close(np.corrcoef(residual.T)[0, 1], row.COVERAGE_PARTIAL_R)
for _, r in result.groupby("SPEC"):
    p = r.PEARSON_P.to_numpy()
    order = np.argsort(p)
    adj = np.minimum.accumulate((p[order]*len(p)/np.arange(1, len(p)+1))[::-1])[::-1].clip(0, 1)
    close(adj, r.PEARSON_Q_BH.to_numpy()[order])
manifest = pd.read_csv(OUT / "orientador_credentials_input_manifest.csv")
for row in manifest.itertuples():
    assert hashlib.md5(Path(row.PATH).read_bytes()).hexdigest() == row.MD5

def md(df):
    return "\n".join(["| " + " | ".join(df.columns) + " |", "| " + " | ".join(["---"]*len(df.columns)) + " |"] +
        ["| " + " | ".join(str(x) for x in row) + " |" for row in df.itertuples(index=False, name=None)])

main = result[result.SPEC.eq("all_orientadores")].copy()
labels = dictionary.set_index("METRIC").METRIC_LABEL
main["CELL"] = main.apply(lambda r: f"{r.PEARSON_R:.3f}" + ("*" if r.PEARSON_Q_BH < .05 else ""), axis=1)
focus = ["Higher-ed enrollment", "High-premium field", "High-premium institution", "Projected income (full)"]
tab = main.pivot(index="METRIC", columns="LABEL", values="CELL").reindex(metrics)[focus]
tab.index = tab.index.map(labels)
tab = tab.reset_index().rename(columns={"METRIC": "School share of orientadores with..."})
income = result[result.OUTCOME.eq("log_program_income_full_clp_m1")]
inc = income.pivot(index="METRIC", columns="SPEC", values="PEARSON_R").reindex(metrics)
inc["coverage_partial"] = main[main.OUTCOME.eq("log_program_income_full_clp_m1")].set_index("METRIC").COVERAGE_PARTIAL_R
inc = inc[["all_orientadores", "coverage_partial", "matched_only", "three_active_years"]].map(lambda v: f"{v:.3f}")
inc.index = inc.index.map(labels)
inc = inc.reset_index()
inc.columns = ["Credential", "Main r", "Coverage partial r", "Matched-only r", ">=3 active years r"]
distribution = pd.read_csv(OUT / "orientador_credentials_distribution.csv").set_index("METRIC").reindex(metrics).reset_index()
dist = distribution[["METRIC_LABEL", "N_SCHOOLS", "MEAN_SHARE", "N_POSITIVE", "N_MATCHED_SCHOOLS"]].copy()
dist.MEAN_SHARE = dist.MEAN_SHARE.map(lambda v: f"{100*v:.1f}%")
dist.columns = ["Credential", "Schools", "Mean share", "Schools with positive share", "Matched-only schools"]
figure = ROOT / "output/figures/orientador_credentials_va/orientador_credentials_nonachievement_va.png"
report = ["# Orientador credentials and non-achievement school VA", "",
    "## Main results", "",
    "These are correlations with the eight observed credential measures, not database-match rates. "
    "The clearest patterns concern training at high-premium institutions and orientation-related qualifications. "
    "Simply having a magister has little association with higher-education outcomes in these data.", "",
    md(tab), "",
    "* BH-adjusted p < 0.05 across all 80 main tests (eight credentials by ten non-score outcomes). "
    "There are 2,110 schools for higher-ed enrollment and 2,111 for field, institution and full-income VA. "
    "The complete outcome family includes exam taking, enrollment, STEM, high-premium field/institution, three projected-income measures and two accreditation outcomes; math/language scores are excluded.", "",
    f"![All correlations]({figure.as_posix()})", "",
    "Orientation-related qualifications correlate 0.080 with enrollment VA, 0.075 with high-premium-field VA and 0.087 with full projected-income VA. "
    "An undergraduate qualification at a high-premium institution correlates 0.144 with high-premium-institution VA; any qualification at one correlates 0.133. "
    "These are modest associations, not estimated causal effects of counselor credentials.", "",
    "## Construction and denominators", "",
    "For each school-year, count each orientador once across appointments and divide each binary credential total by all current primary-or-secondary orientadores. "
    "Qualifications are those reported and obtained by that staff year, using the verified person-role-year indicators and the existing subject mapping. "
    "A zero means no qualifying award observed, not no lifetime qualification. Records begin in 2007. "
    "Average annual shares equally over active orientador years in 2018-2024. Require all seven role counts known and at least one active year. "
    "Schools with no orientador have undefined shares, not zeros. All 3,682 school rows remain in the underlying period data; 2,111 have main rates.", "",
    "High premium is the preexisting centered MiFuturo institution FE >0.1. Non-undergraduate includes diplomados, postitulos, magisters, doctorates and specialties. "
    "A role-specific qualification explicitly concerns orientation/counseling/psychoeducation/family mediation under the reviewable mapping; generic psychology or education alone is insufficient. "
    "A relevant master's must itself satisfy the subject condition. No subject definitions or institution thresholds were changed after looking at VA.", "",
    md(dist), "",
    "## Coverage and support checks: projected-income VA", "",
    md(inc), "",
    "Coverage partial r residualizes both the credential share and VA on an intercept, any-award match share and undergraduate-match share, on the same complete main sample. "
    "It is a descriptive partial correlation, not a causal coefficient. The source does not support a claim that this corrects selective observation.", "",
    "Matched-only shares divide by orientadores with at least one as-of qualification match; the undergraduate-high-premium share instead divides by orientadores with an as-of undergraduate match. "
    "A matched denominator must be nonempty in at least 80% of active years, and matched-only annual shares are averaged over those years. "
    "This changes both the denominator and the school sample: 1,153 schools for the seven any-award-based measures and 526 for undergraduate-high-premium in the full-income comparison. "
    "It is a sensitivity on selected observed records, not an unbiased correction. A separate check requires at least three active orientador years.", "",
    "The orientation-related qualification/income association remains about 0.085 after controlling linearly for match coverage, and about 0.103 among matched records. "
    "Any-magister/income remains close to zero. Matched-only significance is corrected separately across its own 80 tests.", "",
    "## Inference, files and verification", "",
    "Primary correlations use equally weighted schools and existing All-sample EB VA, with outcome-specific complete cases. "
    "The CSV also provides Spearman, broad-VA-student-weighted and unshrunk-VA correlations, Pearson confidence intervals/p-values and per-specification BH corrections. "
    "Spearman ranks use ten significant digits to preserve ties. Inference is conditional on saved VA and does not propagate its estimation uncertainty. "
    "There are no student-level regressions, no re-estimation of VA and no age/school-context adjustment in this exercise.", "",
    f"- [Full correlation table]({(OUT / 'orientador_credentials_va_correlations.csv').as_posix()})",
    f"- [School credential shares]({(OUT / 'orientador_credentials_school_period.csv').as_posix()})",
    f"- [Role-specific definition review]({(ROOT / 'output/reports/staff_credential_specialization_review.md').as_posix()})", "",
    f"Independent checks reconstructed {len(annual):,} annual and {len(period):,} period rows from staff memberships and person credentials, "
    "reconciled roster denominators, checked all 240 main/sensitivity correlation rows and 80 coverage partial correlations, "
    "all BH adjustments, saved-VA equality and unchanged input hashes. These verify implementation, not the substantive validity of the credential taxonomy.", ""]
path = ROOT / "output/reports/orientador_credentials_nonachievement_va.md"
path.write_text("\n".join(report), encoding="utf-8")
audit = {"annual_rows": len(annual), "period_rows": len(period), "correlation_rows": len(result),
         "partial_correlations": 80, "all_checks_passed": True, "source_hashes_unchanged": True}
(OUT / "orientador_credentials_verification.json").write_text(json.dumps(audit, indent=2), encoding="utf-8")
print(json.dumps(audit, indent=2))
print(tab.to_string(index=False))
print(path)
