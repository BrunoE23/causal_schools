"""Independent numerical checks of R-produced school measures and associations."""
from pathlib import Path
import json
import hashlib
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[3]
DATA = ROOT / "data/clean/staff_quality_va"
wide = pd.read_csv(DATA / "school_staff_indices_and_measures.csv", float_precision="round_trip")
results = pd.read_csv(DATA / "staff_va_correlations.csv")
params = pd.read_csv(DATA / "staff_index_standardizations.csv")
dictionary = pd.read_csv(DATA / "staff_analysis_metric_dictionary.csv")
outcomes = pd.read_csv(DATA / "staff_va_outcome_dictionary.csv")
va = pd.read_csv(ROOT / "output/tables/empirical_bayes_school_va/stata_eb_school_rbd_observational_values_for_iv.csv", float_precision="round_trip")
va = va[va.analysis_sample.eq("All")].rename(columns={"school_rbd": "RBD", "outcome": "OUTCOME"})
assert not wide.duplicated(["RBD", "ROLE"]).any()
assert not results.duplicated(["ROLE", "METRIC", "OUTCOME"]).any()
assert len(results) == int(dictionary.ANALYZE.sum()) * len(outcomes)
assert set(results.OUTCOME) == set(outcomes.OUTCOME)
assert set(wide.ROLE) == {"counselor", "teacher"}
assert all(wide.groupby("ROLE").size() == 3682)

# Compact typed columns keep the 1.39m annual rows well below a full raw-data load.
annual = pd.read_csv(DATA / "staff_school_year_measures.csv.gz", usecols=[
    "RBD", "AGNO", "ROLE", "METRIC", "N_ROLE", "N_ELIGIBLE", "N_OBSERVED", "MEAN_OBSERVED", "VALUE"],
    dtype={"RBD": "int32", "AGNO": "int16", "ROLE": "category", "METRIC": "category"})
assert set(annual.AGNO) == set(range(2018, 2025))
assert not annual.duplicated(["RBD", "AGNO", "ROLE", "METRIC"]).any()
annual_valid = annual.N_ROLE.notna() & annual.N_ELIGIBLE.gt(0) & (annual.N_OBSERVED / annual.N_ELIGIBLE).ge(.8)
np.testing.assert_allclose(annual.VALUE, annual.MEAN_OBSERVED.where(annual_valid), atol=1e-12)
annual["active"] = annual.N_ROLE.gt(0)
annual["eligible_year"] = annual.N_ELIGIBLE.gt(0)
period_check = annual.groupby(["RBD", "ROLE", "METRIC"], observed=True).agg(
    known=("N_ROLE", "count"), active=("active", "sum"), eligible=("eligible_year", "sum"),
    valid=("VALUE", "count"), mean=("VALUE", "mean"), members=("N_ELIGIBLE", "sum"), observed=("N_OBSERVED", "sum"))
period_check["expected"] = period_check["mean"].where(
    period_check.known.eq(7) & period_check.eligible.gt(0) & (period_check.valid / period_check.eligible).ge(.8))
period = pd.read_csv(DATA / "staff_school_period_measures.csv").set_index(["RBD", "ROLE", "METRIC"])
period_check = period_check.reindex(period.index)
for export, check in [("VALUE", "expected"), ("N_ROLE_YEARS_KNOWN", "known"), ("N_ACTIVE_YEARS", "active"),
                      ("N_VALID_YEARS", "valid"), ("N_ELIGIBLE_YEARS", "eligible"),
                      ("N_ELIGIBLE_PERSON_YEARS", "members"), ("N_OBSERVED_PERSON_YEARS", "observed")]:
    np.testing.assert_allclose(period[export], period_check[check], atol=1e-12)
annual_checks, period_checks = len(annual), len(period)
del annual, period_check, period

index_checks = 0
for role in ["counselor", "teacher", "teacher_history2016_sensitivity"]:
    is_alt = role.endswith("sensitivity")
    base_role = "teacher" if is_alt else role
    dt = wide[wide.ROLE.eq(base_role)].copy()
    ps = params[params.ROLE.eq(role)]
    components = ps.COMPONENT.tolist()
    if is_alt:
        dt["prior_role_years"] = dt.prior_hs_years_since2016
    selected = dt.N_ACTIVE_YEARS.ge(3) & dt[components].notna().all(axis=1)
    x = dt.loc[selected, components].to_numpy()
    np.testing.assert_allclose(x.mean(axis=0), ps.CENTER, atol=1e-12)
    np.testing.assert_allclose(x.std(axis=0, ddof=1), ps.SCALE, atol=1e-12)
    z = (x - ps.CENTER.to_numpy()) / ps.SCALE.to_numpy()
    career = z[:, :2].mean(axis=1) / ps.BLOCK_SCALE.iloc[0]
    credentials = z[:, 2:].mean(axis=1) / ps.BLOCK_SCALE.iloc[2]
    balanced = (career + credentials) / 2 / ps.BALANCED_SCALE.iloc[0]
    score = "balanced_index_history2016" if is_alt else "balanced_index"
    np.testing.assert_allclose(balanced, dt.loc[selected, score], atol=1e-11)
    assert dt.loc[~selected, score].isna().all()
    index_checks += int(selected.sum())

cor_checks = 0
adjusted_checks = 0
for (role, outcome), group in results.groupby(["ROLE", "OUTCOME"]):
    dt = wide[wide.ROLE.eq(role)].merge(va[va.OUTCOME.eq(outcome)], on="RBD", how="left", validate="one_to_one")
    for row in group.itertuples():
        xy = dt[[row.METRIC, "controlled_value_added_eb_centered_student"]].dropna()
        assert len(xy) == row.N
        if row.STATUS != "estimated":
            continue
        x, y = xy.iloc[:, 0], xy.iloc[:, 1]
        r = np.corrcoef(x, y)[0, 1]
        np.testing.assert_allclose(r, row.PEARSON_R, atol=2e-12)
        rank_r = np.corrcoef(x.map(lambda v: float(format(v, ".10g"))).rank(method="average"),
                            y.map(lambda v: float(format(v, ".10g"))).rank(method="average"))[0, 1]
        np.testing.assert_allclose(rank_r, row.SPEARMAN_R, atol=2e-12, err_msg=f"{role}/{row.METRIC}/{outcome}")
        w = dt.loc[xy.index, "N_VA_STUDENTS"].to_numpy(dtype=float)
        w /= w.sum()
        dx, dy = x.to_numpy() - np.sum(w*x), y.to_numpy() - np.sum(w*y)
        rw = np.sum(w*dx*dy)/np.sqrt(np.sum(w*dx**2)*np.sum(w*dy**2))
        np.testing.assert_allclose(rw, row.STUDENT_WEIGHTED_R, atol=2e-12)
        raw = dt[[row.METRIC, "controlled_value_added_centered_student"]].dropna()
        assert len(raw) == row.N_UNSHRUNK
        np.testing.assert_allclose(np.corrcoef(raw.iloc[:, 0], raw.iloc[:, 1])[0, 1], row.UNSHRUNK_VA_R, atol=2e-12)
        cor_checks += 1
        if row.METRIC not in ["balanced_index", "credentials_index"] or outcome not in [
            "z_year_math_max", "z_year_leng_max", "log_program_income_full_clp_m1"]:
            continue
        factors = ["COD_DEPE", "COD_REG_RBD", "RURAL_RBD", "HAS_TP_OR_ARTISTIC", "HAS_BASIC"]
        cols = [row.METRIC, "controlled_value_added_eb_centered_student", "N_VA_STUDENTS"] + factors
        a = dt[cols].dropna()
        assert len(a) == row.N_ADJUSTED
        az = (a.iloc[:, 0] - a.iloc[:, 0].mean()) / a.iloc[:, 0].std(ddof=1)
        yz = (a.iloc[:, 1] - a.iloc[:, 1].mean()) / a.iloc[:, 1].std(ddof=1)
        ln = np.log(a.N_VA_STUDENTS)
        design = pd.DataFrame({"intercept": 1., "X_Z": az, "LOG_N": ln, "LOG_N2": ln**2})
        for f in factors:
            if a[f].nunique() > 1:
                design = pd.concat([design, pd.get_dummies(a[f], prefix=f, drop_first=True, dtype=float)], axis=1)
        matrix = design.to_numpy(dtype=float)
        coef, _, rank, _ = np.linalg.lstsq(matrix, yz.to_numpy(), rcond=None)
        residual = yz.to_numpy() - matrix @ coef
        bread = np.linalg.inv(matrix.T @ matrix) if rank == matrix.shape[1] else np.linalg.pinv(matrix.T @ matrix)
        covariance = bread @ ((matrix.T * residual**2) @ matrix) @ bread * len(matrix)/(len(matrix)-rank)
        np.testing.assert_allclose(coef[1], row.ADJUSTED_BETA_SD, atol=1e-9)
        np.testing.assert_allclose(np.sqrt(covariance[1,1]), row.ADJUSTED_SE_HC1, atol=1e-8)
        adjusted_checks += 1

for role, group in results.groupby("ROLE"):
    for pcol, qcol in [("PEARSON_P", "PEARSON_Q_BH"), ("ADJUSTED_P", "ADJUSTED_Q_BH")]:
        eligible = group[pcol].notna()
        p = group.loc[eligible, pcol].to_numpy()
        order = np.argsort(p)
        q = np.minimum.accumulate((p[order]*len(p)/np.arange(1, len(p)+1))[::-1])[::-1]
        check = np.empty(len(p)); check[order] = np.minimum(1., q)
        np.testing.assert_allclose(check, group.loc[eligible, qcol], atol=1e-12)

robust = pd.read_csv(DATA / "staff_va_index_robustness.csv")
assert len(robust) == 288
for (role, outcome), group in robust.groupby(["ROLE", "OUTCOME"]):
    dt = wide[wide.ROLE.eq(role)].merge(va[va.OUTCOME.eq(outcome)], on="RBD", how="left", validate="one_to_one")
    restrictions = {"no_private_paid": dt.COD_DEPE.notna() & dt.COD_DEPE.ne(4),
                    "at_least_100_va_students": dt.N_VA_STUDENTS.ge(100),
                    "at_least_5_active_staff_years": dt.N_ACTIVE_YEARS.ge(5)}
    for row in group.itertuples():
        pair = dt.loc[restrictions[row.SUBSAMPLE], [row.METRIC, "controlled_value_added_eb_centered_student"]].dropna()
        assert len(pair) == row.N
        np.testing.assert_allclose(np.corrcoef(pair.iloc[:, 0], pair.iloc[:, 1])[0,1], row.PEARSON_R, atol=2e-12)
for _, group in robust.groupby(["ROLE", "SUBSAMPLE"]):
    a = group[group.ADJUSTED_P.notna()]
    pvals = a.ADJUSTED_P.to_numpy()
    order = np.argsort(pvals)
    qvals = np.minimum.accumulate((pvals[order]*len(pvals)/np.arange(1, len(pvals)+1))[::-1])[::-1]
    expected = np.empty(len(pvals)); expected[order] = np.minimum(1., qvals)
    np.testing.assert_allclose(expected, a.ADJUSTED_Q_BH, atol=1e-12)

summary = {"status": "passed", "school_index_scores_reconstructed": index_checks,
           "annual_metric_coverage_gates_checked": annual_checks, "period_aggregates_checked": period_checks,
           "pearson_spearman_weighted_checks": cor_checks, "independent_HC1_regressions": adjusted_checks,
           "robustness_samples_and_correlations_checked": len(robust),
           "all_BH_adjustments_checked": True, "all_requested_outcomes_present": True,
           "all_3682_schools_retained_per_role": True}
summary["report_input_sha256"] = {name: hashlib.sha256((DATA / name).read_bytes()).hexdigest() for name in [
    "school_staff_indices_and_measures.csv", "staff_va_correlations.csv", "staff_va_index_robustness.csv",
    "staff_index_diagnostics.csv", "staff_pca_loadings.csv", "staff_tenure_audit.csv",
    "staff_va_outcome_dictionary.csv", "staff_va_analysis_warnings.csv"]}
print(json.dumps(summary, indent=2))
(DATA / "staff_va_independent_verification.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
