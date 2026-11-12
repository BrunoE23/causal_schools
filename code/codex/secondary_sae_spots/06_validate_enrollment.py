"""Independent source aggregation and FWL/cluster-sandwich verification."""
from pathlib import Path
import csv
from collections import defaultdict
import json
import numpy as np
import pandas as pd

root = Path(__file__).resolve().parents[3]
out = root / "data/clean/secondary_sae_spots/enrollment_trends"
codes = {310, 410, 510, 610, 710, 810, 910}
saved = pd.read_csv(out / "school_year_enrollment.csv")
source_checks = []
for year in range(2010, 2026):
    files = list((out / "source_courses" / str(year)).glob("*.csv"))
    assert len(files) == 1
    totals = defaultdict(lambda: np.zeros(5, dtype=np.int64))
    with files[0].open(encoding="utf-8-sig", errors="replace", newline="") as handle:
        reader = csv.reader(handle, delimiter=";")
        header = [s.upper() for s in next(reader)]
        ix = {name: n for n, name in enumerate(header)}
        for row in reader:
            if int(row[ix["COD_ENSE"]]) not in codes:
                continue
            if "ESTADO_ESTAB" in ix and int(row[ix["ESTADO_ESTAB"]]) != 1:
                continue
            rbd, n = int(row[ix["RBD"]]), int(row[ix["N_ALU"]])
            totals[rbd][0] += n
            if int(row[ix["COD_TIP_CUR"]]) == 0:
                grade = int(row[ix["COD_GRADO"]])
                assert grade in (1, 2, 3, 4)
                totals[rbd][grade] += n
            else:
                for grade in range(1, 5):
                    totals[rbd][grade] += int(row[ix[f"N_ALU_GRADO{grade}"]])
    s = saved.loc[saved.year == year]
    assert set(totals) == set(s.rbd)
    cols = ["secondary_students"] + [f"grade{g}_students" for g in range(9, 13)]
    for row in s.itertuples():
        np.testing.assert_array_equal(totals[row.rbd], [getattr(row, c) for c in cols])
    source_checks.append({"year": year, "school_rows": len(totals), "students": int(s.secondary_students.sum())})
    print(f"Source {year}: every school and grade matches", flush=True)

columns = ["rbd", "year", "region", "event_time", "outcome", "high_va", "z_va", "listed_2021",
           "positive_pre_grade9", "positive_pre5_secondary", "positive_pre5_grade9",
           "secondary_index", "grade9_index", "baseline_secondary", "baseline_grade9"]
panel = pd.read_csv(out / "enrollment_va_panel.csv", usecols=columns)
results = pd.read_csv(out / "segmented_trend_results.csv")
max_beta_error = max_se_error = 0.0
models_checked = 0
for (outcome, metric, spec), reported in results.groupby(["outcome", "metric", "specification"]):
    d = panel.loc[(panel.outcome == outcome) & panel.event_time.between(-5, 5)].copy()
    if metric == "grade9":
        d = d.loc[d.positive_pre_grade9]
    if spec == "exclude_2020_2021":
        d = d.loc[~d.year.isin([2020, 2021])]
    if spec == "baseline_100plus":
        d = d.loc[d.baseline_secondary >= 100]
    if spec == "listed_2021":
        d = d.loc[d.listed_2021]
    if spec == "established_five_pre_years":
        d = d.loc[d[f"positive_pre5_{metric}"]]
    # Every included school has the same calendar-year support within its region.
    for _, group in d.groupby("region"):
        assert group.groupby("rbd").year.nunique().nunique() == 1
        assert len(group) == group.rbd.nunique() * group.year.nunique()
    q = d.z_va.to_numpy() if spec == "continuous_VA" else d.high_va.to_numpy()
    e = d.event_time.to_numpy()
    X = np.column_stack([q*e, q*(e >= 0), q*np.maximum(e, 0)]).astype(float)
    y = d[f"{metric}_index"].to_numpy()
    w = d[f"baseline_{metric}"].to_numpy() if spec == "baseline_enrollment_weighted" else np.ones(len(d))
    school = np.unique(d.rbd, return_inverse=True)[1]
    region = np.unique(d.region, return_inverse=True)[1]
    region_year = np.unique(d.region*10000 + d.year, return_inverse=True)[1]

    def group_means(v, index):
        return np.bincount(index, weights=w*v)[index] / np.bincount(index, weights=w)[index]

    # School and region-year FE projection is exact here because of balanced
    # within-region support and weights constant within school.
    def within(v):
        return v - group_means(v, school) - group_means(v, region_year) + group_means(v, region)

    X = np.column_stack([within(X[:, j]) for j in range(3)])
    y = within(y)
    bread = np.linalg.inv(X.T @ (w[:, None]*X))
    beta = bread @ (X.T @ (w*y))
    residual = y - X @ beta
    score = np.column_stack([np.bincount(region, weights=w*X[:, j]*residual) for j in range(3)])
    G, N, K = len(np.unique(region)), len(d), 4
    # fixest default nonnested-FE count: three slopes plus one intercept,
    # since both FE sets are nested in the regional clusters (df.K = 4).
    vcov = bread @ (score.T @ score) @ bread * (G/(G-1)) * ((N-1)/(N-K))
    contrasts = {"pre": [1, 0, 0], "post": [1, 0, 1], "slope_change": [0, 0, 1], "level_jump": [0, 1, 0]}
    for row in reported.itertuples():
        a = np.array(contrasts[row.contrast])
        estimate, se = float(a @ beta), float(np.sqrt(a @ vcov @ a))
        max_beta_error = max(max_beta_error, abs(estimate-row.estimate))
        max_se_error = max(max_se_error, abs(se-row.se))
        assert abs(estimate-row.estimate) < 1e-8, (outcome, metric, spec, estimate, row.estimate)
        assert abs(se-row.se) < 1e-8, (outcome, metric, spec, se, row.se)
        assert row.n_schools == d.rbd.nunique() and row.n_school_years == N and row.regions == G
    models_checked += 1

assert models_checked == 56
summary = {"source_checks": source_checks, "school_years_checked": len(saved),
           "regressions_checked": models_checked, "contrasts_checked": len(results),
           "max_coefficient_error": max_beta_error, "max_cluster_se_error": max_se_error}
(out / "independent_validation.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
print(json.dumps({k: v for k, v in summary.items() if k != "source_checks"}, indent=2))
