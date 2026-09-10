"""Independent appointment-to-history and member-to-school checks (no raw writes)."""
from pathlib import Path
import json
import numpy as np
import pandas as pd

DATA = Path(__file__).resolve().parents[3] / "data/clean/leadership_quality_va"
rows = pd.read_csv(DATA / "leadership_source_appointments.csv.gz", dtype={"MRUN": str})
members = pd.read_csv(DATA / "leadership_person_metrics.csv.gz", dtype={"MRUN": str})
assert len(members) == 51397 and members.MRUN.nunique() == 13104
assert not members.duplicated(["MRUN", "RBD", "AGNO"]).any()
codes = [3, 4, 10, 15]
def flag(s, secondary=False):
    return s.isin(codes).astype(float).where(s.isin(range(0 if secondary else 1, 18)))
rows["primary"] = flag(rows.ID_IFP)
second = flag(rows.ID_IFS, True)
rows["any"] = np.where(rows.primary.eq(1) | second.eq(1), 1.,
                       np.where(rows.primary.eq(0) & second.eq(0), 0., np.nan))
def collapse(keys):
    # True beats missing; missing beats an observed false.
    encoded = rows[keys].copy()
    for f in ["primary", "any"]:
        encoded[f] = rows[f].map({0.: 0., 1.: 2.}).fillna(1.)
    a = encoded.groupby(keys, as_index=False, sort=True)[["primary", "any"]].max()
    for f in ["primary", "any"]:
        a[f] = a[f].map({0.: 0., 1.: np.nan, 2.: 1.})
    return a.sort_values(keys)

def histories(a, keys, field):
    records = []
    for _, g in a.groupby(keys, sort=False):
        nknown = positive = spell = prior_n = 0
        prev_year = prev_value = None
        for row in g.itertuples(index=False):
            v, y = getattr(row, field), row.AGNO
            prior = positive if nknown else np.nan
            share = positive / nknown if nknown else np.nan
            exclusive = float(v == 1 and positive == prior_n) if not pd.isna(v) and prior_n > 0 and nknown == prior_n else np.nan
            if v == 1:
                spell = spell + 1 if prev_year == y - 1 and prev_value == 1 else 1
            else:
                spell = 0 if v == 0 else np.nan
            positive += int(v == 1)
            nknown += int(not pd.isna(v))
            cumulative = positive if nknown else np.nan
            records.append([*[getattr(row, k) for k in keys], y, prior, cumulative, spell, share, exclusive])
            prior_n += 1
            prev_year, prev_value = y, v
    return pd.DataFrame(records, columns=keys + ["AGNO", "prior", "cumulative", "spell", "share", "exclusive"])

py = collapse(["MRUN", "AGNO"])
sy = collapse(["MRUN", "RBD", "AGNO"])
direct = sy[sy["any"].eq(1) & sy.AGNO.ge(2018)].merge(members[["MRUN", "RBD", "AGNO"]].drop_duplicates(["RBD", "AGNO"])[["RBD", "AGNO"]], on=["RBD", "AGNO"])
assert len(direct) == len(members)
checks = 0
for a, keys, field, mapping in [
    (py, ["MRUN"], "primary", {"prior_role_years": "prior", "cumulative_role_years": "cumulative", "prior_role_share": "share", "exclusive_history_share": "exclusive"}),
    (py, ["MRUN"], "any", {"prior_any_role_years": "prior", "role_spell_years": "spell"}),
    (sy, ["MRUN", "RBD"], "any", {"prior_school_role_years": "prior", "school_role_spell_years": "spell"}),
    (py[py.AGNO.ge(2015)], ["MRUN"], "primary", {"prior_role_years_2015": "prior"}),
    (sy[sy.AGNO.ge(2015)], ["MRUN", "RBD"], "any", {"school_role_spell_years_2015": "spell"}),
]:
    expected = histories(a, keys, field)
    check = members.merge(expected, on=keys+["AGNO"], how="left", validate="many_to_one")
    for exported, rebuilt in mapping.items():
        np.testing.assert_allclose(check[exported], check[rebuilt], atol=1e-12, err_msg=exported)
        checks += len(check)

dictionary = pd.read_csv(DATA / "leadership_metric_dictionary.csv")
annual = pd.read_csv(DATA / "leadership_school_year_measures.csv.gz")
for metric in dictionary.METRIC:
    expected = members.groupby(["RBD", "AGNO"])[metric].agg(["size", "count", "mean"])
    a = annual[annual.METRIC.eq(metric)].set_index(["RBD", "AGNO"])
    a = a.join(expected)
    active = a["size"].notna()
    np.testing.assert_allclose(a.loc[active, "N_ELIGIBLE"], a.loc[active, "size"])
    np.testing.assert_allclose(a.loc[active, "N_OBSERVED"], a.loc[active, "count"])
    np.testing.assert_allclose(a.loc[active, "MEAN_OBSERVED"], a.loc[active, "mean"], atol=1e-12)

summary = {"status": "passed", "unique_people": members.MRUN.nunique(), "person_school_years": len(members),
    "history_values_rebuilt": checks, "annual_member_means_rebuilt": len(annual),
    "pre2015_director_exposed_people": int(members.loc[members.pre2015_director_history_share.eq(1), "MRUN"].nunique()),
    "possible_2015_code_break_people": int(members.loc[members.possible_2015_code_break_share.eq(1), "MRUN"].nunique())}
(DATA / "leadership_history_verification.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
print(json.dumps(summary, indent=2))
