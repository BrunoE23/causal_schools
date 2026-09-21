"""Construct horizon-specific higher-education persistence outcomes.

The analysis cohorts are grade-8 cohorts 2017--2020. Their expected entry
years are 2022--2025. The 2017 cohort has three follow-up years, 2018 has two,
2019 has one, and 2020 has entry-year enrollment only through full SIES 2025.
"""

from __future__ import annotations

import os
import re
import unicodedata
from collections import defaultdict
from pathlib import Path

import numpy as np
import pandas as pd


REPO = Path(__file__).resolve().parents[3]
DATA_ROOT = Path(os.environ.get("CAUSAL_SCHOOLS_DATA_WD", r"C:\Users\brunem\Box\causal_schools"))
RAW = DATA_ROOT / "data" / "raw"
CLEAN = DATA_ROOT / "data" / "clean"
OUT_CLEAN = REPO / "data" / "clean" / "higher_ed_persistence"
OUT_TABLE = REPO / "output" / "tables" / "higher_ed_persistence"
OUT_CLEAN.mkdir(parents=True, exist_ok=True)
OUT_TABLE.mkdir(parents=True, exist_ok=True)

COHORTS = (2017, 2018, 2019, 2020)
YEARS = (2022, 2023, 2024, 2025)
ELIGIBLE_LEVELS = {"Carreras Profesionales", "Carreras Técnicas"}
HIGH_INST_CUTOFF = 0.1


def normalize_text(value: object) -> str | None:
    if value is None or pd.isna(value):
        return None
    text = unicodedata.normalize("NFKD", str(value)).encode("ascii", "ignore").decode("ascii")
    text = re.sub(r"[^A-Z0-9]+", " ", text.upper()).strip()
    return re.sub(r"\s+", " ", text) or None


def clean_code(value: object) -> str | None:
    if value is None or pd.isna(value):
        return None
    value = str(value).strip()
    return None if value in {"", "NA", "N/A", "nan"} else value


def institution_key(cod_inst: object, tipo_inst: object, nomb_inst: object) -> str | None:
    parts = [clean_code(cod_inst), normalize_text(tipo_inst), normalize_text(nomb_inst)]
    return " || ".join(parts) if all(parts) else None


def high_premium_field(area: object, cine_area: object, cine_subarea: object) -> bool | None:
    area_n = normalize_text(area)
    cine_area_n = normalize_text(cine_area)
    cine_sub_n = normalize_text(cine_subarea)
    field = None
    if cine_sub_n == "VETERINARIA":
        field = "HEALTH AND WELFARE"
    elif cine_area_n in {"AGRICULTURA", "SERVICIOS"}:
        field = None
    elif cine_area_n == "CIENCIAS":
        field = "SCIENCE"
    elif cine_area_n == "EDUCACION":
        field = "TEACHING"
    elif cine_area_n == "HUMANIDADES Y ARTES":
        field = "HUMANITIES AND ARTS"
    elif cine_area_n == "INGENIERIA INDUSTRIA Y CONSTRUCCION":
        field = "ENGINEERING"
    elif cine_area_n == "CIENCIAS SOCIALES ENSENANZA COMERCIAL Y DERECHO" and cine_sub_n == "DERECHO":
        field = "LAW"
    elif cine_area_n == "CIENCIAS SOCIALES ENSENANZA COMERCIAL Y DERECHO" and cine_sub_n == "ENSENANZA COMERCIAL Y ADMINISTRACION":
        field = "BUSINESS"
    elif cine_area_n == "CIENCIAS SOCIALES ENSENANZA COMERCIAL Y DERECHO" and cine_sub_n in {"CIENCIAS SOCIALES Y DEL COMPORTAMIENTO", "PERIODISMO E INFORMACION"}:
        field = "SOCIAL SCIENCES"
    elif cine_area_n == "SALUD Y SERVICIOS SOCIALES" and cine_sub_n == "MEDICINA":
        field = "MEDICINE"
    elif cine_area_n == "SALUD Y SERVICIOS SOCIALES":
        field = "HEALTH AND WELFARE"
    if field is None:
        # The premium-field outcome is a binary positive-list indicator.
        # An observed enrollment outside the positive list is zero even when
        # the broader nine-category field classification is unavailable.
        return False
    if field in {"SCIENCE", "ENGINEERING", "LAW"}:
        return True
    medicine_plus = {
        "MEDICINA", "QUIMICA Y FARMACIA", "ENFERMERIA",
        "OBSTETRICIA Y PUERICULTURA", "TECNOLOGIA MEDICA", "ODONTOLOGIA",
    }
    return bool(field == "MEDICINE" and area_n in medicine_plus)


def read_universe() -> pd.DataFrame:
    path = CLEAN / "univ_gr8_df.csv"
    cols = ["mrun", "cohort_gr8", "sae_proceso", "timely_sae"]
    pieces = []
    for chunk in pd.read_csv(path, usecols=cols, chunksize=250_000, low_memory=False):
        chunk["cohort_gr8"] = pd.to_numeric(chunk["cohort_gr8"], errors="coerce")
        pieces.append(chunk.loc[chunk["cohort_gr8"].isin(COHORTS)])
    universe = pd.concat(pieces, ignore_index=True)
    universe["mrun"] = pd.to_numeric(universe["mrun"], errors="coerce")
    universe = universe.dropna(subset=["mrun", "cohort_gr8"]).copy()
    universe["mrun"] = universe["mrun"].astype("int64")
    universe["cohort_gr8"] = universe["cohort_gr8"].astype("int64")
    universe["expected_entry_year"] = universe["cohort_gr8"] + 5
    # MRUN should identify one grade-8 cohort. Preserve SAE flags at that level.
    conflict = universe.groupby("mrun")["cohort_gr8"].nunique()
    if (conflict > 1).any():
        raise ValueError(f"{int((conflict > 1).sum())} MRUNs appear in multiple grade-8 cohorts")
    universe = (universe.groupby(["mrun", "cohort_gr8", "expected_entry_year"], as_index=False)
                .agg(sae_proceso=("sae_proceso", "first"),
                     timely_sae=("timely_sae", "max")))
    return universe


def read_institution_effects() -> dict[str, float]:
    path = REPO / "output" / "tables" / "mifuturo_matricula_income" / "mifuturo_income_fe_institution_effects.csv"
    effects = pd.read_csv(path)
    effects = effects.loc[(effects["effect_type"] == "institution")]
    return dict(zip(effects["level"].astype(str), effects["effect_log_clp_centered"].astype(float)))


def raw_path(year: int) -> Path:
    paths = list((RAW / str(year) / f"Matricula-Ed-Superior-{year}").glob("*MRUN.csv"))
    if len(paths) != 1:
        raise FileNotFoundError(f"Expected one full SIES MRUN CSV for {year}; found {len(paths)}")
    return paths[0]


def collect_annual_enrollments(target_mruns: set[int], inst_effects: dict[str, float]):
    usecols = [
        "mrun", "codigo_unico", "cod_carrera", "cod_inst", "tipo_inst_1",
        "nomb_inst", "area_carrera_generica", "cine_f_97_area_area",
        "cine_f_97_subarea", "nivel_carrera_2",
    ]
    annual = defaultdict(lambda: {
        "program": set(), "area": set(), "institution": set(),
        "high_field": set(), "high_inst": set(), "rows": 0,
    })
    scan_rows = []
    for year in YEARS:
        path = raw_path(year)
        n_raw = n_eligible = n_target = 0
        print(f"Reading {year}: {path}", flush=True)
        for chunk in pd.read_csv(
            path, sep=";", encoding="latin-1", usecols=usecols,
            dtype=str, chunksize=200_000, low_memory=False,
        ):
            n_raw += len(chunk)
            chunk = chunk.loc[chunk["nivel_carrera_2"].isin(ELIGIBLE_LEVELS)].copy()
            n_eligible += len(chunk)
            chunk["mrun_num"] = pd.to_numeric(chunk["mrun"], errors="coerce")
            chunk = chunk.loc[chunk["mrun_num"].isin(target_mruns)]
            n_target += len(chunk)
            for row in chunk.itertuples(index=False):
                mrun = int(row.mrun_num)
                rec = annual[(mrun, year)]
                rec["rows"] += 1
                program = clean_code(row.codigo_unico)
                area = normalize_text(row.area_carrera_generica)
                inst = institution_key(row.cod_inst, row.tipo_inst_1, row.nomb_inst)
                if program:
                    rec["program"].add(program)
                if area:
                    rec["area"].add(area)
                if inst:
                    rec["institution"].add(inst)
                high_field = high_premium_field(
                    row.area_carrera_generica, row.cine_f_97_area_area, row.cine_f_97_subarea)
                if high_field is not None:
                    rec["high_field"].add(bool(high_field))
                if inst:
                    # Existing outcome rule: unsupported matriculated institutions are zero.
                    rec["high_inst"].add(inst_effects.get(inst, -np.inf) > HIGH_INST_CUTOFF)
        scan_rows.append({"year": year, "raw_rows": n_raw,
                          "eligible_rows": n_eligible, "target_rows": n_target})
        print(f"Finished {year}: {n_raw:,} rows; {n_target:,} target rows", flush=True)
    return annual, pd.DataFrame(scan_rows)


def any_overlap(left: set, right: set) -> int | None:
    if not left or not right:
        return None
    return int(bool(left & right))


def annual_status(rec: dict | None) -> dict:
    if rec is None:
        return {"enrolled": 0, "program": set(), "area": set(), "institution": set(),
                "high_field": set(), "high_inst": set(), "rows": 0}
    return {"enrolled": 1, **rec}


def classified_binary(values: set, enrolled: int) -> float:
    """Return 1/0 for classified enrollment and NA when enrolled but unknown."""
    if not enrolled:
        return 0.0
    if True in values:
        return 1.0
    if False in values:
        return 0.0
    return np.nan


def construct_outcomes(universe: pd.DataFrame, annual: dict) -> pd.DataFrame:
    rows = []
    for u in universe.itertuples(index=False):
        entry_year = int(u.expected_entry_year)
        base = annual_status(annual.get((u.mrun, entry_year)))
        row = {
            "mrun": u.mrun, "cohort_gr8": u.cohort_gr8,
            "sae_proceso": u.sae_proceso, "timely_sae": u.timely_sae,
            "expected_entry_year": entry_year,
            "he_enrolled_entry": base["enrolled"],
            "entry_enrollment_rows": base["rows"],
            "entry_multiple_enrollments": int(base["rows"] > 1),
            "entry_high_premium_field": classified_binary(base["high_field"], base["enrolled"]),
            "entry_high_premium_institution": int(True in base["high_inst"]) if base["enrolled"] else 0,
        }
        for horizon in (1, 2, 3):
            observed = entry_year + horizon <= 2025
            follow = annual_status(annual.get((u.mrun, entry_year + horizon))) if observed else None
            row[f"followup_y{horizon}_observed"] = int(observed)
            if not observed:
                for name in (
                    "he_enrolled", "persist_from_entry", "entered_and_persist",
                    "same_program", "same_area_generica", "same_institution",
                    "high_premium_field", "high_premium_institution",
                    "remain_high_premium_field", "remain_high_premium_institution",
                ):
                    row[f"{name}_y{horizon}"] = np.nan
                continue
            row[f"he_enrolled_y{horizon}"] = follow["enrolled"]
            row[f"persist_from_entry_y{horizon}"] = follow["enrolled"] if base["enrolled"] else np.nan
            row[f"entered_and_persist_y{horizon}"] = base["enrolled"] * follow["enrolled"]
            for label, key in (("same_program", "program"),
                               ("same_area_generica", "area"),
                               ("same_institution", "institution")):
                match = any_overlap(base[key], follow[key])
                row[f"{label}_y{horizon}"] = (match if base["enrolled"] and follow["enrolled"] else
                                               (0 if base["enrolled"] else np.nan))
            follow_high_field = classified_binary(follow["high_field"], follow["enrolled"])
            follow_high_inst = int(True in follow["high_inst"]) if follow["enrolled"] else 0
            row[f"high_premium_field_y{horizon}"] = follow_high_field
            row[f"high_premium_institution_y{horizon}"] = follow_high_inst
            row[f"remain_high_premium_field_y{horizon}"] = (
                follow_high_field if row["entry_high_premium_field"] == 1 else np.nan)
            row[f"remain_high_premium_institution_y{horizon}"] = (
                follow_high_inst if row["entry_high_premium_institution"] else np.nan)

        # Uninterrupted outcomes are horizon-specific and never pool unequal windows.
        for through in (2, 3):
            observed = bool(row[f"followup_y{through}_observed"])
            names = ["persist", "entered_and_persist", "same_program",
                     "same_area_generica", "same_institution",
                     "remain_high_premium_field", "remain_high_premium_institution"]
            if not observed:
                for name in names:
                    row[f"{name}_continuous_through_y{through}"] = np.nan
                continue
            row[f"persist_continuous_through_y{through}"] = (
                np.prod([row[f"persist_from_entry_y{h}"] for h in range(1, through + 1)])
                if base["enrolled"] else np.nan)
            row[f"entered_and_persist_continuous_through_y{through}"] = (
                row["he_enrolled_entry"] * np.prod(
                    [row[f"he_enrolled_y{h}"] for h in range(1, through + 1)]))
            for label in ("same_program", "same_area_generica", "same_institution"):
                row[f"{label}_continuous_through_y{through}"] = (
                    np.prod([row[f"{label}_y{h}"] for h in range(1, through + 1)])
                    if base["enrolled"] else np.nan)
            for label, entry_label in (("remain_high_premium_field", "entry_high_premium_field"),
                                       ("remain_high_premium_institution", "entry_high_premium_institution")):
                row[f"{label}_continuous_through_y{through}"] = (
                    np.prod([row[f"{label}_y{h}"] for h in range(1, through + 1)])
                    if row[entry_label] == 1 else np.nan)
        rows.append(row)
    return pd.DataFrame(rows)


def make_summary(outcomes: pd.DataFrame) -> pd.DataFrame:
    outcome_cols = [c for c in outcomes if c.startswith((
        "he_enrolled_", "persist_", "entered_and_", "same_", "high_premium_", "remain_high_"))]
    rows = []
    for cohort, group in outcomes.groupby("cohort_gr8"):
        for col in outcome_cols:
            observed = group[col].notna()
            rows.append({"cohort_gr8": cohort, "outcome": col,
                         "n_nonmissing": int(observed.sum()),
                         "mean": group.loc[observed, col].mean() if observed.any() else np.nan})
    return pd.DataFrame(rows)


def main() -> None:
    universe = read_universe()
    print(f"Universe: {len(universe):,} students in cohorts {COHORTS}", flush=True)
    inst_effects = read_institution_effects()
    annual, scan = collect_annual_enrollments(set(universe["mrun"]), inst_effects)
    outcome_path = OUT_CLEAN / "higher_ed_persistence_outcomes.csv"
    summary_parts = []
    check_parts = []
    first_batch = True
    batch_size = 50_000
    for start in range(0, len(universe), batch_size):
        part = construct_outcomes(universe.iloc[start:start + batch_size], annual)
        part.to_csv(outcome_path, mode="w" if first_batch else "a",
                    header=first_batch, index=False)
        first_batch = False
        part_summary = make_summary(part)
        part_summary["sum"] = part_summary["mean"] * part_summary["n_nonmissing"]
        summary_parts.append(part_summary)
        check_parts.append(part.groupby("cohort_gr8").agg(
            students=("mrun", "size"),
            entry_sum=("he_enrolled_entry", "sum"),
            y1_available_sum=("followup_y1_observed", "sum"),
            y2_available_sum=("followup_y2_observed", "sum"),
            y3_available_sum=("followup_y3_observed", "sum"),
        ).reset_index())
        print(f"Wrote outcome rows {start + 1:,}--{min(start + batch_size, len(universe)):,}", flush=True)

    summary = pd.concat(summary_parts, ignore_index=True)
    summary = summary.groupby(["cohort_gr8", "outcome"], as_index=False).agg(
        n_nonmissing=("n_nonmissing", "sum"), total=("sum", "sum"))
    summary["mean"] = summary["total"] / summary["n_nonmissing"]
    summary = summary.drop(columns="total")
    scan.to_csv(OUT_CLEAN / "higher_ed_persistence_scan_diagnostics.csv", index=False)
    summary.to_csv(OUT_TABLE / "higher_ed_persistence_summary.csv", index=False)

    checks = pd.concat(check_parts).groupby("cohort_gr8", as_index=False).sum()
    checks["entry_rate"] = checks["entry_sum"] / checks["students"]
    for h in (1, 2, 3):
        checks[f"y{h}_followup_available"] = checks[f"y{h}_available_sum"] / checks["students"]
    print(checks.to_string(), flush=True)
    print(f"Wrote {outcome_path}", flush=True)


if __name__ == "__main__":
    main()
