"""Read-only verification of the R exports and an aggregate subject-review document."""
from pathlib import Path
import hashlib
import json
import unicodedata
import pandas as pd
import numpy as np

ROOT = Path(__file__).resolve().parents[3]
BASE = ROOT / "data/clean/titulados_staff_linkage"
OUT = BASE / "credentials"
REPORT = ROOT / "output/reports/staff_credential_specialization_review.md"
CORE = ["UG_HIGH_PREMIUM", "ANY_POST_UG", "POST_UG_HIGH_PREMIUM", "ANY_HIGH_PREMIUM",
        "ROLE_SPECIFIC_QUALIFICATION", "ANY_MAGISTER", "MAGISTER_HIGH_PREMIUM", "ROLE_SPECIFIC_MAGISTER"]
ROLE_COL = {"HS teachers": "TEACHER_RELEVANT", "Non-HS teachers": "TEACHER_RELEVANT",
            "Orientadores": "ORIENTADOR_RELEVANT", "Leadership": "LEADERSHIP_RELEVANT"}
FIELDS = ["NOMB_CARRERA", "NOMBRE_TITULO", "NOMBRE_GRADO", "AREA_CONOCIMIENTO"]

def norm(value):
    if pd.isna(value):
        return ""
    return " ".join("".join(c for c in unicodedata.normalize("NFD", str(value))
                           if not unicodedata.combining(c)).upper().split())

def table(df):
    def cell(v):
        if pd.isna(v):
            return ""
        return str(v).replace("|", "/").replace("\n", " ").replace("\r", " ")
    rows = ["| " + " | ".join(map(cell, df.columns)) + " |",
            "| " + " | ".join(["---"] * len(df.columns)) + " |"]
    rows += ["| " + " | ".join(map(cell, row)) + " |" for row in df.itertuples(index=False, name=None)]
    return "\n".join(rows)

# Read only the needed columns, not repeated age/birth/title histories.
panel = pd.read_csv(OUT / "staff_credentials_person_role_year.csv.gz",
                    usecols=["MRUN", "ROLE", "AGNO"] + CORE, dtype={"MRUN": str})
assert not panel.duplicated(["MRUN", "ROLE", "AGNO"]).any()
assert panel[CORE].isin([0, 1]).all().all()
people = pd.read_csv(BASE / "staff_person_role_year.csv.gz", usecols=["MRUN", "ROLE", "AGNO"], dtype={"MRUN": str})
assert len(panel) == len(people)
assert len(panel.merge(people, on=["MRUN", "ROLE", "AGNO"], validate="one_to_one")) == len(people)
aw = pd.read_csv(OUT / "credential_award_evidence.csv.gz", dtype={"MRUN": str, "INSTITUTION_ID": str})
assert not aw.RECORD_ID.duplicated().any()
# Independently reconstruct the six non-subject award conditions from raw level
# and institution inputs, rather than trusting the R binary columns.
fe = pd.read_csv(ROOT / "output/tables/mifuturo_matricula_income/mifuturo_income_fe_institution_effects.csv")
fe = fe[fe.effect_type.eq("institution")].copy()
fe["INSTITUTION_ID"] = fe.level.str.split(" || ", regex=False).str[0]
assert not fe.INSTITUTION_ID.duplicated().any()
high_ids = set(fe.loc[fe.effect_log_clp_centered.gt(.1), "INSTITUTION_ID"])
is_high = aw.INSTITUTION_ID.isin(high_ids)
is_master = aw.NIVEL_CARRERA_1.map(norm).eq("MAGISTER")
is_post = aw.LEVEL.isin(["postgraduate", "postitulo"])
expected_aw = {"UG_HIGH_PREMIUM": is_high & aw.LEVEL.eq("undergraduate"),
               "ANY_POST_UG": is_post, "POST_UG_HIGH_PREMIUM": is_high & is_post,
               "ANY_HIGH_PREMIUM": is_high, "ANY_MAGISTER": is_master,
               "MAGISTER_HIGH_PREMIUM": is_high & is_master}
for col, values in expected_aw.items():
    assert np.array_equal(values.astype(int), aw[col])
checked = 0
for (role, year), part in panel.groupby(["ROLE", "AGNO"]):
    eligible = aw.ASOF_YEAR.le(year)
    relevance = aw[ROLE_COL[role]].eq(1)
    flags = {**expected_aw, "ROLE_SPECIFIC_QUALIFICATION": relevance,
             "ROLE_SPECIFIC_MAGISTER": relevance & is_master}
    for col, values in flags.items():
        ids = set(aw.loc[eligible & values, "MRUN"])
        assert np.array_equal(part.MRUN.isin(ids).astype(int), part[col])
        checked += len(part)

current = panel[panel.AGNO.eq(2024)]
snapshot = pd.read_csv(OUT / "staff_credentials_2024.csv.gz", usecols=list(panel.columns), dtype={"MRUN": str})
pd.testing.assert_frame_equal(current.sort_values(["MRUN", "ROLE"]).reset_index(drop=True),
                              snapshot[list(panel.columns)].sort_values(["MRUN", "ROLE"]).reset_index(drop=True))
matched = current[["MRUN", "ROLE"]].merge(aw[aw.ASOF_YEAR.le(2024)], on="MRUN", how="inner")
matched["PROGRAM_LABEL"] = matched.NOMB_CARRERA.map(norm)
parts = []
for role, col in ROLE_COL.items():
    r = matched[matched.ROLE.eq(role)].copy()
    r["INCLUDED"] = r[col]
    r["ROLE_SPECIFIC_MASTER_AWARD"] = r[col] * r.ANY_MAGISTER
    rules = col.replace("_RELEVANT", "_RULE")
    key = ["ROLE"] + FIELDS + ["LEVEL", "NIVEL_CARRERA_1", "INCLUDED", rules,
          "ORIENTADOR_PROGRAM_ONLY", "LEADERSHIP_PROGRAM_ONLY",
          "ORIENTADOR_MENTION_CONFLICT", "LEADERSHIP_MENTION_CONFLICT", "SUBJECT_OVERRIDE_REASON"]
    group = r.groupby(key, dropna=False).agg(N_PEOPLE=("MRUN", "nunique"), N_AWARD_RECORDS=("RECORD_ID", "size")).reset_index()
    group = group.rename(columns={rules: "MATCH_RULE"})
    parts.append(group)
review = pd.concat(parts, ignore_index=True).sort_values(["ROLE", "N_PEOPLE"], ascending=[True, False])
review.to_csv(OUT / "role_specific_review_2024.csv", index=False, encoding="utf-8-sig")

summary_rows = []
for role in ROLE_COL:
    r = current[current.ROLE.eq(role)]
    summary_rows.append([role, len(r), int(r.ROLE_SPECIFIC_QUALIFICATION.sum()), int(r.ROLE_SPECIFIC_MAGISTER.sum())])
lines = ["# Staff credentials: role-specific classification review", "",
         "Status: computed, rule-based first specification for review. Population: 2024 staff at the existing VA schools. "
         "The full output additionally covers 2018–2024 for HS teachers, orientadores and leadership; non-HS comparison teachers are available only in 2024.", "",
         "## What is being counted", "",
         "An indicator is one if at least one observed qualification meets its condition by the staff year. "
         "The reporting year and actual award year must both be no later than that year. No later qualification changes an earlier observation. "
         "A role-specific master's must itself be relevant: a general master's plus a relevant diploma is insufficient. "
         "Undergraduate qualifications can satisfy the all-level specialization indicator, but not the master's indicator.", "",
         "All zeros mean no qualifying award observed, not proven lifetime absence. Records begin in 2007. "
         "This is a measure of observed relevant training, not a validated measure of staff effectiveness.", "",
         table(pd.DataFrame(summary_rows, columns=["Role", "Staff", "Any role-specific qualification", "Role-specific magíster"])), "",
         "Counts below are distinct people within each program and role. People can have multiple programs and roles, so rows must not be summed as distinct people.", "",
         "## Current rules and review choices", "",
         "### Teachers (HS and non-HS)", "",
         "Include qualifications in the SIES Education area, or with explicit education/teaching, curriculum, didactics, psychopedagogy or neuroeducation subjects in the program, title or degree. "
         "A generic teaching degree or Magíster en Educación counts. No teacher-subject assignment match is attempted. "
         "This broad rule also includes education management, early-childhood, special education and higher-education teaching. "
         "Review choice: narrow these if the intended measure is additional classroom-specific training or HS-specific expertise rather than any education-related training.", "",
         "### Orientadores", "",
         "Include explicit educational/vocational/professional/family orientation, corresponding counseling, psychoeducation, "
         "and family mediation/advising. Related title variants such as Orientación, Familia y Educación and Orientación en Relaciones Humanas y Familia qualify. "
         "Psychoeducational inclusion programs count under the stated psychoeducation domain. "
         "Psychology, psychopedagogy, family science, family law, generic education or school management alone do not qualify. "
         "Review choices: whether to include these adjacent fields, and whether family mediation and autism-focused psychoeducational training are too broad.", "",
         "### Leadership", "",
         "Require management, administration, direction, leadership, directiva or gerencia together with an education/school/teaching context in the same program, title or degree field. "
         "Generic MBAs and business management alone do not qualify. Educational management, curriculum management and school-climate management do. "
         "Review choice: whether to admit general management qualifications or exclude narrowly pedagogical/convivencia management.", "",
         "### Alternative mentions and program-only evidence", "",
         "If the program lists a relevant mention but the title/degree explicitly names a different mention and neither award field supports the role, exclude the match. "
         "These cases have a MENTION_CONFLICT flag. If the title/degree is generic or missing, a relevant program name still qualifies but is marked PROGRAM_ONLY. "
         "Program-only matches are priority review cases: a program name listing several tracks does not establish the exact track completed. "
         "These flags preserve uncertainty rather than claiming manual validation of every qualification.", "",
         "## How to modify the definition", "",
         "The complete review CSV retains program, awarded title, awarded degree, area, qualification level, inclusion decision, rule and review flags. "
         "It includes excluded qualifications as well as included ones. The full historical subject mapping covers all linked records, including later awards that cannot enter earlier indicators.", "",
         f"- [Full 2024 review table]({(OUT / 'role_specific_review_2024.csv').as_posix()})",
         f"- [Full historical subject mapping]({(OUT / 'credential_subject_mapping.csv').as_posix()})",
         f"- [Editable override file]({(ROOT / 'code/codex/titulados_staff_linkage/credential_subject_overrides.csv').as_posix()})", "",
         "To override an individual classification, copy its four source fields (program/title/degree/area) into the override file, "
         "set TEACHER_OVERRIDE, ORIENTADOR_OVERRIDE or LEADERSHIP_OVERRIDE to 0 or 1, and record a reason. "
         "Leave an override cell blank to preserve that role's default. Missing source fields must stay blank; they are not wildcards. "
         "Exact matching ignores case, accents and punctuation. Unmatched or duplicate override keys stop the rebuild. "
         "Broader definition changes belong in the subject rules and require rerunning the build and this document. "
         "The override file starts empty: the baseline is fully rule-based.", "",
         "## High-premium institution caveat", "",
         "High premium reuses the existing MiFuturo two-way-model institution effect: centered log-income premium strictly greater than 0.1. "
         "This is a fixed classification applied to all staff years, not an institution ranking estimated at their graduation date and not a postgraduate-specific return estimate. "
         "Institution codes link awards to the existing model; no fuzzy name matching. Institutions absent from the model receive observed zero, consistent with the existing student outcome, "
         "and separate uncovered-institution flags identify this limitation. It must not be interpreted as proof of low quality.", "",
         "## Programs included for each role", "",
         "The following tables list every included normalized program name held by the 2024 staff in that role. "
         "Counts include only qualifying title/degree variants. For differing mentions under the same program name, consult the complete review CSV. "
         "Programs observed only in other staff years are in the historical mapping.", ""]
for role, col in ROLE_COL.items():
    r = matched[matched.ROLE.eq(role)]
    yes = r[r[col].eq(1)]
    counts = yes.groupby("PROGRAM_LABEL").MRUN.nunique().rename("People")
    masters = yes[yes.ANY_MAGISTER.eq(1)].groupby("PROGRAM_LABEL").MRUN.nunique().rename("With qualifying magíster")
    display = pd.concat([counts, masters], axis=1).fillna(0).astype(int).reset_index()
    display = display.sort_values(["People", "PROGRAM_LABEL"], ascending=[False, True]).rename(columns={"PROGRAM_LABEL": "Program"})
    lines += [f"### {role}", "", table(display), ""]
    if role in ["Orientadores", "Leadership"]:
        prefix = "ORIENTADOR" if role == "Orientadores" else "LEADERSHIP"
        flagcols = [prefix + "_PROGRAM_ONLY", prefix + "_MENTION_CONFLICT"]
        flags = r[r[flagcols].eq(1).any(axis=1)]
        detail = flags.groupby(FIELDS + [col] + flagcols, dropna=False).MRUN.nunique().reset_index(name="People")
        detail = detail.sort_values("People", ascending=False)
        lines += [f"#### Priority review: {role}", "",
                  "All program-only and conflicting-mention signatures in this role are listed below; flags describe the baseline rule before any override.", "",
                  table(detail.rename(columns={col: "Included", flagcols[0]: "Program only", flagcols[1]: "Different awarded mention"})), ""]
lines += ["## Verification", "",
          f"The Python check independently rebuilt the six non-subject award conditions from level and institution inputs and checked all eight indicators across {checked:,} person-role-year cells. "
          "Role-specific aggregation was independently checked against the exported subject decisions. "
          "Synthetic tests cover the subject boundaries, different awarded mentions, exact overrides, missing institution coverage, the strict premium cutoff, "
          "future awards, role overlap, unmatched people and the same-award master's requirement. "
          "This validates implementation of the stated rules, not the substantive validity of the subject taxonomy.", ""]
REPORT.parent.mkdir(parents=True, exist_ok=True)
REPORT.write_text("\n".join(lines), encoding="utf-8")
verification = {"person_role_years": len(panel), "unique_people": panel.MRUN.nunique(),
                "current_role_memberships": len(current), "current_unique_people": current.MRUN.nunique(),
                "binary_cells_checked": checked, "source_conditions_verified": 6,
                "subject_aggregation_verified": True, "snapshot_verified": True,
                "review_signatures_2024": len(review),
                "output_hashes": {p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in
                    [OUT / "staff_credentials_person_role_year.csv.gz", OUT / "staff_credentials_2024.csv.gz", REPORT]}}
(OUT / "credential_independent_verification.json").write_text(json.dumps(verification, indent=2), encoding="utf-8")
print(json.dumps(verification, indent=2))
print(f"Review document: {REPORT}")
