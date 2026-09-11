"""Independent source-to-link and coverage verification; no raw/source writes."""
from pathlib import Path
import hashlib
import json
import unicodedata
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[3]
DATA = ROOT / "data/clean/titulados_staff_linkage"
manifest = pd.read_csv(DATA / "graduation_source_manifest.csv")
audit = pd.read_csv(DATA / "graduation_cleaning_audit.csv").set_index("YEAR")
people = pd.read_csv(DATA / "staff_person_role_year.csv.gz", dtype={"MRUN":str,"STAFF_BIRTH_YM":str})
links = pd.read_csv(DATA / "staff_linked_awards.csv.gz", dtype={"MRUN":str,"BIRTH_YM":str,"INSTITUTION_ID":str})
panel = pd.read_csv(DATA / "staff_match_person_role_year.csv.gz", dtype={"MRUN":str})
assert not people.duplicated(["MRUN","ROLE","AGNO"]).any()
assert not links.RECORD_ID.duplicated().any()
assert set(manifest.loc[manifest.INCLUDED,"YEAR"]) == set(range(2007,2026))
ids = set(people.MRUN)

def normalize_id(series):
    s = series.astype("string").str.strip()
    valid = s.str.fullmatch(r"[0-9]+").fillna(False) & s.str.contains("[1-9]",regex=True).fillna(False)
    return s.str.lstrip("0").where(valid)

def normtext(s):
    return " ".join("".join(c for c in unicodedata.normalize("NFD",s) if unicodedata.category(c)!="Mn").upper().split())

source_link_rows = 0
all_raw_ids = set()
for record in manifest.itertuples():
    if not record.INCLUDED:
        same = manifest[manifest.INCLUDED & manifest.YEAR.eq(record.YEAR)].iloc[0]
        assert record.MD5 == same.MD5
        continue
    path = Path(record.PATH)
    assert path.stat().st_size == record.BYTES
    # Hash streaming avoids loading the largest source as a byte buffer.
    md5 = hashlib.md5()
    with path.open("rb") as stream:
        for block in iter(lambda:stream.read(1024*1024),b""):
            md5.update(block)
    assert md5.hexdigest() == record.MD5
    chunks, n = [], 0
    for chunk in pd.read_csv(path,sep=";",dtype=str,encoding="utf-8",keep_default_na=False,chunksize=50000):
        original = list(chunk.columns)
        # Match the declared fread parser: trim surrounding ASCII whitespace
        # and treat its default NA token as missing, without changing content.
        for col in original:
            chunk[col] = chunk[col].str.strip(" \t").replace("NA", "")
        chunk["SOURCE_ROW"] = np.arange(n+1,n+len(chunk)+1)
        n += len(chunk)
        key = normalize_id(chunk.mrun)
        all_raw_ids.update(key.dropna())
        selected = chunk[key.isin(ids)].copy()
        selected["MRUN"] = key.loc[selected.index]
        chunks.append(selected)
    raw = pd.concat(chunks,ignore_index=True).drop_duplicates(original,keep="first")
    assert n == audit.loc[record.YEAR,"N_INPUT"] == audit.loc[record.YEAR,"N_CODEBOOK"]
    raw["RECORD_ID"] = str(record.YEAR)+":"+raw.SOURCE_ROW.astype(str)
    raw = raw.set_index("RECORD_ID").sort_index()
    actual = links[links.REPORT_YEAR.eq(record.YEAR)].set_index("RECORD_ID").sort_index()
    assert set(raw.index) == set(actual.index), f"Raw-to-clean link support differs in {record.YEAR}"
    actual = actual.reindex(raw.index)
    assert (raw.MRUN == actual.MRUN).all()
    assert (raw.nomb_inst == actual.NOMB_INST.fillna("")).all()
    mismatch = raw.nomb_carrera != actual.NOMB_CARRERA.fillna("")
    assert not mismatch.any(), list(zip(raw.loc[mismatch,"nomb_carrera"].head(3),actual.loc[mismatch,"NOMB_CARRERA"].head(3)))
    assert (raw.nombre_titulo == actual.NOMBRE_TITULO.fillna("")).all()
    parsed = pd.to_datetime(raw.fecha_obtencion_titulo,format="%Y%m%d",errors="coerce")
    parsed = parsed.where(raw.fecha_obtencion_titulo.str.fullmatch(r"[0-9]{8}") & raw.fecha_obtencion_titulo.ne("19000101"))
    np.testing.assert_array_equal(parsed.dt.strftime("%Y-%m-%d").fillna(""),actual.AWARD_DATE.fillna(""))
    expected_asof = np.maximum(record.YEAR,parsed.dt.year.fillna(record.YEAR))
    np.testing.assert_array_equal(expected_asof,actual.ASOF_YEAR)
    level = raw.nivel_global.map(normtext).map({"PREGRADO":"undergraduate","POSGRADO":"postgraduate","POSTGRADO":"postgraduate","POSTITULO":"postitulo"})
    assert (level==actual.LEVEL).all()
    np.testing.assert_array_equal(level.eq("undergraduate"),actual.IS_UNDERGRAD)
    np.testing.assert_array_equal(level.eq("postgraduate"),actual.IS_POSTGRAD)
    np.testing.assert_array_equal(level.eq("postitulo"),actual.IS_POSTITULO)
    np.testing.assert_array_equal(raw.area_conocimiento.map(normtext).eq("EDUCACION"),actual.IS_EDUCATION)
    yob=pd.to_numeric(raw.fec_nac_alu.str[:4],errors="coerce")
    month=pd.to_numeric(raw.fec_nac_alu.str[4:6],errors="coerce")
    birth_ok=raw.fec_nac_alu.str.fullmatch(r"[0-9]{6}([0-9]{2})?") & yob.gt(1900) & yob.le(2025) & month.between(1,12)
    np.testing.assert_array_equal(raw.fec_nac_alu.str[:6].where(birth_ok,"").to_numpy(),actual.BIRTH_YM.fillna("").to_numpy())
    sex=pd.to_numeric(raw.gen_alu,errors="coerce").where(raw.gen_alu.isin(["1","2"]))
    np.testing.assert_allclose(sex,actual.SEX,equal_nan=True)
    source_link_rows += len(raw)
    print(f"Verified cohort {record.YEAR}: {n:,} source records; {len(raw):,} linked records.",flush=True)

assert set(links.MRUN) == ids.intersection(all_raw_ids)
criteria = {
    "MATCH_ANY_BY_STAFF_YEAR":np.ones(len(links),dtype=bool),
    "MATCH_UNDERGRAD_BY_STAFF_YEAR":links.IS_UNDERGRAD,
    "MATCH_EDUCATION_UNDERGRAD_BY_STAFF_YEAR":links.IS_EDUCATION_UNDERGRAD,
    "MATCH_POSTGRAD_BY_STAFF_YEAR":links.IS_POSTGRAD,
    "MATCH_POSTITULO_BY_STAFF_YEAR":links.IS_POSTITULO,
    "MATCH_NAMED_INSTITUTION_BY_STAFF_YEAR":links.INSTITUTION_NAMED,
}
expected = people[["MRUN","ROLE","AGNO"]].copy()
expected["MATCH_ANY_2007_2025"] = expected.MRUN.isin(set(links.MRUN))
for flag,mask in criteria.items():
    earliest = links[mask].groupby("MRUN").ASOF_YEAR.min()
    expected[flag] = expected.MRUN.map(earliest).le(expected.AGNO)
keys=["MRUN","ROLE","AGNO"]
expected=expected.set_index(keys).sort_index()
actual=panel.set_index(keys).reindex(expected.index)
flags=list(expected.columns)
np.testing.assert_array_equal(expected,actual[flags])

checks=0
for filename,restriction,groups in [
    ("staff_match_annual_summary.csv",panel,["ROLE","AGNO"]),
    ("staff_match_2024_summary.csv",panel[panel.AGNO.eq(2024)],["ROLE","AGNO"]),
    ("staff_match_2024_by_age.csv",panel[panel.AGNO.eq(2024)],["ROLE","AGE_BAND"]),
    ("staff_match_2024_by_reported_title_year.csv",panel[panel.AGNO.eq(2024)],["ROLE","TITLE_COHORT"]),
    ("staff_match_unique_period_people.csv",panel.sort_values("AGNO").drop_duplicates(["ROLE","MRUN"],keep="last"),["ROLE"]),
]:
    saved=pd.read_csv(DATA/filename).set_index(groups+["METRIC"])
    for flag in flags:
        x=restriction.groupby(groups)[flag].agg(["size","sum","mean"])
        got=saved.xs(flag,level="METRIC").reindex(x.index)
        np.testing.assert_array_equal(x["size"],got.N_PEOPLE)
        np.testing.assert_array_equal(x["sum"],got.N_MATCHED)
        np.testing.assert_allclose(x["mean"],got.SHARE_MATCHED,atol=1e-14)
        checks+=len(x)
members=pd.read_csv(DATA/"staff_membership_person_school_year.csv.gz",dtype={"MRUN":str})
assert not members.duplicated(["MRUN","ROLE","RBD","AGNO"]).any()
assert set(map(tuple,members[keys].drop_duplicates().to_numpy())) == set(expected.index)
school=members.merge(expected.reset_index(),on=keys,validate="many_to_one").groupby(["ROLE","RBD","AGNO"])
school_saved=pd.read_csv(DATA/"staff_match_school_year.csv.gz").set_index(["ROLE","RBD","AGNO"])
np.testing.assert_array_equal(school.size(),school_saved.reindex(school.size().index).N_STAFF)
np.testing.assert_allclose(school[flags].mean(),school_saved.reindex(school.size().index)[flags],atol=1e-14)
identity=people[people.AGNO.eq(2024)].merge(links,on="MRUN",how="inner",validate="many_to_many")
identity=identity[identity.ASOF_YEAR.le(identity.AGNO)].copy()
identity["DOB_COMPARABLE"]=identity.STAFF_BIRTH_YM.notna() & identity.BIRTH_YM.notna()
identity["DOB_AGREE"]=identity.DOB_COMPARABLE & identity.STAFF_BIRTH_YM.eq(identity.BIRTH_YM)
identity["DOB_DISAGREE"]=identity.DOB_COMPARABLE & ~identity.DOB_AGREE
identity["SEX_COMPARABLE"]=identity.STAFF_SEX.notna() & identity.SEX.notna()
identity["SEX_AGREE"]=identity.SEX_COMPARABLE & identity.STAFF_SEX.eq(identity.SEX)
identity["BOTH_AGREE"]=identity.DOB_AGREE & identity.SEX_AGREE
person_identity=identity.groupby(["ROLE","MRUN"]).agg(N_AWARDS_BY2024=("RECORD_ID","size"),
    N_DOB_COMPARABLE=("DOB_COMPARABLE","sum"),N_DOB_AGREE=("DOB_AGREE","sum"),N_DOB_DISAGREE=("DOB_DISAGREE","sum"),
    N_SEX_COMPARABLE=("SEX_COMPARABLE","sum"),N_SEX_AGREE=("SEX_AGREE","sum"),ANY_MATCH_WITH_BIRTH_AND_SEX=("BOTH_AGREE","max"))
saved_identity=pd.read_csv(DATA/"staff_match_2024_identity_validation.csv.gz",dtype={"MRUN":str}).set_index(["ROLE","MRUN"])
np.testing.assert_array_equal(person_identity.to_numpy(),saved_identity.reindex(person_identity.index)[person_identity.columns].to_numpy())
clean_check=pd.read_csv(DATA/"clean_file_verification.csv").set_index("YEAR")
np.testing.assert_array_equal(clean_check.reindex(audit.index).N_CLEAN,audit.N_CLEAN)
for row in pd.read_csv(DATA/"preserved_staff_sources.csv").itertuples():
    assert hashlib.md5(Path(row.PATH).read_bytes()).hexdigest() == row.MD5
for row in pd.read_csv(DATA/"graduation_clean_manifest.csv").itertuples():
    assert hashlib.md5(Path(row.PATH).read_bytes()).hexdigest() == row.MD5
summary={"status":"passed","cohorts_checked":19,"source_records_checked":int(audit.N_INPUT.sum()),
    "linked_award_records_traced_to_source":source_link_rows,"person_role_year_flags_checked":len(expected)*len(flags),
    "summary_cells_checked":checks,"school_role_years_checked":len(school_saved),
    "matched_2024_person_role_identity_checks":len(person_identity),"raw_and_prior_staff_sources_unchanged":True}
summary["report_input_sha256"]={name:hashlib.sha256((DATA/name).read_bytes()).hexdigest() for name in [
    "staff_match_2024_summary.csv","staff_match_2024_by_age.csv","staff_match_2024_by_reported_title_year.csv",
    "staff_match_annual_summary.csv","staff_match_unique_period_people.csv","staff_match_2024_identity_summary.csv",
    "graduation_cleaning_audit.csv","clean_file_verification.csv"]}
(DATA/"independent_verification.json").write_text(json.dumps(summary,indent=2),encoding="utf-8")
print(json.dumps(summary,indent=2))
