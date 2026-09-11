"""Write a compact aggregate table and reader-facing report from verified results."""
from pathlib import Path
import hashlib
import json
import pandas as pd

ROOT=Path(__file__).resolve().parents[3]
DATA=ROOT/"data/clean/titulados_staff_linkage"
qa=json.loads((DATA/"independent_verification.json").read_text())
assert qa["status"]=="passed"
for name,digest in qa["report_input_sha256"].items():
    assert hashlib.sha256((DATA/name).read_bytes()).hexdigest()==digest
summary=pd.read_csv(DATA/"staff_match_2024_summary.csv")
quality=pd.read_csv(DATA/"staff_match_2024_identity_summary.csv").set_index("ROLE")
audit=pd.read_csv(DATA/"graduation_cleaning_audit.csv")
age=pd.read_csv(DATA/"staff_match_2024_by_age.csv")
roles=["HS teachers","Non-HS teachers","Orientadores","Leadership"]
rows=[]
for role in roles:
    s=summary[summary.ROLE.eq(role)].set_index("METRIC")
    rows.append(dict(ROLE=role,N_STAFF=int(s.N_PEOPLE.iloc[0]),
        N_ANY_AWARD_BY2024=int(s.loc["MATCH_ANY_BY_STAFF_YEAR","N_MATCHED"]),
        PCT_ANY_AWARD_BY2024=100*s.loc["MATCH_ANY_BY_STAFF_YEAR","SHARE_MATCHED"],
        N_UNDERGRAD_BY2024=int(s.loc["MATCH_UNDERGRAD_BY_STAFF_YEAR","N_MATCHED"]),
        PCT_UNDERGRAD_BY2024=100*s.loc["MATCH_UNDERGRAD_BY_STAFF_YEAR","SHARE_MATCHED"],
        N_EDUCATION_UNDERGRAD_BY2024=int(s.loc["MATCH_EDUCATION_UNDERGRAD_BY_STAFF_YEAR","N_MATCHED"]),
        PCT_EDUCATION_UNDERGRAD_BY2024=100*s.loc["MATCH_EDUCATION_UNDERGRAD_BY_STAFF_YEAR","SHARE_MATCHED"],
        PCT_ANY_AWARD_ALL_FILES=100*s.loc["MATCH_ANY_2007_2025","SHARE_MATCHED"]))
overview=pd.DataFrame(rows)
overview.to_csv(DATA/"staff_match_2024_overview.csv",index=False)
readback=pd.read_csv(DATA/"staff_match_2024_overview.csv")
pd.testing.assert_frame_equal(overview,readback,check_exact=False,rtol=1e-12)
lines=["# Graduation-record coverage of school staff","",
    "2024 staff at the existing VA schools. Unique people within role; groups can overlap.","",
    "## Main finding","",
    "Named institutions and programs can be recovered for a substantial share of staff. "
    "However, an observed postgraduate degree or post-titulo does not recover the original teaching degree. "
    "Undergraduate coverage is much stronger for younger staff.","",
    "| Staff role | People | Any award by 2024 | Undergraduate by 2024 | Education undergraduate by 2024 |",
    "|---|---:|---:|---:|---:|"]
for row in overview.itertuples():
    lines.append(f"| {row.ROLE} | {row.N_STAFF:,} | {row.N_ANY_AWARD_BY2024:,} ({row.PCT_ANY_AWARD_BY2024:.1f}%) | "
        f"{row.N_UNDERGRAD_BY2024:,} ({row.PCT_UNDERGRAD_BY2024:.1f}%) | "
        f"{row.N_EDUCATION_UNDERGRAD_BY2024:,} ({row.PCT_EDUCATION_UNDERGRAD_BY2024:.1f}%) |")
lines += ["","Any award includes undergraduate, postgraduate and postitulo records. "
    "Education undergraduate means the broad recorded Education field, not a verified teaching license. "
    "An undergraduate match need not be the first lifetime degree.","",
    "Using all supplied cohorts, including qualifications later than the 2024 staff year, any-award coverage is "
    + "; ".join(f"{row.ROLE}: {row.PCT_ANY_AWARD_ALL_FILES:.1f}%" for row in overview.itertuples())+".","",
    "## Undergraduate matching by age","",
    "Age is age attained during 2024, derived from staff birth year. Percentages use all staff in each age band, not only matches.","",
    "| Age | HS teachers | Non-HS teachers | Orientadores | Leadership |","|---|---:|---:|---:|---:|"]
for band in ["18-29","30-39","40-42","43-49","50-59","60-69","70+"]:
    cells=[]
    for role in roles:
        x=age[age.ROLE.eq(role)&age.AGE_BAND.eq(band)&age.METRIC.eq("MATCH_UNDERGRAD_BY_STAFF_YEAR")].iloc[0]
        cells.append(f"{100*x.SHARE_MATCHED:.1f}% (N={x.N_PEOPLE:,})")
    lines.append("| "+" | ".join([band]+cells)+" |")
lines += ["","## Identifier validation","",
    "The match uses exact normalized MRUN only; no fuzzy names or birthdate matching. "
    "Among matched people with comparable birth months, the fraction with at least one agreeing record is "
    + "; ".join(f"{role}: {100*quality.loc[role,'SHARE_DOB_AGREE_WHEN_COMPARABLE']:.3f}%" for role in roles)+".","",
    "Some people have both agreeing and disagreeing award records. These disagreements remain flagged rather "
    "than silently repaired. Identity-validation tables also compare sex and reported undergraduate title years.","",
    "## Cleaning and temporal scope","",
    f"Cleaned all 19 cohorts, 2007-2025: {audit.N_INPUT.sum():,} source award records, "
    f"{audit.N_EXACT_DUPLICATES_REMOVED.sum():,} exact parsed-row duplicates removed, and {audit.N_CLEAN.sum():,} retained records. "
    f"{audit.N_INVALID_MRUN.sum():,} retained records have invalid/missing identifiers and cannot be linked. "
    "The extra 2017 CSV inside the 2018 folder is byte-identical and excluded. Real multiple awards remain.","",
    "A dated match requires report cohort and actual award year no later than the staff year. "
    "Dates mean by year-end, not by the staff census day. Missing award dates would use report year "
    "with an explicit flag; none were found. This is retrospective chronology, not real-time publication availability.","",
    "Pre-2007 qualifications, foreign degrees and nonreporting institutions can be missing. A nonmatch is "
    "not evidence of no degree. Codebook nonreporting: Universidad Gabriela Mistral in 2009-2010 and CFT INFOMED "
    "in 2009-2011. Non-HS teachers here are the earlier VA-school comparison group, not all teachers nationally.","",
    "## Saved products and verification","",
    "Clean data: `data/clean/titulados_staff_linkage/`. Each `annual/titulados_YEAR.rds` retains all source "
    "fields plus normalized IDs, dates, level flags and provenance. `staff_linked_awards.rds` retains all "
    "linked qualification records with institution and program names.","",
    "`staff_match_2024_overview.csv` is the compact table. Detailed tables cover age, reported title-year "
    "cohorts, staff year, unique period members and school-year coverage. Staff-year matches never "
    "use qualifications recorded after that year. The prior staff indices are unchanged.","",
    f"Independent Python checks traced all {qa['linked_award_records_traced_to_source']:,} linked awards "
    f"back to the raw files and verified {qa['person_role_year_flags_checked']:,} person-role-year flags, "
    f"{qa['summary_cells_checked']:,} aggregate cells and {qa['school_role_years_checked']:,} school-role-years. "
    "A separate R audit verifies all 19 complete cleaned files. Raw and prior staff files are unchanged.","",
    "Source: MINEDUC/SIES, ER titulados Ed.Superior 2007 - 2025, WEB, pp. 1-6, "
    "and the corresponding 19 annual MRUN files; linked to the project's MINEDUC staff directories.",""]
report=ROOT/"output/reports/titulados_staff_match_report.md"
report.parent.mkdir(parents=True,exist_ok=True)
report.write_text("\n".join(lines),encoding="utf-8")
print(overview.to_string(index=False))
print(f"Saved report: {report}")
