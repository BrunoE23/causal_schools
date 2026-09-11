# Graduation-record coverage of school staff

2024 staff at the existing VA schools. Unique people within role; groups can overlap.

## Main finding

Named institutions and programs can be recovered for a substantial share of staff. However, an observed postgraduate degree or post-titulo does not recover the original teaching degree. Undergraduate coverage is much stronger for younger staff.

| Staff role | People | Any award by 2024 | Undergraduate by 2024 | Education undergraduate by 2024 |
|---|---:|---:|---:|---:|
| HS teachers | 72,854 | 54,825 (75.3%) | 49,981 (68.6%) | 44,165 (60.6%) |
| Non-HS teachers | 64,604 | 48,152 (74.5%) | 42,489 (65.8%) | 41,628 (64.4%) |
| Orientadores | 1,986 | 1,381 (69.5%) | 722 (36.4%) | 637 (32.1%) |
| Leadership | 7,692 | 4,842 (62.9%) | 2,302 (29.9%) | 2,082 (27.1%) |

Any award includes undergraduate, postgraduate and postitulo records. Education undergraduate means the broad recorded Education field, not a verified teaching license. An undergraduate match need not be the first lifetime degree.

Using all supplied cohorts, including qualifications later than the 2024 staff year, any-award coverage is HS teachers: 76.0%; Non-HS teachers: 75.1%; Orientadores: 70.3%; Leadership: 63.9%.

## Undergraduate matching by age

Age is age attained during 2024, derived from staff birth year. Percentages use all staff in each age band, not only matches.

| Age | HS teachers | Non-HS teachers | Orientadores | Leadership |
|---|---:|---:|---:|---:|
| 18-29 | 95.4% (N=10,633) | 96.7% (N=8,683) | 100.0% (N=33) | 100.0% (N=30) |
| 30-39 | 94.6% (N=29,251) | 94.3% (N=24,461) | 96.4% (N=363) | 94.8% (N=1,086) |
| 40-42 | 80.6% (N=6,333) | 72.0% (N=6,565) | 72.0% (N=168) | 68.2% (N=641) |
| 43-49 | 46.4% (N=9,715) | 39.7% (N=9,841) | 33.1% (N=338) | 31.0% (N=1,421) |
| 50-59 | 20.8% (N=9,492) | 20.6% (N=9,430) | 14.7% (N=483) | 12.8% (N=2,130) |
| 60-69 | 8.0% (N=6,749) | 8.3% (N=5,017) | 5.7% (N=528) | 4.6% (N=1,807) |
| 70+ | 5.7% (N=645) | 4.4% (N=596) | 6.8% (N=73) | 1.4% (N=565) |

## Identifier validation

The match uses exact normalized MRUN only; no fuzzy names or birthdate matching. Among matched people with comparable birth months, the fraction with at least one agreeing record is HS teachers: 99.980%; Non-HS teachers: 99.971%; Orientadores: 100.000%; Leadership: 99.938%.

Some people have both agreeing and disagreeing award records. These disagreements remain flagged rather than silently repaired. Identity-validation tables also compare sex and reported undergraduate title years.

## Cleaning and temporal scope

Cleaned all 19 cohorts, 2007-2025: 4,021,401 source award records, 426 exact parsed-row duplicates removed, and 4,020,975 retained records. 7,369 retained records have invalid/missing identifiers and cannot be linked. The extra 2017 CSV inside the 2018 folder is byte-identical and excluded. Real multiple awards remain.

A dated match requires report cohort and actual award year no later than the staff year. Dates mean by year-end, not by the staff census day. Missing award dates would use report year with an explicit flag; none were found. This is retrospective chronology, not real-time publication availability.

Pre-2007 qualifications, foreign degrees and nonreporting institutions can be missing. A nonmatch is not evidence of no degree. Codebook nonreporting: Universidad Gabriela Mistral in 2009-2010 and CFT INFOMED in 2009-2011. Non-HS teachers here are the earlier VA-school comparison group, not all teachers nationally.

## Saved products and verification

Clean data: `data/clean/titulados_staff_linkage/`. Each `annual/titulados_YEAR.rds` retains all source fields plus normalized IDs, dates, level flags and provenance. `staff_linked_awards.rds` retains all linked qualification records with institution and program names.

`staff_match_2024_overview.csv` is the compact table. Detailed tables cover age, reported title-year cohorts, staff year, unique period members and school-year coverage. Staff-year matches never use qualifications recorded after that year. The prior staff indices are unchanged.

Independent Python checks traced all 209,792 linked awards back to the raw files and verified 4,301,045 person-role-year flags, 511 aggregate cells and 60,327 school-role-years. A separate R audit verifies all 19 complete cleaned files. Raw and prior staff files are unchanged.

Source: MINEDUC/SIES, ER titulados Ed.Superior 2007 - 2025, WEB, pp. 1-6, and the corresponding 19 annual MRUN files; linked to the project's MINEDUC staff directories.
