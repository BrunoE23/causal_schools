# Regional persistence of school lottery variation

The user requested the earlier always / sometimes / never school distribution
by region and confirmed that variation means lottery-assignment variation across
years. The earlier tally was not found in saved code, tables, or inspected chat
history. Reconstruct transparently using the paper's 2018-2020 SAE window,
without claiming exact replication of the earlier universe.

Use each RBD appearing in regular-round grade-9 A1 offerings in at least one year,
including zero listed vacancies. Main evidence uses all simulated grade-9
applicants in annual DA_probs files; save a paper-roster-only supplement with the
same school denominator. Sum program probabilities within student-year-RBD
before checking 0 < p < 1 (1e-10 floating-point tolerance). Any such student
establishes variation in that school-year; no k=100 support threshold applies.

Always/sometimes/never is defined over the school's observed offering years.
Pre-rollout years and years without a grade-9 offering are excluded, not zero.
Report observation lengths and a balanced 2019-2020 table, plus a supplement
requiring two observed years. Schools count equally within their region.
Region is school location from its latest 2018-2020 directory record. In
particular, use the latest regional allocation for Nuble. The PAES residence
variable used in the preceding individual regressions is not applicable here.

No math, HE, VA-availability, or later-outcome selection. Missing school region
must fail verification, not silently drop a school. All simulated students must
have probability mass one; all paper-roster students must have simulations.
Audit probability schools outside A1 separately. A zero variation flag means no
observed interior probability in finite simulations, not proof of zero true
assignment risk. Read only small/selected-column source files, preserve raw and
master data, and save reusable school-year and school classifications.

Completed reconstruction: 2,529 RBDs, all matched in the 2020 directory;
2,211 always, 299 sometimes, 19 never (87.43%, 11.82%, 0.75%). Of these,
37 schools have one observed offering year, 831 have two, and 1,661 have three.
Metropolitana has only 2019-2020 observations. The balanced common 2019-2020
window has 2,482 schools: 2,216 always, 225 sometimes, and 41 never.
The paper-roster supplement changes five always schools to sometimes; never
is unchanged. All probability mass, region, unique-key, classification, and
count checks pass. Every probability school is inside its annual A1 roster.
Input hashes remain unchanged. No original classification replication is claimed.
