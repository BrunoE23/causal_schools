# Kinder capacity accounting

User request: quantify available places and changes within existing schools
versus newly opening schools, motivated by anticipated compulsory kindergarten.

Primary seats measure is declared total program capacity (`cupos_totales`) from
SAE A1 regular-round offerings, summing unique RBD-COD_CURSO rows for
COD_NIVEL=0 / COD_ENSE=10 / COD_GRADO=5. `vacantes` measures availability to
new SAE entrants after continuity/repeaters, and is reported separately; changes
in vacancies are not interpreted as changes in supply. Programs are not physical
classrooms. The count is restricted to SAE-participating regular kindergarten
offerings, rather than all preschool providers or independently certified seats.

Process t admits students for entry year t+1. The Metropolitan Region first
participated in process 2019, including kindergarten. Thus national comparable
capacity starts with SAE 2019 and is observed through 2021 locally. Earlier
transitions use only regions participating in both years, grouping Nuble with
the earlier Biobio boundary. Year-to-year changes are exact accounting:
increases at continuing schools - decreases at continuing schools + capacity
in new positive-capacity listings - capacity of departing listings.

Directories are annual April snapshots available from 2015 through 2025.
Operating means ESTADO_ESTAB=1 and MATRICULA=1; MATRICULA is an indicator.
ENS=10 identifies preschool enrollment without distinguishing kinder/pre-kinder.
No founding date is present. Newly appearing positive-capacity kinder listings
are therefore classified as previously operating schools newly listing kinder,
candidate school openings, or unresolved. Candidate openings first operate in
directory process year t or entry year t+1, with no earlier operating observation
in the available history since 2015. They are not confirmed legal openings:
earlier histories, reactivations, and identifier changes can matter. Existing
schools' new listings may be grade additions, SAE participation, or reporting.
Earlier preschool enrollment and SAE participation are retained as diagnostics.

Only directory years up to the entry year determine opening histories; later
directories supply location and names. All accounting identities and program
uniqueness are checked. No pre-2013 comparison or causal attribution to
expectations of compulsory kindergarten is claimed.

The single national candidate-opening listing in process 2021 is RBD 41859,
Colegio Trigales del Maipo: it first appears and operates in directory April
2021 and first lists SAE kinder capacity (72 seats) for entry 2022. It must not
be described as a school known to open in 2022. Independent reconstruction
directly from the A1 files verifies every national level, incumbent gain/loss,
and entrant/exit contribution for both transitions.

Rollout references:
- https://metropolitana.mineduc.cl/2019/07/22/el-2019-comienza-a-implementarse-el-sistema-de-admision-en-la-region-metropolitana/
- https://metropolitana.mineduc.cl/2020/07/28/mineduc-presenta-novedades-en-la-implementacion-del-sistema-de-admision-escolar-en-el-contexto-covid-19/

## Follow-up: changes greater than 10 and a time series of candidate openings

User requested capacity changes of more than 10 seats and school openings over
time. Define a large intensive change as strictly abs(delta cupos_totales)>10
at an RBD with positive kinder capacity in both adjacent years; changes of
exactly 10 and new/departing offerings are excluded. Report counts and gross
seat changes separately for increases and decreases. National changes are
available for entry 2021 and 2022. A four-change series for entry 2019-2022
restricts every transition to the five regions already covered in process
2017: Tarapaca, Coquimbo, O'Higgins, Los Lagos and Magallanes. It does not
concatenate different rollout geographies into a nominal national trend.

Extend the directory opening series to all RBDs first observed operating with
enrollment, independently of SAE participation. The 2015 snapshot is the
left-censored baseline and has no opening count. Report directory years
2016-2025 using the first operating year's school type, dependency, and
preschool enrollment. Public/subsidized is COD_DEPE!=4; observed codes are
1, 2, 3, 4 and 6 with no missing classifications among first-operation rows.
Preschool is any ENS code=10 and combines kinder with pre-kinder/other
preschool. Report all candidate openings and the subset with preschool.

Distinguish RBDs first recorded in the same year they first operate from those
registered earlier but never previously observed operating since 2015. Do not
call the latter verified reactivations. First operation is not a legal founding
date: pre-baseline histories and RBD changes remain unobserved. In particular,
directory registry growth in inactive/no-enrollment schools must not be
substituted for openings. The broader directory series cannot supply kinder
capacity for every candidate opening and uses directory years rather than SAE
entry years.

Independent read-only recomputation confirms national large-change counts:
2020->2021, 102 expanding schools (+2654 seats) and 111 contracting (-2551);
2021->2022, 46 expanding (+1065) and 113 contracting (-2963). Annual first
observed operating totals for 2016-2025 are 83,35,28,38,42,33,41,35,25,23;
preschool subsets are 26,12,13,20,13,16,24,19,14,12. Figures and tables are
generated by code/codex/kinder_capacity/03_plot_large_changes_and_openings.R.
