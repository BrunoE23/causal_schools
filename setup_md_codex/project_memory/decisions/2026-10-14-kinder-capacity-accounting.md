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
