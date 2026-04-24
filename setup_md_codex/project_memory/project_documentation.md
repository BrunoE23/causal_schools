# Project Documentation

## `br_code`

`br_code` is a course-level school identifier, not just a school identifier. In practice, it behaves like the combination of school and specific track/course/process information, so multiple `br_code`s can sit inside the same `rbd`.

Operationally:
- `rbd` is the school the student applied to.
- `br_code` is the more granular school-course-process unit inside that school.
- In `sample_students`, students are stacked by application unit, so the same student can appear multiple times for the same `rbd` if that school has multiple relevant `br_code`s.

This matters for construction work. For the school-level first-stage exercise, first-round offers were merged at the `br_code` level and then collapsed back to one `mrun`-`proceso`-`rbd` row.

## Timing Convention in Tracking Databases

In the current tracking databases, relative time is defined as:

`year_rel_sae_change = AGNO - sae_proceso - 1`

So if a student applies in SAE process `2017`, they are assigned for the `2018` school year. Under this convention:

- `2018` is `t_0`
- `2019` is `t_1`
- `2020` is `t_2`
- `2021` is `t_3`

This means:

- "1 year after SAE" corresponds to `t_0`
- "4 years after SAE" corresponds to `t_3`

This convention may change later, but for now it is the convention used in the tracking databases.

## Current Research Priority

The current project priority is to measure causal effects of schools on higher-education field choice and causal effects of schools on academic scores as two central outcome objects.

Separating those effects into achievement and non-achievement channels is now a secondary goal rather than the main framing of the project. That decomposition may still matter later, but it should not be treated as the primary summary of the research agenda in project descriptions.

The current version of the project is reduced-form rather than structural. Reusable summaries should not describe the project as containing an active structural component unless that changes again later.

## Writing Narrative From Overleaf Drafts

The current writing drafts in `writing/overleaf/` emphasize a few narrative points that should carry over into future project summaries:

- The project is about school effects on higher-education outcomes at unusually fine granularity, not just on college attendance in the abstract.
- The distinctive contribution of the setting is the coexistence of a centralized school assignment system and a centralized higher-education application system in Chile.
- This combination makes it possible to observe program-institution application portfolios, exam-taking choices, field choice, enrollment, and later program switches.
- A recurring motivation in the drafts is that broad outcomes such as college enrollment or number of semesters are too coarse to capture how schools shape later choices.
- The paper frames major choice, especially STEM and related field composition, as a substantively important outcome in its own right.

The older December 2025 draft still uses a "beyond achievement" headline. The more recent materials keep that mechanism question alive, but the current project priority is better summarized as estimating causal school effects on scores and higher-education field choice first, with mechanism decomposition treated as a later extension.

## Scale and Design Facts Used in the Narrative

- The paper narrative focuses on oversubscribed high schools identified through the Chilean centralized assignment system.
- The assignment-probability exercise is treated as a practical cornerstone of identification, not just a technical appendix item.
- Avoid relying on provisional result counts, coverage numbers, or draft-specific estimates in reusable project summaries unless those details are explicitly needed for the task at hand.
