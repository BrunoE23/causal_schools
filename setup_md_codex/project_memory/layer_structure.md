# Layer Structure

This file describes a portable layered memory structure for agent sessions. This repository implements it inside `setup_md_codex/project_memory/`.

The organizing principle is access pattern: keep always-relevant facts short, put rationale in decision records, track live problems separately, and use session notes only for recent work.

## Layer 1: Curated Project Facts

Files:
- `project_overview.md`
- `project_documentation.md`

Use these for facts that should be true at the start of most sessions:
- current research framing,
- stable data conventions,
- timing conventions,
- settled project-specific definitions.

Keep these files compact. If a section starts carrying lots of rationale or history, move that detail into `decisions/` and leave a short pointer.

## Layer 2: Open Issues

File:
- `issues.md`

Use this for live problems, unresolved risks, and follow-ups:
- data quirks that still need checking,
- code or workflow hazards,
- issues where the final rule is not yet settled.

An issue can stay open even after a task-level workaround exists. When an issue is resolved, update its status and link to the decision record that explains the resolution.

## Layer 3: Decision Log

Folder:
- `decisions/`

Use this for resolved choices and their rationale:
- why a data-handling rule was chosen,
- why one research framing replaced another,
- what alternatives were considered and rejected,
- what remains risky after the decision.

Every solved issue should either:
- link to an existing decision file, or
- be promoted into a new decision file.

Do not delete old decision files. If a decision changes, mark the old one as superseded and link to the replacement.

## Layer 4: Session Ledger

Optional future file:
- `session_log.md`

Use this for chronological session summaries:
- what was changed,
- what was checked,
- what remains open,
- what the next session should do first.

This file does not exist yet. Create it when session-to-session continuity starts needing more than `issues.md` and `decisions/`.

## Practical Rule for Future Sessions

At the start of a substantial task:
1. Read `project_overview.md` for the current project framing.
2. Read `project_documentation.md` for stable conventions.
3. Check `issues.md` for live risks related to the task.
4. If a live issue references a decision, read that decision before changing the convention.

At the end of a substantial task:
1. Add newly discovered unresolved problems to `issues.md`.
2. If a problem was resolved or a durable choice was made, add a file under `decisions/`.
3. Keep `project_documentation.md` short by recording only the current rule plus a pointer to the decision.
