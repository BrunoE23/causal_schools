# AGENTS

This file defines how Codex should work in this repository.

## Source of Truth
- Personal defaults: `PROFILE.md`
- Process defaults: `WORKFLOW.md`
- Project context: `project_overview.md`
- Project memory structure: `project_memory/layer_structure.md`
- Repository-specific constraints in this file override generic defaults when needed.

## Default Intent
- Optimize for reliable, maintainable progress.
- Favor reproducibility, clear reasoning, and verification.

## Operating Instructions
- Follow the workflow in `WORKFLOW.md`.
- For substantial tasks, use the memory structure in `project_memory/layer_structure.md`: stable facts in `project_documentation.md`, live issues in `issues.md`, and resolved rationale in `decisions/`.
- Keep edits small, scoped, and reversible.
- Prefer ASCII unless the file already requires Unicode.
- Use explicit paths and commands in explanations.
- Record assumptions when context is incomplete.

## Setup and Execution
- Use the repository's existing toolchain and conventions.
- Do not assume OS-specific paths unless documented in this repository.
- If setup docs exist, follow them before introducing new setup steps.

## Validation Expectations
- For code changes: run targeted checks/tests when available.
- For setup/config changes: run a minimal smoke test (open project, run one safe command, validate expected behavior).
- If a check cannot run, state the blocker and provide a manual verification path.

## Safety and Boundaries
- Do not expose secrets from local auth/config files.
- Avoid destructive commands unless explicitly requested.
- Do not overwrite unrelated changes in dirty worktrees.

## Optional Repository Overrides
- Primary shell: `<fill if needed>`
- Key test command(s): `<fill if needed>`
- Key lint/format command(s): `<fill if needed>`
- Paths or directories with special rules: `<fill if needed>`
