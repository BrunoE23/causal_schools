# WORKFLOW

## Standard Execution Flow
1. Clarify objective and success criteria.
2. Inspect current state (files, configs, commands).
3. For substantial tasks, pause after light inspection and present a short execution plan before running heavier work.
4. Implement with minimal, targeted edits once the plan is aligned.
5. Verify with tests or concrete manual checks.
6. Summarize outcomes, residual risks, and next options.

## Data workflow
- Never modify anything that is in the data/raw folder of a project.
- Unless modifying a script that exports data to data/clean, code should not modify existing databases there. Creating new databases on data/clean is fine and encouraged if necessary.  

## Code location
- Place new Codex-written code in `code/codex/` by default.
- Organize new Codex-written work in task-specific folders under `code/codex/`.
- Use descriptive folder and file names that reflect the task.
- Each task folder should include a `README.md` or similar instructions file explaining the goal, key assumptions, inputs, outputs, and how to run the code.
- Do not modify existing project code outside `code/codex/` unless explicitly requested.

## Git Commit Cadence
- Create one commit per meaningful completed unit of work.
- Use clear commit messages that describe outcome, not just file names.
- When Codex creates a commit, append `[codex]` to the end of the message.
- Do not batch unrelated changes into one commit.


## Decision Rules
- Ask questions only when ambiguity changes implementation significantly.
- If ambiguity is low-risk, choose a sensible default and state it.
- Escalate early when a command is blocked by permissions or environment limits.
- For larger research tasks, prefer a staged workflow: light inspection first, then a concise plan, then heavy execution only after user confirmation.

## Coding and Review Rules
- Preserve existing behavior unless change is requested.
- Do not revert unrelated user changes.
- Prefer non-destructive commands.
- Include file references when explaining changes.

## Validation Rules
- Run available tests/checks relevant to changed scope.
- If tooling is missing, provide exact commands to run later.
- For setup tasks, include pass/fail smoke tests.

## Communication Rules
- Provide short progress updates during longer tasks.
- Report what was done, what is pending, and why.
- Be explicit about assumptions and unverified items.
- When a conversation resolves a documentation question or clarifies a project convention, add that information to `setup_md_codex/project_memory/project_documentation.md`.
- When a task uncovers a data, code, or workflow issue that may matter later, record it in `setup_md_codex/project_memory/issues.md`.
