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
- Store reusable derived data objects, analysis CSVs, and diagnostic CSVs in the project's clean data area, usually `data/clean/`.
- Store exploratory or analysis figures in the repository output figures area, usually `output/figures/<task_name>/`.
- Store paper-ready tables, especially `.tex` tables meant for manuscripts, in the repository output tables area, usually `output/tables/<task_name>/`.
- Do not put paper `.tex` tables in `data/clean/`. Do not put ordinary analysis CSVs in `output/tables/` unless they are temporary companions to paper table generation.

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
- When code introduces or changes a substantive research normalization, scaling rule, sample restriction, support definition, estimand definition, or regression specification, document it in the appropriate project-memory file before finalizing. Use `empirical_methods.md` and/or a decision-log entry for empirical-design choices; do not leave the reasoning only in code comments or task READMEs.

## Validation Rules
- Run available tests/checks relevant to changed scope.
- If tooling is missing, provide exact commands to run later.
- For setup tasks, include pass/fail smoke tests.

## Communication Rules
- Provide short progress updates during longer tasks.
- Report what was done, what is pending, and why.
- Be explicit about assumptions and unverified items.
- When Bruno says he wants to explain, discuss, "tell you what's up", or is still deciding the target, pause and listen. Do not run code, inspect files, or make edits unless explicitly asked. If the next action is ambiguous, first restate the understood goal and wait for confirmation.
- When a conversation resolves a documentation question or clarifies a project convention, add that information to `setup_md_codex/project_memory/project_documentation.md`.
- When a task uncovers a data, code, or workflow issue that may matter later, record it in `setup_md_codex/project_memory/issues.md`.
