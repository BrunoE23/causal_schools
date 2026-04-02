# WORKFLOW

## Standard Execution Flow
1. Clarify objective and success criteria.
2. Inspect current state (files, configs, commands).
3. Plan implementation steps.
4. Implement with minimal, targeted edits.
5. Verify with tests or concrete manual checks.
6. Summarize outcomes, residual risks, and next options.

## Data workflow
- Never modify anything that is in the data/raw folder of a project.
- Unless modifying a script that exports data to data/clean, code should not modify existing databases there. Creating new databases on data/clean is fine and encouraged if necessary.  

## Git Commit Cadence
- Create one commit per meaningful completed unit of work.
- Use clear commit messages that describe outcome, not just file names.
- Do not batch unrelated changes into one commit.


## Decision Rules
- Ask questions only when ambiguity changes implementation significantly.
- If ambiguity is low-risk, choose a sensible default and state it.
- Escalate early when a command is blocked by permissions or environment limits.

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

