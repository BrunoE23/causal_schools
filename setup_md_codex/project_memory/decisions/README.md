# Decision Log

This folder stores durable decisions and their rationale.

Use this layer for questions where future readers may ask why a choice was made, what alternatives were considered, or whether an issue has already been resolved.

## Relationship to `issues.md`

- `issues.md` tracks live problems, risks, and follow-ups.
- `decisions/` records resolved choices and the reasoning behind them.
- When an issue is resolved, add or update a decision file and link the issue to it.
- If a decision later changes, do not delete the old file. Mark it as superseded and link to the new decision.

## Suggested File Structure

```markdown
# Decision: [title]

**Date:** YYYY-MM-DD
**Status:** active | superseded | under review

## Context

What situation prompted this decision?

## Options Considered

What alternatives were available?

## Decision

What was chosen?

## Rationale

Why this choice?

## Implementation

How should this decision be applied?

## Risks and Follow-Up

What remains uncertain?
```
