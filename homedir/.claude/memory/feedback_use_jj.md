---
name: Use jj instead of git
description: Always use jj (Jujutsu) commands instead of git when running version control operations
type: feedback
---

Use `jj` (Jujutsu) instead of `git` for all version control operations.

**Why:** The user uses Jujutsu as their VCS. Running git commands works but is inconsistent with their workflow.

**How to apply:** When you need to run VCS commands (status, diff, log, commit, etc.), use `jj` equivalents:
- `git status` → `jj status`
- `git diff` → `jj diff`
- `git log` → `jj log`
- `git commit` → `jj commit` or `jj describe` + `jj new`
- `git diff HEAD~1` → `jj diff -r @-`
- `git log --oneline -N` → `jj log -n N`
