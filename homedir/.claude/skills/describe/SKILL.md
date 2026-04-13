---
name: describe
description: Generate a commit message for a jj commit diff and apply it with `jj describe`. Defaults to the working copy (`@`); accepts a jj revision argument to target a specific commit.
disable-model-invocation: true
allowed-tools: Bash(jj:*)
---

Generate a concise commit message for a jj commit and apply it with `jj describe`.

## Arguments

Optionally accepts a single jj revision (revset) as an argument (e.g. `@`, `@-`, a change ID, or any valid revset resolving to one commit). If no argument is given, defaults to `@` (the current working copy).

## Steps

1. Let `REV` be the argument if provided, otherwise `@`.
2. Run `jj diff -r "$REV"` to see the changes in that commit.
3. If there are no changes, inform the user and stop.
4. Analyze the diff and write a commit message following these rules:
   - First line: short summary (under 70 characters), imperative mood (e.g. "Add", "Fix", "Update", not "Added", "Fixed", "Updated")
   - If the change is scoped to a specific module/component, prefix with it (e.g. "kakoune: Add Python LSP support")
   - No period at the end of the first line
   - Only add a body (separated by blank line) if the *why* isn't obvious from the summary
   - Match the style of recent commits visible in `jj log --limit 10`
5. Apply the message with `jj describe -r "$REV" -m '<message>'`.
6. Show the user the applied message.
