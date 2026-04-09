---
name: describe
description: Generate a commit message for the current jj working copy diff and apply it with `jj describe`
disable-model-invocation: true
allowed-tools: Bash(jj:*)
---

Generate a concise commit message for the current jj working copy and apply it with `jj describe`.

## Steps

1. Run `jj diff` to see the current changes in the working copy.
2. If there are no changes, inform the user and stop.
3. Analyze the diff and write a commit message following these rules:
   - First line: short summary (under 70 characters), imperative mood (e.g. "Add", "Fix", "Update", not "Added", "Fixed", "Updated")
   - If the change is scoped to a specific module/component, prefix with it (e.g. "kakoune: Add Python LSP support")
   - No period at the end of the first line
   - Only add a body (separated by blank line) if the *why* isn't obvious from the summary
   - Match the style of recent commits visible in `jj log --limit 10`
4. Apply the message with `jj describe -m '<message>'`.
5. Show the user the applied message.
