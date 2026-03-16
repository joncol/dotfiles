---
name: jira
description: Fetch Jira issue details for the current branch or a given issue key. Only use when working in a project whose path contains "kontrakcja".
argument-hint: "[ISSUE-KEY]"
---

Fetch and display Jira issue details.

## First-time setup

If `~/.claude/jira-tokens.json` does not exist, tell the user to run `~/.claude/skills/jira/auth.sh` to authorize via OAuth, then retry.

## Steps

1. Determine the issue key and fetch the issue in parallel:

   - If `$ARGUMENTS` is provided, use it as the issue key (e.g. `CORE-1234`).
   - Otherwise, extract it from the nearest bookmark by running:
     ```
     jj log -r 'latest(bookmarks() & ancestors(@) & ~ancestors(trunk()))' --no-graph -T 'bookmarks'
     ```
     The bookmark typically has the format `jco/<ISSUE-KEY>-description` (e.g. `jco/CORE-8356-some-description`). Strip the `jco/` prefix and extract the issue key (the `XXXX-1234` part).
   - If no issue key can be determined, ask the user.

   When extracting from a bookmark, run both the `jj log` command and `~/.claude/skills/jira/fetch.sh <ISSUE-KEY>` in a **single bash command** piped together:

   ```bash
   ISSUE_KEY=$(jj log -r 'latest(bookmarks() & ancestors(@) & ~ancestors(trunk()))' --no-graph -T 'bookmarks' | sed 's|.*/||' | grep -oiP '^[A-Z]+-\d+' | tr '[:lower:]' '[:upper:]') && ~/.claude/skills/jira/fetch.sh "$ISSUE_KEY"
   ```

   When the issue key is already known, just run:
   ```bash
   ~/.claude/skills/jira/fetch.sh <ISSUE-KEY>
   ```

   If `fetch.sh` fails with a token error, tell the user to run `~/.claude/skills/jira/auth.sh` to re-authorize.

2. Present the issue in a readable format:

   - **Key**: issue key
   - **Type**: issue type (Bug, Story, Task, etc.)
   - **Status**: current status
   - **Priority**: priority level
   - **Summary**: title
   - **Assignee**: who it's assigned to
   - **Reporter**: who created it
   - **Description**: the issue description (summarize if very long)
   - **Recent comments**: show the last 5-6 comments if any exist (author, date, body). Do NOT use blockquotes (`>`) for comment bodies — they render with low-contrast styling. Use plain text under a bold **Author — Date:** header instead.
