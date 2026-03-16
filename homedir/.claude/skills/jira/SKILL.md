---
name: jira
description: Fetch Jira issue details for the current branch or a given issue key. Only use when working in a project whose path contains "kontrakcja".
argument-hint: "[ISSUE-KEY]"
---

Fetch and display Jira issue details.

## First-time setup

If `~/.claude/jira-tokens.json` does not exist, tell the user to run `~/.claude/skills/jira/auth.sh` to authorize via OAuth, then retry.

## Steps

1. Fetch the issue JSON using `fetch.sh` (it handles token refresh, cloud ID, and the API call internally):

   - If `$ARGUMENTS` is provided, use it as the issue key:
     ```bash
     ~/.claude/skills/jira/fetch.sh CORE-1234
     ```
   - Otherwise, use `--from-bookmark` to auto-detect the issue key from the current jj bookmark:
     ```bash
     ~/.claude/skills/jira/fetch.sh --from-bookmark
     ```
   - If `fetch.sh` fails with a bookmark error, ask the user for the issue key.
   - If it fails with a token error, tell the user to run `~/.claude/skills/jira/auth.sh` to re-authorize.

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
