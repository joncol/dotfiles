---
name: jira
description: Fetch or create Jira issues. Only use when working in a project whose path contains "kontrakcja".
argument-hint: "[ISSUE-KEY] or [create PROJECT-KEY]"
---

Fetch, display, or create Jira issues.

## First-time setup

If `~/.claude/jira-tokens.json` does not exist, tell the user to run `~/.claude/skills/jira/auth.sh` to authorize via OAuth, then retry.

## Detecting the mode

- If `$ARGUMENTS` starts with `create` (case-insensitive), enter **Create mode**.
- Otherwise, enter **Fetch mode**.

---

## Fetch mode

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

---

## Create mode

Create a new Jira issue. Parse `$ARGUMENTS` for the project key (the word after `create`). If no project key is given, ask the user.

1. Gather required information. The user may provide these inline or you may need to ask:
   - **Project key** (e.g. `FN`, `CORE`) — required
   - **Issue type** (e.g. `Bug`, `Story`, `Task`, `Sub-task`) — required, default to `Bug` if the context suggests a bug report
   - **Summary** (title) — required
   - **Description** — optional but recommended; compose one from conversation context if available. Use Jira wiki markup (e.g. `h3.` for headings, `{{code}}` for inline code, `{code}` blocks for multi-line code).
   - **Notes for QA** (`customfield_10115`) — always include this field. Write numbered steps (using `#` for ordered lists in Jira wiki markup) that a QA engineer can follow to verify the fix. This is a separate field, not part of the description.
   - **Do NOT set the assignee** — leave it unassigned.

2. If the issue type is **Bug**, you must include the `customfield_11966` (Bug Classification) field. Choose the most appropriate value:
   - `UI Defect` — visual/layout issues
   - `Software defect` — general code bugs
   - `Regression Bug` — something that previously worked but is now broken
   - `Pre-Release Defect` — found before release
   - `Security Issue` — security-related bugs
   - `Defect in Prod` — bugs found in production

3. Create the issue by piping a JSON fields object to `create.sh`:
   ```bash
   echo '<JSON>' | ~/.claude/skills/jira/create.sh
   ```
   The JSON should be the fields object (NOT wrapped in `{"fields": ...}`). Example:
   ```bash
   echo '{"project":{"key":"FN"},"issuetype":{"name":"Bug"},"summary":"Title","description":"Description text","customfield_11966":{"value":"Regression Bug"},"customfield_10115":"# Step one\n# Step two"}' | ~/.claude/skills/jira/create.sh
   ```

4. Parse the JSON response and present the result:
   - On success, show the new issue **key**, **URL** (`https://scriveab.atlassian.net/browse/<KEY>`), and **summary**.
   - On error, show the error message from the API response and suggest corrective action. If the error mentions a required field, fetch the field's allowed values using:
     ```bash
     ~/.claude/skills/jira/fetch_createmeta.sh PROJECT_KEY "Issue Type"
     ```
     Then retry with the missing field included.
