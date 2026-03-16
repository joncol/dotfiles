---
name: jira
description: Fetch Jira issue details for the current branch or a given issue key. Only use when working in a project whose path contains "kontrakcja".
argument-hint: "[ISSUE-KEY]"
---

Fetch and display Jira issue details.

## First-time setup

If `~/.claude/jira-tokens.json` does not exist, tell the user to run `~/.claude/skills/jira/auth.sh` to authorize via OAuth, then retry.

## Steps

1. Determine the issue key:
   - If `$ARGUMENTS` is provided, use it as the issue key (e.g. `CORE-1234`).
   - Otherwise, extract it from the nearest bookmark on the current line by running:
     ```
     jj log -r 'latest(bookmarks() & ancestors(@) & ~ancestors(trunk()))' --no-graph -T 'bookmarks'
     ```
     The bookmark typically has the format `jco/<ISSUE-KEY>-description` (e.g. `jco/CORE-8356-some-description`). Strip the `jco/` prefix and extract the issue key (the `XXXX-1234` part).
   - If no issue key can be determined, ask the user.

2. Get a valid access token and cloud ID:

   ```bash
   ACCESS_TOKEN=$(~/.claude/skills/jira/refresh.sh)
   CLOUD_ID=$(python3 -c "import json; print(json.load(open('$HOME/.claude/jira-tokens.json'))['cloud_id'])")
   ```

   If `refresh.sh` fails, tell the user to run `~/.claude/skills/jira/auth.sh` to re-authorize.

3. Fetch the issue from the Jira REST API using curl:

   ```bash
   curl -s -H "Authorization: Bearer $ACCESS_TOKEN" \
     "https://api.atlassian.com/ex/jira/${CLOUD_ID}/rest/api/2/issue/<ISSUE-KEY>?fields=summary,status,assignee,reporter,priority,issuetype,description,comment"
   ```

4. Present the issue in a readable format:

   - **Key**: issue key
   - **Type**: issue type (Bug, Story, Task, etc.)
   - **Status**: current status
   - **Priority**: priority level
   - **Summary**: title
   - **Assignee**: who it's assigned to
   - **Reporter**: who created it
   - **Description**: the issue description (summarize if very long)
   - **Recent comments**: show the last 5-6 comments if any exist (author, date, body)
