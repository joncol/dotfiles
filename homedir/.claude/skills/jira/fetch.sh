#!/usr/bin/env bash
set -euo pipefail

# Fetches a Jira issue by key. Handles token refresh internally.
# Usage: fetch.sh <ISSUE-KEY>
# Outputs the JSON response from the Jira REST API.

ISSUE_KEY="${1:?Usage: fetch.sh <ISSUE-KEY>}"
SKILL_DIR="$(cd "$(dirname "$0")" && pwd)"

# Get a valid access token (refresh.sh prints it to stdout, logs to stderr)
ACCESS_TOKEN=$("$SKILL_DIR/refresh.sh")

# Get cloud ID
CLOUD_ID=$(python3 -c "import json; print(json.load(open('$HOME/.claude/jira-tokens.json'))['cloud_id'])")

# Fetch the issue
curl -s -H "Authorization: Bearer $ACCESS_TOKEN" \
  "https://api.atlassian.com/ex/jira/${CLOUD_ID}/rest/api/2/issue/${ISSUE_KEY}?fields=summary,status,assignee,reporter,priority,issuetype,description,comment"
