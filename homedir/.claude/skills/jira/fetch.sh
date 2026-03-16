#!/usr/bin/env bash
set -euo pipefail

# Fetches a Jira issue by key. Handles token refresh internally.
# Usage: fetch.sh <ISSUE-KEY>
#        fetch.sh --from-bookmark   (extracts issue key from jj bookmark)
# Outputs the JSON response from the Jira REST API.

SKILL_DIR="$(cd "$(dirname "$0")" && pwd)"

if [[ "${1:-}" == "--from-bookmark" ]]; then
  BOOKMARK=$(jj log -r 'latest(bookmarks() & ancestors(@) & ~ancestors(trunk()))' --no-graph -T 'bookmarks' 2>/dev/null || true)
  if [[ -z "$BOOKMARK" ]]; then
    echo "Error: No bookmark found on current line." >&2
    exit 1
  fi
  ISSUE_KEY=$(echo "$BOOKMARK" | sed 's|.*/||' | grep -oiP '^[A-Z]+-\d+' | tr '[:lower:]' '[:upper:]')
  if [[ -z "$ISSUE_KEY" ]]; then
    echo "Error: Could not extract issue key from bookmark: $BOOKMARK" >&2
    exit 1
  fi
else
  ISSUE_KEY="${1:?Usage: fetch.sh <ISSUE-KEY> or fetch.sh --from-bookmark}"
fi

# Get a valid access token (refresh.sh prints it to stdout, logs to stderr)
ACCESS_TOKEN=$("$SKILL_DIR/refresh.sh")

# Get cloud ID
CLOUD_ID=$(python3 -c "import json; print(json.load(open('$HOME/.claude/jira-tokens.json'))['cloud_id'])")

# Fetch the issue
curl -s -H "Authorization: Bearer $ACCESS_TOKEN" \
  "https://api.atlassian.com/ex/jira/${CLOUD_ID}/rest/api/2/issue/${ISSUE_KEY}?fields=summary,status,assignee,reporter,priority,issuetype,description,comment"
