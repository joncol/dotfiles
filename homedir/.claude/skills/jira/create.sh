#!/usr/bin/env bash
set -euo pipefail

# Creates a Jira issue. Handles token refresh internally.
# Accepts the "fields" object as JSON via stdin.
#
# Usage:
#   echo '{"project":{"key":"FN"},"issuetype":{"name":"Bug"},"summary":"Title"}' | create.sh
#
# The stdin JSON should be the contents of the "fields" object (not wrapped in {"fields": ...}).
# Outputs the JSON response from the Jira REST API.

SKILL_DIR="$(cd "$(dirname "$0")" && pwd)"

# Read fields JSON from stdin
FIELDS_JSON=$(cat)

if [[ -z "$FIELDS_JSON" ]]; then
  echo "Error: No JSON provided on stdin." >&2
  echo "Usage: echo '{\"project\":{\"key\":\"FN\"},\"issuetype\":{\"name\":\"Bug\"},\"summary\":\"Title\"}' | create.sh" >&2
  exit 1
fi

# Get a valid access token (refresh.sh prints it to stdout, logs to stderr)
ACCESS_TOKEN=$("$SKILL_DIR/refresh.sh")

# Get cloud ID
CLOUD_ID=$(python3 -c "import json; print(json.load(open('$HOME/.claude/jira-tokens.json'))['cloud_id'])")

# Wrap in {"fields": ...}
PAYLOAD=$(python3 -c "
import json, sys
fields = json.loads(sys.stdin.read())
print(json.dumps({'fields': fields}))
" <<< "$FIELDS_JSON")

# Create the issue
curl -s -X POST \
  -H "Authorization: Bearer $ACCESS_TOKEN" \
  -H "Content-Type: application/json" \
  -d "$PAYLOAD" \
  "https://api.atlassian.com/ex/jira/${CLOUD_ID}/rest/api/2/issue"
