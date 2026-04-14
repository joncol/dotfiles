#!/usr/bin/env bash
set -euo pipefail

# Fetches Confluence content. Handles token refresh via the shared jira refresh.sh.
# Usage:
#   fetch.sh search <CQL-query>        Search for pages using CQL
#   fetch.sh page <page-id>            Fetch a page by ID (includes body)
#   fetch.sh spaces                    List all spaces

JIRA_SKILL_DIR="$HOME/.claude/skills/jira"
TOKEN_FILE="$HOME/.claude/jira-tokens.json"

ACTION="${1:?Usage: fetch.sh search <CQL>|page <ID>|spaces}"
shift

# Get a valid access token (shared with jira skill)
ACCESS_TOKEN=$("$JIRA_SKILL_DIR/refresh.sh")

# Get cloud ID
CLOUD_ID=$(python3 -c "import json; print(json.load(open('$TOKEN_FILE'))['cloud_id'])")

BASE_URL="https://api.atlassian.com/ex/confluence/${CLOUD_ID}/wiki/rest/api"

case "$ACTION" in
  search)
    CQL="${1:?Usage: fetch.sh search <CQL-query>}"
    ENCODED_CQL=$(python3 -c "import urllib.parse,sys; print(urllib.parse.quote(sys.argv[1]))" "$CQL")
    curl -s -H "Authorization: Bearer $ACCESS_TOKEN" \
      "${BASE_URL}/content/search?cql=${ENCODED_CQL}&limit=10&expand=space,version"
    ;;
  page)
    PAGE_ID="${1:?Usage: fetch.sh page <page-id>}"
    curl -s -H "Authorization: Bearer $ACCESS_TOKEN" \
      "${BASE_URL}/content/search?cql=id%3D${PAGE_ID}&expand=body.storage,space,version"
    ;;
  spaces)
    curl -s -H "Authorization: Bearer $ACCESS_TOKEN" \
      "${BASE_URL}/content/search?cql=type%3Dpage&limit=50&expand=space" \
      | python3 -c "
import sys, json
data = json.load(sys.stdin)
spaces = {}
for r in data.get('results', []):
    s = r.get('space', {})
    key = s.get('key', '')
    if key and key not in spaces:
        spaces[key] = {'key': key, 'name': s.get('name', ''), 'type': s.get('type', '')}
json.dump({'spaces': sorted(spaces.values(), key=lambda x: x['key'])}, sys.stdout, indent=2)
"
    ;;
  *)
    echo "Unknown action: $ACTION" >&2
    echo "Usage: fetch.sh search <CQL>|page <ID>|spaces" >&2
    exit 1
    ;;
esac
