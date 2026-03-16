#!/usr/bin/env bash
set -euo pipefail

# Refreshes the Jira OAuth access token if expired.
# Outputs the current valid access token to stdout.

TOKEN_FILE="$HOME/.claude/jira-tokens.json"

if [[ ! -f "$TOKEN_FILE" ]]; then
  echo "Error: No tokens found. Run auth.sh first." >&2
  exit 1
fi

if [[ -z "${JIRA_OAUTH_CLIENT_ID:-}" || -z "${JIRA_OAUTH_CLIENT_SECRET:-}" ]]; then
  echo "Error: JIRA_OAUTH_CLIENT_ID and JIRA_OAUTH_CLIENT_SECRET must be set." >&2
  exit 1
fi

# Read current tokens
TOKENS=$(cat "$TOKEN_FILE")
ACCESS_TOKEN=$(echo "$TOKENS" | python3 -c "import sys,json; print(json.load(sys.stdin)['access_token'])")
REFRESH_TOKEN=$(echo "$TOKENS" | python3 -c "import sys,json; print(json.load(sys.stdin)['refresh_token'])")
EXPIRES_AT=$(echo "$TOKENS" | python3 -c "import sys,json; print(json.load(sys.stdin)['expires_at'])")
CLOUD_ID=$(echo "$TOKENS" | python3 -c "import sys,json; print(json.load(sys.stdin)['cloud_id'])")
SITE_NAME=$(echo "$TOKENS" | python3 -c "import sys,json; print(json.load(sys.stdin).get('site_name', ''))")

NOW=$(python3 -c "import time; print(int(time.time()))")

# Refresh if expired (with 60s buffer)
if (( NOW >= EXPIRES_AT - 60 )); then
  TOKEN_RESPONSE=$(curl -s -X POST "https://auth.atlassian.com/oauth/token" \
    -H "Content-Type: application/json" \
    -d "{
      \"grant_type\": \"refresh_token\",
      \"client_id\": \"${JIRA_OAUTH_CLIENT_ID}\",
      \"client_secret\": \"${JIRA_OAUTH_CLIENT_SECRET}\",
      \"refresh_token\": \"${REFRESH_TOKEN}\"
    }")

  NEW_ACCESS=$(echo "$TOKEN_RESPONSE" | python3 -c "import sys,json; print(json.load(sys.stdin)['access_token'])" 2>/dev/null)
  NEW_REFRESH=$(echo "$TOKEN_RESPONSE" | python3 -c "import sys,json; print(json.load(sys.stdin)['refresh_token'])" 2>/dev/null)
  NEW_EXPIRES_IN=$(echo "$TOKEN_RESPONSE" | python3 -c "import sys,json; print(json.load(sys.stdin).get('expires_in', 3600))" 2>/dev/null)

  if [[ -z "$NEW_ACCESS" || -z "$NEW_REFRESH" ]]; then
    echo "Error: Token refresh failed. Run auth.sh to re-authorize." >&2
    echo "Response: $TOKEN_RESPONSE" >&2
    exit 1
  fi

  NEW_EXPIRES_AT=$(python3 -c "import time; print(int(time.time() + ${NEW_EXPIRES_IN}))")

  # Update token file (rotating refresh token)
  python3 -c "
import json
data = {
    'access_token': '${NEW_ACCESS}',
    'refresh_token': '${NEW_REFRESH}',
    'expires_at': ${NEW_EXPIRES_AT},
    'cloud_id': '${CLOUD_ID}',
    'site_name': '${SITE_NAME}'
}
with open('${TOKEN_FILE}', 'w') as f:
    json.dump(data, f, indent=2)
"
  chmod 600 "$TOKEN_FILE"
  ACCESS_TOKEN="$NEW_ACCESS"
  echo "Token refreshed." >&2
fi

echo "$ACCESS_TOKEN"
