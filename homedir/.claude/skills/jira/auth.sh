#!/usr/bin/env bash
set -euo pipefail

# OAuth 2.0 (3LO) authorization flow for Atlassian/Jira
# Prerequisites:
#   1. Create an OAuth 2.0 app at https://developer.atlassian.com/console/myapps/
#   2. Set callback URL to http://localhost:21730/callback
#   3. Add Jira API permission with read:jira-work scope
#   4. Set JIRA_OAUTH_CLIENT_ID and JIRA_OAUTH_CLIENT_SECRET env vars

TOKEN_FILE="$HOME/.claude/jira-tokens.json"
CALLBACK_PORT=21730
CALLBACK_URL="http://localhost:${CALLBACK_PORT}/callback"

if [[ -z "${JIRA_OAUTH_CLIENT_ID:-}" || -z "${JIRA_OAUTH_CLIENT_SECRET:-}" ]]; then
  echo "Error: JIRA_OAUTH_CLIENT_ID and JIRA_OAUTH_CLIENT_SECRET must be set." >&2
  echo "Add them to .claude/settings.local.json under env." >&2
  exit 1
fi

# Build authorization URL
AUTH_URL="https://auth.atlassian.com/authorize?audience=api.atlassian.com&client_id=${JIRA_OAUTH_CLIENT_ID}&scope=read%3Ajira-work%20offline_access&redirect_uri=$(python3 -c "import urllib.parse; print(urllib.parse.quote('${CALLBACK_URL}', safe=''))")&response_type=code&prompt=consent"

echo "Opening browser for Atlassian authorization..."
echo "If the browser doesn't open, visit this URL:"
echo "$AUTH_URL"
echo

# Open browser (works on Linux and macOS)
if command -v xdg-open &>/dev/null; then
  xdg-open "$AUTH_URL" 2>/dev/null &
elif command -v open &>/dev/null; then
  open "$AUTH_URL" &
fi

# Start a temporary HTTP server to capture the callback
echo "Waiting for OAuth callback on port ${CALLBACK_PORT}..."

AUTH_CODE=$(python3 -c "
import http.server
import urllib.parse
import sys

class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        parsed = urllib.parse.urlparse(self.path)
        params = urllib.parse.parse_qs(parsed.query)

        if 'code' in params:
            code = params['code'][0]
            self.send_response(200)
            self.send_header('Content-Type', 'text/html')
            self.end_headers()
            self.wfile.write(b'<html><body><h2>Authorization successful!</h2><p>You can close this tab.</p></body></html>')
            print(code, file=sys.stderr)
            raise KeyboardInterrupt
        elif 'error' in params:
            error = params.get('error', ['unknown'])[0]
            desc = params.get('error_description', [''])[0]
            self.send_response(400)
            self.send_header('Content-Type', 'text/html')
            self.end_headers()
            self.wfile.write(f'<html><body><h2>Authorization failed</h2><p>{error}: {desc}</p></body></html>'.encode())
            print(f'ERROR:{error}:{desc}', file=sys.stderr)
            raise KeyboardInterrupt
        else:
            self.send_response(404)
            self.end_headers()

    def log_message(self, format, *args):
        pass  # suppress default logging

server = http.server.HTTPServer(('127.0.0.1', ${CALLBACK_PORT}), Handler)
try:
    server.handle_request()
except KeyboardInterrupt:
    pass
server.server_close()
" 2>&1 1>/dev/null)

if [[ "$AUTH_CODE" == ERROR:* ]]; then
  echo "Authorization failed: $AUTH_CODE" >&2
  exit 1
fi

if [[ -z "$AUTH_CODE" ]]; then
  echo "Error: No authorization code received." >&2
  exit 1
fi

echo "Authorization code received. Exchanging for tokens..."

# Exchange auth code for tokens
TOKEN_RESPONSE=$(curl -s -X POST "https://auth.atlassian.com/oauth/token" \
  -H "Content-Type: application/json" \
  -d "{
    \"grant_type\": \"authorization_code\",
    \"client_id\": \"${JIRA_OAUTH_CLIENT_ID}\",
    \"client_secret\": \"${JIRA_OAUTH_CLIENT_SECRET}\",
    \"code\": \"${AUTH_CODE}\",
    \"redirect_uri\": \"${CALLBACK_URL}\"
  }")

ACCESS_TOKEN=$(echo "$TOKEN_RESPONSE" | python3 -c "import sys,json; print(json.load(sys.stdin)['access_token'])" 2>/dev/null)
REFRESH_TOKEN=$(echo "$TOKEN_RESPONSE" | python3 -c "import sys,json; print(json.load(sys.stdin)['refresh_token'])" 2>/dev/null)
EXPIRES_IN=$(echo "$TOKEN_RESPONSE" | python3 -c "import sys,json; print(json.load(sys.stdin).get('expires_in', 3600))" 2>/dev/null)

if [[ -z "$ACCESS_TOKEN" || -z "$REFRESH_TOKEN" ]]; then
  echo "Error: Failed to exchange authorization code for tokens." >&2
  echo "Response: $TOKEN_RESPONSE" >&2
  exit 1
fi

# Calculate expiry timestamp
EXPIRES_AT=$(python3 -c "import time; print(int(time.time() + ${EXPIRES_IN}))")

echo "Tokens received. Fetching accessible resources..."

# Fetch cloud ID
RESOURCES=$(curl -s -H "Authorization: Bearer ${ACCESS_TOKEN}" \
  "https://api.atlassian.com/oauth/token/accessible-resources")

CLOUD_ID=$(echo "$RESOURCES" | python3 -c "import sys,json; print(json.load(sys.stdin)[0]['id'])" 2>/dev/null)
SITE_NAME=$(echo "$RESOURCES" | python3 -c "import sys,json; print(json.load(sys.stdin)[0]['name'])" 2>/dev/null)

if [[ -z "$CLOUD_ID" ]]; then
  echo "Error: Failed to fetch cloud ID." >&2
  echo "Response: $RESOURCES" >&2
  exit 1
fi

# Save tokens
python3 -c "
import json
data = {
    'access_token': '${ACCESS_TOKEN}',
    'refresh_token': '${REFRESH_TOKEN}',
    'expires_at': ${EXPIRES_AT},
    'cloud_id': '${CLOUD_ID}',
    'site_name': '${SITE_NAME}'
}
with open('${TOKEN_FILE}', 'w') as f:
    json.dump(data, f, indent=2)
"

chmod 600 "$TOKEN_FILE"

echo
echo "OAuth setup complete!"
echo "  Site: ${SITE_NAME}"
echo "  Cloud ID: ${CLOUD_ID}"
echo "  Tokens saved to: ${TOKEN_FILE}"
