#!/usr/bin/env bash
set -euo pipefail

# Fetches the create metadata for a project/issue type, showing required fields
# and their allowed values. Useful for discovering custom fields needed to create issues.
#
# Usage: fetch_createmeta.sh <PROJECT_KEY> <ISSUE_TYPE>
# Outputs a human-readable summary of required fields.

SKILL_DIR="$(cd "$(dirname "$0")" && pwd)"

PROJECT_KEY="${1:?Usage: fetch_createmeta.sh <PROJECT_KEY> <ISSUE_TYPE>}"
ISSUE_TYPE="${2:?Usage: fetch_createmeta.sh <PROJECT_KEY> <ISSUE_TYPE>}"

# Get a valid access token
ACCESS_TOKEN=$("$SKILL_DIR/refresh.sh")

# Get cloud ID
CLOUD_ID=$(python3 -c "import json; print(json.load(open('$HOME/.claude/jira-tokens.json'))['cloud_id'])")

# Fetch create metadata
RESPONSE=$(curl -s -H "Authorization: Bearer $ACCESS_TOKEN" \
  "https://api.atlassian.com/ex/jira/${CLOUD_ID}/rest/api/2/issue/createmeta?projectKeys=${PROJECT_KEY}&issuetypeNames=$(python3 -c "import urllib.parse; print(urllib.parse.quote('${ISSUE_TYPE}'))")&expand=projects.issuetypes.fields")

# Parse and display required fields
python3 -c "
import json, sys

data = json.loads(sys.stdin.read())
projects = data.get('projects', [])
if not projects:
    print('Error: No project found for key', file=sys.stderr)
    sys.exit(1)

issuetypes = projects[0].get('issuetypes', [])
if not issuetypes:
    print('Error: No issue type found', file=sys.stderr)
    sys.exit(1)

fields = issuetypes[0].get('fields', {})
print(f'Required fields for {projects[0][\"key\"]} / {issuetypes[0][\"name\"]}:')
print()

for field_id, field_info in sorted(fields.items(), key=lambda x: x[1].get('required', False), reverse=True):
    if not field_info.get('required', False):
        continue
    name = field_info.get('name', field_id)
    allowed = field_info.get('allowedValues', [])
    print(f'  {field_id} ({name}) [required]')
    if allowed:
        for v in allowed:
            val = v.get('value', v.get('name', str(v)))
            print(f'    - {val}')
    print()
" <<< "$RESPONSE"
