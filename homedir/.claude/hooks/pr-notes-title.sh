#!/usr/bin/env bash
# UserPromptSubmit hook: for a /pr-notes prompt, set the session title to
# "<KEY> PR notes", where KEY is the Jira key (for example CORE-1234).
# Key lookup follows the pr-notes skill: PR number argument, else the
# nearest jj bookmark. Prints nothing (no title change) if no key is found.
set -euo pipefail

input=$(cat)
prompt=$(jq -r '.prompt // ""' <<<"$input")
cwd=$(jq -r '.cwd // ""' <<<"$input")

[[ $prompt =~ ^/pr-notes([[:space:]]|$) ]] || exit 0
[[ -n $cwd ]] && cd "$cwd"

extract_key() { grep -oiE '\b[a-z]+-[0-9]+\b' | head -n1 | tr '[:lower:]' '[:upper:]'; }

key=""
pr=$(grep -oE '(^|[[:space:]])[0-9]+([[:space:]]|$)' <<<"${prompt#/pr-notes}" | tr -d '[:space:]' | head -n1 || true)

if [[ -n $pr ]]; then
  key=$(gh pr view "$pr" --json title,headRefName --jq '.title + " " + .headRefName' 2>/dev/null | extract_key || true)
  label="PR #$pr"
else
  bookmark=$(jj log --ignore-working-copy -r 'latest(bookmarks() & ancestors(@) & ~ancestors(trunk()))' \
    --no-graph -T 'bookmarks' 2>/dev/null | tr -d '*' | awk '{print $1}' || true)
  [[ -n $bookmark ]] || exit 0
  # Bookmarks follow jco/<key>-description, so try the name before calling gh.
  key=$(sed 's|^[^/]*/||' <<<"$bookmark" | extract_key || true)
  if [[ -z $key ]]; then
    key=$(gh pr view "$bookmark" --json title --jq .title 2>/dev/null | extract_key || true)
  fi
  label=$bookmark
fi

title="${key:-$label} PR notes"
jq -n --arg t "$title" '{hookSpecificOutput: {hookEventName: "UserPromptSubmit", sessionTitle: $t}}'
