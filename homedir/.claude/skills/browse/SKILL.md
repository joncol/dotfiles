---
name: browse
description: Open the current PR, Jira issue, or Reviewable page in the browser. Only use when working in a project whose path contains "kontrakcja".
argument-hint: "<pr|jira|reviewable>"
---

Open a relevant page in the browser for the current branch.

## Steps

1. Determine the bookmark:
   ```
   jj log -r 'latest(bookmarks() & ancestors(@) & ~ancestors(trunk()))' --no-graph -T 'bookmarks'
   ```
   The bookmark typically has the format `jco/<ISSUE-KEY>-description`.

2. Determine what to open based on `$ARGUMENTS` (case-insensitive). If no argument is given, ask the user which page to open.

   - **`pr`**: Run `gh pr view <bookmark> --json url -q .url` to get the PR URL and open it.
   - **`jira`**: Extract the issue key from the bookmark (the `XXXX-1234` part after `jco/`). The URL is `https://scriveab.atlassian.net/browse/<ISSUE-KEY>`.
   - **`reviewable`**: Run `gh pr view <bookmark> --json number -q .number` to get the PR number. The URL is `https://reviewable.io/reviews/scrive/kontrakcja/<PR-NUMBER>`.

3. Open the URL with `xdg-open <URL>`.
