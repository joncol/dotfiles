---
name: pr-comments
description: Fetch and display GitHub PR review comments for the current branch or a given PR number. Only use when working in a project whose path contains "kontrakcja".
argument-hint: "[PR-NUMBER]"
---

Fetch and display review comments on a GitHub pull request.

## Steps

1. Determine the PR:
   - If `$ARGUMENTS` is provided, use it as the PR number (e.g. `9003`).
   - Otherwise, find the PR from the current bookmark by running:
     ```
     jj log -r 'latest(bookmarks() & ancestors(@) & ~ancestors(trunk()))' --no-graph -T 'bookmarks'
     ```
     Then use the bookmark name with `gh pr view` to resolve the PR.
   - If no PR can be determined, ask the user.

2. Fetch PR metadata and reviews:

   ```bash
   gh pr view <PR> --json number,title,state,url,reviewDecision,reviews,comments
   ```

3. Present the results in a readable format:

   - **PR**: number, title, URL
   - **State**: open/closed/merged
   - **Review decision**: approved, changes requested, etc.
   - **Reviews**: for each review, show:
     - Author
     - State (APPROVED, CHANGES_REQUESTED, COMMENTED, etc.)
     - Date submitted
     - Body (summarize if very long; if the body contains Reviewable.io formatted content with file-level comments, extract and present them clearly with file paths and comment text)
   - **PR comments**: show any issue-level comments (author, date, body)

   Focus on actionable feedback — skip empty review bodies and bot comments. Show the most recent reviews first.
