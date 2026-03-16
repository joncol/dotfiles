---
name: review-pr
description: Review a PR by comparing the Jira ticket requirements against the actual implementation. Only use when working in a project whose path contains "kontrakcja".
argument-hint: "[PR-NUMBER]"
---

Review a pull request by comparing what the Jira ticket requires against what was actually implemented.

## Steps

1. **Fetch the Jira ticket** by invoking the `/jira` skill (no arguments — let it auto-detect from the current bookmark). Read and internalize the ticket summary, description, acceptance criteria, and any relevant comments.

2. **Determine the PR**:
   - If `$ARGUMENTS` is provided, use it as the PR number.
   - Otherwise, find the PR from the current bookmark:
     ```
     jj log -r 'latest(bookmarks() & ancestors(@) & ~ancestors(trunk()))' --no-graph -T 'bookmarks'
     ```
     Then use the bookmark name with `gh pr view` to resolve the PR number.
   - If no PR can be determined, ask the user.

3. **Fetch the PR diff and metadata**:
   ```bash
   gh pr view <PR> --json number,title,body,baseRefName,headRefName,url
   gh pr diff <PR>
   ```

4. **Read the changed files** to understand the full implementation context. Don't rely solely on the diff — read the complete files (or relevant sections) when needed to understand the change in context.

5. **Compare requirements vs implementation** and present a structured review:

   - **Ticket**: key, summary, link
   - **PR**: number, title, link
   - **Requirements coverage**: For each requirement or acceptance criterion from the Jira ticket, state whether it appears to be addressed by the implementation. Use clear indicators:
     - Addressed — the change clearly covers this requirement
     - Partially addressed — some aspects are covered but not all
     - Not addressed — no corresponding change found
     - N/A — requirement doesn't apply to code changes (e.g. "deploy to staging")
   - **Implementation notes**: Anything notable about the implementation that goes beyond or diverges from the ticket (extra refactoring, scope changes, etc.)
   - **Potential concerns**: Any issues spotted during the review — bugs, missing edge cases, inconsistencies with the ticket description, or things that look unintentional.

6. Keep the review concise and actionable. Focus on whether the PR fulfills the ticket, not on style nitpicks.
