---
name: pr-notes
description: Save GitHub PR review comments to an editable Markdown notes file, so the user can add a note per comment ("done", "dismissed", "look into this next") and Claude can read the notes back. Use when the user wants to track or give feedback on PR review comments in a file. Only use when working in a project whose path contains "kontrakcja".
argument-hint: "[PR-NUMBER] [all] | read"
---

Keep a numbered notes file of PR review comments. The user edits the file, and Claude reads it back.

The notes file is `~/.claude/pr-notes/<key>.md`, where `<key>` is the Jira issue key in lowercase (for example, `core-1234.md`). It persists across sessions.

To find the key, look for a pattern like `CORE-1234` in the PR title (`[CORE-1234] …`). If the title has no key, look in the branch name (`headRefName`, for example `jco/CORE-1234-some-description`). If neither has a key, use the PR number (`<PR>.md`).

## Arguments

- A number: use it as the PR number.
- `all`: include all review comments, not only the comments after the cutoff.
- `read`: do not fetch. Go to "Read the notes" below.
- No arguments: find the PR from the bookmark, and create or update the file.

## Find the PR

1. If no PR number is given, run:
   ```
   jj log -r 'latest(bookmarks() & ancestors(@) & ~ancestors(trunk()))' --no-graph -T 'bookmarks'
   ```
   Remove a trailing `*` from the bookmark name. Then resolve the PR with `gh pr view <bookmark> --json number`.
2. If no PR can be found, ask the user.

## Fetch the comments

The raw output is often more than 100 KB. Always filter it with `jq`, and save it to the scratchpad first:

```bash
gh pr view <PR> --json number,title,headRefName,state,url,reviewDecision,reviews,comments > <scratchpad>/pr.json
```

1. Merge `.reviews[]` (date in `submittedAt`) and `.comments[]` (date in `createdAt`) into one list. Drop the items with an empty body. Sort by date.
2. Find the cutoff: the date of the newest item whose author is the user (`gh api user --jq .login`). Skip this step if the argument is `all`.
3. Keep the items after the cutoff. Drop bot comments.
4. Print the bodies. Remove `<!-- Sent from Reviewable.io -->` and the `([raw file](...))` links.

Reviewable bodies contain one block per file comment. Each block starts with `*[\`<path>\` line <N> at r<rev>](...)`. Make one item per block. If a block quotes an earlier comment (`Previously, X wrote…`), it is a reply. Show only the new text, and give one line of context.

## Write the notes file

Use this format. Do not use tables, because tables are hard to edit.

```markdown
# PR #<number> — <title>

URL: <url>
Fetched: <date>. Comments after: <cutoff date, or "all">.

Write your notes after each `Note:`. For example: "Done", "Dismissed", "Look into this next", "Reply: …".

## <reviewer> — <STATE> (<date>)

<general review text, summarized in one or two lines, if it contains a request>

1. `<path>:<line>`
   <the request, in one or two sentences>
   Note:

2. ...
```

- Number the items in one sequence for the whole file.
- Give the full path from the repository root.
- If an item needs no action (for example, a remark about unrelated code), say so in the item text. Leave the `Note:` line empty.

### If the file exists

1. Read the file first.
2. Do not change existing items or their `Note:` lines.
3. Add only the comments that are not in the file. Add them under a new reviewer heading, and continue the numbers.
4. Update the `Fetched:` line.

## Tell the user

After you write the file:

1. Give the full path of the file.
2. Give a short summary: the number of items for each reviewer, and the review decision.
3. Tell the user to edit the `Note:` lines and to say when they are done. They can also give short notes in the chat, for example "3, 5 done, 18 dismissed".

## Read the notes

When the user says that the notes are updated, or the argument is `read`:

1. Find the file. Use the key from the current bookmark (see "Find the PR"). If you cannot find a key, list `~/.claude/pr-notes/` and ask the user.
2. Read the whole file. Notes can be longer than one line.
3. Group the items by status: done, dismissed, next, other notes, and open (no note).
4. For "open", list the numbers only. Mark the items that need no action.
5. If a note asks a question or asks for work, offer to do it.

## Update the file when you finish work

When you finish an item for the user, write a short `Note:` on that item. For example: `Note: Done (removed the stale CORE-8968 reference)`. Do not change notes that the user wrote.
