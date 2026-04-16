---
name: bookmark
description: Create a jj bookmark from a Jira ticket number. Fetches the ticket summary, generates a descriptive bookmark name, and creates+tracks it.
argument-hint: "[TICKET-KEY]"
---

Create a jj bookmark from a Jira ticket, using the ticket summary to generate a descriptive name.

## Steps

1. **Check trunk status.** Run `jj log -r @ --no-graph -T 'separate(" ", if(immutable, "immutable", "mutable"), if(empty, "empty", "nonempty"))'` to check the current revision. Proceed without warning if:
   - The revision is immutable (directly on trunk), OR
   - The revision is mutable AND empty (a clean working copy on top of trunk — this is the normal jj state)
   
   Only warn the user if the revision is mutable AND nonempty (i.e. they have uncommitted work that isn't on trunk). Ask whether to continue anyway.

2. **Get the ticket key.**
   - If `$ARGUMENTS` is provided, use it as the ticket key.
   - Otherwise, ask the user for the ticket key (e.g. `FN-1234`).

3. **Fetch the ticket** using the jira skill's fetch script:
   ```bash
   ~/.claude/skills/jira/fetch.sh <TICKET-KEY>
   ```
   Extract the `summary` field from the returned JSON.

4. **Generate the bookmark name.** Create a name in the format:
   ```
   jco/<ticket-key>-<short-description>
   ```
   Rules for the short description:
   - Derived from the ticket summary
   - Lowercase, words separated by hyphens
   - Remove articles (a, an, the), prepositions, and filler words
   - Keep it concise: aim for 3-5 meaningful words
   - Only use alphanumeric characters and hyphens
   - Examples:
     - "Simplify participant role selection in signing view" -> `jco/fn-1263-simplify-participant-roles`
     - "Add bulk download button to documents list" -> `jco/fn-1400-bulk-download-documents`
     - "Fix broken tooltip on mobile devices" -> `jco/fn-1500-fix-tooltip-mobile`

5. **Confirm with the user.** Show the proposed bookmark name and ask the user to confirm or suggest changes.

6. **Create the bookmark.** Run:
   ```bash
   jj bookmark create <bookmark-name> -r @
   ```

7. **Track the bookmark.** Run:
   ```bash
   jj bookmark track <bookmark-name> --remote=origin
   ```
   Note: this may fail if the bookmark doesn't exist on the remote yet, which is expected for new bookmarks. That's fine — tracking will be established on the first push.

8. **Report success.** Show the created bookmark name.
