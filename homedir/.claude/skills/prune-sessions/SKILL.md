---
name: prune-sessions
description: List and delete old Claude Code sessions. Shows all sessions with dates and summaries, and lets the user pick which ones to remove.
---

Prune old Claude Code sessions for the current project.

## Steps

1. **Find the sessions index** for the current project directory:
   ```
   ~/.claude/projects/<encoded-project-path>/sessions-index.json
   ```
   where `<encoded-project-path>` is the project path with `/` replaced by `-` (e.g. `/home/jco/work/scrive/kontrakcja` becomes `-home-jco-work-scrive-kontrakcja`).

   Read the file and parse the `entries` array.

2. **Display a numbered table** of all sessions, sorted by `modified` date (most recent last). For each session show:
   - Row number (1-based)
   - Date (`modified`, formatted as `YYYY-MM-DD HH:MM`)
   - Message count
   - Summary (or `firstPrompt` truncated to 60 chars if no summary)

   Format as a markdown table for readability.

3. **Ask the user** which sessions to delete. Accept:
   - Comma-separated numbers: `1, 3, 5`
   - Ranges: `1-10`
   - A combination: `1-5, 8, 12-15`
   - `all` to delete everything
   - `older than N days` to delete sessions older than N days

   Use free-text input (AskUserQuestion with an "Other" option) to let the user specify flexibly.

4. **Show a confirmation** listing the sessions that will be deleted (number, date, and first prompt) along with total count and size. **Ask the user to explicitly confirm** before proceeding. Do NOT delete anything until the user confirms.

5. **Delete the selected sessions**. For each session:
   - Remove the `.jsonl` file at `fullPath` (if it exists)
   - Remove the corresponding directory (same path without `.jsonl`) if it exists
   - Remove the entry from `sessions-index.json`

   Write the updated `sessions-index.json` back to disk.

6. **Report** how many sessions were deleted and how much disk space was freed.
