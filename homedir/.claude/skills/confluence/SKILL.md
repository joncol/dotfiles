---
name: confluence
description: Search and read Confluence pages. Only use when working in a project whose path contains "kontrakcja".
argument-hint: "[search <query> | page <id> | spaces]"
---

Search and read Confluence pages.

## First-time setup

This skill shares OAuth tokens with the jira skill. If `~/.claude/jira-tokens.json` does not exist, or if Confluence API calls return 401/403, tell the user to:
1. Add Confluence API permissions (`read:confluence-content.all`, `read:confluence-space.summary`) to their OAuth app at https://developer.atlassian.com/console/myapps/
2. Run `~/.claude/skills/jira/auth.sh` to re-authorize with the new scopes

## Steps

1. Determine what the user wants:

   - **Search for pages**: Use CQL (Confluence Query Language) to search.
     ```bash
     ~/.claude/skills/confluence/fetch.sh search 'title ~ "some topic" AND space = "TEAMSPACE"'
     ```
   - **Read a specific page**: Fetch by page ID.
     ```bash
     ~/.claude/skills/confluence/fetch.sh page 12345678
     ```
   - **List spaces**: Show available spaces.
     ```bash
     ~/.claude/skills/confluence/fetch.sh spaces
     ```
   - If `$ARGUMENTS` starts with "search", "page", or "spaces", pass it directly.
   - Otherwise, treat `$ARGUMENTS` as a search query and use CQL: `title ~ "$ARGUMENTS" OR text ~ "$ARGUMENTS"`

2. Present results in a readable format:

   - **Search results**: Show title, space, and page ID for each result. Offer to fetch the full content of any page.
   - **Page content**: The body is returned in Confluence storage format (HTML-like XML). Convert it to readable text, preserving structure (headings, lists, tables). Summarize if very long.
   - **Spaces**: Show space key, name, and type.
