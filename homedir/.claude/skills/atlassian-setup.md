# Atlassian Skills Setup (Jira & Confluence)

Claude Code skills for reading and creating Jira issues and searching/reading Confluence pages. Uses OAuth 2.0 (3LO) for authentication.

## 1. Create an Atlassian OAuth App

1. Go to https://developer.atlassian.com/console/myapps/
2. Click **Create** → **OAuth 2.0 integration**
3. Give it a name (e.g. "Claude Code")
4. Under **Authorization**, click **Add** next to "OAuth 2.0 (3LO)"
5. Set the callback URL to: `http://localhost:21730/callback`
6. Under **Permissions**, add scopes for each product:

   **Jira API:**
   - `read:jira-work`
   - `write:jira-work`

   **Confluence API (classic scopes):**
   - `read:confluence-content.all`
   - `read:confluence-space.summary`
   - `search:confluence`

7. Save your changes
8. Go to **Settings** and copy the **Client ID** and **Client Secret**

## 2. Install the Skills

Copy the skill directories into your Claude Code config:

```
~/.claude/skills/
├── jira/
│   ├── SKILL.md
│   ├── auth.sh
│   ├── refresh.sh
│   ├── fetch.sh
│   ├── create.sh
│   └── fetch_createmeta.sh
└── confluence/
    ├── SKILL.md
    └── fetch.sh
```

Make sure the shell scripts are executable:

```bash
chmod +x ~/.claude/skills/jira/auth.sh
chmod +x ~/.claude/skills/jira/refresh.sh
chmod +x ~/.claude/skills/jira/fetch.sh
chmod +x ~/.claude/skills/jira/create.sh
chmod +x ~/.claude/skills/jira/fetch_createmeta.sh
chmod +x ~/.claude/skills/confluence/fetch.sh
```

## 3. Configure Environment Variables

Add your OAuth client credentials to `~/.claude/settings.local.json`:

```json
{
  "env": {
    "JIRA_OAUTH_CLIENT_ID": "<your-client-id>",
    "JIRA_OAUTH_CLIENT_SECRET": "<your-client-secret>"
  }
}
```

If the file already exists, merge the `env` keys into it.

## 4. Authorize

Run the authorization script (this opens a browser for OAuth consent):

```bash
~/.claude/skills/jira/auth.sh
```

This will:
- Open your browser to log in to Atlassian and grant access
- Exchange the authorization code for OAuth tokens
- Save tokens to `~/.claude/jira-tokens.json` (permissions set to 0600)

Both the Jira and Confluence skills share this token file.

## 5. Usage

Inside Claude Code, use the slash commands:

### Jira

```
/jira CORE-1234          # Fetch a specific issue
/jira                    # Auto-detect issue from current jj bookmark
/jira create FN          # Create a new issue in the FN project
```

### Confluence

```
/confluence spaces                          # List available spaces
/confluence search title ~ "deployment"     # Search with CQL
/confluence page 12345678                   # Fetch a page by ID
/confluence deployment guide                # Free-text search
```

## Troubleshooting

**Token expired / 401 errors:**
Tokens auto-refresh, but if the refresh token has expired (~90 days), re-run:
```bash
~/.claude/skills/jira/auth.sh
```

**403 or "scope does not match" on create/write calls:**
Your OAuth app is missing write permissions. Go to the developer console, add the `write:jira-work` scope under Jira API permissions, then re-authorize with `~/.claude/skills/jira/auth.sh`.

**"scope does not match" on Confluence calls:**
Your OAuth app is missing Confluence permissions. Go to the developer console, add the Confluence scopes listed in step 1, then re-authorize.

**"JIRA_OAUTH_CLIENT_ID must be set":**
The environment variables aren't reaching Claude Code. Check that `~/.claude/settings.local.json` has the correct `env` block and restart Claude Code.

**Port 21730 already in use:**
The auth flow uses a temporary local server on port 21730. Make sure nothing else is listening on that port when you run `auth.sh`.
