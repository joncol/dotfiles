---
name: changelog
description: Add a changelog entry for the current Jira issue. Creates a YAML file in doc/changelog.d/. Only use when working in a project whose path contains "kontrakcja".
---

Add a changelog entry for the current work.

## Steps

1. Determine the Jira issue from the current git branch name. The branch name typically starts with the issue key (e.g. `CORE-8356-...`). Run `git branch --show-current` to get it. Convert the issue key to lowercase for the filename (e.g. `core-8356`). If the issue key cannot be determined in this way, please use the current PR number to determine the filename of the changelog. E.g. `pull-1234.yaml`.

2. Check whether a file `doc/changelog.d/<issue-key>.yaml` already exists. If it does, read it so you can append a new document to it (YAML multi-document format, separated by `---`). If it already has an entry for the same section you're about to add, just update rather than append.

3. Determine the appropriate section. Ask the user which section applies if it isn't obvious from context. The available sections are:
   - `esign` — eSign functionality (API changes, new eID providers, sign/view flow changes)
   - `core` — Core backend changes
   - `flow` — FLOW changes
   - `revert` — Reverts
   - `breaking_api` — Breaking API changes
   - `migrations` — Database migrations
   - `configuration` — Configuration changes
   - `routing` — URL or routing changes
   - `validation` — Validation changes
   - `feature_flags` — Feature flag additions/removals
   - `rollout_flags` — Rollout flag additions/removals
   - `audit_log` — Audit log changes
   - `chargeable_items` — Chargeable item changes
   - `billing` — Billing changes
   - `cron_jobs` — Cron job changes
   - `bugfix` — Bug fixes
   - `other` — Anything that doesn't fit above

4. Write (or update) the file `doc/changelog.d/<issue-key>.yaml` with the following format:

   ```yaml
   section: <section>
   title: <Short title, no trailing dot>
   ```

   Optionally include a `description` field for longer explanations:

   ```yaml
   section: <section>
   title: <Short title, no trailing dot>
   description: |
     Longer explanation of the change.
   ```

   Multiple entries for the same issue use YAML multi-document format (separated by `---`):

   ```yaml
   section: esign
   title: First entry
   ---
   section: rollout_flags
   title: Second entry
   ```
