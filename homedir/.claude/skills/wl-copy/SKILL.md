---
name: wl-copy
description: Copy the last code snippet or message to the system clipboard
disable-model-invocation: true
allowed-tools: Bash
---

Copy the last code block or formatted message from the conversation to the system clipboard using `wl-copy`.

- If the last assistant message contained a fenced code block, copy the code block contents.
- If it contained a formatted message (e.g. a drafted email, PR description, or similar prose), copy that message.
- Strip any leading indentation that was added purely for display purposes.
- Pipe the content to `wl-copy` using a heredoc to preserve newlines.
- After copying, confirm to the user that it's on their clipboard.
