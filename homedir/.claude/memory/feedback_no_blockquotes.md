---
name: No blockquotes
description: Use code blocks instead of markdown blockquotes (>) for quoting text, because blockquotes render with inverted colors in the terminal and are hard to read
type: feedback
---

Do not use markdown blockquotes (`>` prefix) when quoting text. Use code blocks instead.

**Why:** Blockquotes render with inverted foreground/background colors in the terminal (Alacritty), making them hard to read.

**How to apply:** Whenever you need to quote text, wrap it in a code block (triple backticks) instead of using `>`.
