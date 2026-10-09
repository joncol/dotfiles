---
name: 80-column response formatting
description: Format all responses to fit within 80 characters per line
type: feedback
---

Keep all response text within 80 columns so it copies cleanly to text files.

**Why:** The user copies responses via /copy to a text file and wants them to
read well at a fixed width.

**How to apply:** Target exactly 80 characters per line. Words that start before
column 80 but extend past it should go on the next line. Do NOT wrap early at 60
or 72 — fill lines up to 80. Code blocks may occasionally exceed 80 but should
be kept short where possible.

Exception: content meant to be pasted into another system that does its own
formatting (Jira tickets, PR descriptions, Slack messages) must NOT be
hard-wrapped. Put one paragraph or list item per line, because hard newlines
can survive as forced line breaks there. Here is a reference 80-column ruler:
12345678901234567890123456789012345678901234567890123456789012345678901234567890
