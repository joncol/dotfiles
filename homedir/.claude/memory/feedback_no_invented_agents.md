---
name: No invented agents
description: When rewriting passive voice to active (for example for STE), never invent who decided or said something; attribute only to agents that the source names
type: feedback
---

Do not invent an agent when you change a passive sentence to active voice. Name a person or team ("Product", "Legal", "design") only when the source names them.

**Why:** In 2026-10, during an STE rewrite of Jira tickets (CORE-9130, CORE-9131), "This restriction will never be removed" became "Product will never remove this restriction". The meeting notes did not say who decided it. Comments in a Google Docs export also had no author, but the tickets said "Product confirmed …". Readers use such attributions to decide if a question is closed, so a false one can make an open question look closed.

**How to apply:**
- If the source does not name the agent, keep the passive voice (STE allows it when the agent is unknown), or state the fact without an agent ("This restriction is permanent").
- Cite the real source instead: "The meeting notes say that …", "A comment on the meeting notes proposes …".
- This applies to tickets, docs, commit messages and summaries, not only to STE text.
