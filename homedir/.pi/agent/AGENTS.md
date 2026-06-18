# Global instructions

## Output formatting: Unicode only, never LaTeX

Always write mathematical and directional symbols as literal Unicode
characters. Never use LaTeX / TeX notation.

- Write the actual glyph: → ← ↔ ⇒ ⇐ ≤ ≥ ≠ ≈ × ÷ ± · ∈ ∉ ⊂ ⊆ ∪ ∩
  ∀ ∃ ∧ ∨ ¬ ∅ ∞ √ ∂ ∇ ∫ ∑ ∏ α β γ δ ε θ λ μ π σ φ ω Γ Δ Σ Φ Ω
- Never emit backslash command sequences such as `\rightarrow`,
  `\leftarrow`, `\leq`, `\times`, `\in`, `\alpha`, `\sum`, `\frac`.
- Never wrap expressions in dollar delimiters: no `$...$` and no `$$...$$`.
- Superscripts/subscripts: prefer Unicode (x², xₙ) or plain ASCII
  (`x^2`, `x_n`) — never `$x^2$`.

Examples:
- Correct:  `a → b`,  `n ≤ 10`,  `x ∈ S`,  `λx. x`,  `2 × 3 = 6`
- Wrong:    `a $\rightarrow$ b`,  `$n \leq 10$`,  `$x \in S$`,  `$\lambda$`

This rule applies to every response, code comment, commit message, and
chat message, with no exceptions.
