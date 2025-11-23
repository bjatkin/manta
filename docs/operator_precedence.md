## 





### Implementation notes and future work

- When adding infix parselets, return the appropriate `Precedence` variant from `InfixParselet::precedence()` so the Pratt loop in `Parser::parse_expression_precedence` correctly orders operator binding.
- Decide and document the associativity for comparisons (chained comparisons vs non-associative) before implementing their parselets.

If you'd like, I can also:
- Add an explicit precedence table showing the enum value ordering as code snippets referencing the exact variants.
- Implement missing infix parselets (e.g. binary operators) and tests to verify the precedence behavior.

---
Generated: reference for implementers and language documentation.
