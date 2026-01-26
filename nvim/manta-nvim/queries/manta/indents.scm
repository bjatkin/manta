;; Indentation for Manta code

;; Indent after opening braces
[
  (block "{")
  (struct_type "{")
  (enum_type "{")
  (struct_construct "{")
  (match_statement "{")
] @indent

;; Dedent before closing braces
[
  "}"
] @dedent

;; Indent after opening parentheses (function params, calls)
[
  (parameter_list "(")
  (argument_list "(")
] @indent

;; Indent continuations for multi-line statements
(variable_declaration
  "=" @indent
  (expression) @indent)

(const_declaration
  "=" @indent
  (expression) @indent)

;; Dedent function body and control structures
(function_declaration
  body: (block "{" @indent) @dedent)

(if_statement
  consequence: (block "{" @indent) @dedent)

(match_statement
  "{" @indent)

;; Type declarations with multi-line bodies
(type_declaration
  body: [(struct_type) (enum_type)] @indent)

;; Defer blocks
(defer_statement
  (block "{" @indent) @dedent)

;; Loop constructs
(for_statement
  body: (block "{" @indent) @dedent)

(loop_statement
  (block "{" @indent) @dedent)
