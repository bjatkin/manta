;; Highlighting for Manta code

;; Comments
(comment) @comment

;; Keywords
(module_declaration "mod" @keyword)
(use_statement "use" @keyword)
(type_declaration "type" @keyword)
(function_declaration "fn" @keyword)
(variable_declaration "let" @keyword)
(variable_declaration "mut" @keyword)
(const_declaration "const" @keyword)
(return_statement "return" @keyword)
(defer_statement "defer" @keyword)
(if_statement "if" @keyword)
(if_statement "else" @keyword)
(for_statement "for" @keyword)
(for_statement "in" @keyword)
(loop_statement "loop" @keyword)
(match_statement "match" @keyword)
(break_statement "break" @keyword)
(continue_statement "continue" @keyword)

;; Type keywords
(struct_type "struct" @keyword)
(enum_type "enum" @keyword)

;; Keywords in type and struct contexts
(type_declaration "=" @operator)

;; Literals
(number) @number
(string) @string

;; Booleans
(literal "true" @boolean)
(literal "false" @boolean)

;; Identifiers
(identifier) @variable
(function_declaration name: (identifier) @function)
(type_declaration name: (identifier) @type)
(field_expression "." @punctuation (identifier) @field)

;; Type annotations
(primitive_type) @type.builtin

;; Operators
[
  "+"
  "-"
  "*"
  "/"
  "%"
  "=="
  "!="
  "<"
  ">"
  "<="
  ">="
  "&&"
  "||"
  "&"
  "|"
  "^"
  "!"
  "="
  "+="
  "-="
] @operator

;; Punctuation
[
  "{"
  "}"
  "("
  ")"
  "["
  "]"
  ","
  ";"
  ":"
  "::"
  "."
  "->"
  "*"
] @punctuation

;; Enum variants (highlighted distinctly)
(enum_construct "." @punctuation (identifier) @constant)
(pattern "." @punctuation (identifier) @constant)

;; Module-qualified names
(qualified_identifier (identifier) @namespace "::" @punctuation)

;; Function calls (postfix with parentheses)
(postfix_expression (primary_expression (identifier) @function) . "(" @punctuation)

;; Struct field access
(postfix_expression "." @punctuation (identifier) @field)

;; Parameters
(parameter (identifier) @variable)

;; Struct construction fields
(struct_construct (identifier) @field ":" @punctuation)

;; Error nodes
(ERROR) @error
