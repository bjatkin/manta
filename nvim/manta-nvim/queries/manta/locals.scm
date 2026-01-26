;; Scope and variable tracking for Manta code

;; Function scope
(function_declaration
  name: (identifier) @definition.function)

;; Function parameters - local definitions
(parameter
  (identifier) @definition.parameter)

;; Variable declarations
(variable_declaration
  name: (identifier) @definition.var)

;; Const declarations
(const_declaration
  name: (identifier) @definition.const)

;; Type declarations
(type_declaration
  name: (identifier) @definition.type)

;; Struct fields (definitions within struct)
(struct_field
  name: (identifier) @definition.field)

;; Enum variants (definitions within enum)
(enum_variant
  name: (identifier) @definition.enum)

;; Block scope
(block) @scope

;; Match arm bindings
(pattern
  name: (identifier) @definition.var)

;; For loop variables
(for_statement
  variable: (identifier) @definition.var)

;; Scopes
(function_declaration
  body: (block) @scope)

(if_statement
  consequence: (block) @scope
  alternative: (block) @scope)

(match_statement) @scope

(for_statement
  body: (block) @scope)

(loop_statement
  (block) @scope)

(defer_statement
  (block) @scope)

;; Use statements - module imports are references
(use_statement
  (string) @reference)

;; Reference to types
(type_
  (identifier) @reference)

;; Reference to qualified identifiers
(qualified_identifier
  (identifier) @reference)
