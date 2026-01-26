;; Code folding for Manta code

;; Fold function declarations
(function_declaration) @fold

;; Fold type declarations
(type_declaration) @fold

;; Fold blocks
(block) @fold

;; Fold multi-line match statements
(match_statement) @fold

;; Fold if statements with blocks
(if_statement) @fold

;; Fold for and loop statements
(for_statement) @fold
(loop_statement) @fold

;; Fold use statements with multiple imports
(use_statement
  "("
  (_)+
  ")") @fold

;; Fold struct and enum type bodies
(struct_type) @fold
(enum_type) @fold

;; Fold defer statements
(defer_statement) @fold

;; Fold multi-line match arms
(match_arm) @fold
