/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: 'manta',

  rules: {
    // Entry point
    source_file: $ => repeat($.declaration),

    declaration: $ => choice(
      $.module_declaration,
      $.use_statement,
      $.type_declaration,
      $.variable_declaration,
      $.const_declaration,
      $.function_declaration,
    ),

    // Module system
    module_declaration: $ => seq(
      'mod',
      $.identifier,
    ),

    use_statement: $ => seq(
      'use',
      '(',
      repeat(seq(
        $.string,
        optional(','),
      )),
      ')',
    ),

    // Type declarations
    type_declaration: $ => seq(
      'type',
      $.identifier,
      choice(
        $.struct_type,
        $.enum_type,
        $.type_alias,
      ),
    ),

    type_alias: $ => seq(
      '=',
      $.type_,
    ),

    struct_type: $ => seq(
      'struct',
      '{',
      repeat(seq(
        $.struct_field,
        optional(','),
      )),
      '}',
    ),

    struct_field: $ => seq(
      '.',
      $.identifier,
      $.type_,
    ),

    enum_type: $ => seq(
      'enum',
      '{',
      repeat(seq(
        $.enum_variant,
        optional(','),
      )),
      '}',
    ),

    enum_variant: $ => seq(
      '.',
      $.identifier,
      optional(seq('(', $.type_, ')')),
    ),

    // Variable declarations
    variable_declaration: $ => choice(
      seq('let', $.identifier, $.type_, optional(seq('=', $.expression))),
      seq('let', $.identifier, '=', $.expression),
      seq('mut', $.identifier, $.type_, optional(seq('=', $.expression))),
      seq('mut', $.identifier, '=', $.expression),
    ),

    const_declaration: $ => seq(
      'const',
      $.identifier,
      '=',
      $.expression,
    ),

    // Function declaration
    function_declaration: $ => seq(
      'fn',
      $.identifier,
      '(',
      optional($.parameter_list),
      ')',
      optional($.type_),
      $.block,
    ),

    parameter_list: $ => seq(
      $.parameter,
      repeat(seq(',', $.parameter)),
      optional(','),
    ),

    parameter: $ => seq(
      $.identifier,
      repeat(seq(',', $.identifier)),
      $.type_,
    ),

    // Types
    type_: $ => choice(
      $.primitive_type,
      $.type_constructor,
      $.qualified_type,
      $.identifier,
    ),

    primitive_type: $ => choice(
      'i8', 'i16', 'i32', 'i64',
      'u8', 'u16', 'u32', 'u64',
      'f32', 'f64',
      'bool', 'str', 'meta',
    ),

    type_constructor: $ => choice(
      seq('*', $.type_),
      seq('[', optional($.expression), ']', $.type_),
    ),

    qualified_type: $ => seq(
      $.identifier,
      '::',
      $.identifier,
    ),

    // Statements
    block: $ => seq(
      '{',
      repeat($.statement),
      '}',
    ),

    statement: $ => choice(
      $.variable_declaration,
      $.expression_statement,
      $.return_statement,
      $.defer_statement,
      $.if_statement,
      $.match_statement,
      $.for_statement,
      $.loop_statement,
      $.break_statement,
      $.continue_statement,
    ),

    expression_statement: $ => seq(
      $.expression,
      optional(';'),
    ),

    return_statement: $ => choice(
      seq('return', ';'),
      prec(1, seq('return', $.expression, optional(';'))),
      'return',
    ),

    defer_statement: $ => seq(
      'defer',
      $.block,
    ),

    if_statement: $ => seq(
      'if',
      $.expression,
      $.block,
      repeat(seq('else', 'if', $.expression, $.block)),
      optional(seq('else', $.block)),
    ),

    match_statement: $ => seq(
      'match',
      $.expression,
      '{',
      repeat($.match_arm),
      '}',
    ),

    match_arm: $ => seq(
      $.pattern,
      $.block,
    ),

    pattern: $ => choice(
      seq('.', $.identifier),
      seq('.', $.identifier, '(', $.identifier, ')'),
      $.literal,
      $.identifier,
      '_',
    ),

    for_statement: $ => seq(
      'for',
      $.identifier,
      'in',
      $.expression,
      $.block,
    ),

    loop_statement: $ => seq(
      'loop',
      $.block,
    ),

    break_statement: $ => seq(
      'break',
      optional(';'),
    ),

    continue_statement: $ => seq(
      'continue',
      optional(';'),
    ),

    // Expressions
    expression: $ => $.assignment_expression,

    assignment_expression: $ => choice(
      $.ternary_expression,
      seq($.postfix_expression, choice('=', '+=', '-='), $.assignment_expression),
    ),

    ternary_expression: $ => $.logical_or_expression,

    logical_or_expression: $ => prec.left(1, seq(
      $.logical_and_expression,
      repeat(seq('||', $.logical_and_expression)),
    )),

    logical_and_expression: $ => prec.left(1, seq(
      $.bitwise_or_expression,
      repeat(seq('&&', $.bitwise_or_expression)),
    )),

    bitwise_or_expression: $ => prec.left(1, seq(
      $.bitwise_xor_expression,
      repeat(seq('|', $.bitwise_xor_expression)),
    )),

    bitwise_xor_expression: $ => prec.left(1, seq(
      $.bitwise_and_expression,
      repeat(seq('^', $.bitwise_and_expression)),
    )),

    bitwise_and_expression: $ => prec.left(1, seq(
      $.equality_expression,
      repeat(seq('&', $.equality_expression)),
    )),

    equality_expression: $ => prec.left(1, seq(
      $.relational_expression,
      repeat(seq(choice('==', '!='), $.relational_expression)),
    )),

    relational_expression: $ => prec.left(1, seq(
      $.additive_expression,
      repeat(seq(choice('<', '>', '<=', '>='), $.additive_expression)),
    )),

    additive_expression: $ => prec.left(2, seq(
      $.multiplicative_expression,
      repeat(seq(choice('+', '-'), $.multiplicative_expression)),
    )),

    multiplicative_expression: $ => prec.left(3, seq(
      $.unary_expression,
      repeat(seq(choice('*', '/', '%'), $.unary_expression)),
    )),

    unary_expression: $ => choice(
      $.postfix_expression,
      seq(choice('!', '-', '*', '&'), $.unary_expression),
    ),

    postfix_expression: $ => prec.left(seq(
      $.primary_expression,
      repeat(choice(
        seq('(', optional($.argument_list), ')'),
        seq('[', $.expression, ']'),
        seq('.', $.identifier),
      )),
    )),

    argument_list: $ => seq(
      $.expression,
      repeat(seq(',', $.expression)),
      optional(','),
    ),

    primary_expression: $ => choice(
      $.literal,
      $.identifier,
      $.qualified_identifier,
      $.enum_construct,
      $.struct_construct,
      $.array_construct,
      seq('(', $.expression, ')'),
    ),

    qualified_identifier: $ => seq(
      $.identifier,
      '::',
      $.identifier,
    ),

    enum_construct: $ => prec.right(seq(
      '.',
      $.identifier,
      optional(seq('(', $.expression, ')')),
    )),

    struct_construct: $ => prec(2, seq(
      $.identifier,
      '{',
      repeat(seq(
        $.identifier,
        ':',
        $.expression,
        optional(','),
      )),
      '}',
    )),

    array_construct: $ => seq(
      '[',
      optional($.expression),
      repeat(seq(',', $.expression)),
      ']',
    ),

    // Literals and identifiers
    literal: $ => choice(
      $.number,
      $.string,
      'true',
      'false',
    ),

    number: $ => choice(
      /0x[0-9a-fA-F_]+/,
      /0b[01_]+/,
      /[0-9][0-9_]*\.[0-9_]+([eE][+-]?[0-9_]+)?/,
      /[0-9][0-9_]*([eE][+-]?[0-9_]+)?/,
    ),

    string: $ => seq(
      '"',
      repeat(choice(
        /[^"\\]/,
        seq('\\', /./),
      )),
      '"',
    ),

    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    comment: $ => token(choice(
      seq('//', /[^\n]*/),
      seq('/*', /(.|\n)*?/, '*/'),
    )),
  },

  extras: $ => [
    /\s+/,
    $.comment,
  ],

  word: $ => $.identifier,
});
