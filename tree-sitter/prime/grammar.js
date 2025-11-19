const PREC = {
  call: 14,
  unary: 13,
  multiplicative: 12,
  additive: 11,
  comparison: 10,
  equality: 9,
  bit_and: 8,
  bit_xor: 7,
  bit_or: 6,
  logical_and: 5,
  logical_or: 4,
  range: 3,
};

module.exports = grammar({
  name: 'prime',

  extras: $ => [
    /\s|\\\r?\n/,
    $.comment,
  ],

  supertypes: $ => [
    $.statement,
    $.expression,
    $.literal,
    $.type_expression,
    $.pattern,
  ],

  conflicts: $ => [
    [$.statement, $.expression],
    [$.return_type, $.tuple_type],
    [$.pattern, $.enum_pattern],
    [$.pattern, $.enum_pattern, $.type_expression],
    [$.module_path],
    [$.type_path, $.type_expression],
  ],

  rules: {
    program: $ => seq(
      optional($.module_declaration),
      repeat($.import_declaration),
      repeat($.item)
    ),

    module_declaration: $ => seq(
      'module',
      field('name', $.module_path),
      ';'
    ),

    import_declaration: $ => seq(
      optional('pub'),
      'import',
      field('path', choice($.module_path, $.string_literal)),
      optional(seq('as', field('alias', $.identifier))),
      ';'
    ),

    module_path: $ => prec(1, seq(
      field('head', $.identifier),
      repeat(seq('::', field('tail', $.identifier)))
    )),

    type_path: $ => seq(
      optional(seq($.module_path, '::')),
      $.type_identifier
    ),

    item: $ => seq(
      optional('pub'),
      choice(
        $.function_definition,
        $.struct_definition,
        $.enum_definition,
        $.interface_definition,
        $.impl_definition,
        $.const_definition
      )
    ),

    struct_definition: $ => seq(
      'struct',
      field('name', $.type_identifier),
      optional(field('type_parameters', $.type_parameters)),
      '{',
      repeat($.struct_field),
      '}'
    ),

    struct_field: $ => choice(
      seq(
        field('name', $.identifier),
        ':',
        field('type', $.type_expression),
        ';'
      ),
      seq(
        field('embedded', $.type_expression),
        ';'
      )
    ),

    enum_definition: $ => seq(
      'enum',
      field('name', $.type_identifier),
      optional(field('type_parameters', $.type_parameters)),
      '{',
      commaSep($.enum_variant),
      '}'
    ),

    enum_variant: $ => seq(
      field('name', $.identifier),
      optional(seq(
        '(',
        commaSep1($.type_expression),
        ')'
      ))
    ),

    interface_definition: $ => seq(
      'interface',
      field('name', $.type_identifier),
      optional(field('type_parameters', $.type_parameters)),
      '{',
      repeat($.interface_method),
      '}'
    ),

    interface_method: $ => seq(
      'fn',
      field('name', $.identifier),
      field('parameters', $.parameter_list),
      optional(field('returns', $.return_type)),
      ';'
    ),

    impl_definition: $ => seq(
      'impl',
      field('interface', $.type_expression),
      'for',
      field('target', $.type_expression),
      '{',
      repeat($.function_definition),
      '}'
    ),

    const_definition: $ => seq(
      'const',
      field('name', $.identifier),
      optional(seq(':', field('type', $.type_expression))),
      '=',
      field('value', $.expression),
      ';'
    ),

    function_definition: $ => seq(
      'fn',
      field('name', $.identifier),
      optional(field('type_parameters', $.type_parameters)),
      field('parameters', $.parameter_list),
      optional(field('returns', $.return_type)),
      field('body', choice($.block, $.expression_body))
    ),

    type_parameters: $ => seq(
      '[',
      commaSep1($.identifier),
      ']'
    ),

    parameter_list: $ => seq(
      '(',
      commaSep($.parameter),
      ')'
    ),

    parameter: $ => seq(
      optional('mut'),
      field('name', $.identifier),
      ':',
      field('type', $.type_expression)
    ),

    return_type: $ => seq(
      '->',
      choice(
        $.type_expression,
        seq('(', commaSep1($.type_expression), ')')
      )
    ),

    expression_body: $ => seq(
      '=>',
      field('value', $.expression),
      ';'
    ),

    block: $ => seq(
      '{',
      repeat($.statement),
      optional(field('tail', $.expression)),
      '}'
    ),

    statement: $ => choice(
      $.let_statement,
      $.assign_statement,
      $.return_statement,
      $.while_statement,
      $.for_statement,
      $.defer_statement,
      $.break_statement,
      $.continue_statement,
      $.match_expression,
      $.if_expression,
      $.expression_statement,
      $.block
    ),

    let_statement: $ => seq(
      'let',
      optional('mut'),
      choice(
        seq(
          field('pattern', $.pattern),
          optional(seq(':', field('annotation', $.type_expression))),
          optional(seq('=', field('value', $.expression)))
        ),
        seq(
          field('type', $.type_expression),
          field('name', $.identifier),
          optional(seq('=', field('value', $.expression)))
        )
      ),
      ';'
    ),

    assign_statement: $ => seq(
      field('target', $.expression),
      '=',
      field('value', $.expression),
      ';'
    ),

    return_statement: $ => seq(
      'return',
      optional(commaSep1($.expression)),
      ';'
    ),

    while_statement: $ => seq(
      'while',
      field('condition', $._condition),
      field('body', $.block)
    ),

    for_statement: $ => seq(
      'for',
      field('binding', $.identifier),
      'in',
      field('target', $.expression),
      field('body', $.block)
    ),

    defer_statement: $ => seq(
      'defer',
      field('expression', $.expression),
      ';'
    ),

    break_statement: $ => seq('break', ';'),
    continue_statement: $ => seq('continue', ';'),

    expression_statement: $ => seq(field('expression', $.expression), ';'),

    expression: $ => choice(
      $.try_expression,
      $.match_expression,
      $.if_expression,
      $.range_expression,
      $.binary_expression,
      $.unary_expression,
      $.call_expression,
      $.field_expression,
      $.question_expression,
      $.struct_literal,
      $.map_literal,
      $.array_literal,
      $.format_string_literal,
      $.block,
      $.tuple_expression,
      $.parenthesized_expression,
      $.identifier,
      $.literal
    ),

    try_expression: $ => seq(
      'try',
      field('body', $.block)
    ),

    match_expression: $ => seq(
      'match',
      field('value', $.expression),
      '{',
      optional(seq(
        $.match_arm,
        repeat(seq(choice(',', ';'), $.match_arm)),
        optional(choice(',', ';'))
      )),
      '}'
    ),

    match_arm: $ => seq(
      field('pattern', $.pattern),
      optional(seq('if', field('guard', $.expression))),
      '=>',
      field('value', $.expression)
    ),

    if_expression: $ => prec.right(seq(
      'if',
      field('condition', $._condition),
      field('consequence', $.block),
      optional(field('alternative', $.else_clause))
    )),

    else_clause: $ => seq(
      'else',
      field('value', choice($.block, $.if_expression))
    ),

    let_condition: $ => seq(
      'let',
      field('pattern', $.pattern),
      '=',
      field('value', prec.left(PREC.logical_and, $.expression))
    ),

    _condition: $ => choice(
      $.let_condition,
      $.expression
    ),

    range_expression: $ => prec.left(PREC.range,
      seq(
        field('start', $.expression),
        field('operator', choice('..', '..=')),
        field('end', $.expression)
      )
    ),

    binary_expression: $ => choice(
      prec.left(PREC.logical_or, seq($.expression, '||', $.expression)),
      prec.left(PREC.logical_and, seq($.expression, '&&', $.expression)),
      prec.left(PREC.bit_or, seq($.expression, '|', $.expression)),
      prec.left(PREC.bit_xor, seq($.expression, '^', $.expression)),
      prec.left(PREC.bit_and, seq($.expression, '&', $.expression)),
      prec.left(PREC.equality, seq($.expression, choice('==', '!='), $.expression)),
      prec.left(PREC.comparison, seq($.expression, choice('<', '<=', '>', '>='), $.expression)),
      prec.left(PREC.additive, seq($.expression, choice('+', '-'), $.expression)),
      prec.left(PREC.multiplicative, seq($.expression, choice('*', '/', '%'), $.expression))
    ),

    unary_expression: $ => prec.right(PREC.unary, choice(
      seq('-', $.expression),
      seq('!', $.expression),
      seq('&', optional('mut'), $.expression),
      seq('*', $.expression),
      seq('move', $.expression)
    )),

    call_expression: $ => prec.left(PREC.call,
      seq(
        field('function', $.expression),
        optional(field('type_arguments', $.type_arguments)),
        field('arguments', $.argument_list)
      )
    ),

    question_expression: $ => prec.left(PREC.call,
      seq(
        field('value', $.expression),
        '?'
      )
    ),

    argument_list: $ => seq('(', commaSep($.expression), ')'),

    field_expression: $ => prec.left(PREC.call,
      seq(
        field('value', $.expression),
        '.',
        field('field', $.identifier)
      )
    ),

    struct_literal: $ => seq(
      field('type', $.type_path),
      '{',
      commaSep($.struct_literal_field),
      '}'
    ),

    struct_literal_field: $ => choice(
      seq(field('name', $.identifier), ':', field('value', $.expression)),
      field('value', $.expression)
    ),

    map_literal: $ => seq(
      '#',
      '{',
      commaSep($.map_entry),
      '}'
    ),

    map_entry: $ => seq(
      field('key', $.expression),
      ':',
      field('value', $.expression)
    ),

    array_literal: $ => seq(
      '[',
      commaSep($.expression),
      ']'
    ),

    tuple_expression: $ => seq(
      '(',
      choice(
        seq($.expression, ',', $.expression, repeat(seq(',', $.expression)), optional(',')),
        seq($.expression, ',')
      ),
      ')'
    ),

    format_string_literal: $ => token(
      seq('`', repeat(choice(/[^\\`]/, /\\./)), '`')
    ),

    parenthesized_expression: $ => seq('(', $.expression, ')'),

    pattern: $ => choice(
      '_',
      $.literal,
      $.enum_pattern,
      $.tuple_pattern,
      $.map_pattern,
      $.struct_pattern,
      $.slice_pattern,
      $.identifier
    ),

    enum_pattern: $ => seq(
      optional(seq(field('enum', $.module_path), '::')),
      field('variant', $.identifier),
      optional(seq('(', commaSep($.pattern), ')'))
    ),

    tuple_pattern: $ => prec(1, seq(
      '(',
      commaSep($.pattern),
      ')'
    )),

    map_pattern: $ => seq(
      '#',
      '{',
      commaSep($.map_pattern_entry),
      '}'
    ),

    map_pattern_entry: $ => seq(
      field('key', $.string_literal),
      ':',
      field('value', $.pattern)
    ),

    struct_pattern: $ => seq(
      field('type', $.module_path),
      '{',
      commaSep(choice($.struct_pattern_field, '..')),
      '}'
    ),

    struct_pattern_field: $ => seq(
      field('name', $.identifier),
      optional(seq(':', field('value', $.pattern)))
    ),

    slice_pattern: $ => seq(
      '[',
      commaSep(choice($.pattern, $.slice_rest)),
      ']'
    ),

    slice_rest: $ => seq('..', optional($.pattern)),

    literal: $ => prec(1, choice(
      $.integer_literal,
      $.float_literal,
      $.string_literal,
      $.rune_literal,
      $.boolean_literal
    )),

    boolean_literal: $ => choice('true', 'false'),

    integer_literal: $ => token(/[0-9]+/),

    float_literal: $ => token(
      seq(
        /[0-9]+/,
        choice(
          seq('.', /[0-9]+/, optional(seq(/[eE]/, optional(/[+-]/), /[0-9]+/))),
          seq(/[eE]/, optional(/[+-]/), /[0-9]+/)
        )
      )
    ),

    string_literal: $ => token(seq('"', repeat(choice(/[^"\\]/, /\\./)), '"')),
    rune_literal: $ => token(seq("'", choice(/[^'\\]/, /\\./), "'")),

    type_identifier: $ => token(prec(1, /[A-Z][A-Za-z0-9_]*/)),
    identifier: $ => token(/[A-Za-z_][A-Za-z0-9_]*/),

    type_expression: $ => choice(
      $.pointer_type,
      $.reference_type,
      $.slice_type,
      $.array_type,
      $.tuple_type,
      $.generic_type,
      $.type_path,
      $.type_identifier,
      $.identifier
    ),

    pointer_type: $ => seq('*', optional('mut'), $.type_expression),
    reference_type: $ => seq('&', optional('mut'), $.type_expression),
    slice_type: $ => seq('[', ']', $.type_expression),
    array_type: $ => seq('[', $.integer_literal, ']', $.type_expression),
    tuple_type: $ => seq('(', commaSep1($.type_expression), ')'),
    generic_type: $ => seq(
      field('name', $.type_path),
      '[',
      commaSep1($.type_expression),
      ']'
    ),

    type_arguments: $ => seq(
      '[',
      commaSep1($.type_expression),
      ']'
    ),

    comment: $ => token(choice(
      seq('//', /[^\n]*/),
      seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/')
    )),
  }
});

function commaSep(rule) {
  return optional(commaSep1(rule));
}

function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)), optional(','));
}
