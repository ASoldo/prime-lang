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
    [$.expression, $.struct_literal],
    [$.return_type, $.tuple_type],
    [$.type_expression, $.module_path],
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

    module_path: $ => seq(
      field('head', $.identifier),
      repeat(seq('::', field('tail', $.identifier)))
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
      field('name', $.identifier),
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
      field('name', $.identifier),
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
      field('name', $.identifier),
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
      $.return_statement,
      $.while_statement,
      $.for_range_statement,
      $.defer_statement,
      $.break_statement,
      $.continue_statement,
      $.expression_statement,
      $.block
    ),

    let_statement: $ => choice(
      seq(
        'let',
        optional('mut'),
        field('name', $.identifier),
        optional(seq(':', field('type', $.type_expression))),
        optional(seq('=', field('value', $.expression))),
        ';'
      ),
      seq(
        'let',
        optional('mut'),
        field('type', $.type_expression),
        field('name', $.identifier),
        optional(seq('=', field('value', $.expression))),
        ';'
      )
    ),

    return_statement: $ => seq(
      'return',
      optional(commaSep1($.expression)),
      ';'
    ),

    while_statement: $ => seq(
      'while',
      field('condition', $.expression),
      field('body', $.block)
    ),

    for_range_statement: $ => seq(
      'for',
      field('binding', $.identifier),
      'in',
      field('range', $.range_expression),
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
      $.match_expression,
      $.if_expression,
      $.range_expression,
      $.binary_expression,
      $.unary_expression,
      $.call_expression,
      $.field_expression,
      $.struct_literal,
      $.block,
      $.tuple_expression,
      $.parenthesized_expression,
      $.identifier,
      $.literal
    ),

    match_expression: $ => seq(
      'match',
      field('value', $.expression),
      '{',
      commaSep($.match_arm),
      '}'
    ),

    match_arm: $ => seq(
      field('pattern', $.pattern),
      '=>',
      field('value', $.expression)
    ),

    if_expression: $ => seq(
      'if',
      field('condition', $.expression),
      field('consequence', $.block),
      optional(seq('else', field('alternative', $.block)))
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
      seq('*', $.expression)
    )),

    call_expression: $ => prec.left(PREC.call,
      seq(
        field('function', $.expression),
        field('arguments', $.argument_list)
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
      field('type', $.identifier),
      '{',
      commaSep($.struct_literal_field),
      '}'
    ),

    struct_literal_field: $ => choice(
      seq(field('name', $.identifier), ':', field('value', $.expression)),
      field('value', $.expression)
    ),

    tuple_expression: $ => seq(
      '(',
      choice(
        seq($.expression, ',', $.expression, repeat(seq(',', $.expression)), optional(',')),
        seq($.expression, ',')
      ),
      ')'
    ),

    parenthesized_expression: $ => seq('(', $.expression, ')'),

    pattern: $ => choice(
      $.identifier,
      '_',
      $.literal,
      $.enum_pattern
    ),

    enum_pattern: $ => seq(
      field('variant', $.identifier),
      '(',
      commaSep($.pattern),
      ')'
    ),

    literal: $ => choice(
      $.integer_literal,
      $.float_literal,
      $.string_literal,
      $.rune_literal,
      $.boolean_literal
    ),

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

    identifier: $ => token(/[A-Za-z_][A-Za-z0-9_]*/),

    type_expression: $ => choice(
      $.pointer_type,
      $.reference_type,
      $.slice_type,
      $.array_type,
      $.tuple_type,
      $.generic_type,
      $.module_path,
      $.identifier
    ),

    pointer_type: $ => seq('*', optional('mut'), $.type_expression),
    reference_type: $ => seq('&', optional('mut'), $.type_expression),
    slice_type: $ => seq('[', ']', $.type_expression),
    array_type: $ => seq('[', $.integer_literal, ']', $.type_expression),
    tuple_type: $ => seq('(', commaSep1($.type_expression), ')'),
    generic_type: $ => seq(
      field('name', $.identifier),
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
