:- begin_tests(zanzibar_config_ns_parser_tests).

% Enable double-quoted strings for ease of use.
:- set_prolog_flag(double_quotes, chars).
:- use_module(zanzibar_config_ns_parser).

test(valid_nested_blocks, [ true( Out =
    ast([
        block(relation, [
            block(nested_block3, [
                block(nested_block3_2, [
                    block(child, [
                        kv(name, "child 3.2")
                    ])
                ]),
                block(nested_block3_1, [])
            ]),
            block(nested_block2, []),
            block(nested_block1, [
                block(child, [
                    kv(name, "child")
                ]),
                kv(name, "nested name")
            ])
        ]),
        kv(property4, var(variable)),
        kv(property3, false),
        kv(property2, 42),
        kv(property1, 1.0),
        kv(name, "name1")
    ])
    )]) :- parse_config("
    name = \"name1\"

    property1 = 1.0
    property2 = 42
    property3 = false
    property4 = $variable

    relation {
        nested_block1 {
            name = \"nested name\"
            child {
                name = \"child\"
            }
        }
        nested_block2 {
        }
        nested_block3 {
            nested_block3_1 {
            }
            nested_block3_2 {
                child {
                    name = \"child 3.2\"
                }
            }
        }
    }
    ", Out).

test(invalid_direct, [ true( Out =
    ast(error(invalid, expected([block,kv]),
            remaining([
                token(kw, unexpected),
                token(kw, relation),
                token(scope_s, '{'),
                token(kw, name),
                token(assign, '='),
                token(qs, "relation name"),
                token(scope_e, '}')
            ]),
            captured([
                kv(property4, var(variable)),
                kv(property3, false),
                kv(property2, 42),
                kv(property1, 1.0),
                kv(name, "name1")
            ])
    ))
    )]) :- parse_config("
    name = \"name1\"

    property1 = 1.0
    property2 = 42
    property3 = false
    property4 = $variable

    unexpected

    relation {
        name = \"relation name\"
    }
    ", Out).

test(invalid_nested, [ true( Out =
    ast([
        block(relation,
                error(invalid,
                        expected([block,kv]),
                        remaining([
                            token(kw, unexpected),
                            token(scope_e, '}')
                        ]),
                        captured([
                            kv(name,"relation name")
                        ])
                    )
        ),
        kv(property4, var(variable)),
        kv(property3, false),
        kv(property2, 42),
        kv(property1, 1.0),
        kv(name,"name1")
    ])
    )]) :- parse_config("
    name = \"name1\"

    property1 = 1.0
    property2 = 42
    property3 = false
    property4 = $variable

    relation {
        name = \"relation name\"
        unexpected
    }
    ", Out).

:- end_tests(zanzibar_config_ns_parser_tests).