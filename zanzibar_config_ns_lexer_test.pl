:- begin_tests(zanzibar_config_ns_lexer_tests).

% Enable double-quoted strings for ease of use.
:- set_prolog_flag(double_quotes, chars).
:- use_module(zanzibar_config_ns_lexer).

test(valid_complete, true( Out = ns(
    kvs([
        kv(name, "name1"),
        kv(property1, 1.0),
        kv(property2, 42),
        kv(property3, false),
        kv(property4, var(variable))
    ]),
    relations([
        relation(kvs([]), rewrites([
            rewrite(kvs([]), expressions([
                union(
                    expression(
                        children([
                            child(
                                usersets([
                                    userset(this([]))
                                ]),
                                errors([])
                            ),
                            child(
                                usersets([
                                    computed_userset(
                                        kvs([
                                            kv(relation, "editor")
                                        ]),
                                        errors([])
                                    )
                                ]),
                                errors([])
                            ),
                            child(
                                usersets([
                                    tuple_to_userset(
                                        tuplesets([
                                            tupleset(
                                                kvs([
                                                    kv(relation, "parent")
                                                ]),
                                                errors([])
                                            )
                                        ]),
                                        computed_usersets([
                                            computed_userset(
                                                kvs([
                                                    kv(object, var('TUPLESET_OBJECT')),
                                                    kv(relation, "viewer")
                                                ]),
                                                errors([])
                                            )
                                        ]),
                                        errors([])
                                    )
                                ]),
                                errors([])
                            )
                        ]),
                        errors([])
                    )
                )
            ]),
            errors([]))
        ]),
        errors([]))
    ]),
    errors([])
    ) )) :- lex("
    name = \"name1\"

    property1 = 1.0
    property2 = 42
    property3 = false
    property4 = $variable

    relation {
        rewrite {
            union {
                child {
                    this {}
                }
                child {
                    computed_userset {
                        relation = \"editor\"
                    }
                }
                child {
                    tuple_to_userset {
                        tupleset {
                            relation = \"parent\"
                        }
                        computed_userset {
                            object = $TUPLESET_OBJECT
                            relation = \"viewer\"
                        }
                    }
                }
            }
        }
    }
    ", Out).

:- end_tests(zanzibar_config_ns_lexer_tests).
