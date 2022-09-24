% Enable double-quoted strings for ease of use.
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(zanzibar_tokenizer).
:- use_module(zanzibar_utils).

child([token(kw, child), token(scope_s, '{'), token(symbol, '_'), token(scope_e, '}') | T], T, child(this), _).
child([H|T], Rest, Out, Acc) :-
    block([child], [H|T], child_body, Rest, Out, Acc).

computed_userset([H|T], Rest, Out, Acc) :-
    block([computed_userset], [H|T], computed_userset_body, Rest, Out, Acc).

config(tokens(Ts), Out) :- config_body(Ts, Out, []).

expression([H|T], Rest, Out, Acc) :-
    block([union, intersect, exclude], [H|T], expression_body, Rest, Out, Acc).

relation([H|T], Rest, Out, Acc) :-
    block([relation], [H|T], relation_body, Rest, Out, Acc).

rewrite([H|T], Rest, Out, Acc) :-
    block([rewrite], [H|T], rewrite_body, Rest, Out, Acc).

tuple_to_userset([H|T], Rest, Out, Acc) :-
    block([tuple_to_userset], [H|T], tuple_to_userset_body, Rest, Out, Acc).

tupleset([H|T], Rest, Out, Acc) :-
    block([tupleset], [H|T], tupleset_body, Rest, Out, Acc).

% -------------------------------------------|
% Key/value handling:
% -------------------------------------------|
kv([token(kw, K), token(assign,'='), token(qs, V) | T], T, kv(K, V)).
kv([token(kw, K), token(assign,'='), token(float, V) | T], T, kv(K, V)).
kv([token(kw, K), token(assign,'='), token(number, V) | T], T, kv(K, V)).
kv([token(kw, K), token(assign,'='), token(kw, true) | T], T, kv(K, true)).
kv([token(kw, K), token(assign,'='), token(kw, false) | T], T, kv(K, false)).
kv([token(kw, K), token(assign,'='), token(symbol, '$'), token(kw, V) | T], T, kv(K, var(V))).

% -------------------------------------------|
% Child body:
% -------------------------------------------|
child_body([], [], error(unterminated_child, consumed(Acc)), Acc).
child_body([H|T], Rest, Out, Acc) :-
    scope_end([H|T], Rest, Out, Acc),
    !.

% Matching by [H|T] instead of an explicit token results in a wrong case being
% picked up with a very counter-intuitive error message.
child_body([token(kw, computed_userset)|T], Rest, Out, Acc) :-
    computed_userset([token(kw, computed_userset)|T], T2, Item, []),
    !, child_body(T2, Rest, Out, [Item|Acc]).

child_body([token(kw, tuple_to_userset)|T], Rest, Out, Acc) :-
    tuple_to_userset([token(kw, tuple_to_userset)|T], T2, Item, []),
    !, child_body(T2, Rest, Out, [Item|Acc]).

% -------------------------------------------|
% Computed userset body:
% -------------------------------------------|
computed_userset_body([], [], error(unterminated_computed_userset, consumed(Acc)), Acc).
computed_userset_body([H|T], Rest, Out, Acc) :-
    scope_end([H|T], Rest, Out, Acc),
    !.
computed_userset_body([H|T], Rest, Out, Acc) :-
    kv([H|T], T2, Item),
    !, computed_userset_body(T2, Rest, Out, [Item|Acc]).

% -------------------------------------------|
% Config body:
% -------------------------------------------|
config_body([H|T], Out, Acc) :-
    kv([H|T], T2, Item),
    !, config_body(T2, Out, [Item|Acc]).
config_body([H|T], Out, Acc) :-
    relation([H|T], T2, Item, []),
    !, config_body(T2, Out, [Item|Acc]).
config_body([H|T], error(unsupported, rest([H|T])), _).
config_body([], Acc, Acc).

% -------------------------------------------|
% Relation body:
% -------------------------------------------|
relation_body([], [], error(unterminated_relation, consumed(Acc)), Acc).
relation_body([H|T], Rest, Out, Acc) :-
    scope_end([H|T], Rest, Out, Acc),
    !.

relation_body([H|T], Rest, Out, Acc) :-
    kv([H|T], T2, Item),
    !, relation_body(T2, Rest, Out, [Item|Acc]).

relation_body([H|T], Rest, Out, Acc) :-
    rewrite([H|T], T2, Item, []),
    !, relation_body(T2, Rest, Out, [Item|Acc]).

% -------------------------------------------|
% Rewrite body:
% -------------------------------------------|
rewrite_body([], [], error(unterminated_rewrite, consumed(Acc)), Acc).
rewrite_body([H|T], Rest, Out, Acc) :-
    scope_end([H|T], Rest, Out, Acc),
    !.

rewrite_body([H|T], Rest, Out, Acc) :-
    expression([H|T], T2, Item, []),
    !, rewrite_body(T2, Rest, Out, [Item|Acc]).

% -------------------------------------------|
% Expression body:
% -------------------------------------------|
expression_body([], [], error(unterminated_expression, consumed(Acc)), Acc).
% Careful, returns a list only, type is wrapped in relevant expression/4.
expression_body([H|T], Rest, Out, Acc) :-
    scope_end([H|T], Rest, Out, Acc),
    !.

expression_body([H|T], Rest, Out, Acc) :-
    child([H|T], T2, Item, []),
    !, expression_body(T2, Rest, Out, [Item|Acc]).

% -------------------------------------------|
% Tuple to userset body:
% -------------------------------------------|
tuple_to_userset_body([], [], error(unterminated_tuple_to_userset, consumed(Acc)), Acc).
tuple_to_userset_body([H|T], Rest, Out, Acc) :-
    scope_end([H|T], Rest, Out, Acc),
    !.

% Variations require a helper to match properly.
% Matching by [H|T] instead of an explicit token results in a wrong case being
% picked up with a very counter-intuitive error message.
tuple_to_userset_body([token(kw, computed_userset)|T], Rest, Out, Acc) :-
    computed_userset([token(kw, computed_userset)|T], T2, Item, []),
    !, tuple_to_userset_body(T2, Rest, Out, [Item|Acc]).

tuple_to_userset_body([token(kw, tupleset)|T], Rest, Out, Acc) :-
    tupleset([token(kw, tupleset)|T], T2, Item, []), 
    !, tuple_to_userset_body(T2, Rest, Out, [Item|Acc]).

% -------------------------------------------|
% Tupleset body:
% -------------------------------------------|
tupleset_body([], [], error(unterminated_tupleset, consumed(Acc)), Acc).
tupleset_body([H|T], Rest, Out, Acc) :-
    scope_end([H|T], Rest, Out, Acc),
    !.
tupleset_body([H|T], Rest, Out, Acc) :-
    kv([H|T], T2, Item),
    !, tupleset_body(T2, Rest, Out, [Item|Acc]).

% -------------------------------------------|
% General:
% -------------------------------------------|
block(Search, [token(kw, Kw), token(scope_s, '{') | T], Body, Rest, Out, Acc) :-
    memberchk(Kw, Search),
    BodyPred =.. [Body, T, Rest, Res, Acc],
    BodyPred, !,
    zanzibar_utils:error_or_wrap(Res, Kw, Out).

block(Search, [token(kw, Kw), token(scope_s, '{') | _], _, _, Out, _) :-
    \+ memberchk(Kw, Search),
    !, Out = error(expected_block_one_of, choices(Search), found(Kw)).

block(_, [token(Ty, Tv) | _], _, _, Out, _) :-
    Out = error(expected_block_start, found(Ty, Tv)),
    !.

block(_, [], _, _, Out, _) :-
    Out = error(expected_block_start, found(end_of_input)),
    !.

scope_end([H|T], Type, T2, Out, Acc) :-
    [token(scope_e, '}') | T2] = [H|T],
    Out =.. [Type, Acc].
scope_end([H|T], T2, Acc, Acc) :-
    [token(scope_e, '}') | T2] = [H|T].

% -------------------------------------------|
% API:
% -------------------------------------------|
parse_config(In, Out) :-
    zanzibar_tokenizer:tokenize(In, Tokens),
    config(Tokens, Out).

:- begin_tests(zanzibar_ns_config_tests).
:- set_prolog_flag(double_quotes, chars).

test(valid_no_rewrite, [ true( Out = [
    relation([
        kv(name, "owner")
    ]),
    kv(name, "namespace") ])]) :- parse_config("
        name = \"namespace\"
        relation {
            name = \"owner\"
        }
        ", Out).

test(valid_union, [ true( Out = [
    relation([
        rewrite([
            union([
                child([
                    computed_userset([
                        kv(name, "owner")
                    ])
                ]),
                child(this)
            ])
        ]),
        kv(name, "editor")
    ]),
    kv(name, "namespace")])]) :- parse_config("
        name = \"namespace\"
        relation {
            name = \"editor\"
            rewrite {
                union {
                    child { _ }
                    child {
                        computed_userset {
                            name = \"owner\"
                        }
                    }
                }
            }
        }
    ", Out).

test(valid_intersect, [ true( Out = [
    relation([
        rewrite([
            intersect([
                child([
                    computed_userset([
                        kv(name, "owner")
                    ])
                ]),
                child(this)
            ])
        ]),
        kv(name, "editor")
    ]),
    kv(name, "namespace")])]) :- parse_config("
        name = \"namespace\"
        relation {
            name = \"editor\"
            rewrite {
                intersect {
                    child { _ }
                    child {
                        computed_userset {
                            name = \"owner\"
                        }
                    }
                }
            }
        }
    ", Out).

test(valid_exclude, [ true( Out = [
    relation([
        rewrite([
            exclude([
                child([
                    computed_userset([
                        kv(name, "owner")
                    ])
                ]),
                child(this)
            ])
        ]),
        kv(name, "editor")
    ]),
    kv(name, "namespace")])]) :- parse_config("
        name = \"namespace\"
        relation {
            name = \"editor\"
            rewrite {
                exclude {
                    child { _ }
                    child {
                        computed_userset {
                            name = \"owner\"
                        }
                    }
                }
            }
        }
    ", Out).

test(valid_complete, [ true( Out = [
    relation([
        rewrite([
            union([
                child([
                    tuple_to_userset([
                        computed_userset([
                            kv(relation, "viewer"),
                            kv(object, var(tuple_userset_object))
                        ]),
                        tupleset([
                            kv(relation, "parent")
                        ])
                    ])
                ]),
                child([
                    computed_userset([
                        kv(name, "editor")
                    ])
                ]),
                child(this)
            ])
        ]),
        kv(name, "viewer")
    ]),
    relation([
        rewrite([
            union([
                child([
                    computed_userset([
                        kv(name, "owner")
                    ])
                ]),
                child(this)
            ])
        ]),
        kv(name, "editor")
    ]),
    relation([
        kv(prop2, 123),
        kv(name, "owner")
    ]),
    kv(boolean, false),
    kv(property2, 123.4),
    kv(property2, 123),
    kv(name1, "value") ])]) :- parse_config("
        name1=\"value\"
        property2 = 123
        property2 = 123.4
        boolean = false

        relation {
            name = \"owner\"
            prop2 = 123
        }

        relation {
            name = \"editor\"
            rewrite {
                union {
                    child { _ }
                    child {
                        computed_userset {
                            name = \"owner\"
                        }
                    }
                }
            }
        }

        relation {
            name = \"viewer\"
            rewrite {
                union {
                    child { _ }
                    child {
                        computed_userset {
                            name = \"editor\"
                        }
                    }
                    child {
                        tuple_to_userset {
                            tupleset {
                                relation = \"parent\"
                            }
                            computed_userset {
                                object = $tuple_userset_object
                                relation = \"viewer\"
                            }
                        }
                    }
                }
            }
        }
        ", Out).

test(invalid_unsupported_expression, [ true( Out = [
    error(unterminated_relation,consumed([
        error(unterminated_rewrite,consumed([
            error(expected_block_one_of,
                    choices([union,intersect,exclude]),
                    found(unsupported)
            )]
        )),
        kv(name,[e,d,i,t,o,r])
    ])),
    kv(name, "namespace")])]) :- parse_config("
        name = \"namespace\"
        relation {
            name = \"editor\"
            rewrite {
                unsupported {
                    child { _ }
                    child {
                        computed_userset {
                            name = \"owner\"
                        }
                    }
                }
            }
        }
    ", Out).

:- end_tests(zanzibar_ns_config_tests).