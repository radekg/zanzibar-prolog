:- module(zanzibar_config_ns_parser,
    [parse/2]).

% Enable double-quoted strings for ease of use.
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(zanzibar_config_ns_lexer).

program(ast([H|T]), Out) :-
    parse_config([H|T], Out, ns(kvs([]), relations([]), errors([]))).

parse_config([], Acc, Acc).
parse_config([kv(K, V) | T],
    ns(KVs2, Rs2, Es2),
    ns(kvs(KVs), Rs, Es)) :-
    !, parse_config(T, ns(KVs2, Rs2, Es2), ns(kvs([kv(K, V)|KVs]), Rs, Es)).
parse_config([block(relation, Ts) | T],
    ns(KVs2, Rs2, Es2),
    ns(KVs, relations(Rs), Es)) :-
    parse_relation(Ts, Relation, relation(kvs([]), rewrites([]), errors([]))),
    !, parse_config(T, ns(KVs2, Rs2, Es2), ns(KVs, relations([Relation|Rs]), Es)).
parse_config([H|T],
    ns(KVs2, Rs2, Es2),
    ns(KVs, Rs, errors(Es))) :-
    parse_config(T, ns(KVs2, Rs2, Es2), ns(KVs, Rs, errors([error(unexpected, H)|Es]))).

parse_relation([], Acc, Acc).
parse_relation([kv(K, V) | T],
    relation(KVs2, Rs2, Es2),
    relation(kvs(KVs), Rs, Es)) :-
    !, parse_relation(T, relation(KVs2, Rs2, Es2), relation(kvs([kv(K, V)|KVs]), Rs, Es)).
parse_relation([block(rewrite, Ts) | T],
    relation(KVs2, Rs2, Es2),
    relation(KVs, rewrites(Rs), Es)) :-
    parse_rewrite(Ts, Rewrite, rewrite(kvs([]), expressions([]), errors([]))),
    !, parse_relation(T, relation(KVs2, Rs2, Es2), relation(KVs, rewrites([Rewrite|Rs]), Es)).
parse_relation([H|T],
    relation(KVs2, Rs2, Es2),
    relation(KVs, Rs, errors(Es))) :-
    parse_relation(T, relation(KVs2, Rs2, Es2), relation(KVs, Rs, errors([error(unexpected, H)|Es]))).

parse_rewrite([], Acc, Acc).
parse_rewrite([kv(K, V) | T],
    rewrite(KVs2, Exprs2, Es2),
    rewrite(kvs(KVs), Exprs, Es)) :-
    !, parse_rewrite(T, rewrite(KVs2, Exprs2, Es2), rewrite(kvs([kv(K, V)|KVs]), Exprs, Es)).
parse_rewrite([block(ExprType, Ts) | T],
    rewrite(KVs2, Exprs2, Es2),
    rewrite(KVs, expressions(Exprs), Es)) :-
    memberchk(ExprType, [union, intersect, exclude]),
    parse_expression(Ts, Expression, expression(children([]), errors([]))),
    !, Expr =.. [ExprType, Expression],
    parse_rewrite(T, rewrite(KVs2, Exprs2, Es2), rewrite(KVs, expressions([Expr|Exprs]), Es)).
parse_rewrite([H|T],
    rewrite(KVs2, Exprs2, Es2),
    rewrite(KVs, Exprs, errors(Es))) :-
    parse_rewrite(T, rewrite(KVs2, Exprs2, Es2), rewrite(KVs, Exprs, errors([error(unexpected, H)|Es]))).

parse_expression([], Acc, Acc).
parse_expression([block(child, Ts) | T],
    expression(Cs2, Es2),
    expression(children(Cs), Es)) :-
    parse_child(Ts, Child, child(usersets([]), errors([]))),
    !, parse_expression(T, expression(Cs2, Es2), expression(children([Child|Cs]), Es)).
parse_expression([H|T],
    expression(Cs2, Es2),
    expression(Cs, errors(Es))) :-
    parse_expression(T, expression(Cs2, Es2), expression(Cs, errors([error(unexpected, H)|Es]))).

parse_child([], Acc, Acc).
parse_child([block(this, Ctx) | _],
    child(usersets([userset(this(Ctx))|Us]), Es),
    child(usersets(Us), Es)) :-
    !.
parse_child([block(computed_userset, Ts) | T],
    child(Us2, Es2),
    child(usersets(Us), Es)) :-
    parse_computed_userset(Ts, ComputedUserset, computed_userset(kvs([]), errors([]))),
    !, parse_child(T, child(Us2, Es2), child(usersets([ComputedUserset|Us]), Es)).
parse_child([block(tuple_to_userset, Ts) | T],
    child(Us2, Es2),
    child(usersets(Us), Es)) :-
    parse_tuple_to_userset(Ts, TupleToUserset, tuple_to_userset(tuplesets([]), computed_usersets([]), errors([]))),
    !, parse_child(T, child(Us2, Es2), child(usersets([TupleToUserset|Us]), Es)).
parse_child([H|T],
    child(Us2, Es2),
    child(Us, errors(Es))) :-
    parse_child(T, child(Us2, Es2), child(Us, errors([error(unexpected, H)|Es]))).

parse_computed_userset([], Acc, Acc).
parse_computed_userset([kv(K, V) | T],
    computed_userset(KVs2, Es2),
    computed_userset(kvs(KVs), Es)) :-
    !, parse_computed_userset(T, computed_userset(KVs2, Es2), computed_userset(kvs([kv(K, V)|KVs]), Es)).
parse_computed_userset([H|T],
    computed_userset(KVs2, Es2),
    computed_userset(KVs, errors(Es))) :-
    parse_computed_userset(T, computed_userset(KVs2, Es2), computed_userset(KVs, errors([error(unexpected, H)|Es]))).

parse_tupleset([], Acc, Acc).
parse_tupleset([kv(K, V) | T],
    tupleset(KVs2, Es2),
    tupleset(kvs(KVs), Es)) :-
    !, parse_tupleset(T, tupleset(KVs2, Es2), tupleset(kvs([kv(K, V)|KVs]), Es)).
parse_tupleset([H|T],
    tupleset(KVs2, Es2),
    tupleset(KVs, errors(Es))) :-
    parse_tupleset(T, tupleset(KVs2, Es2), tupleset(KVs, errors([error(unexpected, H)|Es]))).

parse_tuple_to_userset([], Acc, Acc).
parse_tuple_to_userset([block(computed_userset, Ts) | T],
    tuple_to_userset(Tss2, Us2, Es2),
    tuple_to_userset(Tss, computed_usersets(Us), Es)) :-
    parse_computed_userset(Ts, ComputedUserset, computed_userset(kvs([]), errors([]))),
    !, parse_tuple_to_userset(T, tuple_to_userset(Tss2, Us2, Es2), tuple_to_userset(Tss, computed_usersets([ComputedUserset|Us]), Es)).
parse_tuple_to_userset([block(tupleset, Ts) | T],
    tuple_to_userset(Tss2, Us2, Es2),
    tuple_to_userset(tuplesets(Tss), Us, Es)) :-
    parse_tupleset(Ts, Tupleset, tupleset(kvs([]), errors([]))),
    !, parse_tuple_to_userset(T, tuple_to_userset(Tss2, Us2, Es2), tuple_to_userset(tuplesets([Tupleset|Tss]), Us, Es)).
parse_tuple_to_userset([H|T],
    tuple_to_userset(Tss2, Us2, Es2),
    tuple_to_userset(Tss, Us, errors(Es))) :-
    parse_tuple_to_userset(T, tuple_to_userset(Tss2, Us2, Es2), tuple_to_userset(Tss, Us, errors([error(unexpected, H)|Es]))).

parse(In, Out) :-
    zanzibar_config_ns_lexer:lex(In, Ast),
    program(Ast, Out).
