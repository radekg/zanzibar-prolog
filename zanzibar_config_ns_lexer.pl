:- module(zanzibar_config_ns_lexer,
    [lex/2]).

% Enable double-quoted strings for ease of use.
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(zanzibar_config_ns_parser).

program(ast([H|T]), Out) :-
    lex_config([H|T], Out, ns(kvs([]), relations([]), errors([]))).

lex_config([], Acc, Acc).
lex_config([kv(K, V) | T],
    ns(KVs2, Rs2, Es2),
    ns(kvs(KVs), Rs, Es)) :-
    !, lex_config(T, ns(KVs2, Rs2, Es2), ns(kvs([kv(K, V)|KVs]), Rs, Es)).
lex_config([block(relation, Ts) | T],
    ns(KVs2, Rs2, Es2),
    ns(KVs, relations(Rs), Es)) :-
    lex_relation(Ts, Relation, relation(kvs([]), rewrites([]), errors([]))),
    !, lex_config(T, ns(KVs2, Rs2, Es2), ns(KVs, relations([Relation|Rs]), Es)).
lex_config([H|T],
    ns(KVs2, Rs2, Es2),
    ns(KVs, Rs, errors(Es))) :-
    lex_config(T, ns(KVs2, Rs2, Es2), ns(KVs, Rs, errors([error(unexpected, H)|Es]))).

lex_relation([], Acc, Acc).
lex_relation([kv(K, V) | T],
    relation(KVs2, Rs2, Es2),
    relation(kvs(KVs), Rs, Es)) :-
    !, lex_relation(T, relation(KVs2, Rs2, Es2), relation(kvs([kv(K, V)|KVs]), Rs, Es)).
lex_relation([block(rewrite, Ts) | T],
    relation(KVs2, Rs2, Es2),
    relation(KVs, rewrites(Rs), Es)) :-
    lex_rewrite(Ts, Rewrite, rewrite(kvs([]), expressions([]), errors([]))),
    !, lex_relation(T, relation(KVs2, Rs2, Es2), relation(KVs, rewrites([Rewrite|Rs]), Es)).
lex_relation([H|T],
    relation(KVs2, Rs2, Es2),
    relation(KVs, Rs, errors(Es))) :-
    lex_relation(T, relation(KVs2, Rs2, Es2), relation(KVs, Rs, errors([error(unexpected, H)|Es]))).

lex_rewrite([], Acc, Acc).
lex_rewrite([kv(K, V) | T],
    rewrite(KVs2, Exprs2, Es2),
    rewrite(kvs(KVs), Exprs, Es)) :-
    !, lex_rewrite(T, rewrite(KVs2, Exprs2, Es2), rewrite(kvs([kv(K, V)|KVs]), Exprs, Es)).
lex_rewrite([block(ExprType, Ts) | T],
    rewrite(KVs2, Exprs2, Es2),
    rewrite(KVs, expressions(Exprs), Es)) :-
    memberchk(ExprType, [union, intersect, exclude]),
    lex_expression(Ts, Expression, expression(children([]), errors([]))),
    !, Expr =.. [ExprType, Expression],
    lex_rewrite(T, rewrite(KVs2, Exprs2, Es2), rewrite(KVs, expressions([Expr|Exprs]), Es)).
lex_rewrite([H|T],
    rewrite(KVs2, Exprs2, Es2),
    rewrite(KVs, Exprs, errors(Es))) :-
    lex_rewrite(T, rewrite(KVs2, Exprs2, Es2), rewrite(KVs, Exprs, errors([error(unexpected, H)|Es]))).

lex_expression([], Acc, Acc).
lex_expression([block(child, Ts) | T],
    expression(Cs2, Es2),
    expression(children(Cs), Es)) :-
    lex_child(Ts, Child, child(usersets([]), errors([]))),
    !, lex_expression(T, expression(Cs2, Es2), expression(children([Child|Cs]), Es)).
lex_expression([H|T],
    expression(Cs2, Es2),
    expression(Cs, errors(Es))) :-
    lex_expression(T, expression(Cs2, Es2), expression(Cs, errors([error(unexpected, H)|Es]))).

lex_child([], Acc, Acc).
lex_child([block(this, Ctx) | _],
    child(usersets([userset(this(Ctx))|Us]), Es),
    child(usersets(Us), Es)) :-
    !.
lex_child([block(computed_userset, Ts) | T],
    child(Us2, Es2),
    child(usersets(Us), Es)) :-
    lex_computed_userset(Ts, ComputedUserset, computed_userset(kvs([]), errors([]))),
    !, lex_child(T, child(Us2, Es2), child(usersets([ComputedUserset|Us]), Es)).
lex_child([block(tuple_to_userset, Ts) | T],
    child(Us2, Es2),
    child(usersets(Us), Es)) :-
    lex_tuple_to_userset(Ts, TupleToUserset, tuple_to_userset(tuplesets([]), computed_usersets([]), errors([]))),
    !, lex_child(T, child(Us2, Es2), child(usersets([TupleToUserset|Us]), Es)).
lex_child([H|T],
    child(Us2, Es2),
    child(Us, errors(Es))) :-
    lex_child(T, child(Us2, Es2), child(Us, errors([error(unexpected, H)|Es]))).

lex_computed_userset([], Acc, Acc).
lex_computed_userset([kv(K, V) | T],
    computed_userset(KVs2, Es2),
    computed_userset(kvs(KVs), Es)) :-
    !, lex_computed_userset(T, computed_userset(KVs2, Es2), computed_userset(kvs([kv(K, V)|KVs]), Es)).
lex_computed_userset([H|T],
    computed_userset(KVs2, Es2),
    computed_userset(KVs, errors(Es))) :-
    lex_computed_userset(T, computed_userset(KVs2, Es2), computed_userset(KVs, errors([error(unexpected, H)|Es]))).

lex_tupleset([], Acc, Acc).
lex_tupleset([kv(K, V) | T],
    tupleset(KVs2, Es2),
    tupleset(kvs(KVs), Es)) :-
    !, lex_tupleset(T, tupleset(KVs2, Es2), tupleset(kvs([kv(K, V)|KVs]), Es)).
lex_tupleset([H|T],
    tupleset(KVs2, Es2),
    tupleset(KVs, errors(Es))) :-
    lex_tupleset(T, tupleset(KVs2, Es2), tupleset(KVs, errors([error(unexpected, H)|Es]))).

lex_tuple_to_userset([], Acc, Acc).
lex_tuple_to_userset([block(computed_userset, Ts) | T],
    tuple_to_userset(Tss2, Us2, Es2),
    tuple_to_userset(Tss, computed_usersets(Us), Es)) :-
    lex_computed_userset(Ts, ComputedUserset, computed_userset(kvs([]), errors([]))),
    !, lex_tuple_to_userset(T, tuple_to_userset(Tss2, Us2, Es2), tuple_to_userset(Tss, computed_usersets([ComputedUserset|Us]), Es)).
lex_tuple_to_userset([block(tupleset, Ts) | T],
    tuple_to_userset(Tss2, Us2, Es2),
    tuple_to_userset(tuplesets(Tss), Us, Es)) :-
    lex_tupleset(Ts, Tupleset, tupleset(kvs([]), errors([]))),
    !, lex_tuple_to_userset(T, tuple_to_userset(Tss2, Us2, Es2), tuple_to_userset(tuplesets([Tupleset|Tss]), Us, Es)).
lex_tuple_to_userset([H|T],
    tuple_to_userset(Tss2, Us2, Es2),
    tuple_to_userset(Tss, Us, errors(Es))) :-
    lex_tuple_to_userset(T, tuple_to_userset(Tss2, Us2, Es2), tuple_to_userset(Tss, Us, errors([error(unexpected, H)|Es]))).

lex(In, Out) :-
    zanzibar_config_ns_parser:parse(In, Ast),
    program(Ast, Out).
