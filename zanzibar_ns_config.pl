% Enable double-quoted strings for ease of use.
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(zanzibar_tokenizer).

child([token(kw, "child"), token(scope_s, '{') | T], Rest, Out, Acc) :-
    child_body(T, Rest, Out, Acc).

computed_userset([token(kw, "computed_userset"), token(scope_s, '{') | T], Rest, Out, Acc) :-
    computed_userset_body(T, Rest, Out, Acc).

config(tokens(Ts), Out) :- config_body(Ts, Out, []).

expression([token(kw, "union"), token(scope_s, '{') | T], Rest, union(Out), Acc) :- expression_body(T, Rest, Out, Acc).
expression([token(kw, "intersect"), token(scope_s, '{') | T], Rest, intersect(Out), Acc) :- expression_body(T, Rest, Out, Acc).
expression([token(kw, "exclude"), token(scope_s, '{') | T], Rest, exclude(Out), Acc) :- expression_body(T, Rest, Out, Acc).
expression([token(kw, Unknown), token(scope_s, '{') | _], [], error(unsupported_expression, Unknown), []).

relation([token(kw, "relation"), token(scope_s, '{') | T], Rest, Out, Acc) :-
    relation_body(T, Rest, Out, Acc).

rewrite([token(kw, "rewrite"), token(scope_s, '{') | T], Rest, Out, Acc) :-
    rewrite_body(T, Rest, Out, Acc).

tuple_to_userset([token(kw, "tuple_to_userset"), token(scope_s, '{') | T], Rest, Out, Acc) :-
    tuple_to_userset_body(T, Rest, Out, Acc).
tupleset([token(kw, "tupleset"), token(scope_s, '{') | T], Rest, Out, Acc) :-
    tupleset_body(T, Rest, Out, Acc).

kv([token(kw, K), token(assign,'='), token(qs, V) | T], T, kv(K, V)).
kv([token(kw, K), token(assign,'='), token(float, V) | T], T, kv(K, V)).
kv([token(kw, K), token(assign,'='), token(number, V) | T], T, kv(K, V)).
kv([token(kw, K), token(assign,'='), token(kw, "true") | T], T, kv(K, true)).
kv([token(kw, K), token(assign,'='), token(kw, "false") | T], T, kv(K, false)).

child_body([], [], error(unterminated_child, consumed(Acc)), Acc).
child_body([H|T], Rest, Out, Acc) :- scope_end([H|T], child, Rest, Out, Acc).
child_body([H|T], Rest, Out, Acc) :- computed_userset([H|T], T2, Item, []), !, child_body(T2, Rest, Out, [Item|Acc]).
child_body([H|T], Rest, Out, Acc) :- tuple_to_userset([H|T], T2, Item, []), !, child_body(T2, Rest, Out, [Item|Acc]).
child_body([token(symbol, '_'), token(scope_e, '}') | T], T, child(this), _).

computed_userset_body([], [], error(unterminated_computed_userset, consumed(Acc)), Acc).
computed_userset_body([H|T], Rest, Out, Acc) :- scope_end([H|T], computed_userset, Rest, Out, Acc).
computed_userset_body([H|T], Rest, Out, Acc) :- kv([H|T], T2, Item), !, computed_userset_body(T2, Rest, Out, [Item|Acc]).

config_body([H|T], Out, Acc) :- kv([H|T], T2, Item), !, config_body(T2, Out, [Item|Acc]).
config_body([H|T], Out, Acc) :- relation([H|T], T2, Item, []), !, config_body(T2, Out, [Item|Acc]).
config_body([H|T], error(unsupported, rest([H|T])), _).
config_body([], Acc, Acc).

relation_body([], [], error(unterminated_relation, consumed(Acc)), Acc).
relation_body([H|T], Rest, Out, Acc) :- scope_end([H|T], relation, Rest, Out, Acc).
relation_body([H|T], Rest, Out, Acc) :- kv([H|T], T2, Item), !, relation_body(T2, Rest, Out, [Item|Acc]).
relation_body([H|T], Rest, Out, Acc) :- rewrite([H|T], T2, Item, []), !, relation_body(T2, Rest, Out, [Item|Acc]).

rewrite_body([], [], error(unterminated_rewrite, consumed(Acc)), Acc).
rewrite_body([H|T], Rest, Out, Acc) :- scope_end([H|T], rewrite, Rest, Out, Acc).
rewrite_body([H|T], Rest, Out, Acc) :- expression([H|T], T2, Item, []), !, rewrite_body(T2, Rest, Out, [Item|Acc]).

expression_body([], [], error(unterminated_expression, consumed(Acc)), Acc).
% Careful, returns a list only, type is wrapped in relevant expression/4.
expression_body([H|T], Rest, Out, Acc) :- scope_end([H|T], Rest, Out, Acc).
expression_body([H|T], Rest, Out, Acc) :- child([H|T], T2, Item, []), !, expression_body(T2, Rest, Out, [Item|Acc]).

tuple_to_userset_body([], [], error(unterminated_tuple_to_userset, consumed(Acc)), Acc).
tuple_to_userset_body([H|T], Rest, Out, Acc) :- scope_end([H|T], tupleset_to_userset, Rest, Out, Acc).
tuple_to_userset_body([H|T], Rest, Out, Acc) :- computed_userset([H|T], T2, Item, []), !, tuple_to_userset_body(T2, Rest, Out, [Item|Acc]).
tuple_to_userset_body([H|T], Rest, Out, Acc) :- tupleset([H|T], T2, Item, []), !, tuple_to_userset_body(T2, Rest, Out, [Item|Acc]).

tupleset_body([], [], error(unterminated_tupleset, consumed(Acc)), Acc).
tupleset_body([H|T], Rest, Out, Acc) :- scope_end([H|T], tupleset, Rest, Out, Acc).
tupleset_body([H|T], Rest, Out, Acc) :- kv([H|T], T2, Item), !, tupleset_body(T2, Rest, Out, [Item|Acc]).

scope_end([H|T], Type, T2, Out, Acc) :-
    [token(scope_e, '}') | T2] = [H|T],
    Out =.. [Type, Acc].
scope_end([H|T], T2, Acc, Acc) :-
    [token(scope_e, '}') | T2] = [H|T].

parse_config(In, Out) :-
    zanzibar_tokenizer:tokenize(In, Tokens),
    config(Tokens, Out).

example(Out) :-
    parse_config("
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
            child { tuple_to_userset {
                tupleset {
                    relation = \"parent\"
                }
                computed_userset {
                    object = 0
                    relation = \"viewer\"
                }
            } }
        }
    }
}

", Out).