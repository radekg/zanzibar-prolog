:- module(zanzibar_tuple,
    [parse_tuple/2]).

% Enable double-quoted strings for ease of use.
:- set_prolog_flag(double_quotes, chars).
:- use_module(zanzibar_tokenizer).

% Per the Zanzibar whitepaper:
% -------------------------------------------|
% ⟨tuple⟩ ::= ⟨object⟩‘#’⟨relation⟩‘@’⟨user⟩
% ⟨object⟩ ::= ⟨namespace⟩‘:’⟨object id⟩
% ⟨user⟩ ::= ⟨user id⟩ | ⟨userset⟩
% ⟨userset⟩ ::= ⟨object⟩‘#’⟨relation⟩
% -------------------------------------------|

tuple(List, t(Object, Relation, User)) :-
    object(List, Object, R1),
    relation(R1, Relation, R2),
    user(R2, User, _), !.

tuple(List, E) :- object(List, _, E), E = error(_, _), !.

object(error(E1, E2), error(E1, E2), []).
object([token(kw, Ns), token(symbol, ':'), token(kw, Obj) | T], o(Ns, Obj), T).
object(Tokens, error(expected, object), Tokens).

relation(error(E1, E2), error(E1, E2), []).
relation([token(symbol, '#'), token(kw, Relation)| T], r(Relation), T).
relation(Tokens, error(expected, relation), Tokens).

user(error(E1, E2), error(E1, E2), []).
user([token(symbol, '@'), token(kw, Ns), token(symbol, ':'), token(kw, Obj), token(symbol, '#'), token(kw, Rel) | T], u(set(o(Ns, Obj), r(Rel))), T).
user([token(symbol, '@'), token(kw, Ns), token(symbol, ':'), token(kw, Obj), token(symbol, '#'), token(symbol, '...') | T], u(set(o(Ns, Obj), r("..."))), T).
user([token(symbol, '@'), token(kw, Id) | []], u(id(Id)), []).
user([token(symbol, '@'), token(number, Id) | []], u(id(Id)), []).
user([], u(none), []).
user(Tokens, error(expected, user_or_userset, received(Tokens)), Tokens).

parse_tuple(In, Out) :-
    zanzibar_tokenizer:tokenize(In, tokens(Tokens)),
    tuple(Tokens, Out).
