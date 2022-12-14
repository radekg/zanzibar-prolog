:- module(zanzibar_config_ns_lexer,
    [lex/2]).

% Enable double-quoted strings for ease of use.
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(zanzibar_tokenizer).
:- use_module(zanzibar_utils).

config(tokens(Ts), Out) :- block_body(Ts, _, Out, []).

block_body([], [], Acc, Acc) :- !.

block_body([H|T], Rest, Out, Acc) :-
    scope_end([H|T], Rest, Out, Acc),
    !.

block_body([H|T], Rest, Out, Acc) :-
    kv([H|T], T2, Item),
    !, block_body(T2, Rest, Out, [Item|Acc]).

block_body([H|T], Rest, Out, Acc) :-
    block([H|T], T2, Item),
    !, block_body(T2, Rest, Out, [Item|Acc]).

block_body([H|T], _, error(invalid, expected([block,kv]), remaining([H|T]), captured(Acc)), Acc).

block([], [], Acc, Acc).
block([token(kw, Kw), token(scope_s, '{') | T], Rest, block(Kw, Out)) :-
    block_body(T, Rest, Out, []), !.

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
% General:
% -------------------------------------------|

scope_end([H|T], T2, Acc, Acc) :-
    [token(scope_e, '}') | T2] = [H|T].

% -------------------------------------------|
% API:
% -------------------------------------------|
lex(In, ast(Out)) :-
    zanzibar_tokenizer:tokenize(In, Tokens),
    config(Tokens, Out).
