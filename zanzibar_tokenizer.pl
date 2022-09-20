:- module(zanzibar_tokenizer,
    [tokenize/2]).

:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(zanzibar_utils).

% -------------------------------------------|
% Whitespace, including new line:
% -------------------------------------------|

spaces      --> space, !, spaces.
spaces      --> [].
space       -->
    [C],
    (
        { code_type(C, space) }, !
        | { code_type(C, white) }, !
    ).

% -------------------------------------------|
% Quoted strings definition:
% -------------------------------------------|

double_quote --> "\"".
single_quote --> "'".

quoted(Out) --> double_quoted(Out), !.
quoted(Out) --> single_quoted(Out), !.

double_quoted(Out) --> quoted_terminated(double_quote, '\"', Out), !.
single_quoted(Out) --> quoted_terminated(single_quote, '\'', Out), !.
quoted_terminated(Terminator, TerminatorChar, Out) -->
    Terminator,
    quoted_chars(TerminatorChar, Content),
    { lists:flatten(Content, Out) },
    Terminator.

quoted_chars(TC, [H|T]) --> next_quoted_char(H), { TC \= H }, quoted_chars(TC, T).
quoted_chars(_, [])     --> [].

next_quoted_char(['\\', C]) --> ['\\', C], !.
next_quoted_char(E)         --> ['\r','\n'], { E = error(quoted_multiline) }.
next_quoted_char(E)         --> ['\n'],      { E = error(quoted_multiline) }.
next_quoted_char(E)         --> ['\r'],      { E = error(quoted_multiline) }.
next_quoted_char(H)         --> [H], !.

% -------------------------------------------|
% Keyword and tokens definition:
% -------------------------------------------|

keyword([H|T]) --> [H], { char_type(H, alnum) }, !, keyword(T).
keyword([])    --> [].

tokens([H|T]) --> spaces, token(H), !,
    (
        { zanzibar_utils:is_error(H) }, ! % Abort reading tokens as soon as there's an error.
        | spaces, tokens(T), !            % Otherwise, continue reading tokens.
    ).
tokens([]) --> [].

% -------------------------------------------|
% Read numbers:
% -------------------------------------------|
% Documentation says that the number//1 predicate handles floats but it doesn't look like it.
% Code suggests so: https://github.com/SWI-Prolog/swipl-devel/blob/96d60f7327a53e2fe3ec3c0ceff49ed7af4c1fd4/library/dcg/basics.pl#L304-L333.
% But...:
%   phrase(number(X), "123.4") gives false.
% Surprisingly:
%   phrase(float(X), "123.4") also gives false.
% Whatever, let's do this ourselves.

to_number(['0', '.', X], Out) :-
    format(string(S), "0.~d", [X]),
    number_string(Out, S).
to_number([X, '.', '0'], Out) :-
    format(string(S), "~d.0", [X]),
    number_string(Out, S).
to_number([X, '.', Y], Out) :-
    format(string(S), "~d.~d", [X, Y]),
    number_string(Out, S).

token(token(float, Out)) --> number(N1), ['.'], number(N2), !,
    { lists:flatten([N1, ['.'], N2], L) },
    { to_number(L, Out) }.
token(token(float, Out)) --> ['.'], number(N1), !,
    { lists:flatten([['0','.'], N1], L) },
    { to_number(L, Out) }.
token(token(float, Out)) --> number(N1), ['.'], !,
    { lists:flatten([N1, ['.','0']], L) },
    { to_number(L, Out) }.
token(token(number, N)) --> number(N).

% -------------------------------------------|
% Read symbols:
% -------------------------------------------|
% After numbers because we have the special treating of .\d+ as a float.

token(token(symbol, '...')) --> ['.','.','.'], !.
token(token(symbol, '..')) --> ['.','.'], !.
token(token(symbol, '<=')) --> ['<','='], !.
token(token(symbol, '>=')) --> ['>','='], !.
token(token(symbol, '==')) --> ['=','='], !.
token(token(symbol, '!=')) --> ['!','='], !.

% Handle escape sequences, even invalid ones.
% Any single character prefixed with \ becomes a symbol.
% This handles dangling double and single quotes.
token(token(symbol, C)) --> ['\\', C], !.

token(token(scope_s, '{')) --> ['{'], !.
token(token(scope_e, '}')) --> ['}'], !.
token(token(array_s, '[')) --> ['['], !.
token(token(array_e, ']')) --> [']'], !.
token(token(symbol, '.')) --> ['.'], !.
token(token(symbol, '(')) --> ['('], !.
token(token(symbol, ')')) --> [')'], !.
token(token(symbol, '>')) --> ['>'], !.
token(token(symbol, '/')) --> ['/'], !.
token(token(symbol, '|')) --> ['|'], !.
token(token(symbol, '<')) --> ['<'], !.
token(token(symbol, '~')) --> ['~'], !.
token(token(symbol, '`')) --> ['`'], !.
token(token(symbol, '!')) --> ['!'], !.
token(token(symbol, '@')) --> ['@'], !.
token(token(symbol, '#')) --> ['#'], !.
token(token(symbol, '$')) --> ['$'], !.
token(token(symbol, '%')) --> ['%'], !.
token(token(symbol, '^')) --> ['^'], !.
token(token(symbol, '&')) --> ['&'], !.
token(token(symbol, '*')) --> ['*'], !.
token(token(symbol, '_')) --> ['_'], !.
token(token(symbol, '-')) --> ['-'], !.
token(token(symbol, '+')) --> ['+'], !.
token(token(symbol, ',')) --> [','], !.
token(token(symbol, ':')) --> [':'], !.
token(token(assign, '=')) --> ['='], !.

% -------------------------------------------|
% Read quoted strings:
% -------------------------------------------|
% Quoted strings: first, try reading as a quoted string.
% On backtracking, if we have a quote, it's an unterminated quoted string.

token(token(qs, Kw)) --> {Kw = [_|_]}, quoted(Kw), !.
token(error(unterminated, '\"', R)) --> ['\"'], remainder(R), !.
token(error(unterminated, '\'', R)) --> ['\''], remainder(R), !.

% -------------------------------------------|
% Read keywords:
% -------------------------------------------|

token(token(kw, Kw)) --> {Kw = [_|_]}, keyword(Kw), !.

% -------------------------------------------|
% Catch-all unknown input:
% -------------------------------------------|

token(error(unknown, C, R)) --> [C], remainder(R), !.

% -------------------------------------------|
% API:
% -------------------------------------------|

tokenize(Input, tokens(Out)) :-
    Phrase =.. [tokens, Out],
    phrase(Phrase, Input).
