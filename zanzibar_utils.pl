:- module(zanzibar_utils,
    [is_error/1]).

is_error(Term) :-
    compound(Term),
    \+ atom(Term),
    functor(Term, error, _), !.

is_error(Term) :-
    compound(Term),
    \+ atom(Term),
    is_error_compound(Term).

is_error_compound(Term) :-
    compound(Term),
    functor(Term, _, N),
    is_error_compound(Term, 1, N).

is_error_compound(Term, I, N) :-
    I =< N,
    arg(I, Term, Value),
    compound(Value),
    is_error(Value), !.

is_error_compound(Term, I, N) :-
    I < N,
    J is I + 1,
    is_error_compound(Term, J, N), !.
