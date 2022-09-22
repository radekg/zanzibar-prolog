:- module(zanzibar_utils,
    [error_or_wrap/3, is_error/1]).

error_or_wrap(Input, _, Input) :-
    functor(Input, error, _),
    !.

error_or_wrap(Input, Wrap, Out) :-
    atom(Wrap),
    Out =.. [Wrap, Input],
    !.

is_error(Term) :-
    compound(Term),
    \+ atom(Term),
    functor(Term, error, _),
    !.

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
    is_error(Value),
    !.

is_error_compound(Term, I, N) :-
    I < N,
    J is I + 1,
    is_error_compound(Term, J, N),
    !.
