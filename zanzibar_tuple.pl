:- module(zanzibar_tuple,
    [parse_tuple/2]).

% Enable double-quoted strings for ease of use.
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).

% Per the Zanzibar whitepaper:
% -------------------------------------------|
% ⟨tuple⟩ ::= ⟨object⟩‘#’⟨relation⟩‘@’⟨user⟩
% ⟨object⟩ ::= ⟨namespace⟩‘:’⟨object id⟩
% ⟨user⟩ ::= ⟨user id⟩ | ⟨userset⟩
% ⟨userset⟩ ::= ⟨object⟩‘#’⟨relation⟩
% -------------------------------------------|

at             --> "@".
colon          --> ":".
hash           --> "#".
dotdotdot(Out) --> "...", { Out = "..." }.

alphanum([H|T])  --> alphanum_char(H), !, alphanum(T).
alphanum([])     --> [].
alphanum_char(L) --> [L], { char_type(L, alnum) }.

object_or_subject([H|T])  --> object_or_subject_char(H), !, object_or_subject(T).
object_or_subject([])     --> [].
object_or_subject_char(L) --> [L],
    (
        { char_type(L, alnum) }, !
        | { char_code(L, 45) }, ! % for -
    ).

tuple(t(O, R, U)) -->
    object(O),
    hash,
    relation(R),
    (
        at, user(U), !
        | [], { U = u(none) } % for the object#relation top level tuple
    ).

object(o(Ns, Name)) -->
    alphanum(Ns),
    colon,
    object_or_subject(Name).

relation_dotdotdot(r(Out)) --> dotdotdot(Out).
relation(r(Out)) --> alphanum(Out).

user(u(Out)) -->
    (
        userset(Out), !
        | user_id(Out), !
    ).

userset(set(Object, Relation)) -->
    object(Object),
    hash,
    (
        relation_dotdotdot(Relation), !
        | relation(Relation) 
    ).

user_id(id(Id)) -->
    object_or_subject(Id).

parse_tuple(In, Out) :- phrase(tuple(Out), In).
