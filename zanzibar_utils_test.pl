:- begin_tests(zanzibar_utils_test).
:- use_module(zanzibar_utils).

test(error_wrap_error, [ true( Out = error(input, data) ) ]) :-
    zanzibar_utils:error_or_wrap(error(input, data), wrap, Out).
    
test(error_wrap_non_error, [ true( Out = wrap([input, data]) ) ]) :-
    zanzibar_utils:error_or_wrap([input, data], wrap, Out).

test(direct_error) :-
    zanzibar_utils:is_error(error(nil)).

test(direct_error_multi_arity) :-
    zanzibar_utils:is_error(error(nil, nil)).

test(error_in_container) :-
    zanzibar_utils:is_error(container([just, a, list], error(nil, nil))).

test(error_in_nested_container) :-
    zanzibar_utils:is_error(container([just, a, list], nested(error(nil, nil)))).

test(error_in_list_container) :-
    zanzibar_utils:is_error(container([just, an, error(nil)])).

test(error_in_list_nested_container) :-
    zanzibar_utils:is_error(container([just, an, nested(error(nil))])).

test(not_an_error) :-
    \+ zanzibar_utils:is_error(container([not, an, error])).

test(not_an_error_when_atom) :-
    \+ zanzibar_utils:is_error(error).

:- end_tests(zanzibar_utils_test).