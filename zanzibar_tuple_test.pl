:- begin_tests(zanzibar_tuple_tests).
% Enable double-quoted strings for ease of use.
:- set_prolog_flag(double_quotes, chars).
:- use_module(zanzibar_tuple).

test(valid_no_userset) :-
    zanzibar_tuple:parse_tuple("ns:object#rel", Out),
    Out = t(o("ns", "object"), r("rel"), u(none)).

test(valid_userset) :-
    zanzibar_tuple:parse_tuple("ns:object#rel@set:object#rel", Out),
    Out = t(o("ns", "object"), r("rel"), u(set(o("set", "object"), r("rel")))).

test(valid_userset_dotdotdot_relation) :-
    zanzibar_tuple:parse_tuple("ns:object#rel@set:object#...", Out),
    Out = t(o("ns", "object"), r("rel"), u(set(o("set", "object"), r("...")))).

test(valid_userid) :-
    zanzibar_tuple:parse_tuple("ns:object#rel@user-id", Out),
    Out = t(o("ns", "object"), r("rel"), u(id("user-id"))).

test(valid_object_id_is_uuid) :-
    zanzibar_tuple:parse_tuple("ns:1e5d2006-89e1-4264-a71a-fd45f3926131#rel@user-id", Out),
    Out = t(o("ns", "1e5d2006-89e1-4264-a71a-fd45f3926131"), r("rel"), u(id("user-id"))).

test(valid_user_id_and_object_id_are_uuid) :-
    zanzibar_tuple:parse_tuple("ns:1e5d2006-89e1-4264-a71a-fd45f3926131#rel@f26fe405-814c-44fd-a188-56cdcefe5de5", Out),
    Out = t(o("ns", "1e5d2006-89e1-4264-a71a-fd45f3926131"), r("rel"), u(id("f26fe405-814c-44fd-a188-56cdcefe5de5"))).

test(invalid_top_level_object_no_ns) :-
    not(zanzibar_tuple:parse_tuple("object#rel@user-id", _Out)).

test(invalid_top_level_dotdotdot_relation) :-
    not(zanzibar_tuple:parse_tuple("ns:object#...@user-id", _Out)).

test(invalid_top_level_relation_is_uuid) :-
    not(zanzibar_tuple:parse_tuple("object#0e7112da-200a-419a-ad65-58cecd1bcfae@user-id", _Out)).

test(invalid_userset_object_no_ns) :-
    not(zanzibar_tuple:parse_tuple("ns:object#rel@object#rel", _Out)).

:- end_tests(zanzibar_tuple_tests).
