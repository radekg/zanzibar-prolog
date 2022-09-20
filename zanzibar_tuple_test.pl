:- begin_tests(zanzibar_tuple_tests).

% Enable double-quoted strings for ease of use.
:- set_prolog_flag(double_quotes, chars).
:- use_module(zanzibar_tuple).

test(valid_no_userset, [ true(
    Out=t(
        o("ns", "object"),
        r("rel"),
        u(none)
    ))]) :- zanzibar_tuple:parse_tuple("ns:object#rel", Out).

test(valid_userset, [ true(
        Out=t(
            o("ns", "object"),
            r("rel"),
            u(set(o("set", "object"), r("rel")))
    ))]) :- zanzibar_tuple:parse_tuple("ns:object#rel@set:object#rel", Out).

test(valid_userset_dotdotdot_relation, [ true(
    Out=t(
        o("ns", "object"), 
        r("rel"), 
        u(set(o("set", "object"), r("...")))
    ))]) :- zanzibar_tuple:parse_tuple("ns:object#rel@set:object#...", Out).

test(valid_userid, [ true(
    Out=t(
        o("ns", "object"), 
        r("rel"), 
        u(id("userid"))
    ))]) :- zanzibar_tuple:parse_tuple("ns:object#rel@userid", Out).

test(valid_numeric_userid, [ true(
    Out=t(
        o("ns", "object"), 
        r("rel"), 
        u(id(42))
    ))]) :- zanzibar_tuple:parse_tuple("ns:object#rel@42", Out).

test(invalid_objectid, [ true(
    Out=t(
        o("ns", "object"),
        error(expected, relation),
        error(expected, user_or_userset,
            received([
                token(symbol,'-'),
                token(kw,"id"),
                token(symbol,'#'),
                token(kw,"rel"),
                token(symbol,'@')]))
    ))]) :- zanzibar_tuple:parse_tuple("ns:object-id#rel@", Out).

test(invalid_userid, [ true(
    Out=t(
        o("ns", "object"),
        r("rel"),
        error(expected, user_or_userset,
            received([
                token(symbol, '@'),
                token(kw, "user"),
                token(symbol, '-'),
                token(kw, "id")]))
    ))]) :- zanzibar_tuple:parse_tuple("ns:object#rel@user-id", Out).

test(invalid_top_level_object_no_ns, [ true(
    Out=t(
        error(expected, object),
        error(expected, relation),
        error(expected, user_or_userset,
            received([
                token(kw,"object"),
                token(symbol, '#'),
                token(kw, "rel"),
                token(symbol, '@'),
                token(kw, "user"),
                token(symbol,'-'),
                token(kw,"id")]))
    ))]) :- zanzibar_tuple:parse_tuple("object#rel@user-id", Out).

test(invalid_top_level_dotdotdot_relation, [ true(
    Out=t(
        o("ns", "object"),
        error(expected, relation),
        error(expected, user_or_userset,
            received([
                token(symbol, '#'),
                token(symbol, '...'),
                token(symbol, '@'),
                token(kw, "user"),
                token(symbol, '-'),
                token(kw, "id")]))
    ))]) :- zanzibar_tuple:parse_tuple("ns:object#...@user-id", Out).

test(invalid_userset_object_no_ns, [ true(
    Out=t(
        o("ns", "object"),
        r("rel"),
        error(expected, user_or_userset,
            received([
                token(symbol, '@'),
                token(kw, "object"),
                token(symbol, '#'),
                token(kw, "rel")]))
    ))]) :- zanzibar_tuple:parse_tuple("ns:object#rel@object#rel", Out).

:- end_tests(zanzibar_tuple_tests).
