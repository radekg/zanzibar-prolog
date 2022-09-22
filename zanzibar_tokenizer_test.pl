:- begin_tests(zanzibar_tokenizer_tests).

% Enable double-quoted strings for ease of use.
:- set_prolog_flag(double_quotes, chars).
:- use_module(zanzibar_tokenizer).

test(valid_keywords, [ true(
    Out=tokens([token(kw, kw1), token(kw, kw2)])
    )]) :- zanzibar_tokenizer:tokenize("kw1 kw2", Out).

test(valid_floats, [ true(
    Out=tokens([token(float, 1234.56), token(float, 0.123), token(float, 987.0)])
    )]) :- zanzibar_tokenizer:tokenize("1234.56 .123 987.", Out).

test(valid_int, [ true(
    Out=tokens([token(number, 42), token(number, 2022)])
    )]) :- zanzibar_tokenizer:tokenize("   42    2022   ", Out).

test(valid_tokens, [ true(
    Out=tokens([
        token(kw, name),
        token(assign, '='),
        token(qs, "value"),
        token(kw, relation),
        token(scope_s, '{'),
        token(symbol, '+'),
        token(symbol, '-'),
        token(symbol, '\\'),
        token(symbol, '\"'),
        token(symbol, '\''),
        token(qs, "\""),
        token(symbol, '=='),
        token(symbol, '!='),
        token(symbol, '...'),
        token(symbol, '.'),
        token(kw, nested),
        token(array_s, '['),
        token(array_e, ']'),
        token(qs, "single quoted"),
        token(scope_e, '}')])
    )]) :- zanzibar_tokenizer:tokenize("name = \"value\" relation { +- \\\\ \\\" \\' '\"' == != ... . nested [] 'single quoted' }", Out).

test(handles_unterminated_double_quoted, [ true(
    Out=tokens([
        token(kw, name),
        token(assign, '='),
        token(qs, "value"),
        token(kw, relation),
        token(scope_s, '{'),
        error(unterminated, '\"', "unterminated follows")
        | _])
    )]) :- zanzibar_tokenizer:tokenize("name = \"value\" relation { \"unterminated follows", Out).

test(handles_unterminated_single_quoted, [ true(
    Out=tokens([
        token(kw, name),
        token(assign, '='),
        token(qs, "value"),
        token(kw, relation),
        token(scope_s, '{'),
        error(unterminated, '\'', "unterminated follows")
        | _])
    )]) :- zanzibar_tokenizer:tokenize("name = \"value\" relation { 'unterminated follows", Out).

:- end_tests(zanzibar_tokenizer_tests).
