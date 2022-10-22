.PHONY: test
test: test-config-ns-lexer test-config-ns-parser test-tokenizer test-tuple test-utils

.PHONY: test-config-ns-lexer
test-config-ns-lexer:
	swipl -g run_tests -t halt zanzibar_config_ns_lexer_test.pl

.PHONY: test-config-ns-parser
test-config-ns-parser:
	swipl -g run_tests -t halt zanzibar_config_ns_parser_test.pl

.PHONY: test-tokenizer
test-tokenizer:
	swipl -g run_tests -t halt zanzibar_tokenizer_test.pl

.PHONY: test-tuple
test-tuple:
	swipl -g run_tests -t halt zanzibar_tuple_test.pl

.PHONY: test-utils
test-utils:
	swipl -g run_tests -t halt zanzibar_utils_test.pl