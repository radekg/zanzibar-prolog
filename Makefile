.PHONY: test
test: test-tuple test-utils

.PHONY: test-tokenizer
test-tokenizer:
	swipl -g run_tests -t halt zanzibar_tokenizer_test.pl

.PHONY: test-tuple
test-tuple:
	swipl -g run_tests -t halt zanzibar_tuple_test.pl

.PHONY: test-utils
test-utils:
	swipl -g run_tests -t halt zanzibar_utils_test.pl