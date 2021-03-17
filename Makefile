PACKAGE       = dssl2
RACOTEST      = raco test
RACOTESTLONG  = $(RACOTEST) ++arg -a

.PHONY: default
default: test

.PHONY: test
test: compiler_test parser_test

# TODO: fix grader_test
.PHONY: long_test
long_test:
	make RACOTEST="$(RACOTESTLONG)" compiler_test # grader_test

.PHONY: compiler_test
compiler_test:
	$(RACOTEST) test/run-dssl2-tests.rkt

.PHONY: grader_test
grader_test:
	$(RACOTEST) test/grader.rkt

.PHONY: parser_test
parser_test:
	$(RACOTEST) test/parser.rkt

BENCH_FILE = test/dssl2/and.rkt
.PHONY: startup_bench
startup_bench:
	raco make $(BENCH_FILE)
	racket $(BENCH_FILE) > /dev/null; \
	time for i in 0 1 2 3 4; do \
		racket $(BENCH_FILE) > /dev/null; \
	done

.PHONY: clean
clean:
	find . -name compiled -type d -print0 | xargs -0 rm -R

.PHONY: fast_setup
fast_setup:
	raco setup -D $(PACKAGE)

.PHONY: setup
setup:
	raco setup $(PACKAGE)
