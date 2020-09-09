PACKAGE       = dssl2
RACOTEST      = raco test
RACOTESTLONG  = $(RACOTEST) ++arg -a

default: lang

test: compiler_test parser_test

# TODO: fix grader_test
long_test:
	make RACOTEST="$(RACOTESTLONG)" compiler_test # grader_test

compiler_test: lang
	$(RACOTEST) test/run-dssl2-tests.rkt

grader_test: lang
	$(RACOTEST) test/grader.rkt

parser_test: lang
	$(RACOTEST) test/parser.rkt

BENCH_FILE = test/dssl2/and.rkt

startup_bench:
	raco make $(BENCH_FILE)
	racket $(BENCH_FILE) > /dev/null; \
	time for i in 0 1 2 3 4; do \
		racket $(BENCH_FILE) > /dev/null; \
	done

all:
	find . -name '*.rkt' | xargs raco make

lang:
	raco make language.rkt lang/reader.rkt

clean:
	find . -name compiled -type d | xargs rm -R

DOC_SRC = $(shell git ls-files scribblings)

doc: scribblings/$(PACKAGE).scrbl $(DOC_SRC)
	raco make scribblings/*.rkt
	raco scribble --dest $@ $<

setup:
	raco setup $(PACKAGE)

.PHONY: default all clean lang setup
.PHONY: test long_test compiler_test parser_test grader_test
