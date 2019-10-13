PACKAGE = dssl2

default: lang

hard: doc

test: lang
	raco test test/parser.rkt test/run-dssl2-tests.rkt

long_test: test
	raco test test/grader.rkt

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

.PHONY: test all clean lang

DOC_SRC = $(shell git ls-files scribblings)

doc: scribblings/$(PACKAGE).scrbl $(DOC_SRC)
	raco make scribblings/*.rkt
	raco scribble --dest $@ $<

setup:
	raco setup $(PACKAGE)
