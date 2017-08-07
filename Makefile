PACKAGE = dssl2

default: doc

test:
	racket test/run-dssl2-tests.rkt

all:
	find . -name '*.rkt' | xargs raco make

clean:
	find . -name compiled -type d | xargs rm -R

.PHONY: test all clean


doc: scribblings/$(PACKAGE).scrbl
	raco scribble --dest $@ $^
	echo "<meta http-equiv='refresh' content='0;url=$(PACKAGE).html'>" > doc/index.html

setup:
	raco setup $(PACKAGE)

upload-doc:
	$(MAKE) doc
	ghp-import -n doc
	git push -f git@github.com:tov/dssl2.git gh-pages
