PACKAGE = dssl2

default: doc

doc: scribblings/$(PACKAGE).scrbl
	raco scribble --dest $@ $^
	echo "<meta http-equiv='refresh' content='0;url=$(PACKAGE).html'>" > doc/index.html

setup:
	raco setup $(PACKAGE)

upload-doc:
	$(MAKE) doc
	ghp-import -n doc
	git push -f git@github.com:tov/dssl.git gh-pages
