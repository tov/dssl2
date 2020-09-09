#lang setup/infotab
(define name "dssl2: a data structures student language")
(define categories '(devtools))
(define can-be-loaded-with 'all)
(define required-core-version "6.10")
(define version "2.21.1")
(define deps '("base"
               "gui-lib"
               "rackunit-lib"
               "parser-tools-lib"
               "plot-gui-lib"
               "plot-lib"
               "sandbox-lib"
               "snip-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/dssl2.scrbl" (multi-page))))
(define test-omit-paths '("test/dssl2" "test/grader"))
(define blurb
  '("A language for data structures students, version 2"))
(define release-notes
  '((p "No longer the first release")))
