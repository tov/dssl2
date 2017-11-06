#lang setup/infotab
(define name "dssl2: a data structures student language")
(define categories '(devtools))
(define can-be-loaded-with 'all)
(define required-core-version "6.10")
(define version "2.11.4")
(define deps '("base" "rackunit-lib" "parser-tools-lib" "sandbox-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/dssl2.scrbl" ())))
(define blurb
    '("A language for data structures students, version 2"))
(define release-notes
    '((p "First release")))
