#lang scribble/manual

@require["common.rkt"]

@title[#:style 'toc]{DSSL2: Data Structures Student Language}
@author{Jesse A. Tov <jesse@"@"cs.northwestern.edu>}

@defmodulelang[dssl2]

DSSL2 is a programming language for data structures students. It’s
designed to be simple, providing only the basic elements needed to
implement a variety of data structures.

DSSL2 uses alignment and indentation to delimit @deftech{blocks}. In
particular, compound statements such as
@racket[if]-@racket[elif]-@racket[else] take @nt[block]s for each
condition, where a @nt[block] can be either one simple statement
followed by a newline, or a sequence of statements on subsequent lines
that are all indented by four additional spaces. Here is an example of a
tree insertion function written using indentation:

@codeblock|{
#lang dssl2

def insert(t, k):
    if empty?(t): new_node(k)
    elif zero?(random(size(t) + 1)):
        root_insert(t, k)
    elif k < t.key:
        t.left = insert(t.left, k)
        fix_size(t)
        t
    elif k > t.key:
        t.right = insert(t.right, k)
        fix_size(t)
        t
    else: t
}|

Each block follows a colon and newline, and is indented 4 spaces more
than the previous line. In particular, the block started by @racket[def]
is indented by 4 spaces, and the @racket[elif] blocks by
8. When a block is a simple statement, it can be placed on the same
line, as in the @racket[if] and @racket[else] cases.

Extraneous indentation is an error.

@include-section["formal-grammar.scrbl"]
@include-section["lexical-syntax.scrbl"]
@include-section["statement-forms.scrbl"]
@include-section["expression-forms.scrbl"]
@include-section["built-ins.scrbl"]
@include-section["contracts.scrbl"]
@include-section["cons.scrbl"]
@include-section["sbox_hash.scrbl"]
