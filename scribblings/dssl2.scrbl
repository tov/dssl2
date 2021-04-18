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

struct node:
    let data
    let left
    let right

def insert(t, k):
    if t is None:
        t = node(k, None, None)
    elif k < t.key:
        t.left = insert(t.left, k)
    elif k > t.key:
        t.right = insert(t.right, k)
    return t
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
@include-section["array.scrbl"]
