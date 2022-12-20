#lang scribble/manual

@require["common.rkt"]

@title[#:style 'toc]{DSSL2: Data Structures Student Language}
@author[@author+email["Jesse A. Tov" "jesse@cs.northwestern.edu"] @author+email["Vincent St-Amour" "stamourv@cs.northwestern.edu"]]

@defmodulelang[dssl2]

DSSL2 is a programming language for data structures students. It’s designed to
be simple, providing the essential building blocks one needs to implement a
variety of data structures, but leaving out the finished products for students
to build themselves.

DSSL2 is a close cousin of Python, with a similar indentation-sensitive syntax
and similar function and keyword names when possible. For example, this is a
DSSL2 program:

@codeblock|{
#lang dssl2

struct node:
    let key
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

let t1 = insert(None, 1)
let t2 = insert(t1, 2)
let t3 = insert(t2, 3)
}|

Like Python, DSSL2 uses alignment and indentation to delimit
@deftech{blocks}. In particular, compound statements such as @racket[def] or
@racket[if]-@racket[elif]-@racket[else] take @nt[block]s for each condition,
where a @nt[block] can be either one simple statement followed by a newline, or
a sequence of statements on subsequent lines that are all indented by four
additional spaces. Extraneous indentation is an error.

DSSL2 deviates from Python in a number of places for pedagogical reasons.
Some of these deviations include (non-exhaustive list):

@itemlist[
@item{@bold{the base data structures}: replaced instead by basic arrays and structs}
@item{@bold{the object system}: which emphasizes encapsulation and interfaces}
@item{@bold{variable declarations}: to avoid Python's scope gotchas.}
]

Nonetheless, students familiar with Python should feel at home in DSSL2. And
students learning DSSL2 should have a head start when learning Python.

@include-section["formal-grammar.scrbl"]
@include-section["lexical-syntax.scrbl"]
@include-section["statement-forms.scrbl"]
@include-section["expression-forms.scrbl"]
@include-section["built-ins.scrbl"]
@include-section["contracts.scrbl"]
@include-section["cons.scrbl"]
@include-section["sbox_hash.scrbl"]
