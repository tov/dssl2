#lang scribble/manual

@require["common.rkt"]

@title{The @tt{sbox_hash} library}

This library provides a way to create high-quality hash functions based on
@emph{substitution boxes} (from cryptography).

You can use it to create as many different hash functions as you need.

These definitions are not part of the base DSSL2 language, and must be imported
explicitly using: @racket[import sbox_hash]

@section{Class @tt{SboxHash64}}

@tt{SboxHash64} objects are wrappers around a substitution-box-based hash
function. Constructing a new object creates a new hash function.

The @tt{SboxHash64} constructor takes no arguments.

@defmethform[SboxHash64 hash]{@proto[AnyC nat?]}

This method is the hash function proper. It accepts any value and hashes it to
a (possibly very large!) natural number.

@section{Convenience}

@defprocform[make_sbox_hash]{(): @racket[FunC][@racket[AnyC], @racket[nat?]]}

Creates a new standalone substitution-box-based hash function and returns it.
