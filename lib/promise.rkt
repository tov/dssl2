#lang dssl2

# A PromiseOf[A] is promise(InnerOf[A])
defstruct promise(inner)

# An InnerOf[A] is one of:
#  - incomplete(FunC(A))
#  - complete(A)
defstruct incomplete(thunk)
defstruct complete(value)

# delay : FunC(A) -> PromiseOf[A]
# Creates a promise that will be computed by the given thunk.
def delay(thunk):
    promise(incomplete(thunk))

# forced? : PromiseOf[A] -> Boolean
# Has the promise been forced yet?
def forced?(promise):
    complete?(promise.inner)

# force : PromiseOf[A] -> A
# Gets the value of the promise, forcing if necessary (and remembering).
def force(promise):
    if forced?(promise): promise.inner.value
    else:
        let value = promise.inner.thunk()
        promise.inner= complete(value)
        value

