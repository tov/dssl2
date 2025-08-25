#lang dssl2

# regression test for bad contract error message
# previously, using a non-contract as contract parameter (i.e., `T`)
#   would lead to `contract-name` being called on a non-contract

interface QUEUE[T]:
    def enqueue(self, element: T) -> NoneC

class ListQueue[T] (QUEUE):
    def __init__ (self): pass
    # need at leat one method that uses `T` to trigger the issue
    def enqueue(self, element: T) -> NoneC: pass

struct song:
    pass

test "ListQueue playlist":
    # this is the error we want
    assert_error ListQueue[song](), "expected a contract"
