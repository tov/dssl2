#lang dssl2

# A Promise[T] is an abstract data type.
class Promise[T]:
    let _ready
    let _data

    # Promise: (T: contract?) (-> T) -> Promise[T]
    def __init__(self, thunk: FunC(T)):
        self._ready = False
        self._data  = thunk
    
    # forced?: -> bool?
    def forced?(self):
        self._ready

    # force: -> T
    def force(self) -> T:
        if not self._ready:
            self._data  = self._data()
            self._ready = True
        self._data

# delay : (-> AnyC) -> Promise[AnyC]
# Creates a promise that will be computed by the given thunk.
def delay(thunk):
    Promise(thunk)

