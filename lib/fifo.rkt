#lang dssl2

import dll

def MakeFifo(X: contract?):
    let Dll = MakeDll(X)

    # A MaybeC(X) is one of:
    # - X
    # - False
    def MaybeC(X): OrC(X, False)

    defstruct Fifo (
        ElementC,               # contract?
        empty?,                 # -> bool?
        size,                   # -> int?
        peek,                   # -> MaybeC(X)
        enqueue,                # X -> VoidC
        dequeue,                # -> MaybeC(X)
    )

    def empty():
        let repr = Dll.empty()
        Fifo {
            ElementC:   X,
            empty?:     repr.empty?,
            size:       repr.size,
            peek:       repr.front,
            enqueue:    repr.push_back,
            dequeue:    repr.pop_front,
        }

    object FifoFactory {
        Fifo?,                  # predicate
        empty,                  # -> Fifo?
    }

let Fifo = MakeFifo(AnyC)

