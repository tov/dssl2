#lang dssl2

interface PERS_STACK[T]:
    def not_pop(self, other: PERS_STACK[T]) -> PERS_STACK[T]
    # having method prototypes inside interface definitions introduce extra
    # contract parameters have a whole extra code path (second clause of
    # `square-bracket-proc-contract`), but weren't tested before and were
    # (predictably) broken before. maybe they work now?
    def also_not_pop[U](self, other: PERS_STACK[T]) -> PERS_STACK[T]

class PS[T]:
    def __init__(self):
        return self
    def not_pop(self, other):
        # this should succeed:
        # return other
        # this should fail, but currently does not:
        return 3
        # future work: add the actual checking
    def also_not_pop[V](self, other):
        return other

PS().not_pop(PS())
