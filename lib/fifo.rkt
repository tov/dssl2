#lang dssl2

import dll
import adt

def _MaybeC(X): OrC(X, NoneC)

class Fifo[X] (QUEUE):
    let repr_

    def __init__(self):
        self.repr_ = Dll[X]()

    def ElementC(self) -> contract?:
        X

    def empty?(self) -> bool?:
        self.repr_.empty?()

    def len(self) -> nat?:
        self.repr_.len()

    def peek(self) -> _MaybeC(X):
        self.repr_.front()

    def enqueue(self, value: X) -> NoneC:
        self.repr_.push_back(value)

    def dequeue(self) -> _MaybeC(X):
        self.repr_.pop_front()

