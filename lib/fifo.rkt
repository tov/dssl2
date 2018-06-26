#lang dssl2

import dll

def MaybeC(X): OrC(False, X)

class Fifo[X]:
    let repr_

    def __init__(self):
        self.repr_ = Dll(X)

    def ElementC(self) -> contract?:
        X

    def empty?(self) -> bool?:
        self.repr_.empty?()

    def size(self) -> nat?:
        self.repr_.size()

    def peek(self) -> MaybeC(X):
        self.repr_.front()

    def enqueue(self, value: X) -> VoidC:
        self.repr_.push_back(value)

    def dequeue(self) -> MaybeC(X):
        self.repr_.pop_front()

