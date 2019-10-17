#lang dssl2

interface QUEUE[T]:
    def empty?(self) -> bool?
    def len(self) -> nat?
    def enqueue(self, value: T) -> NoneC
    def dequeue(self) -> OrC(T, NoneC)

interface STACK[T]:
    def empty?(self) -> bool?
    def len(self) -> nat?
    def push(self, value: T) -> NoneC
    def pop(self) -> OrC(T, NoneC)

interface CONTAINER[T]:
    def empty?(self) -> bool?
    def len(self) -> nat?
    def add(self, value: T) -> NoneC
    def remove(self) -> OrC(T, NoneC)
