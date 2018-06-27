#lang dssl2

interface I:
    def m1(self, x: nat?) -> nat?
    def m2(self, x)

class C(I):
    let x
    def __init__(self, x):
        self.x = x
    def m1(self, boo):
        self.x
    def m2(self, boo):
        self.m1()
    def m3(self):
        self.x = λ: 10

class D:
    def __init__(s): pass
    
def f(x: I): x

interface Container[X]:
    def add(self, x: X) -> VoidC
    def get(self) -> X
    
class IntCell(Container):
    let val: int?
    def add(self, x: int?) -> VoidC:
        self.val = x
    def get(self) -> int?:
        self.val

def g(x: Container(int?)): x