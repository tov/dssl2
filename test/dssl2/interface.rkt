#lang dssl2

interface I:
    def m1(self, x: nat?) -> nat?
    def m2(self, x)

class C(I):
    let x
    def m1(self, boo: int?):
        self.x
    def __init__(self, x):
        self.x = x
    def m2(self, boo) -> str?:
        self.m1(5)
    def m3(self):
        self.x = λ: 10

class D:
    def __init__(s): pass
    
def f(x: I!): x

interface CONTAINER[X]:
    def add(self, x: X) -> VoidC
    def get(self) -> X
    
class IntCell (CONTAINER):
    let val: int?
    def __init__(self, x):
        self.val = x
    def add(self, x: int?) -> VoidC:
        self.val = x
    def get(self):
        'a'

def g(x: CONTAINER![int?]): x
def h(x: CONTAINER!): x

let c = IntCell(5)
c.add(6)
assert_error c.add('hi'), 'IntCell'

let o = g(c)
o.add(7)
assert_error o.add('hi'), 'CONTAINER'

