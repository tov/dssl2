#lang dssl2

interface INT_CELL:
    def __print__(_, _)
    def set!(_, _: int?)
    def get(_) -> int?

class IntCell (INT_CELL):
    let _contents
    def __init__(self, value: int?):
        self._contents = value
    def set!(self, value: int?):
        self._contents = value
    def get(self) -> int?:
        self._contents
    def __print__(self, print):
        print("IntCell(%p)", self._contents)
        
let a = IntCell(5)
let b: INT_CELL! = a

assert_eq str(a), "IntCell(5)"
assert_eq str(b), "IntCell(5)"

interface CELL[T]:
    def __print__(_, _)
    def set!(_, _: T)
    def get(_) -> T
    
class Cell[T] (CELL):
    let _contents
    def __init__(self, value: T):
        self._contents = value
    def __print__(self, print):
        print("Cell(%p)", self._contents)
    def set!(self, value: T):
        self._contents = value
    def get(self) -> T:
        self._contents
       
let c = Cell(nat?, 5)
let d = Cell(nat?, 5)

assert_eq str(c), "Cell(5)"      
assert_eq c, d

class AnyCell:
    let _contents
    def __init__(self, value): self._contents = value
    def get(self): self._contents
    def set!(self, value): self._contents = value
    def __print__(self, fmt):
        fmt("Cell(%p)", self._contents)
  
let e = AnyCell(5)
e.set!(e)
assert_eq str(e), "#0=Cell(#0#)"

