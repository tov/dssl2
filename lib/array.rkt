#lang dssl2

class Array[T] (ITERABLE):
    let _size: nat?
    let _data: vec?
    
    def __init__(self, capacity: nat?):
        self._size = 0
        self._data = [False; capacity]

    def empty?(self) -> bool?:
        self._size == 0

    def size(self) -> nat?:
        self._size

    def capacity(self) -> nat?:
        self._data.len()

    def ensure_capacity(self, req_cap: nat?) -> VoidC:
        if req_cap > self.capacity():
            let new_cap = max(req_cap, 2 * self.capacity())
            self._data = [ self._data[i] if i < self._size else False
                               for i in new_cap ]

    def _check_index(self, index):
        if index >= self._size:
            error("Array index out of bounds: ~a >= ~a", index, self._size)

    def get(self, index: nat?):
        self._check_index(index)
        self._data[index]

    def set(self, index: nat?, value: T) -> VoidC:
        self._check_index(index)
        self._data[index] = value

    def push(self, value: T) -> VoidC:
        self.ensure_capacity(self._size + 1)
        self._data[self._size] = value
        self._size = self._size + 1

    def pop(self) -> OrC(False, T):
        if self._size == 0: return False
        self._size = self._size - 1
        let result = self._data[self._size]
        self._data[self._size] = False
        result
        
    def clear(self) -> VoidC:
        self._size = 0

    def shrink_to_fit(self) -> VoidC:
        if self._data.len() > self._size:
            self._data = [ self._data[i] for i in self._size ]

    def clone(self) -> Array?:
        let result = Array(T, self._size)
        for i in self._size:
            result.push(self.get(i))
        result
    
    def to_vec(self) -> vec?:
        [ self._data[i] for i in self._size ]

    def equals_with?(self, other, pred?) -> bool?:
        if self._size != other.size(): return False
        for i in self._size:
            if not pred?(self.get(i), other.get(i)):
                return False
        return True

    def equals?(self, other) -> bool?:
        self.equals_with?(other, lambda x, y: x == y)
    
    def iterator(self) -> index_iterator?:
        index_iterator(self, 0, self.size())

    def __eq__(self, other) -> bool?:
        self.equals?(other)
        
    def __print__(self, print):
        print("array_of_vec(%p)", self.to_vec())
        
    def __index_ref__(self, n):
        self.get(n)
        
    def __index_set__(self, n, v):
        self.set(n, v)

def array() -> Array?:
    Array(8)
    
def array_of_vec(v: vec?) -> Array?:
    let a = Array(v.len())
    for i in v: a.push(i)
    a

test "array":
    let a = array()
    a.push(5)
    a.push('hello')
    assert_eq a.size(), 2
    assert_eq a.to_vec(), [5, 'hello']
