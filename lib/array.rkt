#lang dssl2

class Array[T] (ITERABLE):
    let _len: nat?
    let _data: vec?

    def __init__(self, capacity: nat?):
        self._len = 0
        self._data = vec(capacity)

    def empty?(self) -> bool?:
        self._len == 0

    def len(self) -> nat?:
        self._len

    def capacity(self) -> nat?:
        self._data.len()

    def ensure_capacity(self, req_cap: nat?) -> NoneC:
        if req_cap > self.capacity():
            let new_cap = max(req_cap, 2 * self.capacity())
            self._data = [ self._data[i] if i < self._len else None
                               for i in range(new_cap) ]

    def _check_index(self, index):
        if index >= self._len:
            error("Array index out of bounds: %p >= %p", index, self._len)

    def get(self, index: nat?):
        self._check_index(index)
        self._data[index]

    def set(self, index: nat?, value: T) -> NoneC:
        self._check_index(index)
        self._data[index] = value

    def push(self, value: T) -> NoneC:
        self.ensure_capacity(self._len + 1)
        self._data[self._len] = value
        self._len = self._len + 1

    def pop(self) -> OrC(NoneC, T):
        if self._len == 0: return None
        self._len = self._len - 1
        let result = self._data[self._len]
        self._data[self._len] = None
        result

    def push_back(self, value): self.push(value)

    def pop_back(self): self.pop()

    def push_front(self, value: T) -> NoneC:
        let len = self._len
        self.ensure_capacity(len + 1)
        for rev_i in range(len):
            let i = len - rev_i
            self._data[i] = self._data[i - 1]
        self._data[0] = value
        self._len     = len + 1

    def pop_front(self) -> OrC(NoneC, T):
        let len = self._len - 1
        if len < 0: return None
        let result = self._data[0]
        for i in range(len):
            self._data[i] = self._data[i + 1]
        self._data[len] = None
        self._len       = len
        result

    def clear(self) -> NoneC:
        self._len = 0

    def shrink_to_fit(self) -> NoneC:
        if self._data.len() > self._len:
            self._data = [ self._data[i] for i in range(self._len) ]

    def clone(self) -> Array?:
        let result = Array(T, self._len)
        for i in range(self._len):
            result.push(self.get(i))
        result

    def to_vec(self) -> vec?:
        [ self._data[i] for i in range(self._len) ]

    def equals_with?(self, other, pred?) -> bool?:
        if self._len != other.len(): return False
        for i in range(self._len):
            if not pred?(self.get(i), other.get(i)):
                return False
        return True

    def equals?(self, other) -> bool?:
        self.equals_with?(other, lambda x, y: x == y)

    def iterator(self) -> index_iterator?:
        index_iterator(self, 0, self.len())

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

def build_array(n: nat?, f) -> Array?:
    let a = Array(n)
    for i in range(n): a.push(f(i))
    a

