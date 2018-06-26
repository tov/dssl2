#lang dssl2

class Array[T]:
    let size_: nat?
    let data_: vec?
    
    def __init__(self, capacity: nat?):
        self.size_ = 0
        self.data_ = [False; capacity]

    def empty?(self) -> bool?:
        self.size_ == 0

    def size(self) -> nat?:
        self.size_

    def capacity(self) -> nat?:
        len(self.data_)

    def ensure_capacity(self, req_cap: nat?) -> VoidC:
        if req_cap > self.capacity():
            let new_cap = max(req_cap, 2 * self.capacity())
            self.data_ = [ self.data_[i] if i < self.size_ else False
                               for i in new_cap ]

    def _check_index(self, index):
        if index >= self.size_:
            error("Array index out of bounds: ~a >= ~a", index, self.size_)

    def get(self, index: nat?):
        self._check_index(index)
        self.data_[index]

    def set(self, index: nat?, value: T) -> VoidC:
        self._check_index(index)
        self.data_[index] = value

    def push(self, value: T) -> VoidC:
        self.ensure_capacity(self.size_ + 1)
        self.data_[self.size_] = value
        self.size_ = self.size_ + 1

    def pop(self) -> OrC(False, T):
        if self.size_ == 0: return False
        self.size_ = self.size_ - 1
        let result = self.data_[self.size_]
        self.data_[self.size_] = False
        result

    def shrink_to_fit(self) -> VoidC:
        if len(self.data_) > self.size_:
            self.data_ = [ self.data_[i] for i in self.size_ ]

    def clone(self) -> Array?:
        let result = Array(T, self.size_)
        for i in self.size_:
            result.push(self.get(i))
        result
    
    def to_vec(self) -> vec?:
        [ self.data_[i] for i in self.size_ ]

    def equals_with?(self, other, pred?) -> bool?:
        if self.size_ != other.size(): return False
        for i in self.size_:
            if !pred?(self.get(i), other.get(i)):
                return False
        return True

    def equals?(self, other) -> bool?:
        self.equals_with?(other, lambda x, y: x == y)

def array() -> Array?:
    Array(AnyC, 8)
    
test "array":
    let a = array()
    a.push(5)
    a.push('hello')
    assert_eq a.size(), 2
    assert_eq a.to_vec(), [5, 'hello']