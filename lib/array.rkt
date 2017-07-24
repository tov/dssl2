#lang dssl2

# An Array is an object with the methods listed below.

# : Natural VectorOf<X or False> -> ArrayOf<X>
# Creates a new dynamic array with the given size and elements.
#
# Precondition:
#  - size_ <= len(data_)
#  - The first size_ elements of data_ are initialized to Xs, not False
def Array_from_parts(size_, data_):
    assert int?(size_) and !negative?(size_)
    assert size_ <= len(data_)

    # : Self -> Boolean    
    def empty?(): size_ == 0
    
    # : Self -> Natural
    def size(): size_

    # : Self -> Natural        
    def capacity(): len(data_)
    
    # : Self Natural -> Void
    def ensure_capacity(req_cap):
        if req_cap > capacity():
            let new_cap = max(req_cap, 2 * capacity())
            data_ = [ data_[i] if i < size_ else False for i in new_cap ]

    def check_index_(index):
        if index >= size_:
            error("Array index out of bounds: ~a >= ~a", index, size_)
    
    # : Self Natural -> X
    def get(index):
        check_index_(index)
        data_[index]
        
    # : Self Natural X -> Void
    def set(index, value):
        check_index_(index)
        data_[index] = value
        
    # : Self X -> Void
    def push(value):
        ensure_capacity(size_ + 1)
        data_[size_] = value
        size_ = size_ + 1
        
    # : Self -> X
    def pop():
        size_ = size_ - 1
        let result = data_[size_]
        data_[size_] = False
        result
        
    # : Self -> Void
    def shrink_to_fit():
        if len(data_) > size_:
            data_ = [ data_[i] for i in size_ ]
            
    defstruct Array(empty?, size, capacity,
                    ensure_capacity, shrink_to_fit,
                    get, set, push, pop)
    
    Array {
        empty?: empty?,
        size: size,
        capacity: capacity,
        ensure_capacity: ensure_capacity,
        shrink_to_fit: shrink_to_fit,
        get: get,
        set: set,
        push: push,
        pop: pop,
    }
    
# : Natural -> ArrayOf<X>
# Creates a new, empty dynamic array with the given capacity.
def Array_with_capacity(capacity):
    Array_from_parts(0, [False; capacity])

# : X Natural -> ArrayOf<X>
# Creates a new dynamic array of the given `value` repeated `size` times.
def Array_fill(value, size):
    Array_from_parts(size, [value; size])
    
# : -> ArrayOf<X>
# Creates a new, empty dynamic array with a small capacity.
def Array():
    Array_with_capacity(8)
