#lang dssl2

def _MaybeC(X): OrC(X, NoneC)

struct _DllNode:
    let prev: _MaybeC(_DllNode?)
    let data
    let next: _MaybeC(_DllNode?)

class Dll[X]:
    let _sentinel
    let _len

    def __init__(self):
        self._sentinel = _DllNode(None, None, None)
        self._sentinel.next = self._sentinel
        self._sentinel.prev = self._sentinel
        self._len     = 0

    # Is this list empty?
    def empty?(self) -> bool?:
        self._sentinel.next is self._sentinel


    # Returns the number of elements in this list.
    def len(self) -> nat?:
        self._len

    # Gets the sentinel node (violating encapsulation), which can be used
    # in combination with `set_sentinel_and_len!` to do advanced list
    # manipulations.
    def get_sentinel!(self) -> _DllNode?: self._sentinel

    # Sets the sentinel node and len, violating encapsulation.
    def set_sentinel_and_len!(self, sentinel: _DllNode?, len: nat?) -> NoneC:
        self._sentinel = sentinel
        self._len      = len

    # Swaps the elements of this list with another in constant time.
    def swap(self, other: Dll?[X]) -> NoneC:
        let sentinel = other.get_sentinel!()
        let len      = other.len()
        other.set_sentinel_and_len!(self._sentinel, self._len)
        self.set_sentinel_and_len!(sentinel, len)

    # Precondition: count is number of nodes in [start, limit)
    def _detach(self, start: _DllNode?, limit: _DllNode?, count: int?) -> Dll?:
        let result = Dll[X]
        if start is limit: return result
        let new_sentinel = _DllNode(limit.prev, None, start)
        limit.prev = start.prev
        limit.prev.next = limit
        new_sentinel.next.prev = new_sentinel
        new_sentinel.prev.next = new_sentinel
        self._len = self._len - count
        result.set_sentinel_and_len!(new_sentinel, count)
        result

    # Removes the first `n` elements of the list into a new list.
    # (If there are fewer than `n` elements, removes all of them.)
    def detach_front(self, n: nat?) -> Dll?:
        let count = 0
        let limit = self._sentinel
        while limit.next is not self._sentinel and count < n:
            limit = limit.next
            count = count + 1
        self._detach(self._sentinel.next, limit.next, count)

    # Removes the last `n` elements of the list into a new list.
    # (If there are fewer than `n` elements, removes all of them.)
    def detach_back(self, n: nat?) -> Dll?:
        let count = 0
        let start = self._sentinel
        while start.prev is not self._sentinel and count < n:
            start = start.prev
            count = count + 1
        self._detach(start, self._sentinel, count)

    # Moves the elements from another list to the end of this list
    # in constant time. The other list is left empty.
    def splice(self, other: Dll?[X]) -> NoneC:
        let other_sentinel = other.get_sentinel!()
        self._sentinel.prev.next = other_sentinel.next
        other_sentinel.next.prev = self._sentinel.prev
        self._sentinel.prev = other_sentinel.prev
        self._sentinel.prev.next = self._sentinel
        self._len = self._len + other.len()
        other_sentinel.next = other_sentinel
        other_sentinel.prev = other_sentinel
        other.set_sentinel_and_len!(other_sentinel, 0)

    # Adds an element to the front of this list.
    def push_front(self, value: X) -> NoneC:
        let new_node = _DllNode(self._sentinel, value, self._sentinel.next)
        new_node.next.prev = new_node
        new_node.prev.next = new_node
        self._len = self._len + 1

    # Adds an element to the back of this list.
    def push_back(self, value: X) -> NoneC:
        let new_node = _DllNode(self._sentinel.prev, value, self._sentinel)
        new_node.next.prev = new_node
        new_node.prev.next = new_node
        self._len = self._len + 1

    # Removes and returns the first element of this list; if this
    # list is empty, returns None instead.
    def pop_front(self) -> _MaybeC(X):
        if self._sentinel.next is self._sentinel: return None
        let result = self._sentinel.next.data
        self._sentinel.next = self._sentinel.next.next
        self._sentinel.next.prev = self._sentinel
        self._len = self._len - 1
        result

    # Removes and returns the last element of this list; if this
    # list is empty, returns None instead.
    def pop_back(self) -> _MaybeC(X):
        if self._sentinel.prev is self._sentinel: return None
        let result = self._sentinel.prev.data
        self._sentinel.prev = self._sentinel.prev.prev
        self._sentinel.prev.next = self._sentinel
        self._len = self._len - 1
        result

    # Returns the first element of the list, or None if empty.
    def front(self) -> _MaybeC(X):
        if self._sentinel.next is self._sentinel: None
        else: self._sentinel.next.data

    # Returns the last element of the list, or None if empty.
    def back(self) -> _MaybeC(X):
        if self._sentinel.prev is self._sentinel: None
        else: self._sentinel.prev.data

    # Processes the list elements in order, accumulating a result.
    def foldl[Y](self, f: FunC[Y, X, Y], z: Y) -> Y:
        let current = self._sentinel.next
        while current is not self._sentinel:
            z = f(z, current.data)
            current = current.next
        return z

    # Processes the list elements in reverse order, accumulating a result.
    def foldr[Y](self, f: FunC[X, Y, Y], z: Y) -> Y:
        let current = self._sentinel.prev
        while current is not self._sentinel:
            z = f(current.data, z)
            current = current.prev
        return z

    # Applies the given function to each element along with its
    # position in the list.
    def each_with_index(self, f: FunC[int?, X, NoneC]) -> NoneC:
        self.foldl(lambda i, x: f(i, x); i + 1, 0)
        pass

    # Returns the elements of the list in a new vector.
    # : Self -> VectorOf<X>
    def to_vec(self) -> vec?:
        let v = [None; self.len()]
        self.each_with_index(lambda i, x: v[i] = x)
        v
