#lang dssl2

def MaybeC(X): OrC(False, X)

struct _DllNode(prev: MaybeC(_DllNode?), data, next: MaybeC(_DllNode?))

class Dll[X]:
    let sentinel_
    let size_

    def __init__(self):
        self.sentinel_ = _DllNode(False, False, False)
        self.sentinel_.next = self.sentinel_
        self.sentinel_.prev = self.sentinel_
        self.size_     = 0

    # Is this list empty?
    def empty?(self) -> bool?:
        self.sentinel_.next === self.sentinel_


    # Returns the number of elements in this list.
    def size(self) -> nat?:
        self.size_

    # Gets the sentinel node (violating encapsulation), which can be used
    # in combination with `set_sentinel_and_size!` to do advanced list
    # manipulations.
    def get_sentinel!(self) -> _DllNode?: self.sentinel_

    # Sets the sentinel node and size, violating encapsulation.
    def set_sentinel_and_size!(self, sentinel: _DllNode?, size: nat?) -> VoidC:
        self.sentinel_ = sentinel
        self.size_ = size

    # Swaps the elements of this list with another in constant time.
    def swap(self, other: Dll?) -> VoidC:
        let sentinel = other.get_sentinel!()
        let size = other.size()
        other.set_sentinel_and_size!(self.sentinel_, self.size_)
        self.set_sentinel_and_size!(sentinel, size)

    # Precondition: count is number of nodes in [start, limit)
    def _detach(self, start: _DllNode?, limit: _DllNode?, count: int?) -> Dll?:
        let result = Dll(X)
        if start === limit: return result
        let new_sentinel = _DllNode(limit.prev, False, start)
        limit.prev = start.prev
        limit.prev.next = limit
        new_sentinel.next.prev = new_sentinel
        new_sentinel.prev.next = new_sentinel
        self.size_ = self.size_ - count
        result.set_sentinel_and_size!(new_sentinel, count)
        result

    # Removes the first `n` elements of the list into a new list.
    # (If there are fewer than `n` elements, removes all of them.)
    def detach_front(self, n: nat?) -> Dll?:
        let count = 0
        let limit = self.sentinel_
        while limit.next !== self.sentinel_ and count < n:
            limit = limit.next
            count = count + 1
        self._detach(self.sentinel_.next, limit.next, count)

    # Removes the last `n` elements of the list into a new list.
    # (If there are fewer than `n` elements, removes all of them.)
    def detach_back(self, n: nat?) -> Dll?:
        let count = 0
        let start = self.sentinel_
        while start.prev !== self.sentinel_ and count < n:
            start = start.prev
            count = count + 1
        self._detach(start, self.sentinel_, count)

    # Moves the elements from another list to the end of this list
    # in constant time. The other list is left empty.
    def splice(self, other: Dll?) -> VoidC:
        let other_sentinel = other.get_sentinel!()
        self.sentinel_.prev.next = other_sentinel.next
        other_sentinel.next.prev = self.sentinel_.prev
        self.sentinel_.prev = other_sentinel.prev
        self.sentinel_.prev.next = self.sentinel_
        self.size_ = self.size_ + other.size()
        other_sentinel.next = other_sentinel
        other_sentinel.prev = other_sentinel
        other.set_sentinel_and_size!(other_sentinel, 0)

    # Adds an element to the front of this list.
    def push_front(self, value: X) -> VoidC:
        let new_node = _DllNode(self.sentinel_, value, self.sentinel_.next)
        new_node.next.prev = new_node
        new_node.prev.next = new_node
        self.size_ = self.size_ + 1

    # Adds an element to the back of this list.
    def push_back(self, value: X) -> VoidC:
        let new_node = _DllNode(self.sentinel_.prev, value, self.sentinel_)
        new_node.next.prev = new_node
        new_node.prev.next = new_node
        self.size_ = self.size_ + 1

    # Removes and returns the first element of this list; if this
    # list is empty, returns False instead.
    def pop_front(self) -> MaybeC(X):
        if self.sentinel_.next === self.sentinel_: return False
        let result = self.sentinel_.next.data
        self.sentinel_.next = self.sentinel_.next.next
        self.sentinel_.next.prev = self.sentinel_
        self.size_ = self.size_ - 1
        result

    # Removes and returns the last element of this list; if this
    # list is empty, returns False instead.
    def pop_back(self) -> MaybeC(X):
        if self.sentinel_.prev === self.sentinel_: return False
        let result = self.sentinel_.prev.data
        self.sentinel_.prev = self.sentinel_.prev.prev
        self.sentinel_.prev.next = self.sentinel_
        self.size_ = self.size_ - 1
        result

    # Returns the first element of the list, or False if empty.
    def front(self) -> MaybeC(X):
        if self.sentinel_.next === self.sentinel_: False
        else: self.sentinel_.next.data

    # Returns the last element of the list, or False if empty.
    def back(self) -> MaybeC(X):
        if self.sentinel_.prev === self.sentinel_: False
        else: self.sentinel_.prev.data

    # Processes the list elements in order, accumulating a result.
    def foldl[Y](self, f: FunC(Y, X, Y), z: Y) -> Y:
        let current = self.sentinel_.next
        while current !== self.sentinel_:
            z = f(z, current.data)
            current = current.next
        return z

    # Processes the list elements in reverse order, accumulating a result.
    def foldr[Y](self, f: FunC(X, Y, Y), z: Y) -> Y:
        let current = self.sentinel_.prev
        while current !== self.sentinel_:
            z = f(current.data, z)
            current = current.prev
        return z

    # Applies the given function to each element along with its
    # position in the list.
    def each_with_index(self, f: FunC(int?, X, VoidC)) -> VoidC:
        self.foldl(lambda i, x: f(i, x); i + 1, 0)
        pass

    # Returns the elements of the list in a new vector.
    # : Self -> VectorOf<X>
    def to_vec(self) -> vec?:
        let v = [False; self.size()]
        self.each_with_index(lambda i, x: v[i] = x)
        v

def dll(): Dll(AnyC)

