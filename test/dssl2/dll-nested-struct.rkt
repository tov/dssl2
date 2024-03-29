#lang dssl2

class Dll:
    let Node_
    let _sentinel
    let _len

    def __init__(self):
        struct Node:
            let prev; let data; let next
        self.Node_ = Node
        self._sentinel = self.Node_(False, False, False)
        self._sentinel.next = self._sentinel
        self._sentinel.prev = self._sentinel
        self._len = 0

    # Is this list empty?
    def empty?(self):
        self._sentinel.next is self._sentinel

    # Returns the number of elements in this list.
    def len(self):
        self._len

    # Gets the sentinel node (violating encapsulation), which can be used
    # in combination with `set_sentinel_and_size!` to do advanced list
    # manipulations.
    def get_sentinel!(self): self._sentinel

    # Sets the sentinel node and size, violating encapsulation.
    def set_sentinel_and_size!(self, sentinel, size):
        self._sentinel = sentinel
        self._len = size

    # Swaps the elements of this list with another in constant time.
    def swap(self, other):
        let sentinel = other.get_sentinel!()
        let size = other.len()
        other.set_sentinel_and_size!(self._sentinel, self._len)
        self.set_sentinel_and_size!(sentinel, size)

    # Precondition: count is number of nodes in [start, limit)
    def _detach(self, start, limit, count):
        let result = Dll()
        if start is limit: return result
        let new_sentinel = self.Node_(limit.prev, False, start)
        limit.prev = start.prev
        limit.prev.next = limit
        new_sentinel.next.prev = new_sentinel
        new_sentinel.prev.next = new_sentinel
        self._len = self._len - count
        result.set_sentinel_and_size!(new_sentinel, count)
        result

    # Removes the first `n` elements of the list into a new list.
    # (If there are fewer than `n` elements, removes all of them.)
    def detach_front(self, n):
        let count = 0
        let limit = self._sentinel
        while limit.next is not self._sentinel and count < n:
            limit = limit.next
            count = count + 1
        self._detach(self._sentinel.next, limit.next, count)

    # Removes the last `n` elements of the list into a new list.
    # (If there are fewer than `n` elements, removes all of them.)
    def detach_back(self, n):
        let count = 0
        let start = self._sentinel
        while start.prev is not self._sentinel and count < n:
            start = start.prev
            count = count + 1
        self._detach(start, self._sentinel, count)

    # Moves the elements from another list to the end of this list
    # in constant time. The other list is left empty.
    def splice(self, other: Dll?) -> NoneC:
        let other_sentinel = other.get_sentinel!()
        self._sentinel.prev.next = other_sentinel.next
        other_sentinel.next.prev = self._sentinel.prev
        self._sentinel.prev = other_sentinel.prev
        self._sentinel.prev.next = self._sentinel
        self._len = self._len + other.len()
        other_sentinel.next = other_sentinel
        other_sentinel.prev = other_sentinel
        other.set_sentinel_and_size!(other_sentinel, 0)

    # Adds an element to the front of this list.
    def push_front(self, value):
        let new_node = self.Node_(self._sentinel, value, self._sentinel.next)
        new_node.next.prev = new_node
        new_node.prev.next = new_node
        self._len = self._len + 1

    # Adds an element to the back of this list.
    def push_back(self, value):
        let new_node = self.Node_(self._sentinel.prev, value, self._sentinel)
        new_node.next.prev = new_node
        new_node.prev.next = new_node
        self._len = self._len + 1

    # Removes and returns the first element of this list; if this
    # list is empty, returns False instead.
    def pop_front(self):
        if self._sentinel.next is self._sentinel: return False
        let result = self._sentinel.next.data
        self._sentinel.next = self._sentinel.next.next
        self._sentinel.next.prev = self._sentinel
        self._len = self._len - 1
        result

    # Removes and returns the last element of this list; if this
    # list is empty, returns False instead.
    def pop_back(self):
        if self._sentinel.prev is self._sentinel: return False
        let result = self._sentinel.prev.data
        self._sentinel.prev = self._sentinel.prev.prev
        self._sentinel.prev.next = self._sentinel
        self._len = self._len - 1
        result

    # Returns the first element of the list, or False if empty.
    def front(self):
        if self._sentinel.next is self._sentinel: False
        else: self._sentinel.next.data

    # Returns the last element of the list, or False if empty.
    def back(self):
        if self._sentinel.prev is self._sentinel: False
        else: self._sentinel.prev.data

    # Processes the list elements in order, accumulating a result.
    def foldl(self, f, z):
        let current = self._sentinel.next
        while current is not self._sentinel:
            z = f(z, current.data)
            current = current.next
        return z

    # Processes the list elements in reverse order, accumulating a result.
    def foldr(self, f, z):
        let current = self._sentinel.prev
        while current is not self._sentinel:
            z = f(current.data, z)
            current = current.prev
        return z

    # Applies the given function to each element along with its
    # position in the list.
    def each_with_index(self, f):
        self.foldl(lambda i, x: f(i, x); i + 1, 0)
        pass

    # Returns the elements of the list in a new vector.
    # : Self -> VectorOf<X>
    def to_vec(self):
        let v = [False; self.len()]
        self.each_with_index(lambda i, x: v[i] = x)
        v

let l = Dll()
l.push_back(2)
l.push_back(3)
l.push_back(4)
let m = Dll()
m.push_back(5)
m.splice(l.detach_front(2))
assert l.to_vec() == [4]
assert m.to_vec() == [5, 2, 3]
