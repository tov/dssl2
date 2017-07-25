#lang dssl2

# A DllOf<X> is an object with the methods listed below.

# Dll_from_sentinel_and_size : NodeOf<X> Natural -> DllOf<X>
def Dll_from_sentinel_and_size(sentinel_, size_):
    # A NodeOf<X> is
    #   Node(NodeOf<X>, X or False
    defstruct Node(prev, data, next)

    if sentinel_ === False:
        sentinel_ = Node(False, False, False)
        sentinel_.prev = sentinel_
        sentinel_.next = sentinel_

    # : Self -> Boolean
    def empty?():
        sentinel_.next === sentinel_

    # : Self -> Natural
    def size():
        size_

    # : Self -> NodeOf<X>
    def get_sentinel!(): sentinel_

    # : Self NodeOf<X> Natural -> Void
    def set_sentinel_and_size!(sentinel, size):
        sentinel_ = sentinel
        size_ = size

    # : Self DllOf<X> -> Void
    def swap(other):
        let sentinel = other.get_sentinel!()
        let size = other.size()
        other.set_sentinel_and_size!(sentinel_, size_)
        set_sentinel_and_size!(sentinel, size)

    # : Self NodeOf<X> NodeOf<X> Natural -> DllOf<X>
    # Precondition: count is number of nodes in [start, limit)
    def detach(start, limit, count):
        if start === limit: return Dll()
        let new_sentinel = Node(limit.prev, False, start)
        limit.prev = start.prev
        limit.prev.next = limit
        new_sentinel.next.prev = new_sentinel
        new_sentinel.prev.next = new_sentinel
        size_ = size_ - count
        Dll_from_sentinel_and_size(new_sentinel, count)

    # : Self Natural -> DllOf<X>
    def detach_front(n):
        let count = 0
        let limit = sentinel_
        while limit.next !== sentinel_ and count < n:
            limit = limit.next
            count = count + 1
        detach(sentinel_.next, limit.next, count)

    # : Self Natural -> DllOf<X>
    def detach_back(n):
        let count = 0
        let start = sentinel_
        while start.prev !== sentinel_ and count < n:
            start = start.prev
            count = count + 1
        detach(start, sentinel_, count)

    # : Self DllOf<X> -> Void
    def splice(other):
        let other_sentinel = other.get_sentinel!()
        sentinel_.prev.next = other_sentinel.next
        other_sentinel.next.prev = sentinel_.prev
        sentinel_.prev = other_sentinel.prev
        sentinel_.prev.next = sentinel_
        size_ = size_ + other.size()
        other_sentinel.next = other_sentinel
        other.sentinel.prev = other_sentinel
        other.set_sentinel_and_size!(other_sentinel, 0)

    # : Self X -> Void
    def push_front(value):
        let new_node = Node(sentinel_, value, sentinel_.next)
        new_node.next.prev = new_node
        new_node.prev.next = new_node
        size_ = size_ + 1

    # : Self X -> Void
    def push_back(value):
        let new_node = Node(sentinel_.prev, value, sentinel_)
        new_node.next.prev = new_node
        new_node.prev.next = new_node
        size_ = size_ + 1

    # : Self -> X or False
    def pop_front():
        if sentinel_.next == sentinel_: return False
        let result = sentinel_.next.data
        sentinel_.next = sentinel_.next.next
        sentinel_.next.prev = sentinel_
        result

    # : Self -> X or False
    def pop_back():
        if sentinel_.prev == sentinel_: return False
        let result = sentinel_.prev.data
        sentinel_.prev = sentinel_.prev.prev
        sentinel_.prev.next = sentinel_
        result

    # : Self [Y X -> Y] Y -> Y
    def foldl(f, z):
        let current = sentinel_.next
        while current !== sentinel_:
            z = f(z, current.data)
            current = current.next
        return z

    # : Self [X Y -> Y] Y -> Y
    def foldr(f, z):
        let current = sentinel_.prev
        while current !== sentinel_:
            z = f(current.data, z)
            current = current.prev
        return z

    # : Self [Natural X -> Void] -> Void
    def each_with_index(f):
        def each(i, x):
            f(i, x)
            i + 1
        foldl(each, 0)
        pass

    # : Self -> VectorOf<X>
    def to_vector():
        let v = [False; size()]
        def each(i, x): v[i] = x
        each_with_index(each)
        v

    defstruct Dll (
        empty?,
        size,
        push_front,
        push_back,
        pop_front,
        pop_back,
        detach_front,
        detach_back,
        splice,
        foldl,
        foldr,
        each_with_index,
        to_vector,
        get_sentinel!,
        set_sentinel_and_size!,
    )

    Dll {
        empty?: empty?,
        size: size,
        push_front: push_front,
        push_back: push_back,
        pop_front: pop_front,
        pop_back: pop_back,
        detach_front: detach_front,
        detach_back: detach_back,
        splice: splice,
        foldl: foldl,
        foldr: foldr,
        each_with_index: each_with_index,
        to_vector: to_vector,
        get_sentinel!: get_sentinel!,
        set_sentinel_and_size!: set_sentinel_and_size!,
    }

def Dll():
    Dll_from_sentinel_and_size(False, 0)

