#lang dssl2

# Given an element contract, returns a doubly-linked list factory object.
# The returned object is a circular doubly-linked list that supports
# adding and removing elements from both ends.
def MakeDll(X: contract?):
    # A MaybeC(X) is one of:
    # - X
    # - False
    def MaybeC(X): OrC(X, False)

    # A Node? is Node(Node?, X, Node?), except for the sentinel node,
    # which is Node(Node?, False, Node?)
    defstruct Node(
       prev: MaybeC(Node?), # Only includes False for initialization
       data: MaybeC(X),     # Only includes False for sentinel
       next: MaybeC(Node?)) # Only includes False for initialization

    defstruct Dll (
        empty?,                 # -> bool?
        size,                   # -> int?
        front,                  # -> MaybeC(X)
        back,                   # -> MaybeC(X)
        push_front,             # X -> VoidC
        push_back,              # X -> VoidC
        pop_front,              # -> MaybeC(X)
        pop_back,               # -> MaybeC(X)
        detach_front,           # -> Dll?
        detach_back,            # -> Dll?
        splice,                 # Dll? -> VoidC
        foldl,                  # FunC(Y, X, Y), Y -> Y
        foldr,                  # FunC(X, Y, Y), Y -> Y
        each_with_index,        # FunC(int?, X, VoidC) -> VoidC
        to_vector,              # -> VecOf(X)
        get_sentinel!,          ### internal
        set_sentinel_and_size!, ### internal
    )

    # Factory method used internally for creating Dll? objects. All the
    # state and function definitions actually live here, and the main
    # public factory calls this as a helper.
    def from_sentinel_and_size(sentinel_: MaybeC(Node?), size_: int?) -> Dll?:
        if sentinel_ === False:
            sentinel_ = Node(False, False, False)
            sentinel_.prev = sentinel_
            sentinel_.next = sentinel_

        def empty?() -> bool?:
            sentinel_.next === sentinel_

        def size() -> int?:
            size_

        def get_sentinel!() -> Node?: sentinel_

        def set_sentinel_and_size!(sentinel: Node?, size: int?):
            sentinel_ = sentinel
            size_ = size

        def swap(other: Dll?):
            let sentinel = other.get_sentinel!()
            let size = other.size()
            other.set_sentinel_and_size!(sentinel_, size_)
            set_sentinel_and_size!(sentinel, size)

        # Precondition: count is number of nodes in [start, limit)
        def detach(start: Node?, limit: Node?, count: int?) -> Dll?:
            if start === limit: return empty()
            let new_sentinel = Node(limit.prev, False, start)
            limit.prev = start.prev
            limit.prev.next = limit
            new_sentinel.next.prev = new_sentinel
            new_sentinel.prev.next = new_sentinel
            size_ = size_ - count
            from_sentinel_and_size(new_sentinel, count)

        def detach_front(n: int?) -> Dll?:
            let count = 0
            let limit = sentinel_
            while limit.next !== sentinel_ and count < n:
                limit = limit.next
                count = count + 1
            detach(sentinel_.next, limit.next, count)

        def detach_back(n: int?) -> Dll?:
            let count = 0
            let start = sentinel_
            while start.prev !== sentinel_ and count < n:
                start = start.prev
                count = count + 1
            detach(start, sentinel_, count)

        def splice(other: Dll?) -> VoidC:
            let other_sentinel = other.get_sentinel!()
            sentinel_.prev.next = other_sentinel.next
            other_sentinel.next.prev = sentinel_.prev
            sentinel_.prev = other_sentinel.prev
            sentinel_.prev.next = sentinel_
            size_ = size_ + other.size()
            other_sentinel.next = other_sentinel
            other.sentinel.prev = other_sentinel
            other.set_sentinel_and_size!(other_sentinel, 0)

        def push_front(value: X) -> VoidC:
            let new_node = Node(sentinel_, value, sentinel_.next)
            new_node.next.prev = new_node
            new_node.prev.next = new_node
            size_ = size_ + 1

        def push_back(value: X) -> VoidC:
            let new_node = Node(sentinel_.prev, value, sentinel_)
            new_node.next.prev = new_node
            new_node.prev.next = new_node
            size_ = size_ + 1

        def pop_front() -> MaybeC(X):
            if sentinel_.next === sentinel_: return False
            let result = sentinel_.next.data
            sentinel_.next = sentinel_.next.next
            sentinel_.next.prev = sentinel_
            size_ = size_ - 1
            result

        def pop_back() -> MaybeC(X):
            if sentinel_.prev === sentinel_: return False
            let result = sentinel_.prev.data
            sentinel_.prev = sentinel_.prev.prev
            sentinel_.prev.next = sentinel_
            size_ = size_ - 1
            result

        def front() -> MaybeC(X):
            if sentinel_.next === sentinel_: False
            else: sentinel_.next.data

        def back() -> MaybeC(X):
            if sentinel_.prev === sentinel_: False
            else: sentinel_.prev.data

        def foldl[Y](f: FunC(Y, X, Y), z: Y) -> Y:
            let current = sentinel_.next
            while current !== sentinel_:
                z = f(z, current.data)
                current = current.next
            return z

        def foldr[Y](f: FunC(X, Y, Y), z: Y) -> Y:
            let current = sentinel_.prev
            while current !== sentinel_:
                z = f(current.data, z)
                current = current.prev
            return z

        def each_with_index(f: FunC(int?, X, VoidC)) -> VoidC:
            def each(i, x):
                f(i, x)
                i + 1
            foldl(each, 0)
            pass

        # : Self -> VectorOf<X>
        def to_vector() -> vec?:
            let v = [False; size()]
            def each(i, x): v[i] = x
            each_with_index(each)
            v

        Dll {
            empty?,
            size,
            front,
            back,
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
        }

    # Factory method for creating a new, empty doubly-linked list.
    def empty() -> Dll?:
        from_sentinel_and_size(False, 0)

    object DllFactory {
        Dll?,                   # predicate
        empty,                  # -> Dll?
    }

