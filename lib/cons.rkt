#lang dssl2

# A library of header-free singly-linked lists.
#
# Lists are represented using two structs, nil() and cons(car, cdr).

# A _list? is one of:
# - nil()
# - cons(AnyC, _list?)
let _list? = OrC(cons?, nil?)

struct nil: pass
struct cons:
  let car
  let cdr: _list?

# The library also provides:
#
#   - A class ConsBuilder, for building lists in order:
#
#       - ConsBuilder() -> ConsBuilder?
#         Constructs an empty ConsBuilder.
#
#       - cons(self, value: AnyC) -> VoidC
#         Adds `value` at the beginning of the list.
#
#       - snoc(self, value: AnyC) -> VoidC
#         Adds `value` at the end of the list.
#
#       - take(self) -> list?
#         Takes the built list out of this ConsBuilder and returns it,
#         leaving this ConsBuilder empty.
#
#
#   - A struct Cons containing many functions for working with cons lists:
#
#       - list?(x: AnyC) -> bool?
#         Predicate for cons lists, equivalent to OrC(nil?, cons?).
#
#       - ListC(elt: contract?) -> contract?
#         Constructs a contract for a list, given a contract for the
#         elements. This contract copies the list while applying `elt`
#         to each element.
#
#       - len(lst: list?) -> nat?
#         Finds the length of a list. O(lst) time and O(1) space.
#
#       - app(before: list?, after: list?) -> list?
#         Appends to lists, functionally. The resulting list will share
#         structire with `after`. O(before) time and space.
#
#       - rev(lst: list?) -> list?
#         Reverses a list, functionally. O(lst) time and space.
#
#       - rev_app(before: list?, after: list?) -> list?
#         Reverses `before`, appending it onto `acc`. O(before) time and
#         space.
#
#       - concat(before: list?, after: list?) -> list?
#         Destructively concatenates two lists, returning the concatenated
#         list. O(before) time and O(1) space.
#
#       - into_vec(lst: list?, vec: vec?, where: nat?) -> VoidC
#         Copies a list into a vector starting at index `where`. Assumes
#         there is enough space in the vector. O(lst) time and O(1) space.
#
#       - to_vec(lst: list?) -> vec?
#         Converts a list to a vector. O(lst) time and space.
#
#       - from_vec(vec: vec?) -> list?
#         Creates a list from the elements of a vector. O(vec) time and
#         space.
#
#       - foreach(visit: FunC[AnyC, VoidC], lst: list?) -> VoidC
#         Calls a visitor function on each element of `lst`, in order.
#
#       - foldr[Y](f: FunC[AnyC, Y, Y], z: Y, lst: list?) -> Y
#         Traverses a list from right to left, accumulating a result
#         using the given function. O(lst * f) time and O(lst) space.
#
#       - foldl[Y](f: FunC[Y, AnyC, Y], z: Y, lst: list?) -> Y
#         Traverses a list from left to right, accumulating a result
#         using the given function. O(lst * f) time and O(1) space.
#
#       - map(f: FunC[AnyC, AnyC], lst: list?) -> list?
#         Maps a list by applying a function to each element. O(lst)
#         time and O(lst) space (to allocate the new list).
#
#       - filter(f: FunC[AnyC, AnyC], lst: list?) -> list?
#         Filters a list by applying a predicate to each element. O(lst)
#         time and O(lst) space (or more precisely, O(result) space).
#
#       - andmap(f: FunC[AnyC, AnyC], lst: list?) -> AnyC
#         Applies `f` to each element in turn, returning `False` if the
#         result of any is `False`, and otherwise returning the result of
#         `f` applied to the last element. (Returns `True` if `lst` is
#         empty.)
#
#       - ormap(f: FunC[AnyC, AnyC], lst: list?) -> AnyC
#         Applies `f` to each element in turn, returning the first non-false
#         result, or `False` if none is non-False.
#
#       - sort[T](less_than?: FunC[T, T, AnyC], lst: ListC(T)) -> list?
#         Sorts a list, functionally, using the given `less_than?`
#         predicate to determine the order. O(lst^2).


class ConsBuilder:
    let head
    let tail

    def __init__(self):
        self.head = nil()
        self.tail = nil()

    def cons(self, x):
        self.head = cons(x, self.head)
        if nil?(self.tail): self.tail = self.head

    def snoc(self, x):
        let old_tail = self.tail
        self.tail = cons(x, nil())
        if nil?(old_tail):
            self.head = self.tail
        else:
            old_tail.cdr = self.tail

    def take(self):
        let result = self.head
        self.__init__()
        result

def _rev_app(before: _list?, acc: _list?) -> _list?:
    if cons?(before):
        _rev_app(before.cdr, cons(before.car, acc))
    else:
        acc

def _rev(lst: _list?) -> _list?:
    _rev_app(lst, nil())

def _app(before: _list?, after: _list?) -> _list?:
    if cons?(before):
        cons(before.car, _app(before.cdr, after))
    else:
        after

def _concat(before: _list?, after: _list?) -> _list?:
    if nil?(before): after
    else:
        let current = before
        while cons?(current.cdr): current = current.cdr
        current.cdr = after
        before

def _len(lst: _list?) -> int?:
    let result = 0
    while cons?(lst):
        lst = lst.cdr
        result = result + 1
    result

def _into_vec(lst: _list?, vec: vec?, where: int?) -> VoidC:
    while cons?(lst):
        vec[where] = lst.car
        lst = lst.cdr
        where = where + 1

def _to_vec(lst: _list?) -> vec?:
    let result = [False; _len(lst)]
    _into_vec(lst, result, 0)
    result

def _from_vec(vec: vec?) -> _list?:
    let builder = ConsBuilder()
    for element in vec: builder.snoc(element)
    builder.take()

def _foreach(visit: FunC[AnyC, VoidC], lst: _list?) -> VoidC:
    while cons?(lst):
        visit(lst.car)
        lst = lst.cdr

def _foldr[Y](f: FunC[AnyC, Y, Y], z: Y, lst: _list?) -> Y:
    if cons?(lst):
        f(lst.car, _foldr(f, z, lst.cdr))
    else:
        z

def _foldl[Y](f: FunC[Y, AnyC, Y], z: Y, lst: _list?) -> Y:
    _foreach(λ element: z = f(z, element), lst)
    return z

def _map(f: FunC[AnyC, AnyC], lst: _list?) -> _list?:
    let builder = ConsBuilder()
    _foreach(λ element: builder.snoc(f(element)), lst)
    builder.take()

def _filter(f: FunC[AnyC, AnyC], lst: _list?) -> _list?:
    let builder = ConsBuilder()
    def each(element):
        if f(element): builder.snoc(element)
    _foreach(each, lst)
    builder.take()

def _andmap(f: FunC[AnyC, AnyC], lst: _list?) -> AnyC:
    let result = True
    while cons?(lst):
        result = f(lst.car)
        if not result: break
        lst = lst.cdr
    result

def _ormap(f: FunC[AnyC, AnyC], lst: _list?) -> AnyC:
    let result = False
    while cons?(lst):
        result = f(lst.car)
        if result: break
        lst = lst.cdr
    result

def _sort[T](less_than?: FunC[T, T, AnyC], lst: _ListC(T)) -> _list?:
    def insert(element, link):
        if cons?(link) and less_than?(link.car, element):
            cons(link.car, insert(element, link.cdr))
        else: cons(element, link)
    def loop(link, acc):
        if nil?(link): acc
        else: loop(link.cdr, insert(link.car, acc))
    loop(lst, nil())

def _ListC(element: contract?) -> contract?:
    if num?(element) or bool?(element) or str?(element) or char?(element):
        λ lst: _andmap(λ x: x == element, lst)
    elif flat_contract?(element) and proc?(element):
        λ lst: _andmap(element, lst)
    else:
        def project_element(x: element): x
        def projection(blame!, value): _map(project_element, value)
        make_contract('ListC(%p)'.format(element), _list?, projection)

struct _ConsOperations:
    let list?
    let ListC
    let rev_app
    let rev
    let app
    let concat
    let len
    let into_vec
    let to_vec
    let from_vec
    let foreach
    let foldr
    let foldl
    let map
    let filter
    let andmap
    let ormap
    let sort

let Cons = _ConsOperations {
    list?:    _list?,
    ListC:    _ListC,
    rev_app:  _rev_app,
    rev:      _rev,
    app:      _app,
    concat:   _concat,
    len:      _len,
    into_vec: _into_vec,
    to_vec:   _to_vec,
    from_vec: _from_vec,
    foreach:  _foreach,
    foldr:    _foldr,
    foldl:    _foldl,
    map:      _map,
    filter:   _filter,
    andmap:   _andmap,
    ormap:    _ormap,
    sort:     _sort,
}

