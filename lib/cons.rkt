#lang dssl2

# A list? is one of:
# - nil()
# - cons(AnyC, list?)
let list? = OrC(cons?, nil?)

struct nil: pass
struct cons:
  let car: AnyC
  let cdr: list?

# Creates a contract that copies a list while applying the given contract
# to each element.
def ListOfC(element: contract?) -> contract?:
    def projection(blame!, value):
        map_cons(λ x: apply_contract(element, x, 'list element'), value)
    make_contract('ListOfC(~e)'.format(element), list?, projection)

# Creates an object to help build a list in order. The object has two methods:
#
#   - get_head() -> ListC     returns the list
#   - cons!(AnycC) -> VoidC   adds an element to the end of the list
class Cons_builder:
    let head
    let tail

    def __init__(self):
        self.head = nil()
        self.tail = nil()

    def cons!(self, x):
        if cons?(self.tail):
            self.tail.cdr = cons(x, nil())
            self.tail = self.tail.cdr
        else:
            self.head = cons(x, nil())
            self.tail = self.head

    def get_head(self):
        self.head

# Reverses `before`, appending it onto `acc`. O(before) time and space.
def rev_app_cons(before: list?, acc: list?) -> list?:
    if cons?(before):
        rev_app_cons(before.cdr, cons(before.car, acc))
    else:
        acc

# Reverses a list. O(lst) time and space.
def rev_cons(lst: list?) -> list?:
    rev_app_cons(lst, nil())

# Appends two lists, functionally. The resulting list will share structure
# with `after`. O(before) time and space.
def app_cons(before: list?, after: list?) -> list?:
    if cons?(before):
        cons(before.car, app_cons(before.cdr, after))
    else:
        after

# Destructively concatenates two lists, returning the concatenated list.
# O(before) time and O(1) space.
def concat_cons!(before: list?, after: list?) -> list?:
    if nil?(before): after
    else:
        let current = before
        while cons?(current.cdr): current = current.cdr
        current.cdr = after
        before

# Finds the length of a list. O(lst) time and O(1) space.
def len_cons(lst: list?) -> int?:
    let result = 0
    while cons?(lst):
        lst = lst.cdr
        result = result + 1
    result

# Copies a list into a vector starting at index `where`. Assumes there is enough
# space in the vector. O(lst) time and O(1) space.
def cons_into_vec(lst: list?, vec: vec?, where: int?) -> VoidC:
    while cons?(lst):
        vec[where] = lst.car
        lst = lst.cdr
        where = where + 1

# Converts a list to a vector. O(lst) time and space.
def cons_to_vec(lst: list?) -> vec?:
    let result = [False; len_cons(lst)]
    cons_into_vec(lst, result, 0)
    result

# Creates a list from the elements of a vector. O(vec) time and space.
def cons_from_vec(vec: vec?) -> list?:
    let builder = Cons_builder()
    for element in vec: builder.cons!(element)
    builder.get_head()

# Calls a visitor function on each element of a list, in order.
def foreach_cons(visit: FunC(AnyC, VoidC), lst: list?) -> VoidC:
    while cons?(lst):
        visit(lst.car)
        lst = lst.cdr

# Traverses a list from right to left, accumulating a result using the given
# function. O(lst * f) time and O(lst) space.
def foldr_cons[Y](f: FunC(AnyC, Y, Y), z: Y, lst: list?) -> Y:
    if cons?(lst):
        f(lst.car, foldr_cons(f, z, lst.cdr))
    else:
        z

# Traverses a list from left to right, accumulating a result using the given
# function. O(lst * f) time and O(1) space.
def foldl_cons[Y](f: FunC(Y, AnyC, Y), z: Y, lst: list?) -> Y:
    foreach_cons(λ element: z = f(z, element), lst)
    return z

# Maps a list by applying a function to each element. O(lst) time and O(lst)
# space (to allocate the new list).
def map_cons(f: FunC(AnyC, AnyC), lst: list?) -> list?:
    let builder = Cons_builder()
    foreach_cons(λ element: builder.cons!(f(element)), lst)
    builder.get_head()

# Filters a list by applying a predicate to each element. O(lst) time and O(lst)
# space (or more precisely, O(result) space).
def filter_cons(f: FunC(AnyC, AnyC), lst: list?) -> list?:
    let builder = Cons_builder()
    def each(element):
        if f(element): builder.cons!(element)
    foreach_cons(each, lst)
    builder.get_head()

def cons_tests():
    let list = cons_from_vec

    test 'map_cons':
        def add2(x): x + 2
        assert_eq map_cons(add2, list([2, 3, 4])), list([4, 5, 6])

    test 'filter_cons':
        let lst = list([2, 3, 4, 5, 6])
        assert_eq filter_cons(even?, lst), list([2, 4, 6])
        assert_eq filter_cons(odd?, lst), list([3, 5])
