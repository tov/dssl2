#lang dssl2

defstruct nil()
defstruct cons(car: AnyC, cdr: OrC(cons?, nil?))

let list? = OrC(cons?, nil?)

def rev_app_cons(before: list?, acc: list?) -> list?:
    if cons?(before):
        rev_app_cons(before.cdr, cons(before.car, acc))
    else:
        acc

def rev_cons(lst: list?) -> list?:
    rev_app_cons(lst, nil())
    
def app_cons(before: list?, after: list?) -> list?:
    if cons?(before):
        cons(before.car, app_cons(before.cdr, after))
    else:
        after
        
def concat_cons!(before: list?, after: list?) -> list?:
    if nil?(before): after
    else:
        let current = before
        while cons?(current.cdr): current = current.cdr
        current.cdr = after
        before
        
def len_cons(lst: list?) -> int?:
    let result = 0
    while cons?(lst):
        lst = lst.cdr
        result = result + 1
    result
     
def cons_into_vec(lst: list?, vec: vec?, where: int?) -> VoidC:
    if cons?(lst):
        vec[where] = lst.car
        cons_into_vec(lst.cdr, vec, where + 1)
    
def cons_to_vec(lst: list?) -> vec?:
    let result = [False; len_cons(lst)]
    cons_into_vec(lst, result, 0)
    result

def cons_from_vec_index(vec: vec?, where: int?) -> list?:
    if where < len(vec):
        cons(vec[where], cons_from_vec_index(vec, where + 1))
    else:
        nil()

def cons_from_vec(vec: vec?) -> list?:
    cons_from_vec_index(vec, 0)

def foldr_cons[Y](f: FunC(AnyC, Y, Y), z: Y, lst: list?) -> Y:
    if cons?(lst):
        f(lst.car, foldr_cons(f, z, lst.cdr))
    else:
        z
        
def foldl_cons[Y](f: FunC(Y, AnyC, Y), z: Y, lst: list?) -> Y:
    while cons?(lst):
        z = f(z, lst.car)
        lst = lst.cdr
    z
    
def map_cons(f: FunC(AnyC, AnyC), lst: list?) -> list?:
    if nil?(lst): nil()
    else:
        let result = cons(f(lst.car), nil())
        let current = result
        lst = lst.cdr
        while cons?(lst):
            current.cdr = cons(f(lst.car), nil())
            current = current.cdr
            lst = lst.cdr
        result

def filter_cons(f: FunC(AnyC, AnyC), lst: list?) -> list?:
    while cons?(lst) and !f(lst.car):
       lst = lst.cdr
    if nil?(lst): nil()
    else:
        let result = cons(lst.car, nil())
        let current = result
        lst = lst.cdr
        while cons?(lst):
            if f(lst.car):
                current.cdr = cons(lst.car, nil())
                current = current.cdr
            lst = lst.cdr
        result
        
def cons_tests():
    let list = cons_from_vec
     
    test 'map_cons':
        def add2(x): x + 2
        assert_eq map_cons(add2, list([2, 3, 4])), list([4, 5, 6])
        
    test 'filter_cons':
        let lst = list([2, 3, 4, 5, 6])
        assert_eq filter_cons(even?, lst), list([2, 4, 6])
        assert_eq filter_cons(odd?, lst), list([3, 5])
    
