#lang dssl2

defstruct nil()
defstruct cons(car: AnyC, cdr: OrC(cons?, nil?))

let list? = OrC(cons?, nil?)

def rev_app_cons(before: list?, acc: list?) -> list?:
    if cons?(before):
        rev_app_cons(before.cdr, cons(before.car, acc))
    else:
        acc

def reverse_cons(lst: list?) -> list?:
    rev_app_cons(lst, nil())
    
def append_cons(before: list?, after: list?) -> list?:
    if cons?(before):
        cons(before.car, append_cons(before.cdr, after))
    else:
        after
        
def nconcat_cons!(before: list?, after: list?) -> list?:
    if nil?(before): after
    else:
        let current = before
        while cons?(current.cdr): current = current.cdr
        current.cdr = after
        before
        
def length_cons(lst: list?) -> int?:
    let result = 0
    while cons?(lst):
        lst = lst.cdr
        result = result + 1
    result
     
def into_vector(lst: list?, vec: vec?, where: int?) -> VoidC:
    if cons?(lst):
        vec[where] = lst.car
        into_vector(lst.cdr, vec, where + 1)
    
def to_vector(lst: list?) -> vec?:
    let result = [False; length_cons(lst)]
    into_vector(lst, result, 0)
    result

def from_vector_index(vec: vec?, where: int?) -> list?:
    if where < len(vec):
        cons(vec[where], from_vector_index(vec, where + 1))
    else:
        nil()

def from_vector(vec: vec?) -> list?:
    from_vector_index(vec, 0)

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
    foldr_cons(lambda x, y: cons(f(x), y), nil(), lst)
    
