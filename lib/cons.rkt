#lang dssl2

# A library of header-free singly-linked lists.
#
# Lists are represented using None and a struct, cons(data, next).

# A _list? is one of:
# - cons(AnyC, _list?)
# - None
def _build_list?():
    def Cons_list?(v): return cons?(v) or v is None
    return Cons_list?

let _list? = _build_list?()

def _print_as_vec(head, print):
    print('[')
    if cons?(head):
        print('%p', head.data)
        head = head.next
        while cons?(head):
            print(', %p', head.data)
            head = head.next
    print(']')

struct cons:
    let data
    let next: _list?

# Builds the Cons singleton struct.
def _build_Cons():
    let list? = _list?

    def Cons_rev_app(before: list?, acc: list?) -> list?:
        while cons?(before):
            acc = cons(before.data, acc)
            before = before.next
        return acc

    def Cons_rev(lst: list?) -> list?:
        return Cons_rev_app(lst, None)

    def Cons_app(before: list?, after: list?) -> list?:
        if cons?(before):
            return cons(before.data, Cons_app(before.next, after))
        else:
            return after

    def Cons_concat(before: list?, after: list?) -> list?:
        if cons?(before):
            let current = before
            while cons?(current.next):
                current = current.next
            current.next = after
            return before
        else:
            return after

    def Cons_len(lst: list?) -> int?:
        let result = 0
        while cons?(lst):
            lst = lst.next
            result = result + 1
        return result

    def Cons_into_vec(lst: list?, vec: vec?, where: int?) -> NoneC:
        while cons?(lst):
            vec[where] = lst.data
            lst = lst.next
            where = where + 1

    def Cons_to_vec(lst: list?) -> vec?:
        let result = [False; Cons_len(lst)]
        Cons_into_vec(lst, result, 0)
        return result

    def Cons_from_vec(vec: vec?) -> list?:
        let result = None
        for element in vec:
            result = cons(element, result)
        return Cons_rev(result)

    def Cons_foreach(visit: FunC[AnyC, AnyC], lst: list?) -> NoneC:
        while cons?(lst):
            visit(lst.data)
            lst = lst.next

    def Cons_foldr[Y](f: FunC[AnyC, Y, Y], z: Y, lst: list?) -> Y:
        if cons?(lst):
            return f(lst.data, Cons_foldr(f, z, lst.next))
        else:
            return z

    def Cons_foldl[Y](f: FunC[Y, AnyC, Y], z: Y, lst: list?) -> Y:
        Cons_foreach(λ element: z = f(z, element), lst)
        return z

    def Cons_map(f: FunC[AnyC, AnyC], lst: list?) -> list?:
        let result = None
        Cons_foreach(λ element: result = cons(f(element), result), lst)
        return Cons_rev(result)

    def Cons_filter(f: FunC[AnyC, AnyC], lst: list?) -> list?:
        let result = None
        def each(element):
            if f(element):
                result = cons(element, result)
        Cons_foreach(each, lst)
        return Cons_rev(result)

    def Cons_andmap(f: FunC[AnyC, AnyC], lst: list?) -> AnyC:
        let result = True
        while cons?(lst):
            result = f(lst.data)
            if not result: break
            lst = lst.next
        return result

    def Cons_ormap(f: FunC[AnyC, AnyC], lst: list?) -> AnyC:
        let result = False
        while cons?(lst):
            result = f(lst.data)
            if result: break
            lst = lst.next
        return result

    def Cons_sort[T](less_than?: FunC[T, T, AnyC],
                     lst: Cons_realListC(T)) \
                    -> list?:
        def insert(element, link):
            if cons?(link) and less_than?(link.data, element):
                return cons(link.data, insert(element, link.next))
            else:
                return cons(element, link)
        def loop(link, acc):
            if cons?(link):
                return loop(link.next, insert(link.data, acc))
            else:
                return acc
        return loop(lst, None)

    def Cons_realListC(CTC: contract?) -> contract?:
        let name = 'ListC[%s]'.format(CTC)
        def make(pred?):
            def list_of_pred?(v):
                return list?(v) and Cons_andmap(pred?, v)
            return make_contract(name, list_of_pred?, None)
        if num?(CTC) or bool?(CTC) or str?(CTC) or char?(CTC):
            return make(λ v: v == CTC)
        elif flat_contract?(CTC) and proc?(CTC):
            return make(CTC)
        else:
            def prj(blame!, value):
                def ListElementC(elt: CTC): return elt
                return Cons_map(ListElementC, value)
            return make_contract(name, list?, prj)

    let Cons_ListC = SquareBracketC('ListC', Cons_realListC)

    class Cons_Iterator (ITERATOR):
        let _head

        def __init__(self, head: _list?):
            self._head = head

        def iterator(self):
            return self

        def try_advance(self, visit):
            if cons?(self._head):
                let element = self._head.data
                self._head = self._head.next
                visit(element)
                return True
            else:
                return False

        def __print__(self, print):
            print('#<Cons.Iterator ')
            _print_as_vec(self._head, print)
            print('>')

    struct ConsOperations:
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
        let Iterator
        let Iterator?

    return ConsOperations {
        list?:     list?,
        ListC:     Cons_ListC,
        rev_app:   Cons_rev_app,
        rev:       Cons_rev,
        app:       Cons_app,
        concat:    Cons_concat,
        len:       Cons_len,
        into_vec:  Cons_into_vec,
        to_vec:    Cons_to_vec,
        from_vec:  Cons_from_vec,
        foreach:   Cons_foreach,
        foldr:     Cons_foldr,
        foldl:     Cons_foldl,
        map:       Cons_map,
        filter:    Cons_filter,
        andmap:    Cons_andmap,
        ormap:     Cons_ormap,
        sort:      Cons_sort,
        Iterator:  Cons_Iterator,
        Iterator?: Cons_Iterator?,
    }


let Cons = _build_Cons()
