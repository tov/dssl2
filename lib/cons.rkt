#lang dssl2

# A library of header-free singly-linked lists.
#
# Lists are represented using None and a struct, cons(car, cdr).

# A _list? is one of:
# - cons(AnyC, _list?)
# - None
let _list? = OrC(cons?, NoneC)

struct cons:
    let car
    let cdr: _list?


class ConsBuilder:
    let head
    let tail

    def __init__(self):
        self.head = None
        self.tail = None

    def cons(self, x):
        self.head = cons(x, self.head)
        if self.tail is None: self.tail = self.head

    def snoc(self, x):
        let old_tail = self.tail
        self.tail = cons(x, None)
        if old_tail is None:
            self.head = self.tail
        else:
            old_tail.cdr = self.tail

    def take(self):
        let result = self.head
        self.__init__()
        return result

        
# Builds the Cons singleton struct.
def _build_Cons():
    let Cons_list? = OrC(cons?, NoneC)
   
    def Cons_rev_app(before: _list?, acc: _list?) -> _list?:
        if cons?(before):
            return Cons_rev_app(before.cdr, cons(before.car, acc))
        else:
            return acc

    def Cons_rev(lst: _list?) -> _list?:
        return Cons_rev_app(lst, None)

    def Cons_app(before: _list?, after: _list?) -> _list?:
        if cons?(before):
            return cons(before.car, Cons_app(before.cdr, after))
        else:
            return after

    def Cons_concat(before: _list?, after: _list?) -> _list?:
        if before is None:
            return after
        else:
            let current = before
            while cons?(current.cdr):
                current = current.cdr
            current.cdr = after
            return before

    def Cons_len(lst: _list?) -> int?:
        let result = 0
        while cons?(lst):
            lst = lst.cdr
            result = result + 1
        return result

    def Cons_into_vec(lst: _list?, vec: vec?, where: int?) -> NoneC:
        while cons?(lst):
            vec[where] = lst.car
            lst = lst.cdr
            where = where + 1

    def Cons_to_vec(lst: _list?) -> vec?:
        let result = [False; Cons_len(lst)]
        Cons_into_vec(lst, result, 0)
        return result

    def Cons_from_vec(vec: vec?) -> _list?:
        let builder = ConsBuilder()
        for element in vec:
            builder.snoc(element)
        return builder.take()

    def Cons_foreach(visit: FunC[AnyC, NoneC], lst: _list?) -> NoneC:
        while cons?(lst):
            visit(lst.car)
            lst = lst.cdr

    def Cons_foldr[Y](f: FunC[AnyC, Y, Y], z: Y, lst: _list?) -> Y:
        if cons?(lst):
            return f(lst.car, Cons_foldr(f, z, lst.cdr))
        else:
            return z

    def Cons_foldl[Y](f: FunC[Y, AnyC, Y], z: Y, lst: _list?) -> Y:
        Cons_foreach(λ element: z = f(z, element), lst)
        return z

    def Cons_map(f: FunC[AnyC, AnyC], lst: _list?) -> _list?:
        let builder = ConsBuilder()
        Cons_foreach(λ element: builder.snoc(f(element)), lst)
        return builder.take()

    def Cons_filter(f: FunC[AnyC, AnyC], lst: _list?) -> _list?:
        let builder = ConsBuilder()
        def each(element):
            if f(element):
                return builder.snoc(element)
        Cons_foreach(each, lst)
        return builder.take()

    def Cons_andmap(f: FunC[AnyC, AnyC], lst: _list?) -> AnyC:
        let result = True
        while cons?(lst):
            result = f(lst.car)
            if not result: break
            lst = lst.cdr
        return result

    def Cons_ormap(f: FunC[AnyC, AnyC], lst: _list?) -> AnyC:
        let result = False
        while cons?(lst):
            result = f(lst.car)
            if result: break
            lst = lst.cdr
        return result

    def Cons_sort[T](less_than?: FunC[T, T, AnyC],
                     lst: Cons_realListC(T)) \
                    -> _list?:
        def insert(element, link):
            if cons?(link) and less_than?(link.car, element):
                return cons(link.car, insert(element, link.cdr))
            else:
                return cons(element, link)
        def loop(link, acc):
            if link is None:
                return acc
            else:
                return loop(link.cdr, insert(link.car, acc))
        return loop(lst, None)

    def Cons_realListC(CTC: contract?) -> contract?:
        if num?(CTC) or bool?(CTC) or str?(CTC) or char?(CTC):
            return λ lst: Cons_andmap(λ x: x == CTC, lst)
        elif flat_contract?(CTC) and proc?(CTC):
            return λ lst: Cons_andmap(CTC, lst)
        else:
            def prj_elt(x: CTC): return x
            def prj(blame!, value): return Cons_map(prj_elt, value)
            return make_contract('ListC[%p]'.format(CTC), _list?, prj)

    let Cons_ListC = SquareBracketC('ListC', Cons_realListC)

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

    return ConsOperations {
        list?:    Cons_list?,
        ListC:    Cons_ListC,
        rev_app:  Cons_rev_app,
        rev:      Cons_rev,
        app:      Cons_app,
        concat:   Cons_concat,
        len:      Cons_len,
        into_vec: Cons_into_vec,
        to_vec:   Cons_to_vec,
        from_vec: Cons_from_vec,
        foreach:  Cons_foreach,
        foldr:    Cons_foldr,
        foldl:    Cons_foldl,
        map:      Cons_map,
        filter:   Cons_filter,
        andmap:   Cons_andmap,
        ormap:    Cons_ormap,
        sort:     Cons_sort,
    }


let Cons = _build_Cons()