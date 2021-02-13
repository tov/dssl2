#lang dssl2

# A library of header-free singly-linked lists.
#
# Lists are represented using None and a struct, cons(car, cdr).

# A _list? is one of:
# - cons(AnyC, _list?)
# - None
def _build_list?():
    def Cons_list?(v): return cons?(v) or v is None
    return Cons_list?

let _list? = _build_list?()

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
        if cons?(old_tail):
            old_tail.cdr = self.tail
        else:
            self.head = self.tail

    def take(self):
        let result = self.head
        self.__init__()
        return result

        
# Builds the Cons singleton struct.
def _build_Cons():
    let list? = _list?

    def Cons_rev_app(before: list?, acc: list?) -> list?:
        if cons?(before):
            return Cons_rev_app(before.cdr, cons(before.car, acc))
        else:
            return acc

    def Cons_rev(lst: list?) -> list?:
        return Cons_rev_app(lst, None)

    def Cons_app(before: list?, after: list?) -> list?:
        if cons?(before):
            return cons(before.car, Cons_app(before.cdr, after))
        else:
            return after

    def Cons_concat(before: list?, after: list?) -> list?:
        if cons?(before):
            let current = before
            while cons?(current.cdr):
                current = current.cdr
            current.cdr = after
            return before
        else:
            return after

    def Cons_len(lst: list?) -> int?:
        let result = 0
        while cons?(lst):
            lst = lst.cdr
            result = result + 1
        return result

    def Cons_into_vec(lst: list?, vec: vec?, where: int?) -> NoneC:
        while cons?(lst):
            vec[where] = lst.car
            lst = lst.cdr
            where = where + 1

    def Cons_to_vec(lst: list?) -> vec?:
        let result = [False; Cons_len(lst)]
        Cons_into_vec(lst, result, 0)
        return result

    def Cons_from_vec(vec: vec?) -> list?:
        let builder = ConsBuilder()
        for element in vec:
            builder.snoc(element)
        return builder.take()

    def Cons_foreach(visit: FunC[AnyC, NoneC], lst: list?) -> NoneC:
        while cons?(lst):
            visit(lst.car)
            lst = lst.cdr

    def Cons_foldr[Y](f: FunC[AnyC, Y, Y], z: Y, lst: list?) -> Y:
        if cons?(lst):
            return f(lst.car, Cons_foldr(f, z, lst.cdr))
        else:
            return z

    def Cons_foldl[Y](f: FunC[Y, AnyC, Y], z: Y, lst: list?) -> Y:
        Cons_foreach(λ element: z = f(z, element), lst)
        return z

    def Cons_map(f: FunC[AnyC, AnyC], lst: list?) -> list?:
        let builder = ConsBuilder()
        Cons_foreach(λ element: builder.snoc(f(element)), lst)
        return builder.take()

    def Cons_filter(f: FunC[AnyC, AnyC], lst: list?) -> list?:
        let builder = ConsBuilder()
        def each(element):
            if f(element):
                return builder.snoc(element)
        Cons_foreach(each, lst)
        return builder.take()

    def Cons_andmap(f: FunC[AnyC, AnyC], lst: list?) -> AnyC:
        let result = True
        while cons?(lst):
            result = f(lst.car)
            if not result: break
            lst = lst.cdr
        return result

    def Cons_ormap(f: FunC[AnyC, AnyC], lst: list?) -> AnyC:
        let result = False
        while cons?(lst):
            result = f(lst.car)
            if result: break
            lst = lst.cdr
        return result

    def Cons_sort[T](less_than?: FunC[T, T, AnyC],
                     lst: Cons_realListC(T)) \
                    -> list?:
        def insert(element, link):
            if cons?(link) and less_than?(link.car, element):
                return cons(link.car, insert(element, link.cdr))
            else:
                return cons(element, link)
        def loop(link, acc):
            if cons?(link):
                return loop(link.cdr, insert(link.car, acc))
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
        list?:    list?,
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
