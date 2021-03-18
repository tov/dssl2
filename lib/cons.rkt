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

def _print_as_vec(head, print):
    print('[')
    if cons?(head):
        print('%p', head.car)
        head = head.cdr
        while cons?(head):
            print(', %p', head.car)
            head = head.cdr
    print(']')

struct cons:
    let car
    let cdr: _list?

class ConsBuilder:
    let _head
    let _tail
    let _len

    def __init__(self):
        self._head = None
        self._tail = None
        self._len  = 0

    def len(self):
        return self._len

    def empty?(self):
        return self._len == 0

    def cons(self, x):
        self._head = cons(x, self._head)
        if self._tail is None: self._tail = self._head
        self._len = self._len + 1
        return self

    def snoc(self, x):
        self._snoc(x)
        return self

    def _snoc(self, x):
        let old_tail = self._tail
        self._tail = cons(x, None)
        if cons?(old_tail):
            old_tail.cdr = self._tail
        else:
            self._head = self._tail
        self._len = self._len + 1

    def snoc_all(self, xs):
        for x in xs:
            self._snoc(x)
        return self

    def take(self):
        let result = self._head
        self.__init__()
        return result

    def build(self):
        return self.take()

    def __print__(self, print):
        print('Cons.Builder()')
        let head = self._head
        if head is None: return
        print('.snoc_all(')
        _print_as_vec(head, print)
        print(')')

# Builds the Cons singleton struct.
def _build_Cons():
    let list? = _list?

    def Cons_rev_app(before: list?, acc: list?) -> list?:
        while cons?(before):
            acc = cons(before.car, acc)
            before = before.cdr
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
        let Builder

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
        Builder:  ConsBuilder,
    }


let Cons = _build_Cons()

def _all_tests():
    test 'ConsBuilder':
        let cb = Cons.Builder()
        assert cb.len() == 0
        assert cb.empty?()
        assert cb.take() is None
        cb.snoc(3)
        cb.snoc(5)
        assert cb.len() == 2
        assert not cb.empty?()
        cb.snoc(7)
        cb.cons(1)
        cb.snoc(9)
        assert cb.len() == 5
        assert Cons.to_vec(cb.take()) == [1, 3, 5, 7, 9]
        assert cb.empty?()
        assert cb.snoc(2)
        assert cb.snoc(4)
        assert cb.len() == 2
        assert Cons.to_vec(cb.take()) == [2, 4]
