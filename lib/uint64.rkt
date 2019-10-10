#lang dssl2

let UINT64_MAX = 2 ** 64 - 1

def _as_int(n):
    if int?(n): n
    else: n.as_int()


class uint64:
    let _value

    def __init__(self, value: OrC(int?, uint64?)):
        self._value = _as_int(value) % UINT64_MAX

    def as_int(self):
        self._value

    def __print__(self, print):
        print('%p', self.as_int())

    def __cmp__(self, other):
        cmp(self.as_int(), _as_int(other))

    def __rcmp__(self, other):
        cmp(_as_int(other), self.as_int())

    def __add__(self, other):
        uint64(self.as_int() + _as_int(other))

    def __radd__(self, other):
        uint64(_as_int(other) + self.as_int())

    def __sub__(self, other):
        uint64(self.as_int() - _as_int(other))

    def __rsub__(self, other):
        uint64(_as_int(other) - self.as_int())

    def __mul__(self, other):
        uint64(self.as_int() * _as_int(other))

    def __rmul__(self, other):
        uint64(_as_int(other) * self.as_int())

    def __div__(self, other):
        uint64(self.as_int() / _as_int(other))

    def __rdiv__(self, other):
        uint64(_as_int(other) / self.as_int())

    def __mod__(self, other):
        uint64(self.as_int() % _as_int(other))

    def __rmod__(self, other):
        uint64(_as_int(other) % self.as_int())

    def __pow__(self, other):
        uint64(self.as_int() ** _as_int(other))

    def __rpow__(self, other):
        uint64(_as_int(other) ** self.as_int())

    def __neg__(self):
        uint64(-self.as_int())

    def __and__(self, other):
        uint64(self.as_int() & _as_int(other))

    def __rand__(self, other):
        uint64(_as_int(other) & self.as_int())

    def __or__(self, other):
        uint64(self.as_int() | _as_int(other))

    def __ror__(self, other):
        uint64(_as_int(other) | self.as_int())

    def __xor__(self, other):
        uint64(self.as_int() ^ _as_int(other))

    def __rxor__(self, other):
        uint64(_as_int(other) ^ self.as_int())
        
    def __lshift__(self, other):
        uint64(self.as_int() << _as_int(other))

    def __rlshift__(self, other):
        uint64(_as_int(other) << self.as_int())

    def __rshift__(self, other):
        uint64(self.as_int() >> _as_int(other))

    def __rrshift__(self, other):
        uint64(_as_int(other) >> self.as_int() )

    def __invert__(self):
        uint64(~self.as_int())


test 'uint64':
    assert_eq        2  ** uint64(10),    1024
    assert_eq uint64(2) **        10 ,    1024
    assert_eq uint64(2) ** uint64(10),    1024
    assert_eq        1  << uint64(20), 1048576
    assert_eq uint64(1) <<        20 , 1048576
    assert_eq uint64(1) << uint64(20), 1048576

