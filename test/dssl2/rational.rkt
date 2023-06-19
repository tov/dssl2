#lang dssl2

# Larger program that exercises a bunch of DSSL2

# gcd: Integer Integer -> Integer
# Computes the greatest common divisor.
def _gcd(a, b):
    while b != 0:
        let c = a % b
        a = b
        b = c
    return a

# A rational number class.
class Rational:
    let _num
    let _den

    def __init__(self, num, den):
        if den == 0: error('denominator cannot be zero')
        # Invariant: rational number is stored normalized.
        let divisor = _gcd(num, den)
        num = num // divisor
        den = den // divisor
        # Invariant: only the numerator can be negative.
        if den < 0:
            num = -num
            den = -den
        self._num = num
        self._den = den

    def num(self):
        return self._num

    def den(self):
        return self._den

    def as_float(self):
        return self._num / self._den

    # These "__" methods get called by the language's own operators.
    # It's a way to allow your objects to be pretty-printed, added with `+`,
    # compared with `<` (and others), etc.

    def __print__(self, print):
        if self._den == 1:
            # Printing: %p for nice formatting, %d for raw representation.
            # Mnemonic: 'p' for pretty, 'd' for debug.
            print('%p', self.num())
        else:
            print('%p/%p', self.num(), self.den())

    def __cmp__(self, other):
        return (self.num() * other.den()).__cmp__(self.den() * other.num())

    # Constructor enforces invariants, so we don't need to do so here.
    def __add__(self, other):
        return Rational(self.num() * other.den() + other.num() * self.den(),
                        self.den() * other.den())

    def __mul__(self, other):
        return Rational(self.num() * other.num(),
                        self.den() * other.den())

    def __div__(self, other):
        return Rational(self.num() * other.den(),
                        self.den() * other.num())

    def __neg__(self):
        return Rational(-self.num(), self.den())

    def __sub__(self, other):
        return self + -other

    def reciprocal(self):
        return Rational(self._den, self._num)

    # Inefficient iterative version
    def expt_iter(self, e):
        assert int?(e)
        let acc = Rational(1,1)
        for i in range(e):
            acc = self * acc
        return acc

    # Divide-and-conquer (more efficient) recursive version
    # (Also supports negative exponents)
    def expt(self, e):
        assert int?(e)
        if e < 0:
            return self.expt(-e).reciprocal()
        if e == 0:
            return Rational(1,1)
        elif e == 1:
            return self
        elif (e % 2) == 1:
            return self * self.expt(e-1)
        else: # e is even
            let acc = self.expt(e // 2)
            return acc * acc


test 'comparison':
    assert Rational(1, 3) < Rational(1, 2)
    assert (Rational(1, 3) < Rational(1, 3)) == False
    assert Rational(1, 3) <= Rational(1, 3)
    assert Rational(2, 7) < Rational(1, 3)
    assert Rational(1, 2) > Rational(1, 3)
    assert (Rational(1, 3) > Rational(1, 3)) == False
    assert Rational(1, 3) >= Rational(1, 3)
    assert Rational(1, 3) > Rational(2, 7)

test 'addition':
    assert Rational(1, 5) + Rational(2, 5) == Rational(3, 5)
    assert Rational(1, 3) + Rational(3, 5) == Rational(14, 15)
    assert Rational(1, 3) + Rational(2, 3) == Rational(1, 1)

test 'multiplication':
    assert Rational(1, 3) * Rational(2, 5) == Rational(2, 15)
    assert Rational(2, 5) * Rational(1, 3) == Rational(2, 15)
    assert Rational(-1, 3) * Rational(2, 5) == Rational(-2, 15)
    assert Rational(1, 3) * Rational(-2, 5) == Rational(-2, 15)
    assert Rational(-1, 3) * Rational(-2, 5) == Rational(2, 15)
    assert Rational(2, 3) * Rational(3, 2) == Rational(1, 1)

test 'division':
    assert Rational(1, 3) / Rational(2, 5) == Rational(5, 6)
    assert Rational(2, 5) / Rational(1, 3) == Rational(6, 5)
    assert Rational(-1, 3) / Rational(2, 5) == Rational(-5, 6)
    assert Rational(1, 3) / Rational(-2, 5) == Rational(-5, 6)
    assert Rational(-1, 3) / Rational(-2, 5) == Rational(5, 6)
    assert Rational(2, 3) / Rational(3, 2) == Rational(4, 9)

test 'negation':
    assert - Rational(1, 2) == Rational(-1, 2)
    assert - Rational(-1, 2) == Rational(1, 2)

test 'subtraction':
    assert Rational(1, 2) - Rational(1, 4) == Rational(1, 4)
    assert Rational(1, 4) - Rational(1, 2) == Rational(-1, 4)
    assert Rational(1, 3) - Rational(1, 5) == Rational(2, 15)

test 'reciprocal':
    assert Rational(2, 3).reciprocal() == Rational(3, 2)

test 'expt_iter':
    assert Rational(1, 2).expt_iter(4) == Rational(1, 16)
    assert Rational(1, 3).expt_iter(3) == Rational(1, 27)

test 'expt':
    assert Rational(1, 2).expt(4) == Rational(1, 16)
    assert Rational(1, 3).expt(3) == Rational(1, 27)
    assert Rational(1, 2).expt(-4) == Rational(16, 1)
