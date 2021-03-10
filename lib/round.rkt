#lang dssl2

# Rounds `a` to the nearest integer.
def round_int(a: num?) -> int?:
    return (a + 0.5).floor()

# Rounds `a` to the nearest multiple of `factor`.
def round_multiple(a: num?, factor: num?) -> num?:
    return factor * round_int(a/float(factor))

# Rounds `a` to have `prec` digits after the decimal point.
def round_precision(a: num?, prec: int?) -> num?:
    return round_multiple(a, 0.1 ** prec)


###
### TESTS
###

def _all_tests():
    def _check_round_multiple(a, factor, expected):
        test 'round_multiple(%p, %p) == %p'.format(a, factor, expected):
            assert round_multiple(a, factor) == expected

    _check_round_multiple(3.0, 1, 3)
    _check_round_multiple(3.4, 1, 3)
    _check_round_multiple(3.5, 1, 4)
    _check_round_multiple(3.6, 1, 4)
    _check_round_multiple(-3.0, 1, -3)
    _check_round_multiple(-3.4, 1, -3)
    _check_round_multiple(-3.5, 1, -3)
    _check_round_multiple(-3.6, 1, -4)
    _check_round_multiple(52, 5, 50)
    _check_round_multiple(53, 5, 55)
    _check_round_multiple(52.4, 5, 50)
    _check_round_multiple(52.5, 5, 55)
    _check_round_multiple(3.1, 0.25, 3.)
    _check_round_multiple(3.2, 0.25, 3.25)
    _check_round_multiple(3.3, 0.25, 3.25)
    _check_round_multiple(3.4, 0.25, 3.5)

    def _check_round_precision(a, prec, exp):
        test 'round_precision(%p, %p) == %p'.format(a, prec, exp):
            # Check that actual is within 0.01% of expected:
            assert (round_precision(a, prec) - exp).abs() <= 0.0001 * exp

    _check_round_precision(3.141592, 0, 3.)
    _check_round_precision(3.141592, 1, 3.1)
    _check_round_precision(3.141592, 2, 3.14)
    _check_round_precision(3.141592, 3, 3.142)
    _check_round_precision(3.141592, 4, 3.1416)
    _check_round_precision(3.141592, 5, 3.14159)
    _check_round_precision(31415.92,  0, 31416.)
    _check_round_precision(31415.92, -1, 31420.)
    _check_round_precision(31415.92, -2, 31400.)
    _check_round_precision(31415.92, -3, 31000.)
    _check_round_precision(31415.92, -4, 30000.)
    _check_round_precision(31415.92, -5,     0.)
