#lang dssl2

assert  30 //  7 ==  4
assert -30 // -7 ==  4
assert -30 //  7 == -5
assert  30 // -7 == -5

def check_div_mod(m, n):
    assert n * (m // n) + m % n == m

for m in [30, -30]:
    for n in [7 , -7]:
        check_div_mod(m, n)

assert_error 30 // 7.0, 'type error'
assert_error 30.0 // 7, 'type error'
assert_error 30 % 7.0, 'type error'
