#lang dssl2

assert float(3.4) == 3.4
assert float(3) == 3.0
assert float('3.5') == 3.5
assert float('3') == 3.0
assert float(True) == 1.0
assert float(False) == 0.0
