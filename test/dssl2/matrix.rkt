#lang dssl2

class Matrix:
    let _height
    let _width
    let _data

    def __init__(self, height: nat?, width: nat?):
        self._height = height
        self._width = width
        self._data = [ 0; width * height ]

    def width(self): self._width
    def height(self): self._height

    def _index(self, row, col):
        assert row < self.height()
        assert col < self.width()
        row * self.width() + col

    def __index_ref__(self, row, col):
        self._data[self._index(row, col)]

    def __index_set__(self, row, col, val: int?):
        self._data[self._index(row, col)] = val

let m = Matrix(4, 3)

assert_eq m[0, 0], 0
assert_eq m[0, 1], 0
assert_eq m[0, 2], 0
assert_error m[0, 3]

m[0, 0] = 1
m[0, 1] = 2
m[1, 0] = 3

assert_eq m[0, 0], 1
assert_eq m[0, 1], 2
assert_eq m[1, 0], 3
assert_eq m[1, 1], 0