#lang dssl2

struct cons:
    let car
    let cdr
struct nil:
    pass

let x = cons(2, cons(3, cons(4, nil())))
let y = cons {
          car: 2,
          cdr: cons {
            car: 3,
            cdr: cons {
              car: 4,
              cdr: nil {}
            }
          }
        }

assert x == y
assert x !== y
assert x === x
