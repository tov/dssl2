#lang s-exp dssl2/language

; A Posn that can move vertically but is fixed horizontally.

(class Posn
  (let [x_ num?])
  (let [y_ num?])

  (def (__init__ foo x y)
    (= (struct-ref foo x_) x)
    (= (struct-ref foo y_) y))

  (def (x self)
    (struct-ref self x_))

  (def (_x! it nx)
    (= (struct-ref it x_) nx))

  (def (y! self ny)
    (= (struct-ref self y_) ny))

  (def (y bees)
    (struct-ref bees y_))

  (def (get_self this)
    this))

(test "Posn"
  (let p (Posn 3 4))
  (assert_eq 3 ((struct-ref p x)))
  (assert_eq 4 ((struct-ref p y)))
  ((struct-ref p y!) 10)
  (assert_eq 10 ((struct-ref p y))))

(test "delegate"
  (let p (Posn 3 4))
  (let set_y (struct-ref p y!))
  (let get_y (struct-ref p y))
  (assert_eq 4 (get_y))
  (set_y 5)
  (assert_eq 5 (get_y)))

(test "can't assign x"
  (let p (Posn 3 4))
  (assert_error (struct-ref p x!))
  (assert_error ((struct-ref p x) 5))
  (assert_error (struct-ref p _x!)))