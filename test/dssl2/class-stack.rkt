#lang s-exp dssl2/language

(class Stack #:∀ (T)
  (let [size nat?])
  (let [data vec?])
  
  (def (__init__ self [capacity nat?])
    (= (struct-ref self size) 0)
    (= (struct-ref self data) (make-vec capacity False)))

  (def (get_size self) #:-> nat?
    (struct-ref self size))

  (def (get_capacity self) #:-> nat?
    (len (struct-ref self data)))

  (def (_is_full? self) #:-> bool?
    (== (struct-ref self size) (len (struct-ref self data))))

  (def (is_empty? self) #:-> bool?
    (== (struct-ref self size) 0))

  (def (push! self [value T]) #:-> VoidC
    (if
     [((struct-ref self _is_full?))
      (error "Stack.push!: full")]
     [else
      (= (vec-ref (struct-ref self data) (struct-ref self size)) value)
      (= (struct-ref self size) (+ (struct-ref self size) 1))]))

  (def (pop! self) #:-> T
    (if
     [((struct-ref self is_empty?))
      (error "Stack.pop!: empty")]
     [else
      (= (struct-ref self size) (- (struct-ref self size) 1))
      (let result (vec-ref (struct-ref self data) (struct-ref self size)))
      (= (vec-ref (struct-ref self data) (struct-ref self size)) False)
      result])))
     
(test "int stack"
  (let s (Stack int? 10))
  (assert_eq True ((struct-ref s is_empty?)))
  (assert_eq 0 ((struct-ref s get_size)))
  (assert_eq 10 ((struct-ref s get_capacity)))
  ((struct-ref s push!) 5)
  ((struct-ref s push!) 6)
  (assert_error ((struct-ref s push!) "seven"))
  ((struct-ref s push!) 7)
  (assert_eq 7 ((struct-ref s pop!)))
  ((struct-ref s push!) 8)
  (assert_eq 8 ((struct-ref s pop!)))
  (assert_eq 6 ((struct-ref s pop!)))
  (assert_eq 5 ((struct-ref s pop!)))
  (assert_error ((struct-ref s pop!))))
     
(test "int -> int stack"
  (let s (Stack (FunC int? int?) 5))
  ((struct-ref s push!) (lambda (x) (+ x 2)))
  (assert_error ((struct-ref s push!) (lambda (x y) (+ x y))))
  (let f ((struct-ref s pop!)))
  (assert_eq 10 (f 8))
  (assert_error (f "nine")))

