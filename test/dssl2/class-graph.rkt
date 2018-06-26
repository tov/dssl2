#lang s-exp dssl2/language

(import cons)

(let Vertex nat?)
(let Weight num?)
(let MaybeWeight (OrC False Weight))

(interface WU_GRAPH
  (def (get_size self) #:-> nat?)
  (def (get_edge self [u Vertex] [v Vertex]) #:-> MaybeWeight)
  (def (set_edge! self [u Vertex] [v Vertex] [w MaybeWeight]) #:-> VoidC)
  (def (get_adjacent self [u Vertex]) #:-> [ListOfC Vertex]))

(class WuGraph #:implements WU_GRAPH
  (let [size nat?])
  (let [matrix vec?])

  (def (__init__ self size)
    (= (struct-ref self size) size)
    (= (struct-ref self matrix) (for/vec [_ size] (for/vec [_ size] False))))

  (def (get_size self)
    (struct-ref self size))

  (def (get_edge self u v)
    (vec-ref (vec-ref (struct-ref self matrix) u) v))

  (def (set_edge! self u v w)
    (= (vec-ref (vec-ref (struct-ref self matrix) u) v) w)
    (= (vec-ref (vec-ref (struct-ref self matrix) v) u) w))

  (def (get_adjacent self u)
    (let result (nil))
    (for [v (struct-ref self size)]
      (if
       [((struct-ref self get_edge) u v)
        (= result (cons v result))]
       [else pass]))
    result))

(test "WuGraph"
  (let g (WuGraph 8))
  (assert_eq False ((struct-ref g get_edge) 0 2))
  ((struct-ref g set_edge!) 0 2 5)
  (assert_eq 5 ((struct-ref g get_edge) 0 2))
  (assert_eq 5 ((struct-ref g get_edge) 2 0))
  (assert_eq (cons 2 (nil)) ((struct-ref g get_adjacent) 0)))