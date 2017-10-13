#lang racket/base

(require "../private/parser.rkt")

(module+ test
  (require rackunit)
  (define (test-parse str result)
    (check-equal? (syntax->datum
                    (parse-dssl2 #false (open-input-string str) #false))
                  result))
  (define-syntax-rule (check-parse? source body ...)
    (test-parse source
                '(begin body ...)))

  ; simple expressions

  (check-parse? "a" a)
  (check-parse? "5" 5)
  (check-parse? "-5E-2"
                (- 5E-2))
  (check-parse? "v[i]"
                (vector-ref v i))
  (check-parse? "s.f"
                (struct-ref s f))
  (check-parse? "[0, 1, 2]"
                (vector 0 1 2))
  (check-parse? "[0, 1, 2,]"
                (vector 0 1 2))
  (check-parse? "[0; 10]"
                (make-vector 10 0))
  (check-parse? "posn { x: 3, y: 4 }"
                (m:posn [x 3] [y 4]))
  (check-parse? "a == 4"
                (== a 4))
  (check-parse? "lambda x, y: x == y"
                (lambda (x y) (== x y)))
  (check-parse? "λ x, y: x == y"
                (lambda (x y) (== x y)))
  (check-parse? "f(3, x)"
                (f 3 x))
  (check-parse? "\na"
                a)

  ; compound expressions

  (check-parse? "a + b * c + d"
                (+ (+ a (* b c)) d))
  (check-parse? "a ** b ** c"
                (** a (** b c)))
  (check-parse? "a ** b ** c == 5"
                (== (** a (** b c)) 5))
  (check-parse? "a + -6"
                (+ a (- 6)))
  (check-parse? "[5, lambda x: x + 1]"
                (vector 5 (lambda (x) (+ x 1))))
  (check-parse? "a.b.c"
                (struct-ref (struct-ref a b) c))

  ; simple statements

  (check-parse? "a = b"
                (setf! a b))
  (check-parse? "a = b; c = d.e"
                (setf! a b)
                (setf! c (struct-ref d e)))
  (check-parse? "a = b\nc = d\n"
                (setf! a b)
                (setf! c d))
  (check-parse? "let x"
                (let (x any/c)))
  (check-parse? "defstruct posn(x, y)"
                (defstruct posn ((x any/c) (y any/c))))
  (check-parse? "a.b.c = e[f]"
                (setf! (struct-ref (struct-ref a b) c)
                       (vector-ref e f)))
  (check-parse? "assert False"
                (assert #f))
  (check-parse? "assert_eq a + 1, 6"
                (assert_eq (+ a 1) 6))

  ; compound statements

  (check-parse? "if a: c = d"
                (cond [a (setf! c d)] [else (pass)]))
  (check-parse? "if a: c = d\nelse: e = f"
                (cond [a (setf! c d)] [else (setf! e f)]))
  (check-parse? "if a: c = d\nelif b: e = 3\nelse: f = 4"
                (cond [a (setf! c d)]
                      [b (setf! e 3)]
                      [else (setf! f 4)]))
  (check-parse? "if a:\n  c = d"
                (cond [a (setf! c d)]
                      [else (pass)]))
  (check-parse? "if a:\n  c = d\n  e[0] = 9"
                (cond [a (setf! c d)
                         (setf! (vector-ref e 0) 9)]
                      [else (pass)]))
  (check-parse? "if a:\n  if b:\n    5"
                (cond [a (cond [b 5]
                               [else (pass)])]
                      [else (pass)]))
  (check-parse? "while True:\n  a = 6\n  b = 7"
                (while #t (setf! a 6) (setf! b 7)))
  (check-parse? (string-append "def fact(n):\n"
                               "  if n <= 1: return 1\n"
                               "  else: return n * fact(n - 1)")
                (def (fact () (n any/c))
                     any/c
                     (cond [(<= n 1) (return 1)]
                           [else     (return (* n (fact (- n 1))))])))
  (check-parse? (string-append "for j in v:\n"
                               "  println(j)")
                (for [j v] (println j)))
  (check-parse? (string-append "for i, j in v:\n"
                               "  println(i, j)")
                (for [(i j) v] (println i j)))

  (check-parse? "pass"
                (pass))
  (check-parse? "pass    "
                (pass))
  (check-parse? "pass    \n"
                (pass))
  (check-parse? "pass\n"
                (pass))
  (check-parse? "pass\n    "
                (pass))
  (check-parse? "pass\n    \n"
                (pass)))
