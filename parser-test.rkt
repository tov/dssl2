#lang racket

(require dssl2/parser)

(module+ test
  (require rackunit)
  (define (test-parse str result)
    (check-equal? (syntax->datum
                   (parse-dssl2
                    (open-input-string (string-append str "\n"))))
                  result))

  ; simple expressions
  
  (test-parse "a"
              '(begin a))
  (test-parse "5"
              '(begin 5))
  (test-parse "-5E-2"
              '(begin (- 5E-2)))
  (test-parse "v[i]"
              '(begin (vector-ref v i)))
  (test-parse "s.f"
              '(begin (struct-ref s f)))
  (test-parse "[0, 1, 2]"
              '(begin (vector 0 1 2)))
  (test-parse "[0, 1, 2,]"
              '(begin (vector 0 1 2)))
  (test-parse "[0; 10]"
              '(begin (make-vector 10 0)))
  (test-parse "posn { x: 3, y: 4 }"
              '(begin (posn [x 3] [y 4])))
  (test-parse "a == 4"
              '(begin (== a 4)))
  (test-parse "lambda x, y: x == y"
              '(begin (lambda (x y) (== x y))))
  (test-parse "λ x, y: x == y"
              '(begin (lambda (x y) (== x y))))
  (test-parse "f(3, x)"
              '(begin (f 3 x)))

  ; compound expressions

  (test-parse "a + b * c + d"
              '(begin (+ (+ a (* b c)) d)))
  (test-parse "a ** b ** c"
              '(begin (** a (** b c))))
  (test-parse "a ** b ** c == 5"
              '(begin (== (** a (** b c)) 5)))
  (test-parse "a + -6"
              '(begin (+ a (- 6))))
  (test-parse "[5, lambda x: x + 1]"
              '(begin (vector 5 (lambda (x) (+ x 1)))))
  
  ; simple statements

  (test-parse "a = b\n"
              '(begin (setf! a b)))
  (test-parse "a = b; c = d.e\n"
              '(begin (setf! a b) (setf! c (struct-ref d e))))
  (test-parse "a = b\nc = d\n"
              '(begin (setf! a b) (setf! c d)))
  (test-parse "let x\n"
              '(begin (define x #f)))
  (test-parse "defstruct posn(x, y)\n"
              '(begin (define-struct posn (x y))))

  ; compound statements
  
  (test-parse "if a: c = d\n"
              '(begin (cond [a (setf! c d)] [else (pass)])))
  (test-parse "if a: c = d\nelse: e = f\n"
              '(begin (cond [a (setf! c d)] [else (setf! e f)])))
  (test-parse "if a: c = d\nelif b: e = 3\nelse: f = 4\n"
              '(begin (cond [a (setf! c d)]
                            [b (setf! e 3)]
                            [else (setf! f 4)])))
  (test-parse "if a:\n  c = d\n"
              '(begin (cond [a (setf! c d)]
                            [else (pass)])))
  (test-parse "if a:\n  c = d\n  e[0] = 9\n"
              '(begin (cond [a (setf! c d)
                               (setf! (vector-ref e 0) 9)]
                            [else (pass)])))
  (test-parse "if a:\n  if b:\n    5\n"
              '(begin (cond [a (cond [b 5]
                                     [else (pass)])]
                            [else (pass)])))
  (test-parse "while True:\n  a = 6\n  b = 7"
              '(begin (while #t (setf! a 6) (setf! b 7))))
  (test-parse (string-append "def fact(n):\n"
                             "  if n <= 1: return 1\n"
                             "  else: return n * fact(n - 1)")
              '(begin (define (fact n)
                        (cond [(<= n 1) (return 1)]
                              [else     (return (* n (fact (- n 1))))]))))
  (test-parse (string-append "for j in v:\n"
                             "  println(j)")
              '(begin (for [j v] (println j))))
  (test-parse (string-append "for i, j in v:\n"
                             "  println(i, j)")
              '(begin (for [(i j) v] (println i j)))))
