#lang racket

(provide make-struct struct? struct-name struct-fields struct-assq
         make-field field-name field-value set-field-value!
         make-vec vec? unvec)

(define (write-field field port mode)
  (let ([recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (λ (p port) (print p port mode))])])
    (display (field-name field) port)
    (display ": " port)
    (recur (field-value field) port)))

(define (write-struct struct port mode)
  (let ([recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (λ (p port) (print p port mode))])])
    (display (struct-name struct) port)
    (display "{" port)
    (define first #t)
    (for ([field (struct-fields struct)])
      (if first
        (set! first #f)
        (display ", " port))
      (recur field port))
    (display "}" port)))

(define-struct struct [name fields]
               #:transparent
               #:methods gen:custom-write
               [(define write-proc write-struct)])

(define-struct field [name (value #:mutable)]
               #:transparent
               #:methods gen:custom-write
               [(define write-proc write-field)])

(define (struct-assq name fields)
  (cond
    [(memf (λ (field) (eq? (field-name field) name)) fields)
     => first]
    [else #false]))

(define (write-vec vec port mode)
  (let ([recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (λ (p port) (print p port mode))])])
    (display "[" port)
    (define first #t)
    (for ([element (in-vector (vec-contents vec))])
      (if first
        (set! first #f)
        (display ", " port))
      (recur element port))
    (display "]" port)))

(define-struct vec [contents]
               #:transparent
               #:methods gen:custom-write
               [(define write-proc write-vec)])

(define (unvec v)
  (if (vec? v)
    (vec-contents v)
    (error (format "Runtime error: expected vector, got ~s instead." v))))

