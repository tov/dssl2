#lang racket/base

(provide dssl-print-size-hook
         dssl-print-print-hook)
(require "struct.rkt")
(require (only-in racket/contract/base contract? contract-name)
         (only-in racket/math nan?)
         (only-in racket/string string-contains?))

(define (dssl-print-size-hook value _write _port)
  (define port (open-output-string))
  (print-dssl-value value port)
  (string-length (get-output-string port)))

(define (dssl-print-print-hook value _write port)
  (print-dssl-value value port))

(define (print-dssl-value value port)
  (cond
    [(real? value)
     (cond
       [(= +inf.0 value)        (display "inf" port)]
       [(= -inf.0 value)        (display "-inf" port)]
       [(nan? value)            (display "nan" port)]
       [else                    (display value port)])]
    [(integer? value)           (display value port)]
    [(boolean? value)
     (cond
       [value                   (display "True" port)]
       [else                    (display "False" port)])]
    [(string? value)
     (define contains-sq (string-contains? value "'"))
     (define contains-dq (string-contains? value "\""))
     (if (and contains-sq (not contains-dq))
       (print-dssl-string #\" value port)
       (print-dssl-string #\' value port))]
    [(vector? value)
     (print-dssl-vector value port)]
    [(struct-base? value)
     (dssl-write-struct value port print-dssl-value)]
    [(contract? value)
     (display (contract-name value) port)]
    [(void? value)
     (display "void" port)]
    [else                       (display "#<unknown-value>" port)]))

(define (print-dssl-string q str port)
  (define (esc c)
    (display #\\ port)
    (display c port))
  (display q port)
  (for ([c (in-string str)])
    (case c
      [(#\\)                            (esc #\\)]
      [(#\007)                          (esc #\a)]
      [(#\backspace)                    (esc #\b)]
      [(#\page)                         (esc #\f)]
      [(#\newline)                      (esc #\n)]
      [(#\return)                       (esc #\r)]
      [(#\tab)                          (esc #\t)]
      [(#\vtab)                         (esc #\v)]
      [(#\space)                        (display #\space port)]
      [else
        (cond
          [(char=? c q)                 (esc c)]
          [(not (char-graphic? c))
           (define hex (format "~x" (char->integer c)))
           (fprintf port
                    (if (= 1 (string-length hex)) "\\x0~a" "\\x~a")
                    hex)]
          [else                         (display c port)])]))
  (display q port))

(define (print-dssl-vector vec port)
  (display "[" port)
  (define first #t)
  (for ([element (in-vector vec)])
    (if first
      (set! first #f)
      (display ", " port))
    (print-dssl-value element port))
  (display "]" port))
