#lang racket

(provide dssl-print
         dssl-print-size-hook
         dssl-print-print-hook)

(define (dssl-print value)
  (unless (void? value)
    (pretty-print value)))

(define (dssl-print-size-hook value _write _port)
  (cond
    [(dssl-value->string value) => string-length]
    [else #false]))

(define (dssl-print-print-hook value _write port)
  (display (dssl-value->string value) port))

(define (dssl-value->string value)
  (cond
    [(real? value)
     (cond
       [(= +inf.0 value)        "inf"]
       [(= -inf.0 value)        "-inf"]
       [(nan? value)            "nan"]
       [else                    #false])]
    [(boolean? value)
     (cond
       [value                   "True"]
       [else                    "False"])]
    [(string? value)
     (define contains-sq (string-contains? value "'"))
     (define contains-dq (string-contains? value "\""))
     (if (and contains-sq (not contains-dq))
       (dssl-string->string #\" value)
       (dssl-string->string #\' value))]
    [else #false]))

(define (dssl-string->string q str)
  (define out (open-output-string))
  (define (esc c)
    (display #\\ out)
    (display c out))
  (display q out)
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
      [else
        (cond
          [(char=? c q)                 (esc c)]
          [(not (char-graphic? c))
           (define hex (format "~x" (char->integer c)))
           (fprintf out
                    (if (= 1 (string-length hex)) "\\x0~a" "\\x~a")
                    hex)]
          [else                         (display c out)])]))
  (display q out)
  (get-output-string out))

