#lang racket/base

(provide dssl-print)
(require "struct.rkt")
(require "object.rkt")
(require (only-in racket/set
                  mutable-seteq
                  set-member?
                  set-add!)
         (only-in racket/contract/base
                  contract?
                  contract-name)
         (only-in racket/math nan?)
         (only-in racket/string string-contains?))

; Dssl2Value OutputPort -> Void
(define (dssl-print value0 [port (current-output-port)])
  (define seen (mutable-seteq))
  (define cycle-info (find-cycles value0))
  (define (seen!? value)
    (define info (hash-ref cycle-info value #false))
    (cond
      [(set-member? seen value)
       (fprintf port "#~a#" info)
       #true]
      [(number? info)
        (set-add! seen value)
        (fprintf port "#~a=" info)
        #false]
      [else
        #false]))
  (let visit ([value value0])
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
       (unless (seen!? value)
         (display "[" port)
         (define first #t)
           (for ([element (in-vector value)])
             (if first
               (set! first #f)
               (display ", " port))
             (visit element))
           (display "]" port))]
      [(struct-base? value)
       (unless (seen!? value)
         (define info (struct-base-struct-info value))
         (define first #true)
         (fprintf port "~a {" (struct-info-name info))
         (for ([field-info (in-vector (struct-info-field-infos info))])
           (if first
             (set! first #f)
             (display ", " port))
           (fprintf port "~a: " (field-info-name field-info))
           (visit ((field-info-getter field-info) value)))
         (display "}" port))]
      [(object-base? value)
       (fprintf port "#<object:~a>"
                (object-info-name (object-base-object-info value)))]
      [(and (contract? value)
            (not (string=? "???" (format "~a" (contract-name value)))))
       (display (contract-name value) port)]
      [(procedure? value)
       (if (object-name value)
         (fprintf port "#<proc:~a>" (object-name value))
         (fprintf port "#<proc>"))]
      [(void? value)              (display "#<void>" port)]
      [else                       (display "#<unknown-value>" port)])))

; Dssl2Value -> [HashEq Dssl2Value [Or #true Natural]]
(define (find-cycles value0)
  (define table (make-hasheq))
  (define revisited-count 0)
  (define (seen!? value)
    (define value-info (hash-ref table value #false))
    (cond
      [(number? value-info) #true]
      [value-info
        (hash-set! table value revisited-count)
        (set! revisited-count (add1 revisited-count))
        #true]
      [else
        (hash-set! table value #true)
        #false]))
  (define (visit value)
    (cond
      [(vector? value)
       (unless (seen!? value)
         (for ([element (in-vector value)])
           (visit element)))]
      [(struct-base? value)
       (unless (seen!? value)
         (define info (struct-base-struct-info value))
         (for ([field (in-vector (struct-info-field-infos info))])
           (visit ((field-info-getter field) value))))]))
  (visit value0)
  table)

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
