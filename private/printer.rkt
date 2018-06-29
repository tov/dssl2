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

(define current-printer-state (make-parameter #f))

; Dssl2Value OutputPort -> Void
(define (dssl-print value0 [port (current-output-port)])
  (parameterize
    ([current-printer-state (or (current-printer-state)
                                (cons (mutable-seteq)
                                      (find-cycles value0)))])
    (define seen (car (current-printer-state)))
    (define cycle-info (cdr (current-printer-state)))
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
        [(struct-base? value)
         (unless (seen!? value)
           (write-struct value port visit))]
        [(object-base? value)
         (unless (seen!? value)
           (cond
             [(get-method-value value '__print__)
              =>
              (λ (object-print)
                 (object-print
                   (λ args (apply fprintf port args))))]
             [else (write-object value port visit)]))]
        [(and (contract? value)
              (not (string=? "???" (format "~a" (contract-name value)))))
         (display (contract-name value) port)]
        [(procedure? value)
         (if (object-name value)
           (fprintf port "#<proc:~a>" (object-name value))
           (fprintf port "#<proc>"))]
        [(void? value)              (display "#<void>" port)]
        [else                       (display "#<unknown-value>" port)]))))

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
           (visit ((field-info-getter field) value))))]
      [(object-base? value)
       (unless (seen!? value)
         (for ([field-pair (in-vector ((object-base-reflect value)))])
           (visit (cdr field-pair))))]))
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
