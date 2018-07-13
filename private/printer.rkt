#lang racket/base

(provide dssl-print
         dssl-fprintf
         dssl-printf
         dssl-format
         dssl-contract-name)
(require "struct.rkt")
(require "object.rkt")
(require "generic.rkt")
(require "errors.rkt")
(require (only-in racket/contract
                  contract-name
                  contract?)
         (only-in racket/set
                  mutable-seteq
                  set-member?
                  set-add!)
         (only-in racket/contract/base contract-name)
         (only-in racket/string string-contains?)
         (only-in racket/math nan?))

(define current-printer-state (make-parameter #f))

(define (dssl-contract-name c)
  (cond
    [(or (boolean? c) (number? c) (string? c) (char? c))
     (dssl-format "%p" c)]
    [else
      (contract-name c)]))

(current-dssl-contract-name dssl-contract-name)

(define (dssl-printf fmt . params)
  (apply dssl-fprintf (current-output-port) fmt params))

(define (dssl-format fmt . params)
  (let ([port (open-output-string)])
    (apply dssl-fprintf port fmt params)
    (get-output-string port)))

(current-dssl-error-format
  (λ (fmt . args)
     (if (string-contains? fmt "~")
       (apply format fmt args)
       (apply dssl-format fmt args))))

(define (dssl-fprintf port fmt . params)
  (define parsed-fmt (parse-format-string fmt))
  (define expected-params (length (filter symbol? parsed-fmt)))
  (define actual-params (length params))
  (unless (= expected-params actual-params)
    (dssl-error
      (string-append
        "dssl-printer: format string did not match number of params\n"
        "  format string:   ~s\n"
        "  expected params: ~a\n"
        "  actual params:   ~a")
      fmt expected-params actual-params))
  (let loop ([commands parsed-fmt]
             [params   params])
    (unless (null? commands)
      (define command (car commands))
      (cond
        [(eq? command 'string)
         (define param (car params))
         (if (or (string? param) (char? param) (symbol? param))
           (display param port)
           (dssl-print param port #f))
         (loop (cdr commands) (cdr params))]
        [(eq? command 'debug)
         (dssl-print (car params) port #t)
         (loop (cdr commands) (cdr params))]
        [(eq? command 'print)
         (dssl-print (car params) port #f)
         (loop (cdr commands) (cdr params))]
        [else
         (display command port)
         (loop (cdr commands) params)]))))

; String -> (List-of (Or 'debug 'print String))
(define (parse-format-string s)
  (for/list ([chunk (in-list (regexp-match* #rx"[^%]+|%.|%$" s))])
    (cond
      [(string=? chunk "%d") 'debug]
      [(string=? chunk "%p") 'print]
      [(string=? chunk "%s") 'string]
      [(string=? chunk "%%") "%"]
      [(regexp-match? #rx"^%" chunk)
       (dssl-error "dssl-printer: bad format string code ~s" chunk)]
      [else chunk])))

; Dssl2Value OutputPort -> Void
(define (dssl-print value0 [port (current-output-port)] [debug? #f])
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
        [(boolean? value)
         (cond
           [value                   (display "True" port)]
           [else                    (display "False" port)])]
        [(char? value)
         (fprintf port "char(~a)" (char->integer value))]
        [(string? value)
         (let
           ([contains-sq (string-contains? value "'")]
            [contains-dq (string-contains? value "\"")])
           (if (and contains-sq (not contains-dq))
             (dssl-debug-string #\" value port)
             (dssl-debug-string #\' value port)))]
        [(vector? value)
         (unless (seen!? value)
           (define first #t)
           (display #\[ port)
           (for ([element (in-vector value)])
             (if first
               (set! first #f)
               (display ", " port))
             (visit element))
           (display #\] port))]
        [(struct-base? value)
         (unless (seen!? value)
           (write-struct value port visit))]
         [(object-base? value)
          (unless (seen!? value)
           (cond
             [(and (not debug?)
                   (get-method-value/fun value '__print__))
              =>
              (λ (value.__print__) (value.__print__ (make-print port)))]
             [else (write-object value port visit)]))]
        [(generic-proc? value)
         (fprintf port "#<~a:~a>"
                  (generic-proc-tag value)
                  (generic-base-name value))]
        [(procedure? value)
         (cond
           [(contract? value)
            (define name (format "~a" (contract-name value)))
            (if (string=? name "???")
              (fprintf port "#<proc>")
              (fprintf port "#<proc:~a>" name))]
           [(object-name value)
            (fprintf port "#<proc:~a>" (object-name value))]
           [else
             (fprintf port "#<proc>")])]
        [(contract? value)
         (fprintf port "#<contract:~a>" (contract-name value))]
        [(void? value)              (display "#<void>" port)]
        [else                       (display "#<unprintable>" port)]))))

(define (make-print port)
  (λ (fmt . args)
     (apply dssl-fprintf port fmt args)))

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

(define (dssl-debug-string q str port)
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
