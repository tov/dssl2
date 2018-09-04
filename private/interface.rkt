#lang racket/base

(provide (struct-out imethod-info)
         (struct-out interface-info)
         reflect-interface
         compare-imethod-info
         interface-info-all-tokens
         interface-info-all-tokens/list
         check-interface-consistency)

(require "errors.rkt")

(struct imethod-info
  [name         ; syntax? - the name of the method
    cvs         ; nat? - the number of contract parameters
    arity]      ; nat? - the number of regular parameters
  #:transparent)
(struct interface-info
  [name         ; syntax? - the name of the interface
    token       ; symbol? - the runtime identity of the interface
    runtime     ; syntax? - the name of the runtime method table
    supers      ; (ListOf interface-info?) - list of super interfaces
    methods]    ; (ListOf imethod-info?) - list of methods
  #:transparent)

(define (reflect-imethod info)
  #`(imethod-info #'#,(imethod-info-name info)
                  #,(imethod-info-cvs info)
                  #,(imethod-info-arity info)))

(define (reflect-interface info)
  #`(interface-info #'#,(interface-info-name info)
                    '#,(interface-info-token info)
                    #'#,(interface-info-runtime info)
                    (list
                      #,@(map reflect-interface
                              (interface-info-supers info)))
                    (list
                      #,@(map reflect-imethod
                              (interface-info-methods info)))))

(define (compare-imethod-info i-name expected actual)
  (define expected-cvs   (imethod-info-cvs expected))
  (define actual-cvs     (imethod-info-cvs actual))
  (define expected-arity (imethod-info-arity expected))
  (define actual-arity   (imethod-info-arity actual))
  (cond
    [(not (= expected-cvs actual-cvs))
     (format "method ~a takes ~a contract params, but interface ~a specifies ~a"
             (syntax-e (imethod-info-name expected))
             expected-cvs
             i-name
             actual-cvs)]
    [(not (= expected-arity actual-arity))
     (format "method ~a takes ~a params, but interface ~a specifies ~a"
             (syntax-e (imethod-info-name expected))
             expected-arity
             i-name
             actual-arity)]
    [else #f]))

(define (interface-info-all-tokens info)
  (cons (interface-info-token info)
        (interface-info-all-tokens/list (interface-info-supers info))))

(define (interface-info-all-tokens/list infos)
  (apply append (map interface-info-all-tokens infos)))

(define (check-interface-consistency info0)
  (define method-infos (make-hasheq))
  (define (loop info)
    (define (add-method m-info)
      (define m-name (syntax-e (imethod-info-name m-info)))
      (cond
        [(hash-ref method-infos m-name #f)
         =>
         (λ (other-i-name-&-m-info)
            (define i-name (car other-i-name-&-m-info))
            (define other-m-info (cdr other-i-name-&-m-info))
            (cond
              [(compare-imethod-info i-name other-m-info m-info)
               =>
               (λ (message)
                  (syntax-error
                    (list (imethod-info-name m-info)
                          (imethod-info-name other-m-info))
                    message))]
              [else (void)]))]
        [else
          (hash-set! method-infos m-name
                     (cons (syntax-e (interface-info-name info))
                           m-info))]))
    (for-each loop (interface-info-supers info))
    (for-each add-method (interface-info-methods info)))
  (loop info0))
