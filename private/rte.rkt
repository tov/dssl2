#lang racket/base

(provide setup-rte)

(require (only-in racket/port
                  relocate-input-port)
         "parser.rkt"
         "printer.rkt")

(define (setup-rte)
  (global-port-print-handler
    (λ (value port) (dssl-print value port)))
  (current-read-interaction
    (λ (src in)
       (let loop ()
         (define-values (line column position) (port-next-location in))
         (define the-line (read-line in))
         (cond
           [(eof-object? the-line)
            the-line]
           [(regexp-match #rx"^ *$" the-line)
            (loop)]
           [else
             (define line-port (open-input-string the-line))
             (port-count-lines! line-port)
             (define relocated
               (relocate-input-port line-port line column position
                                    #true #:name src))
             (parse-dssl2 src relocated #t)])))))

