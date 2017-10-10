#lang racket/base

(provide setup-rte)

(require (only-in racket/pretty
                  pretty-print
                  pretty-print-size-hook
                  pretty-print-print-hook)
         (only-in racket/port
                  relocate-input-port)
         "parser.rkt"
         "printer.rkt")

(define (setup-rte)
  (pretty-print-size-hook dssl-print-size-hook)
  (pretty-print-print-hook dssl-print-print-hook)
  (global-port-print-handler
    (λ (v port)
       (pretty-print v port #:newline? #false)))
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

