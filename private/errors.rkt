#lang racket

(provide runtime-error
         type-error
         assertion-error)

(define (runtime-error fmt . args)
  (error (apply format (string-append "Runtime error: " fmt) args)))

(define (type-error who got expected)
  (runtime-error "~a: got ~s where ~a expected" who got expected))

(define (assertion-error fmt . args)
  (error (apply format (string-append "Assertion failed: " fmt) args)))

