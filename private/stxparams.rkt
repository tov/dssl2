#lang racket/base

(provide dssl-return with-return
         dssl-break with-break
         dssl-continue with-continue
         inc-passed-tests! inc-total-tests! with-test-counters
         wrap-procedure-body
         with-masked-control)
(require (for-syntax racket/base
                     "errors.rkt")
         racket/stxparam
         racket/splicing
         syntax/parse/define)

(begin-for-syntax
  (define (stx-param-requires kw cxt)
    (define message (format "~a must appear in ~a" kw cxt))
    (λ (stx) (syntax-error stx message)))

  (define (stx-param-forbids kw cxt)
    (define message (format "~a cannot be used in ~a" kw cxt))
    (λ (stx) (syntax-error stx message)))

  (define expand-no-return
    (stx-param-requires "return statement" "a function"))
  (define expand-no-break
    (stx-param-requires "break statement" "a loop"))
  (define expand-no-continue
    (stx-param-requires "break statement" "a loop"))

  (define expand-no-test-block
    (stx-param-forbids "test blocks" "the interactions window")))

(define-syntax-parameter dssl-return expand-no-return)

(define-simple-macro (with-return body)
  (let/ec return-f
    (syntax-parameterize
      ([dssl-return (syntax-rules ()
                      [(_)        (return-f (void))]
                      [(_ result) (return-f result)])])
      body)))

(define-syntax-parameter dssl-break expand-no-break)
(define-syntax-parameter dssl-continue expand-no-continue)

(define-simple-macro (with-break body)
  (let/ec break-f
    (syntax-parameterize
      ([dssl-break (λ (stx) #'(break-f (void)))])
      body)))

(define-simple-macro (with-continue body)
  (let/ec continue-f
    (syntax-parameterize
      ([dssl-continue (λ (stx) #'(continue-f))])
      body)))

(define-syntax-parameter inc-passed-tests! expand-no-test-block)
(define-syntax-parameter inc-total-tests! expand-no-test-block)

(define-simple-macro
  (with-test-counters [passed++:id total++:id] body:expr)
  (splicing-syntax-parameterize
    ([inc-passed-tests! (λ (stx) #'(passed++))]
     [inc-total-tests!  (λ (stx) #'(total++))])
    body))

(define-simple-macro (with-masked-control body)
  (syntax-parameterize
    ([dssl-break     expand-no-break]
     [dssl-continue  expand-no-continue]
     [dssl-return    expand-no-return])
    body))

(define-simple-macro (wrap-procedure-body body)
  (with-masked-control (with-return body)))

