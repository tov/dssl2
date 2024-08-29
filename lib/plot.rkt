#lang racket/base

(require (only-in racket/contract contract-out))
(require (only-in dssl2/private/prims
                  num?
                  str?
                  AndC
                  AnyC
                  OrC
                  SquareBracketC
                  VecKC
                  VecC))
(require (only-in dssl2/private/generic
                  generic-base-instantiate))

(provide AxisSpec LineSpec PlotSpec PointSpec TransformSpec
         (contract-out
          [plot (->* (str? PlotSpec) (AxisSpec AxisSpec) AnyC)]))

(require (prefix-in p: plot))
(require (only-in racket/contract ->*)
         (only-in racket/format ~a)
         (only-in racket/match match))

(define VecC/proc  (generic-base-instantiate VecC))
(define VecKC/proc (generic-base-instantiate VecKC))

; A flat, non-empty vectorof contract.
(define NEVecC
  (SquareBracketC "NEVecC"
                  (λ (ctc)
                     (AndC
                       (λ (v) (> (vector-length v) 0))
                       (VecC/proc ctc)))))
(define NEVecC/proc (generic-base-instantiate NEVecC))

; A PointSpec is [vector num? num?]
(define PointSpec (VecKC/proc num? num?))

; A LineSpec is [vector PointSpec ...+]
(define LineSpec (NEVecC/proc PointSpec))

; A LabeledLineSpec is [vector str? LineSpec]
(define LabeledLineSpec (VecKC/proc str? LineSpec))

; A PlotSpec is one of:
;  - LineSpec
;  - [vector LabeledLineSpec ...+]
(define PlotSpec
  (OrC LineSpec
       (NEVecC/proc LabeledLineSpec)))

; A TransformSpec is one of:
;  - "id"
;  - "log"
(define TransformSpec
  (OrC "id"
       "log"))

; An AxisSpec is one of:
;  - str?                         -- label, no transform
;  - [vector str? TransformSpec]  -- label & transform
(define AxisSpec
  (OrC str?
       (VecKC/proc str? TransformSpec)))


(define (plot title plot-spec [x-axis ""] [y-axis ""])
  (parameterize ([p:plot-font-size   20]
                 [p:plot-x-transform (axis-spec->inv-fun x-axis)]
                 [p:plot-y-transform (axis-spec->inv-fun y-axis)])
    (p:plot
     (build-renderer plot-spec)
     #:title title
     #:x-label (axis-spec->label x-axis)
     #:y-label (axis-spec->label y-axis))))

; build-renderer : PlotSpec -> renderer2d?
(define (build-renderer plot-spec)
  (cond
    [(real? (vector-ref (vector-ref plot-spec 0) 0))
     (p:lines plot-spec)]
    [else
     (for/list ([line-spec (in-vector plot-spec)]
                [index     (in-naturals 1)])
       (match line-spec
         [(vector label points)
          (p:lines points #:label label #:color index)]))]))

; axis-spec->label : AxisSpec -> str?
(define (axis-spec->label axis)
  (match axis
    [(? str?)         axis]
    [(vector label _) label]))

; axis-spec->function : AxisSpec -> invertible-function?
(define (axis-spec->inv-fun axis)
  (match axis
    [(regexp #px"^log\\b") p:log-transform]
    [(? str?)              p:id-transform]
    [(vector _ ts)         (trans-spec->inv-fun ts)]))

; trans-spec->inv-fun : TransformSpec -> invertible-function?
(define (trans-spec->inv-fun ts)
  (case ts
    [("id")  p:id-transform]
    [("log") p:log-transform]))
