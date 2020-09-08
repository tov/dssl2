#lang racket/base

(require (only-in racket/contract contract-out))
(provide AxisSpec LineSpec PlotSpec PointSpec TransformSpec
         (contract-out
          [plot (->* (string? PlotSpec) (AxisSpec AxisSpec) any/c)]))

(require (prefix-in p: plot))
(require (only-in racket/contract ->* and/c any/c or/c vector/c vectorof)
         (only-in racket/format ~a)
         (only-in racket/match match))

; A flat, non-empty vectorof contract.
(define (ne-vec-of ctc)
  (and/c
   (vectorof ctc #:flat? #t)
   vector-non-empty?))

(define (vector-non-empty? v)
  (> (vector-length v) 0))

; A flat pair-as-vector contract.
(define (pair-of ctc1 ctc2)
  (vector/c ctc1 ctc2 #:flat? #t))

; A PointSpec is [vector real? real?]
(define PointSpec (pair-of real? real?))

; A LineSpec is [vector PointSpec ...+]
(define LineSpec (ne-vec-of PointSpec))

; A LabeledLineSpec is [vector string? LineSpec]
(define LabeledLineSpec (pair-of string? LineSpec))

; A PlotSpec is one of:
;  - LineSpec
;  - [vector LabeledLineSpec ...+]
(define PlotSpec
  (or/c LineSpec
        (ne-vec-of LabeledLineSpec)))

; A TransformSpec is one of:
;  - "id"
;  - "log"
(define TransformSpec
  (or/c "id"
        "log"))

; An AxisSpec is one of:
;  - string?                         -- label, no transform
;  - [vector string? TransformSpec]  -- label & transform
(define AxisSpec
  (or/c string?
        (pair-of string? TransformSpec)))


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

; axis-spec->label : AxisSpec -> string?
(define (axis-spec->label axis)
  (match axis
    [(? string?)      axis]
    [(vector label _) label]))

; axis-spec->function : AxisSpec -> invertible-function?
(define (axis-spec->inv-fun axis)
  (match axis
    [(regexp #px"^log\\b") p:log-transform]
    [(? string?)           p:id-transform]
    [(vector _ ts)         (trans-spec->inv-fun ts)]))

; trans-spec->inv-fun : TransformSpec -> invertible-function?
(define (trans-spec->inv-fun ts)
  (case ts
    [("id")  p:id-transform]
    [("log") p:log-transform]))