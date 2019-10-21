#lang racket/base

(provide do-comment
         do-uncomment
         do-toggle-comment)

(require "line-summary.rkt"
         "editor-helpers.rkt"
         (only-in racket/class send))

(define *indent-size* 4)

; text% ([listof Line-summary]) ->
(define (do-toggle-comment text [summaries (summarize-span text)])
  (case (classify-span summaries)
    [(code)     (do-comment text summaries)]
    [(comment)  (do-uncomment text summaries)]
    [else       (void)]))

; text% ([listof Line-summary]) ->
(define (do-comment text [summaries (summarize-span text)])
  (let ([indent (find-span-indent summaries)])
    (with-edit-sequence (text)
      (for/fold ([adjust 0])
                ([summary    (in-list summaries)])
        (cond
          [(line-summary-blank? summary)
           adjust]
          [else
           (define text-start (+ (line-summary-start summary) indent adjust))
           (send text insert 2 "# " text-start)
           (+ adjust 2)])))))

; text% ([listof Line-summary]) ->
(define (do-uncomment text [summaries (summarize-span text)])
  (let ([indent (find-span-indent summaries)])
    (with-edit-sequence (text)
      (for/fold ([adjust 0])
                ([summary (in-list summaries)])
        (cond
          [(line-summary-hash summary)
           =>
           (λ (hash-start)
             (define hash-limit     (line-summary-hash-limit summary))
             (define adj-hash-start (- hash-start adjust))
             (define pre-comment-gap
               (cond
                 [(and (= (add1 hash-start) hash-limit)
                       (line-summary-comm summary))
                  =>
                  (λ (comm-start) (- comm-start hash-limit))]
                 [else 0]))
             (define change (add1 (modulo pre-comment-gap *indent-size*)))
             (send text delete adj-hash-start (+ adj-hash-start change))
             (+ adjust change))]
          [else adjust])))))


