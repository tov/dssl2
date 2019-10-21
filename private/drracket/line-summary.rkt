#lang racket/base

(provide (struct-out line-summary)
         line-summary-indent
         line-summary-blank?
         summarize-span
         summarize-line
         summarize-line/ls)

(require "editor-helpers.rkt"
         (only-in racket/class send))


; An opt-nat is (or/c nat #f]

; A Line-summary is
;   (line-summary nat opt-nat opt-nat opt-nat opt-nat opt-nat nat)
(struct line-summary [start        ; nat
                      text         ; opt-nat
                      text-limit   ; ^ ditto (correlated)
                      hash         ; opt-nat
                      hash-limit   ; ^ ditto (correlated)
                      comm         ; opt-nat (comm : nat implies hash : nat)
                      comm-limit   ; ^ ditto (correlated)
                      limit]       ; nat
  #:transparent)


; Line-summary -> nat
; Gets the summarized line's indent.
(define (line-summary-indent summary)
  (cond
    [(or (line-summary-text summary)
         (line-summary-hash summary)
         (line-summary-limit summary))
     =>
     (λ (position) (- position (line-summary-start summary)))]
    [else 0]))

; Line-summary -> boolean
(define (line-summary-blank? summary)
  (and (not (line-summary-text summary))
       (not (line-summary-hash summary))))


; summarize-span : text% (nat nat) -> [listof Line-summary]
(define (summarize-span text
                        [start (send text get-start-position)]
                        [end   (send text get-end-position)])
  (let loop ([next end] [acc '()])
    (if (< next start)
        acc
        (let ([summary (summarize-line text next)])
          (loop (sub1 (line-summary-start summary)) (cons summary acc))))))


; summarize-line : text% nat -> Line-summary
(define (summarize-line text position)
  (summarize-line/ls text (find-line-start text position)))

; summarize-line : text% nat -> Line-summary
(define (summarize-line/ls text-obj start)
  (define text       #f)
  (define text-limit #f)
  (define hash       #f)
  (define hash-limit #f)
  (define comm       #f)
  (define comm-limit #f)
  (define limit      #f)
  
  (define (get pos)
    (send text-obj get-character pos))
  
  (define (scan-start pos)
    (case (get pos)
      [(#\space)         (scan-start (add1 pos))]
      [(#\#)             (set! hash pos)
                         (scan-hash (add1 pos))]
      [(#\newline #\nul) (set! limit pos)]
      [else              (set! text pos)
                         (scan-text pos)]))
  
  (define (scan-text pos)
    (case (get pos)
      [(#\space)         (scan-text (add1 pos))]
      [(#\#)             (set! hash pos)
                         (scan-hash (add1 pos))]
      [(#\newline #\nul) (set! limit pos)]
      [else              (set! text-limit (add1 pos))
                         (scan-text (add1 pos))]))

  (define (scan-hash pos)
    (case (get pos)
      [(#\#) (scan-hash (add1 pos))]
      [else  (set! hash-limit pos)
             (scan-post-hash pos)]))

  (define (scan-post-hash pos)
    (case (get pos)
      [(#\space)          (scan-post-hash (add1 pos))]
      [(#\newline #\nul)  (set! limit pos)]
      [else               (set! comm pos)
                          (scan-comment pos)]))
  
  (define (scan-comment pos)
    (case (get pos)
      [(#\newline #\nul)  (set! limit pos)]
      [(#\space)          (scan-comment (add1 pos))]
      [else               (set! comm-limit (add1 pos))
                          (scan-comment (add1 pos))]))
  
  (scan-start start)
  
  (line-summary start text text-limit hash hash-limit comm comm-limit limit))

  
(module+ test
  (require "test-helpers.rkt"
           (only-in racket/format ~a))

  (define-text%-check (check-summarize-line text)
    (summarize-line text 0))

  (define LS line-summary)

  ; blank lines:
  (check-summarize-line ""        (LS 0 #f #f #f #f #f #f 0))
  (check-summarize-line "   "     (LS 0 #f #f #f #f #f #f 3))
  (check-summarize-line "   \nhi" (LS 0 #f #f #f #f #f #f 3))

  ; text-only lines:
  (check-summarize-line "hello"   (LS 0 0 5 #f #f #f #f 5))
  (check-summarize-line "  hello" (LS 0 2 7 #f #f #f #f 7))
  (check-summarize-line "hello  " (LS 0 0 5 #f #f #f #f 7))
  (check-summarize-line "  a b  " (LS 0 2 5 #f #f #f #f 7))
  (check-summarize-line "a\nb\nc" (LS 0 0 1 #f #f #f #f 1))
  
  ; comment-only lines:
  (check-summarize-line "# hi"    (LS 0 #f #f 0 1 2 4 4))
  (check-summarize-line "## hi"   (LS 0 #f #f 0 2 3 5 5))
  (check-summarize-line " # hi"   (LS 0 #f #f 1 2 3 5 5))
  (check-summarize-line " # hi  " (LS 0 #f #f 1 2 3 5 7))

  ; hash-only lines:
  (check-summarize-line "#"    (LS 0 #f #f 0 1 #f #f 1))
  (check-summarize-line "##"   (LS 0 #f #f 0 2 #f #f 2))
  (check-summarize-line "  #"  (LS 0 #f #f 2 3 #f #f 3))
  (check-summarize-line "#  "  (LS 0 #f #f 0 1 #f #f 3))
  (check-summarize-line " # "  (LS 0 #f #f 1 2 #f #f 3))

  ; everything lines:
  (check-summarize-line "pass  # do nothing"
                        (LS 0 0 4 6 7 8 18 18))
  (check-summarize-line "pass  ## meh ##"
                        (LS 0 0 4 6 8 9 15 15))
  (check-summarize-line "pass  ## meh ##  "
                        (LS 0 0 4 6 8 9 15 17))
  (check-summarize-line "  x = 6  # six\n more stuff"
                        (LS 0 2 7 9 10 11 14 14))
  (check-summarize-line "pass# more space"
                        (LS 0 0 4 4 5 6 16 16))
  (check-summarize-line "pass #more space"
                        (LS 0 0 4 5 6 6 16 16))
  (check-summarize-line "pass##more space"
                        (LS 0 0 4 4 6 6 16 16))
  (check-summarize-line "pass##more spa  "
                        (LS 0 0 4 4 6 6 14 16))
  (check-summarize-line "  ss##more spa  "
                        (LS 0 2 4 4 6 6 14 16)))