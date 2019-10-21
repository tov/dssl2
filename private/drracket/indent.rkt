#lang racket/base

(provide enter-and-indent
         backspace-and-align
         find-current-indent
         do-indent
         do-dedent)
(require "editor-helpers.rkt"
         "line-summary.rkt"
         (only-in racket/class send))


(define *indent-size*     4)
(define *indent-string*   (make-string *indent-size* #\space))
(define *max-columns*     80)
(define *max-col-spaces*  (make-string *max-columns* #\space))


; text% ->
; indents the selected lines *indent-size*
(define (do-indent text)
  (define starts (if (get-single-cursor text)
                     (find-starts text)
                     (find-non-blank-starts text)))
  (with-edit-sequence (text)
    (for ([line-start (in-list starts)]
          [line-count (in-naturals)])
      (define adj-line-start (+ line-start (* *indent-size* line-count)))
      (send text insert *indent-size* *indent-string* adj-line-start))))

; text% ->
; dedents the selected lines *indent-size*, if possible
(define (do-dedent text)
  (let* ([summaries   (summarize-span text)]
         [dedent-size (min *indent-size* (find-span-indent summaries))])
    (when (> dedent-size 0)
      (with-edit-sequence (text)
        (for/fold ([adjust   0])
                  ([summary  (in-list summaries)])
          (define change (min dedent-size (line-summary-indent summary)))
          (define line-start (- (line-summary-start summary) adjust))
          (send text delete line-start (+ line-start change))
          (+ adjust change))))))

; text% ->
; Inserts a newline and indents.
(define (enter-and-indent text)
  (let* ([cursor     (send text get-start-position)]
         [summary    (find-non-blank-summary/backward text cursor)]
         [new-block? (and (block-start? text (line-summary-text-limit summary))
                          (>= cursor (line-summary-text-limit summary)))]
         [old-indent (line-summary-indent summary)]
         [new-indent (+ old-indent (if new-block? *indent-size* 0))])
    (with-edit-sequence (text)
      (send text insert #\newline)
      (unless (zero? new-indent)
        (send text insert (min new-indent *max-columns*) *max-col-spaces*)))))

; text% nat -> Line-summary
; Finds the nearest non-blank/non-comment-only line preceding `position`
; and returns its summary.
(define (find-non-blank-summary/backward text position)
  (define summary (summarize-line text (sub1 position)))
  (if (or (zero? (line-summary-start summary))
          (line-summary-text-limit summary))
      summary
      (find-non-blank-summary/backward text (line-summary-start summary))))

; text% opt-nat -> boolean
; Does this line start a new block?
; PRECONDITION: `text-limit` is the first position after the line's
; non-comment, non-space text.
(define (block-start? text text-limit)
  (and text-limit
       (char=? #\: (send text get-character (sub1 text-limit)))))
  

; text% ->
(define (backspace-and-align text)
  (let/ec bail
    (define position (get-single-cursor text))
    (unless position
      (send text delete)
      (bail (void)))
    (define line-start (find-line-start text position))
    (when (or (= position line-start)
              (> position (find-next-non-space text line-start)))
      (send text delete 'start 'back)
      (bail (void)))
    (define change (compute-dedent (- position line-start)))
    (send text delete (- position change) position)))

; positive-integer? -> positive-integer?
(define (compute-dedent current-indent)
  (define extra (modulo current-indent *indent-size*))
  (if (zero? extra) *indent-size* extra))

