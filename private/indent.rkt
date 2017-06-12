#lang racket

(provide find-indent go-to-previous-indent)

; text% natural? [boolean?] -> natural?
; Returns the next indent, or if reverse is #t, the previous, for the
; line given by `position`.
(define (find-indent text position [reverse? #f])
  (let ([indents (find-indents text position)]
        [current (find-current-indent text position)])
    (cond
      [reverse?
        (let loop ([remaining (reverse indents)])
          (cond
            [(< (first remaining) current)  (first remaining)]
            [(empty? (rest remaining))      (last indents)]
            [else                           (loop (rest remaining))]))]
      [else
        (let loop ([remaining indents])
          (cond
            [(> (first remaining) current)  (first remaining)]
            [(empty? (rest remaining))      (first indents)]
            [else                           (loop (rest remaining))]))])))

; text% natural? -> [ne-listof natural?]
; Returns all indents suggested for the line given by `position`.
(define (find-indents text position)
  (let* ([previous (find-previous-indent text position)]
         [farthest (+ 4 (- previous (modulo previous 4)))])
    (let loop ([acc  (list farthest)]
               [last farthest])
      (if (zero? last)
        acc
        (loop (cons (- last 4) acc)
              (- last 4))))))

; text% natural? -> natural?
; Returns the indent of the previous non-blank line.
(define (find-previous-indent text position)
  (let loop ([position (find-beginning-of-line text position)])
    (if (zero? position)
      (find-current-indent text position)
      (let ([c (send text get-character (sub1 position))])
        (if (or (char=? c #\space) (char=? c #\newline))
          (loop (sub1 position))
          (find-current-indent text position))))))

; text% natural? -> natural?
; Returns the current indent of the line.
(define (find-current-indent text position)
  (let loop ([position (find-beginning-of-line text position)]
             [count    0])
    (if (char=? #\space (send text get-character position))
      (loop (add1 position) (add1 count))
      count)))

; text% natural? -> natural?
; Finds the position where the current line begins
(define (find-beginning-of-line text position)
  (if (zero? position)
    position
    (let ([c (send text get-character (sub1 position))])
      (if (char=? c #\newline)
        position
        (find-beginning-of-line text (sub1 position))))))


; text% ->
; Updates the current line to the previous indentation level.
(define (go-to-previous-indent text)
  (define position (send text get-start-position))
  (define indent   (find-indent text position #t))
  (define current  (find-current-indent text position))
  (define start    (find-beginning-of-line text position))
  (define change   (- indent current))
  (if (positive? change)
    (send text insert change (make-string change #\space) start)
    (send text delete start (- start change))))
