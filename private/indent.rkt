#lang racket

(provide enter-and-indent
         find-current-indent
         go-to-indent)

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
  (cond
    [(zero? position) 0]
    [(char=? #\newline (send text get-character (sub1 position)))
     position]
    [else
      (find-beginning-of-line text (sub1 position))]))

; text% [char? -> boolean?] natural? natural? -> natural?
(define (find-forward text pred i limit)
  (cond
    [(>= i limit)
     limit]
    [(pred (send text get-character i))
     i]
    [else
     (find-forward text pred (add1 i) limit)]))

; text% natural? natural? -> [listof [list natural? natural?]]
; Gets the positions of the starts of all lines between the two positions.
; ASSUMPTION: `start` is the beginning of a line.
(define (find-starts-and-indents text start end)
  (cond
    [(< start end)
     (define first-non-space
       (find-forward text
                     (λ (c) (not (or (char=? c #\newline)
                                     (char=? c #\space))))
                     start
                     end))
     (define beginning-of-line
       (find-beginning-of-line text first-non-space))
     (define next-newline
       (find-forward text
                     (λ (c) (char=? c #\newline))
                     first-non-space
                     (send text last-position)))
     (cons (list beginning-of-line (- first-non-space beginning-of-line))
           (find-starts-and-indents text next-newline end))]
    [else
      '()]))

; text% [boolean?] ->
; Updates the indentation of the lines of code in the selection.
(define (go-to-indent text [previous #f])
  (send text begin-edit-sequence)
  (define selection-start (send text get-start-position))
  (define selection-end   (send text get-end-position))
  (define line-start      (find-beginning-of-line text selection-start))
  (cond
    [(= selection-start selection-end)
     (define indent (find-forward text
                                  (λ (c) (not (char=? c #\space)))
                                  line-start
                                  selection-end))
     (adjust-indent text line-start (- indent line-start)
                    (if previous -4 4))]
    [else
      (for ([start-indent
              (reverse
                (find-starts-and-indents text selection-start selection-end))])
        (adjust-indent text (first start-indent) (second start-indent)
                       (if previous -4 4)))])
  (send text end-edit-sequence))

; text% natural? natural? integer? ->
; Adjusts the indentation 
(define (adjust-indent text position old change)
  (if (positive? change)
    (send text insert change (make-string change #\space) position)
    (send text delete position (+ position (min old (- change))))))

; text% ->
; Inserts a newline and indents.
(define (enter-and-indent text)
  (let* ([position (send text get-start-position)]
         [indent (find-current-indent text position)]
         [blank? (current-line-is-blank? text position)])
    (send text begin-edit-sequence)
    (send text insert #\newline)
    (unless blank?
      (send text insert (make-string indent #\space)))
    (send text end-edit-sequence)))

; text% natural? -> boolean?
; Is the current line only spaces?
(define (current-line-is-blank? text position)
  (let loop [(position (find-beginning-of-line text position))]
    (case (send text get-character position)
      [(#\newline) #t]
      [(#\space)   (loop (add1 position))]
      [else        #f])))

