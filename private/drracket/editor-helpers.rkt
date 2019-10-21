#lang racket/base

(provide with-edit-sequence
         find-backward
         find-forward
         find-line-start
         find-first-non-space
         find-next-non-space
         find-current-indent
         find-current-indent/ls
         find-starts
         find-non-blank-starts
         find-starts-and-indents
         get-single-cursor
         =char?
         ≠char?
         whitespace-char?
         non-whitespace-char?)

(require (only-in racket/class send)
         syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser with-edit-sequence
  [(_ (text-e:expr ...) expr:expr ...+)
   (with-syntax
       ([(text ...) (generate-temporaries (syntax->list #'(text-e ...)))])
     #'(let ([text text-e] ...)
         (dynamic-wind (λ () (send text begin-edit-sequence) ...)
                       (λ () expr ...)
                       (λ () (send text end-edit-sequence) ...))))])


; text% [char? -> boolean?] (natural? integer?) -> natural?
(define (find-backward text pred
                       [i (send text get-start-position)]
                       [limit -1])
  (cond
    [(<= i limit)
     limit]
    [(pred (send text get-character i))
     i]
    [else
     (find-backward text pred (sub1 i) limit)]))

; text% [char? -> boolean?] (natural? integer?) -> natural?
(define (find-forward text pred
                      [i (send text get-end-position)]
                      [limit (send text last-position)])
  (cond
    [(>= i limit)
     limit]
    [(pred (send text get-character i))
     i]
    [else
     (find-forward text pred (add1 i) limit)]))

; text% natural? -> natural?
; Finds the position where the current line begins
(define (find-line-start text position)
  (or (send text find-newline 'backward position 0)
      0))

; text% natural? -> natural?
(define (find-first-non-space text position)
  (find-next-non-space text (find-line-start text position)))

; text% natural? -> natural?
; Returns the position of the next non-space character from `start`.
(define (find-next-non-space text start)
  (find-forward text (≠char? #\space) start))

; text% natural? -> natural?
; Returns the current indent of the line.
(define (find-current-indent text position)
  (find-current-indent/ls text (find-line-start text position)))

; text% natural? -> natural?
; Returns the current indent of the line.
; PRECONDITION: `line-start` is a line start
(define (find-current-indent/ls text line-start)
  (- (find-next-non-space text line-start) line-start))

; text% natural? -> boolean?
; Is the current line only spaces?
(define (current-line-is-blank? text position)
  (current-line-is-blank/ls? text (find-line-start text position)))

; text% natural? -> boolean?
; Is the current line only spaces?
; PRECONDITION: `line-start` is a line start
(define (current-line-is-blank/ls? text line-start)
  (eol-char?
   (send text get-character (find-next-non-space text line-start))))

; text% (natural? natural?) -> [listof natural?]
; Gets the positions of the starts of all lines touched by the two positions.
(define (find-starts text
                     [start (send text get-start-position)]
                     [end   (send text get-end-position)])
  (let loop ([next end] [acc '()])
    (if (< next start)
        acc
        (let ([line-start (find-line-start text next)])
          (loop (sub1 line-start) (cons line-start acc))))))

; text% (natural? natural?) -> [listof natural?]
; Gets the positions of the starts of all non-empty lines touched by the
; two positions.
(define (find-non-blank-starts text
                               [start (send text get-start-position)]
                               [end   (send text get-end-position)])
  (filter (λ (line-start)
            (not (current-line-is-blank/ls? text line-start)))
          (find-starts text start end)))


; text% (natural? natural?) -> [listof [list natural? natural?]]
; Gets the starts and indents of all lines touched by the two positions.
(define (find-starts-and-indents text
                                 [start (send text get-start-position)]
                                 [end   (send text get-end-position)])
  (for/list ([line-start (find-starts text start end)])
    (list line-start (find-current-indent/ls text line-start))))

; text% -> opt-nat
(define (get-single-cursor text)
  (define start (box 0))
  (define end   (box 0))
  (send text get-position start end)
  (and (= (unbox start) (unbox end))
       (unbox start)))

; char? -> boolean?
(define (non-whitespace-char? c)
  (not (whitespace-char? c)))

; char? -> boolean?
(define (whitespace-char? c)
  (or (char=? c #\newline) (char=? c #\space)))

; char? -> boolean?
(define (eol-char? c)
  (or (char=? c #\newline) (char=? c #\nul)))

; char? -> [char? -> boolean?]
(define ((=char? c) d)
  (char=? c d))

; char? -> [char? -> boolean?]
(define ((≠char? c) d)
  (not (char=? c d)))

(module+ test
  (require "test-helpers.rkt"
           (only-in racket/format ~a))

  (define-text%-check (check-find-forward text c start)
    (find-forward text (=char? c) start (send text last-position)))

  (table (check-find-forward "abcdeabcde")
         [#\a 0  0]
         [#\b 0  1]
         [#\e 0  4]
         [#\f 0 10]
         [#\a 1  5]
         [#\c 2  2]
         [#\c 3  7]
         [#\c 7  7]
         [#\c 8 10])

  (define-text%-check (check-find-backward text c start)
    (find-backward text (=char? c) start))

  (table (check-find-backward "abcdeabcde")
         [#\e 9  9]
         [#\a 9  5]
         [#\b 6  6]
         [#\b 5  1]
         [#\b 0 -1]
         [#\x 5 -1])

  (define-text%-check (check-find-beginning-of-line text start)
    (find-line-start text start))

  (table (check-find-beginning-of-line "abcde\n")
         [0 0]
         [1 0]
         [2 0]
         [3 0]
         [4 0]
         [5 0]
         [6 6])
  (table (check-find-beginning-of-line "abcde\nabcde")
         [ 0 0]
         [ 1 0]
         [ 4 0]
         [ 5 0]
         [ 6 6]
         [ 7 6]
         [10 6])
  (table (check-find-beginning-of-line "\nabcde\nabcde")
         [3 1]
         [2 1]
         [1 1]
         [0 0])

  (define-text%-check (check-find-current-indent text start)
    (find-current-indent text start))

  (table (check-find-current-indent "XX\n  YYY\n    ZZZZ")
         [ 0 0]
         [ 1 0]
         [ 2 0]
         [ 3 2]
         [ 4 2]
         [ 5 2]
         [ 6 2]
         [ 7 2]
         [ 8 2]
         [ 9 4]
         [10 4]
         [11 4]
         [12 4]
         [13 4]
         [14 4]
         [15 4]
         [16 4])
  (table (check-find-current-indent "\n   \n  ")
         [0 0]
         [1 3]
         [2 3]
         [3 3]
         [4 3]
         [5 2]
         [6 2]
         [7 2])

  (define-text%-check (check-find-starts text start end)
    (find-starts text start end))

  (table (check-find-starts (~a (line 2) (line 4) (line 7) (line 1) (line 6 4)))
         [ 0  0 '(0)]  [ 0  1 '(0)]     [ 0  2 '(0 2)]   [ 0  3 '(0 2)]
         [ 1  1 '(0)]  [ 1  2 '(0 2)]   [ 1  3 '(0 2)]   [ 1  5 '(0 2)]
         [ 2  2 '(2)]  [ 2  3 '(2)]     [ 2  5 '(2)]     [ 2  6 '(2 6)]
         [ 3  3 '(2)]  [ 3  5 '(2)]     [ 3  6 '(2 6)]   [ 3 12 '(2 6)]
         [ 6  6 '(6)]  [ 6 12 '(6)]     [ 6 13 '(6 13)]  [ 6 14 '(6 13 14)]
         [13 13 '(13)] [13 14 '(13 14)] [13 15 '(13 14)] [13 16 '(13 14)]
         ;;
         [ 0  5 '(0 2)]     [ 0  6 '(0 2 6)]     [0 12 '(0 2 6)]     
         [ 1  6 '(0 2 6)]   [ 1 12 '(0 2 6)]     [1 13 '(0 2 6 13)]
         [ 2 12 '(2 6)]     [ 2 13 '(2 6 13)]    [2 14 '(2 6 13 14)]
         [ 3 13 '(2 6 13)]  [ 3 14 '(2 6 13 14)] [3 15 '(2 6 13 14)]
         [ 6 15 '(6 13 14)] [ 6 16 '(6 13 14)]   [6 17 '(6 13 14)]
         [13 17 '(13 14)]   [13 20 '(13 14 20)]
         ;;
         [0 13 '(0 2 6 13)]
         [2 15 '(2 6 13 14)])

  (define-text%-check (check-find-starts-and-indents text)
    (find-starts-and-indents text 0 (send text last-position)))

  (check-find-starts-and-indents "abc\ndef\nghi"
                                 '(( 0 0)
                                   ( 4 0)
                                   ( 8 0)))
  (check-find-starts-and-indents "abc\ndef\nghi\n"
                                 '(( 0 0)
                                   ( 4 0)
                                   ( 8 0)
                                   (12 0)))
  (check-find-starts-and-indents "abc\n    def\n  ghi"
                                 '(( 0 0)
                                   ( 4 4)
                                   (12 2)))
  (check-find-starts-and-indents "  abc\ndef"
                                 '(( 0 2)
                                   ( 6 0)))
  (check-find-starts-and-indents "a\n    "
                                 '(( 0 0)
                                   ( 2 4)))
  (check-find-starts-and-indents "a\n    \nb"
                                 '(( 0 0)
                                   ( 2 4)
                                   ( 7 0)))
  (check-find-starts-and-indents "a\n    \n b"
                                 '(( 0 0)
                                   ( 2 4)
                                   ( 7 1)))
  (check-find-starts-and-indents "a\n    \n b\n"
                                 '(( 0 0)
                                   ( 2 4)
                                   ( 7 1)
                                   (10 0)))

  (define-text%-check (check-current-line-is-blank? text pos)
    (current-line-is-blank? text pos))

  (table (check-current-line-is-blank? "ab\n  c\n  \nd  ")
         [ 0 #false]
         [ 1 #false]
         [ 2 #false]
         [ 3 #false]
         [ 4 #false]
         [ 5 #false]
         [ 6 #false]
         [ 7 #true]
         [ 8 #true]
         [ 9 #true]
         [10 #false]
         [11 #false]
         [12 #false]
         [13 #false])
  (table (check-current-line-is-blank? "a\n  \n\n\nb\n  e  \n")
         [ 0 #false]
         [ 1 #false]
         [ 2 #true]
         [ 3 #true]
         [ 4 #true]
         [ 5 #true]
         [ 6 #true]
         [ 7 #false]
         [ 8 #false]
         [ 9 #false]
         [10 #false]
         [11 #false]
         [12 #false]
         [13 #false]
         [14 #false]))