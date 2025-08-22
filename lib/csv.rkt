#lang racket/base

(require racket/file
         2htdp/batch-io
         "../private/errors.rkt")

(provide read_csv read_csv_headers read_csv_data)

(define (read_csv_raw filename)
  (unless (file-exists? filename)
    (raise-runtime-error "file not found: %p" filename))
  (with-handlers ([exn:fail?
                   ;; most likely a parse error. not 100%, but good enough
                   (lambda (e) (raise-runtime-error "invalid CSV file: %p"
                                                    filename))])
    (read-csv-file filename)))

(define (read_csv filename)
  (for/vector ([row (in-list (read_csv_raw filename))])
    (list->vector row)))
(define (read_csv_headers filename)
  (define raw (read_csv_raw filename))
  (when (null? raw)
    (raise-runtime-error "empty CSV file: %p" filename))
  (list->vector (car raw)))
(define (read_csv_data filename)
  (define raw (read_csv_raw filename))
  (when (null? raw)
    (raise-runtime-error "empty CSV file: %p" filename))
  (for/vector ([row (in-list (cdr raw))])
    (list->vector row)))
