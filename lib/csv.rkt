#lang racket/base

(require racket/file
         csv-reading
         "../private/errors.rkt")

(provide read_csv)

(define (read_csv filename)
  (unless (file-exists? filename)
    (raise-runtime-error "file not found: %p" filename))
  (define as-lists
    (with-handlers ([exn:fail?
                     ;; most likely a parse error. not 100%, but good enough
                     (lambda (e) (raise-runtime-error "invalid CSV file: %p"
                                                      filename))])
      (csv->list (file->string filename))))
  (for/vector ([row (in-list as-lists)])
    (list->vector row)))
