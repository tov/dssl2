#lang racket

(provide lib-directory)
(require racket/runtime-path)

(define-runtime-path lib-directory
                     (build-path 'up "lib"))
