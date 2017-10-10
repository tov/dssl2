#lang racket/base

(provide lib-directory)
(require racket/runtime-path
         (for-syntax racket/base))

(define-runtime-path lib-directory
                     (build-path 'up "lib"))
