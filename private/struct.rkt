#lang racket/base

(provide struct-base
         struct-base?
         struct-base-struct-info
         struct-info
         struct-info-name
         struct-info-field-infos
         field-info
         field-info?
         field-info-name
         field-info-getter
         field-info-setter
         get-field-info
         get-field-list
         write-struct)

(struct field-info (name getter setter) #:prefab)
(struct struct-info (name field-infos) #:prefab)

(struct struct-base (struct-info) #:prefab)

(define (write-struct value port recur)
  (define info (struct-base-struct-info value))
  (define first #true)
  (fprintf port "~a {" (struct-info-name info))
  (for ([field-info (in-vector (struct-info-field-infos info))])
    (if first
      (set! first #f)
      (display ", " port))
    (fprintf port "~a: " (field-info-name field-info))
    (recur ((field-info-getter field-info) value)))
  (display "}" port))

(define (get-field-info struct sym)
  (let/ec return
    (define info-vector (struct-info-field-infos
                          (struct-base-struct-info struct)))
    (for ([info (in-vector info-vector)])
      (when (eq? sym (field-info-name info))
        (return info)))
    #false))

(define (get-field-list struct)
  (for/list ([field-info (in-vector (struct-info-field-infos
                                      (struct-base-struct-info struct)))])
    (symbol->string (field-info-name field-info))))
