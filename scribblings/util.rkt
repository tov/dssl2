#lang racket

(provide defdsslform
         q m)
(require scribble/manual
         scribble/struct
         (only-in scribble/private/manual-bind
                  id-to-form-target-maker
                  definition-site
                  with-exporting-libraries)
         (only-in scribble/scheme to-element))

(define (q x)
  (elem (racketvalfont x)))

(define (m x)
  (elem (larger x)))

(define-syntax-rule (defdsslform kw)
  (*defforms #f #t kw '() '() (λ () '())))

(define (*defforms kind link? kw-id forms form-procs content-thunk)
  (make-box-splice
    (cons
      (make-blockquote
        vertical-inset-style
        (list
          (make-table
            boxed-style
            (append
              (for/list ([form (in-list forms)]
                         [form-proc (in-list form-procs)]
                         [i (in-naturals)])
                (list
                  ((if (zero? i) (add-background-label (or kind "syntax")) values)
                   (list
                     ((or form-proc
                          (λ (x)
                             (make-omitable-paragraph
                               (list (to-element `(,x . ,(cdr form)))))))
                      (and kw-id
                           (if (eq? form (car forms))
                             (if link?
                               (defform-site kw-id)
                               (to-element #:defn? #t kw-id))
                             (to-element #:defn? #t kw-id))))))))
              null
              null))))
      (content-thunk))))

(define (defform-site kw-id)
  (let ([target-maker (id-to-form-target-maker kw-id #t)])
    (define-values (content ref-content)
      (definition-site (syntax-e kw-id) kw-id #t))
    (if target-maker
        (target-maker
         content
         (lambda (tag)
           (make-toc-target2-element
            #f
            (if kw-id
                (make-index-element
                 #f content tag
                 (list (datum-intern-literal (symbol->string (syntax-e kw-id))))
                 (list ref-content)
                 #f)
                content)
            tag
            ref-content)))
        content)))


