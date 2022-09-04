#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/name-root
                     "srcloc.rkt"
                     "introducer.rkt")
         "dot.rkt")

(provide define-simple-name-root
         define-name-root)

(define-syntax-rule (define-simple-name-root id content ...)
  ;; portal syntax with this shape is recognized by "name-root-ref.rkt"
  (#%require (portal id (map id [content content] ...))))

(define-syntax (define-name-root stx)
  (syntax-parse stx
    [(_ id (~alt (~once (~seq #:fields (content ...)))
                 (~optional (~seq #:root root-rhs)
                            #:defaults ([root-rhs #'#f]))
                 (~optional (~seq #:root-as-rename root-rename)
                            #:defaults ([root-rename #'#f]))
                 (~optional (~seq #:space space)
                            #:defaults ([space #'#f]))
                 (~optional (~seq #:orig-id orig-id)
                            #:defaults ([orig-id #'#f])))
        ...)
     #:do [(define in-space
             (let ([space (syntax-e #'space)])
               (if space
                   (make-interned-syntax-introducer/add space)
                   (lambda (id) id))))]
     #:with root-id (in-space (car (generate-temporaries #'(id))))
     #:with space-id (in-space #'id)
     #:with (root-def ...) (if (syntax-e #'root-rhs)
                               #'[(define-syntax root-id root-rhs)]
                               #'[])
     #:with (root-spec ...) (cond
                              [(syntax-e #'root-rhs) #'([#f root-id])]
                              [(syntax-e #'root-rename) #'([#f root-rename])]
                              [else #'()])
     #:with (norm-content ...) (for/list ([c (in-list (syntax->list #'(content ...)))])
                                 (syntax-parse c
                                   [_:identifier #`[#,c #,c]]
                                   [(_:identifier _:identifier) c]))
     #:with the-orig-id (if (syntax-e #'orig-id)
                            #'orig-id
                            #'id)
     #'(begin
         root-def ...
         (#%require (portal space-id (map the-orig-id norm-content ... root-spec ...))))]))
