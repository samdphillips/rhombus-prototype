#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt")
         "expression.rkt")

(provide define-prefix
         define-infix
         
         (for-syntax prefix
                     infix))

(begin-for-syntax
  (require (for-syntax racket/base
                       syntax/parse))
  (define-syntax (prefix stx)
    (syntax-parse stx
      [(_ name:identifier prim:identifier
          (~optional (~seq #:weaker-than (weaker-op ...))
                     #:defaults ([(weaker-op 1) '()]))
          (~optional (~seq #:same-as (same-op ...))
                     #:defaults ([(same-op 1) '()]))
          (~optional (~seq #:same-on-right-as (same-on-right-op ...))
                     #:defaults ([(same-on-right-op 1) '()]))
          (~optional (~seq #:same-on-left-as (same-on-left-op ...))
                     #:defaults ([(same-on-left-op 1) '()]))
          (~optional (~seq #:stronger-than (stronger-op ...))
                     #:defaults ([(stronger-op 1) '()])))
       #`(expression-prefix-operator (quote-syntax name)
                                     (list (cons (quote-syntax weaker-op)
                                                 'weaker)
                                           ...
                                           (cons (quote-syntax same-op)
                                                 'same)
                                           ...
                                           (cons (quote-syntax same-on-right-op)
                                                 'same-on-right)
                                           ...
                                           (cons (quote-syntax same-on-left-op)
                                                 'same-on-left)
                                           ...
                                           (cons (quote-syntax stronger-op)
                                                 'stronger)
                                           ...)
                                     'automatic
                                     (lambda (form stx)
                                       (datum->syntax (quote-syntax here)
                                                      (list (quote-syntax prim) form)
                                                      (span-srcloc stx form)
                                                      stx)))]))

  (define-syntax (infix stx)
    (syntax-parse stx
      [(_ name:identifier prim:identifier
          (~optional (~seq #:weaker-than (weaker-op ...))
                     #:defaults ([(weaker-op 1) '()]))
          (~optional (~seq #:same-as (same-op ...))
                     #:defaults ([(same-op 1) '()]))
          (~optional (~seq #:same-on-left-as (same-on-left-op ...))
                     #:defaults ([(same-on-left-op 1) '()]))
          (~optional (~seq #:stronger-than (stronger-op ...))
                     #:defaults ([(stronger-op 1) '()]))
          (~optional (~seq #:associate assoc)
                     #:defaults ([assoc #''left])))
       #`(expression-infix-operator (quote-syntax name)
                                    (list (cons (quote-syntax weaker-op)
                                                'weaker)
                                          ...
                                          (cons (quote-syntax same-op)
                                                'same)
                                          ...
                                          (cons (quote-syntax same-on-left-op)
                                                'same-on-left)
                                          ...
                                          (cons (quote-syntax stronger-op)
                                                'stronger)
                                          ...)
                                    'automatic
                                    (lambda (form1 form2 stx)
                                      (datum->syntax (quote-syntax here)
                                                     (list (quote-syntax prim) form1 form2)
                                                     (span-srcloc form1 form2)
                                                     stx))
                                    assoc)])))

(define-syntax (define-infix stx)
  (syntax-parse stx
    [(_ name spec ...)
     #'(define-syntax name (infix name spec ...))]))

(define-syntax (define-prefix stx)
  (syntax-parse stx
    [(_ name spec ...)
     #'(define-syntax name (prefix name spec ...))]))
