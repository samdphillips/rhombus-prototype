#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         rhombus/private/expression
         rhombus/private/parse
         racket/class)

(provide (rename-out [rhombus-new new])
         object?
         object-dot-lookup)

(define (object-has-field? o field)
  (and (memq field (field-names o)) #t))

(define (object-has-method? o field)
  (method-in-interface? field (object-interface o)))

(define (object-dot-lookup o field fail)
  (cond
    [(object-has-field? o field)
     (dynamic-get-field field o)]

    [(object-has-method? o field)
     (make-keyword-procedure
       (lambda (kws kwargs . pos-args)
         (keyword-apply dynamic-send kws kwargs o field pos-args))
       (lambda pos-args
         (apply dynamic-send o field pos-args)))]

    [else (fail)]))

(begin-for-syntax
  (define-syntax-class :init-field
    #:datum-literals (block group)
    #:attributes (name expr)
    [pattern (group name-kw:keyword (block (~and expr (group e ...))))
             #:attr name
             (datum->syntax
              #'name-kw
              (string->symbol
               (keyword->string
                (syntax->datum #'name-kw)))
              #'name-kw)]))

(define-syntax rhombus-new
  (expression-transformer
   #'rhombus-new
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block group parens)
       [(form-id cls-expr ... (parens init-field::init-field ...) . tail)
        (values #'(new (rhombus-expression (group cls-expr ...))
                       [init-field.name (rhombus-expression init-field.expr)]
                       ...)
                #'tail)]))))
