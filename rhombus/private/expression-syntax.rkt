#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/transformer-result
                     "srcloc.rkt"
                     "pack.rkt")
         "name-root.rkt"
         "syntax.rkt"
         "expression.rkt"
         "parse.rkt"
         "wrap-expression.rkt"
         (for-syntax "name-root.rkt"))

(provide expr
         expr_only)

(module+ for-define
  (provide (for-syntax make-expression-infix-operator
                       make-expression-prefix-operator)))

(define-simple-name-root expr
  macro
  rule)

(define-name-root expr_only
  #:fields
  ([macro macro-only]
   [rule rule-only]))

(define-operator-definition-transformer macro
  'macro
  (lambda (x) x)
  #'make-expression-prefix-operator
  #'make-expression-infix-operator
  #'expression-prefix+infix-operator)

(define-operator-definition-transformer rule
  'rule
  (lambda (x) x)
  #'make-expression-prefix-operator
  #'make-expression-infix-operator
  #'expression-prefix+infix-operator)

(define-operator-definition-transformer macro-only
  'macro
  in-expression-space
  #'make-expression-prefix-operator
  #'make-expression-infix-operator
  #'expression-prefix+infix-operator)

(define-operator-definition-transformer rule-only
  'rule
  in-expression-space
  #'make-expression-prefix-operator
  #'make-expression-infix-operator
  #'expression-prefix+infix-operator)

(define-for-syntax (make-expression-infix-operator name prec protocol proc assc)
  (expression-infix-operator
   name
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form1 form2 stx)
         (wrap-expression (check-expression-result
                           (proc #`(parsed #,form1) #`(parsed #,form2) stx)
                           proc)))
       (lambda (form1 tail)
         (define-values (form new-tail) (syntax-parse tail
                                          [(head . tail) (proc #`(parsed #,form1) (pack-tail #'tail #:after #'head) #'head)]))
         (check-transformer-result (wrap-expression (check-expression-result form proc))
                                   (unpack-tail new-tail proc #f)
                                   proc)))
   assc))

(define-for-syntax (make-expression-prefix-operator name prec protocol proc)
  (expression-prefix-operator
   name
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form stx)
         (wrap-expression (check-expression-result
                           (proc #`(parsed #,form) stx)
                           proc)))
       (lambda (tail)
         (define-values (form new-tail) (syntax-parse tail
                                          [(head . tail) (proc (pack-tail #'tail #:after #'head) #'head)]))
         (check-transformer-result (wrap-expression (check-expression-result form proc))
                                   (unpack-tail new-tail proc #f)
                                   proc)))))
