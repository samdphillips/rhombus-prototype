#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         "composite.rkt"
         "expression.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "reducer.rkt"
         "call-result-key.rkt"
         "name-root.rkt"
         (submod "dot.rkt" for-dot-provider)
         "parse.rkt"
         "dot-parse.rkt"
         "realm.rkt")

(provide Pair
         (for-space rhombus/binding Pair)
         (for-space rhombus/annotation Pair))

(module+ for-builtin
  (provide pair-method-table))

(define pair-method-table
  (hash 'first car
        'rest cdr))

(define-for-syntax pair-static-infos
  #'((#%dot-provider pair-instance)))

(define-name-root Pair
  #:fields
  (cons
   [first car]
   [rest cdr])
  #:root
  (expression-transformer
   #'Pair
   (lambda (stx)
     (syntax-parse stx
       [(head . tail) (values (syntax/loc #'head cons) #'tail)]))))

(define-name-root Pair
  #:space rhombus/binding
  #:fields
  ([cons Pair])
  #:root
  (binding-transformer
   #'Pair
   (make-composite-binding-transformer "Pair"
                                       #'pair?
                                       #:static-infos #'((#%dot-provider pair-instance))
                                       (list #'car #'cdr)
                                       (list #'() #'()))))

(define-annotation-constructor Pair
  ()
  #'pair? pair-static-infos
  2
  (lambda (arg-id predicate-stxs)
    #`(and (#,(car predicate-stxs) (car #,arg-id))
           (#,(cadr predicate-stxs) (cdr #,arg-id))))
  (lambda (static-infoss)
    #`((#%first-result #,(car static-infoss))
       (#%rest-result #,(cadr static-infoss)))))

(define-syntax pair-instance
  (dot-provider-more-static
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(first) (field (lambda (x) (add-info #`(car #,x) x #'#%first-result)))]
        [(rest) (field (lambda (x) (add-info #`(cdr #,x) x #'#%rest-result)))]
        [else #f])))))

(define-for-syntax (add-info e on-e key)
  (define result-static-infos (syntax-local-static-info on-e key))
  (if result-static-infos
      (wrap-static-info* e result-static-infos)
      e))

(define-static-info-syntax cons
  (#%call-result #,pair-static-infos))
