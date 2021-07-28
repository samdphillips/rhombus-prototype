#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "binding.rkt")

(provide values
         (for-space rhombus/binding values))

(define-binding-syntax values
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(head . _)
        (raise-syntax-error #f
                            "not allowed as a nested pattern"
                            #'head)]))))