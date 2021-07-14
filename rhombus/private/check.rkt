#lang racket/base
(require syntax/stx)

(provide check-expression-result
         check-pattern-result
         check-transformer-result

         proc-name)

(define (check-expression-result form proc)
  (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
  form)

(define (check-pattern-result ids filter-form proc)
  (unless (and (stx-list? ids)
               (let loop ([ids ids])
                 (or (stx-null? ids)
                     (and (stx-pair? ids)
                          (identifier? (stx-car ids))
                          (loop (stx-cdr ids))))))
    (raise-result-error (proc-name proc) "(listof identifier?))" ids))
  (unless (syntax? filter-form) (raise-result-error (proc-name proc) "syntax?" filter-form))
  (datum->syntax #f (list ids filter-form)))

(define (check-transformer-result form tail proc)
  (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
  (unless (stx-list? tail) (raise-result-error (proc-name proc) "stx-list?" tail))
  (values form tail))

(define (proc-name proc)
  (define s (object-name proc))
  (if (symbol? s)
      s
      'transformer-procedure))