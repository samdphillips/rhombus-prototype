#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/name-parse
                     "pack.rkt"
                     "static-info-pack.rkt")
         "definition.rkt"
         "name-root.rkt"
         "quasiquote.rkt"
         "static-info.rkt"
         "parse.rkt"
         "wrap-expression.rkt"
         (for-syntax "name-root.rkt")
         (for-syntax "parse.rkt"))

(provide static_info
         (for-syntax static_info_ct))

(define-simple-name-root static_info
  macro)

(begin-for-syntax
  (define-simple-name-root static_info_ct
    pack
    unpack
    wrap))

(define-syntax macro
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (block quotes)
        [(_ (quotes (group name::name)) ((~and body-tag block) body ...))
         #`((define-syntax #,(in-static-info-space #'name.name)
              (convert-static-info 'name.name (rhombus-body-at body-tag body ...))))]))))

(define-for-syntax (convert-static-info who stx)
  (unless (syntax? stx)
    (raise-result-error who "syntax?" stx))
  (static-info (syntax->list (pack stx))))

(define-for-syntax (pack v)
  (pack-static-infos (unpack-term v 'static_info_ct.pack) 'static_info_ct.pack))

(define-for-syntax (unpack v)
  (unpack-static-infos v))

(define-for-syntax (wrap form info)
  (pack-term #`(parsed #,(wrap-static-info* (wrap-expression form)
                                            (pack info)))))
