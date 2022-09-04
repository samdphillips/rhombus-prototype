#lang racket/base
(require syntax/stx
         enforest/proc-name
         "realm.rkt")

;; We represent Rhombus syntax objects as a syntax object with one of
;; the following forms:
;;   (multi (group term ...) ...) - as a general multi-group syntax object
;;   (group term ...) - for a group syntax object
;;   term - for a single-term syntax object
;; Coercions among these happen automatically. So, the Rhombus expression
;;   'x'
;; could be represented for most purposes as #'(multi (group x)) or just #'x.

;; "Pack" means going form the internal Racket-side representation to
;; the Rhombus repesentation, such as when sending syntax to a
;; Rhombus-implemented macro, and "unpack" means going back.

;; "Tail" below refers to the argument to a macro for the rest of the
;; enclosing sequence. We represent it using `multi` on the Rhombus
;; side, but it's just a syntax list on the Racket side, which means
;; adding both `group` and them `multi` to get from here to there. On
;; the way back, we coerce from any of the three possible shapes,
;; containing `multi` to a single group.

;; When we "unpack" here, group unpacking is asymmetric and is really
;; just about coercsions, mot about removing a `group` wrapper.

(provide pack-term
         unpack-term
         pack-group
         unpack-group
         pack-multi
         pack-tagged-multi
         unpack-multi

         pack-tail
         unpack-tail
         pack-multi-tail
         unpack-multi-tail
         
         pack-term*
         unpack-term*
         pack-group*
         unpack-group*
         pack-multi*
         pack-tagged-multi*
         pack-block*
         unpack-multi*
         unpack-multi-as-term*

         pack-tail*
         unpack-tail*
         pack-multi-tail*
         unpack-multi-tail*

         unpack-tail-list*
         unpack-multi-tail-list*
         unpack-list-tail*
         unpack-multi-list-tail*

         unpack-element*

         repack-as-term
         repack-as-multi

         insert-multi-front-group)

(define multi-blank (syntax-property (syntax-property (datum->syntax #f 'multi) 'raw "") 'from-pack #t))
(define group-blank (syntax-property (syntax-property (datum->syntax #f 'group) 'raw "") 'from-pack #t))

(define (group-syntax? r)
  (and (syntax? r)
       (pair? (syntax-e r))
       (eq? 'group (syntax-e (car (syntax-e r))))))

(define (multi-syntax? r)
  (and (syntax? r)
       (pair? (syntax-e r))
       (eq? 'multi (syntax-e (car (syntax-e r))))))

;; ----------------------------------------

;; `stx` is a single term
(define (pack-term stx) stx)

;; Unpacking is mostly about coercing from different representations.
;; The result is always a syntax object.
(define (unpack-term form who at-stx)
  (define (fail)
    (and who
         (raise-error who "multi-term syntax not allowed in term context" form)))
  (let loop ([r form])
    (cond
      [(multi-syntax? r)
       (define l (syntax->list r))
       (if (= 2 (length l))
           (loop (cadr l))
           (fail))]
      [(group-syntax? r)
       (define l (syntax->list r))
       (if (= 2 (length l))
           (cadr l)
           (fail))]
      [(list? r) (and who (cannot-coerce-list who r))]
      [else (datum->syntax at-stx r)])))

;; "Packs" to a `group` form, but `r` starts with `group` already
(define (pack-group r) r)

;; "Unpacks" to a `(group term ...)` form, as opposed to just `(term
;; ...)`, which makes it symmetric with `pack-group` and preserves
;; properties on the `group` tag. So, unpacking here is really about
;; coercing from different representations, as opposed to changing a
;; `group` representation. The result is always a syntax object.
(define (unpack-group r who at-stx)
  (cond
    [(multi-syntax? r)
     (define l (syntax->list r))
     (cond
       [(= 2 (length l)) (cadr l)]
       [else (raise-error who "multi-group syntax not allowed in group context" r)])]
    [(group-syntax? r) r]
    [(or (null? r) (pair? r)) (cannot-coerce-list who r)]
    [else (datum->syntax at-stx (list group-blank r))]))

;; `r` is a sequence of groups
(define (pack-multi r)
  (datum->syntax #f (cons multi-blank r)))

;; `r` is a term like `(parens ....)` or `(block ....)`, and
;; the `parens` or `block` part gets changed to `multi`
(define (pack-tagged-multi r)
  (datum->syntax #f (cons multi-blank (stx-cdr r))))

;; `r` can be any of the allowed representations (multi-group,
;; single-group, or single-term), and the result is a list of group
;; syntax objects (symmetric to `pack-multi`); the result is always a
;; plain list of syntax objects
(define (unpack-multi r who at-stx)
  (cond
    [(multi-syntax? r) (cdr (syntax->list r))]
    [(group-syntax? r) (list r)]
    [(or (null? r) (pair? r)) (cannot-coerce-list who r)]
    [else (list (datum->syntax at-stx (list group-blank r)))]))

;; assumes that `tail` is a syntax list of terms, and wraps it with `multi`;
;; an empty list turns into `multi` with no groups
(define (pack-tail tail #:after [after #f])
  (if (stx-null? tail)
      (cond
        [(and after
              (syntax-position after)
              (syntax-span after))
         (define loc (srcloc (syntax-source after)
                             (syntax-line after)
                             (let ([col (syntax-column after)])
                               (and col (+ col (syntax-span after))))
                             (+ (syntax-position after) (syntax-span after))
                             0))
         (datum->syntax #f (list (syntax-property (syntax/loc loc multi) 'raw "")))]
        [else (datum->syntax #f (list multi-blank))])
      (datum->syntax #f (list multi-blank (cons group-blank tail)))))

;; unpacks by removing `multi` and/or `group` wrapper to arrive at a
;; sequence of terms; the result is a list-shaped syntax object
(define (unpack-tail r who at-stx)
  (datum->syntax
   #f
   (cond
     [(multi-syntax? r)
      (define l (syntax->list r))
      (cond
        [(null? (cdr l)) '()]
        [(= 2 (length l)) (cdr (syntax-e (cadr l)))]
        [else (raise-error who "multi-group syntax not allowed in group context" r)])]
     [(group-syntax? r) (cdr (syntax-e r))]
     [(pair? r) (cannot-coerce-pair who r)]
     [else (list (datum->syntax at-stx r))])))

;; similar to `pack-tail` but for a list of groups, so no
;; special case for empty is needed
(define (pack-multi-tail tail)
  (datum->syntax #f (cons multi-blank tail)))

;; produces a sequence of groups
(define (unpack-multi-tail r who at-stx)
  (datum->syntax
   #f
   (cond
     [(multi-syntax? r) (cdr (syntax-e r))]
     [(group-syntax? r) (list r)]
     [(pair? r) (cannot-coerce-pair who r)]
     [else (list (datum->syntax at-stx r))])))

;; ----------------------------------------

;; The `pack*` and `unpack*` variants deal with repetition
;; `depth` layers deep as needed for patterns and templates;
;; we can assume that the lists and nesting have a suitable
;; shape, because they come from `syntax-parse` or from a
;; repetition binding

(define (pack* stx depth wrap)
  (cond
    [(eqv? depth 0) (wrap stx)]
    [else (for/list ([t (in-list (syntax->list stx))])
            (pack* t (sub1 depth) wrap))]))

(define (unpack* qs r depth unpack)
  (let unpack* ([r r] [depth depth])
    (cond
      [(eqv? depth 0) (unpack r (syntax-e qs) qs)]
      [else (for/list ([r (in-list r)])
              (unpack* r (sub1 depth)))])))

(define (pack-term* stx depth)
  (pack* stx depth pack-term))

(define (unpack-term* qs r depth)
  (unpack* qs r depth unpack-term))

;; Packs to a `group` form
(define (pack-group* stx depth)
  (pack* stx depth pack-group))

;; "Unpacks" to a `group` form, which is really more about coercsions and
;; is asymmetric to `pack-group*`
(define (unpack-group* qs r depth)
  (unpack* qs r depth unpack-group))

;; Packs to a `multi` form
(define (pack-multi* stxes depth)
  (pack* stxes depth pack-multi))

;; Packs to a `multi` form, because that's the useful result from matching
(define (pack-tagged-multi* stxes depth)
  (pack* stxes depth pack-tagged-multi))

;; Unpacks a multi to a `multi` form instead of a 
(define (unpack-multi* qs r depth)
  (unpack* qs r depth unpack-multi))

;; Unpacks a multi to a `multi` form instead of a 
(define (unpack-multi-as-term* qs r depth)
  (unpack* qs r depth (lambda (r name qs)
                        (datum->syntax qs (cons multi-blank (unpack-multi r name qs))))))

(define (unpack-element* qs r depth)
  (unpack* qs r depth (lambda (r name qs) r)))

;; An extra layer of unpacking to convert to a list
(define (unpack-multi-group* qs r depth)
  (cond
    [(= depth 0) r]
    [else (unpack-multi-as-term* qs r (sub1 depth))]))

;; Like `pack-multi*, but preserves `block` instead of converting to `multi`:
(define (pack-block* stxes depth)
  (pack* stxes depth (lambda (r) r)))

(define (pack-tail* stxes depth)
  (pack* stxes depth pack-tail))

(define (unpack-tail* qs r depth)
  (unpack* qs r depth unpack-tail))

(define (pack-multi-tail* stxes depth)
  (pack* stxes depth pack-multi-tail))

(define (unpack-multi-tail* qs r depth)
  (unpack* qs r depth unpack-multi-tail))

;; similar to `unpack-tail*`, but converts each tail syntax
;; list to a plain list, and expects `depth` to be off by 1
(define (unpack-tail-list* qs r depth)
  (unpack* qs r (sub1 depth) (lambda (r name qs)
                               (syntax->list (unpack-tail r (syntax-e qs) qs)))))

;; similar to `unpack-multi-tail*`, but converts each multi-tail syntax
;; list to a plain list, and expects `depth` to be off by 1
(define (unpack-multi-tail-list* qs r depth)
  (unpack* qs r (sub1 depth) (lambda (r name qs)
                               (syntax->list (unpack-multi-tail r (syntax-e qs) qs)))))

;; similar to `unpack-tail*`, but each leaf is a plain list of terms
(define (unpack-list-tail* qs r depth)
  (unpack-term* qs r (add1 depth)))

;; similar to `unpack-multi-tail*`, but each leaf is a plain list of groups
(define (unpack-multi-list-tail* qs r depth)
  (unpack-group* qs r (add1 depth)))

;; normalize for multi-term pattern matching:
(define (repack-as-multi r)
  (cond
    [(group-syntax? r) (list multi-blank r)]
    [(multi-syntax? r) r]
    [else (list multi-blank (list group-blank r))]))

;; normalize for single-term pattern matching:
(define (repack-as-term r)
  (or (unpack-term r #f #f)
      ;; can't match a term pattern:
      #'(group)))

;; insert `term` at the front of the first group, and strip away
;; `multi` and `group` wrappers for a splicing match; this improves
;; error reporting when a match fails
(define (insert-multi-front-group term r)
  (cond
    [(group-syntax? r) (cons term
                             (cdr (syntax-e r)))]
    [(multi-syntax? r) (let ([r (cdr (syntax-e r))])
                         (if (stx-null? r)
                             (list term)
                             (if (stx-null? (stx-cdr r))
                                 (insert-multi-front-group term (stx-car r))
                                 (error "unexpected multi-group sequence for macro match"))))]
    [else (list term r)]))

(define (cannot-coerce-list who r)
  (raise-arguments-error* (if (syntax? who) (syntax-e who) who) rhombus-realm
                          "cannot coerce list to syntax"
                          "list" r))

(define (cannot-coerce-pair who r)
  (raise-arguments-error* (if (syntax? who) (syntax-e who) who) rhombus-realm
                          "cannot coerce pair to syntax"
                          "pair" r))

(define (raise-error who msg r)
  (if (procedure? who)
      (raise-result-error (proc-name who) msg r)
      (raise-arguments-error* who rhombus-realm
                              msg
                              "syntax" r)))
