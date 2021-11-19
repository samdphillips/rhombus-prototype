#lang racket/base
(require shrubbery/print
         shrubbery/syntax-color
         scribble/racket
         syntax/parse
         racket/list
         (only-in scribble/core
                  element
                  paragraph
                  table
                  style
                  plain))

(provide typeset-rhombus
         typeset-rhombusblock)

(define (typeset-rhombus stx)
  ;; "pretty" prints to a single line
  ;; FIXME: doesn't yet insert `«` and `»` where needed
  (syntax-parse stx
    #:datum-literals (parens)
    [(parens g)
     (let loop ([stx #'g])
       (define (seq open elems-stx close)
         (list (element paren-color open)
               (add-between (map loop (syntax->list elems-stx))
                            tt-comma)
               (element paren-color close)))
       (define (group elems-stx)
         (let g-loop ([elems (syntax->list elems-stx)] [pre-space? #f])
           (cond
             [(null? elems) null]
             [else
              (define elem (car elems))
              (define alt-elem (syntax-parse elem
                                 #:datum-literals (op)
                                 [(op x) #'x]
                                 [_ #f]))
              (define (prefixed? stx)
                (not (null? (or (syntax-property stx 'raw-prefix) '()))))
              (define (suffixed? stx)
                (not (null? (or (syntax-property stx 'raw-suffix) '()))))
              (define add-space?
                (or pre-space? (prefixed? elem) (and alt-elem (prefixed? alt-elem))))
              (define e (loop (car elems)))
              (cons (cond
                      [add-space? (list tt-space e)]
                      [else e])
                    (g-loop (cdr elems)
                            (or (suffixed? elem) (and alt-elem (suffixed? alt-elem)))))])))
       (syntax-parse stx
         #:datum-literals (group parens brackets braces block alts)
         [(group elem ... (block g ...))
          (list (group #'(elem ...))
                (element tt-style ": ")
                (add-between (map loop (syntax->list #'(g ...)))
                             (element tt-style "; ")))]
         [(group elem ... (alts (block g ...) ...+))
          (list (group #'(elem ...))
                (element tt-style " | ")
                (add-between
                 (for/list ([gs (in-list (syntax->list #'((g ...) ...)))])
                   (add-between (map loop (syntax->list gs))
                                (element tt-style "; ")))
                 (element tt-style " | ")))]
         [(group elem ...)
          (group #'(elem ...))]
         [(parens elem ...) (seq "(" #'(elem ...) ")")]
         [(brackets elem ...) (seq "[" #'(elem ...) "]")]
         [(braces elem ...) (seq "{" #'(elem ...) "}")]
         [(op . _)
          (element tt-style (shrubbery-syntax->string stx))]
         [_
          (define d (syntax->datum stx))
          (element (cond
                     [(symbol? d) symbol-color]
                     [(keyword? d) paren-color]
                     [else value-color])
            (shrubbery-syntax->string stx))]))]))

(define (typeset-rhombusblock stx)
  ;; Go back to a string, then parse again using the
  ;; colorer. Why didn't we use a string to start with?
  ;; Because having `rhm` work on implicitly quoted syntax
  ;; means that you get nice editor support.
  (define block-stx
    (syntax-case stx ()
      [(_ (_ block)) #'block]))
  (define tag-stx
    (syntax-case stx ()
      [(_ (_ self)) #'self]))
  (define str (block-string->content-string (shrubbery-syntax->string block-stx
                                                                      #:keep-suffix? #t)
                                            (syntax-case block-stx ()
                                              [(b . _)
                                               (syntax-column #'b)])))
  (define init-col (infer-indentation str))
  (define in (open-input-string str))
  (port-count-lines! in)
  (define elements+linebreaks
    (let loop ([state #f] [pos 0] [skip-ws init-col])
      (define-values (lexeme attribs paren start+1 end+1 backup new-state)
        (shrubbery-lexer in 0 state))
      (cond
        [(eof-object? lexeme) null]
        [else
         (define start (sub1 start+1))
         (define end (sub1 end+1))
         (let t-loop ([pos pos] [skip-ws skip-ws])
           (cond
             [(pos . < . start)
              (define amt (- start pos))
              (cons (element hspace-style (make-string (max 0 (- amt skip-ws)) #\space))
                    (t-loop start (- skip-ws amt)))]
             [else
              (define type (if (hash? attribs)
                               (hash-ref attribs 'type 'other)
                               attribs))
              (define (make-element start end skip-ws)
                (define style
                  (case type
                    [(string text constant) value-color]
                    [(symbol) symbol-color]
                    [(parenthesis hash-colon-keyword) paren-color]
                    [(error) error-color]
                    [(comment) comment-color]
                    [(white-space) hspace-style]
                    [else tt-style]))
                (define show-str
                  (substring str
                             (let loop ([start start] [skip-ws skip-ws])
                               (cond
                                 [(skip-ws . <= . 0) start]
                                 [(start . >= . end) start]
                                 [(char=? #\space (string-ref str start))
                                  (loop (add1 start) (sub1 skip-ws))]
                                 [else start]))
                             end))
                (define m (and (not (eq? style hspace-style))
                               (regexp-match-positions #rx"[^ ]" show-str)))
                (cond
                  [(and m (positive? (caar m)))
                   (list
                    (element hspace-style (make-string (caar m) #\space))
                    (element style (substring show-str (caar m))))]
                  [else
                   (element style show-str)]))
              (let token-loop ([start start] [end end] [skip-ws skip-ws])
                (define nl (regexp-match-positions #rx"\n" str start end))
                (cond
                  [nl
                   (cond
                     [(= (caar nl) start)
                      (cons 'linebreak
                            (if (= (cdar nl) end)
                                (loop state end init-col)
                                (token-loop (add1 start) end init-col)))]
                     [else
                      (define upto (caar nl))
                      (cons (make-element start upto skip-ws)
                            (token-loop upto end (- skip-ws (- upto start))))])]
                  [else
                   (cons (make-element start end skip-ws)
                         (loop new-state end (- skip-ws (- end start))))]))]))])))
  (define elementss (let loop ([l elements+linebreaks])
                      (cond
                        [(null? l) (list null)]
                        [(eq? 'linebreak (car l))
                         (define rl (cdr l))
                         (define r (loop (if (and (pair? rl)
                                                  (eq? 'linebreak (car rl)))
                                             (cons (element hspace-style " ")
                                                   rl)
                                             rl)))
                         (if (null? (car r))
                             r
                             (cons null r))]
                        [else
                         (define r (loop (cdr l)))
                         (cons (cons (car l) (car r))
                               (cdr r))])))
  (define indent (element hspace-style "  "))
  (define (make-line elements)
    (paragraph plain (cons indent elements)))
  (cond
    [(null? elementss)
     (element plain "")]
    [(null? (cdr elementss))
     (make-line (car elementss))]
    [else
     (table plain
            (map (lambda (elements)
                   (list (make-line elements)))
                 elementss))]))

(define tt-style (style 'tt null))
(define hspace-style (style 'hspace null))

(define tt-space (element tt-style " "))
(define tt-comma (element tt-style ","))

(define (block-string->content-string str col)
  ;; strip `:` from the beginning, add spaces
  ;; corresponding to `col`, then strip any blank newlines
  (define-values (content-str prefix-len)
    (cond
      [(regexp-match-positions #rx"^:«(.*)»[ ]*$" str)
       => (lambda (m)
            (values (substring str (caadr m) (cdadr m))
                    (+ (or col 0) 2)))]
      [(regexp-match? #rx"^:" str)
       (values (substring str 1) (+ (or col 0) 1))]))
  (define full-str
    (string-append (make-string prefix-len #\space) content-str))
  (regexp-replace* #px"\\s*\n\\s*$"
                   (regexp-replace* #px"^\\s*\n" full-str "")
                   ""))

(define (infer-indentation str)
  (define m (regexp-match-positions #px"[^ ]" str))
  (if m
      (caar m)
      0))

  
