#lang racket

;; This file contains the reader/expander for SliPy. It is supposed to be used
;; as a subprocess of the interpreter, and will read code from the standard
;; input.
;; The Racket reader is used to parse the S-expressions, after which standard
;; macros (such as `or') are expanded and the code is transformed to ANF.
;; Transformed code is serialized to JSON which is then returned to the
;; interpreter.

;; Input language:

;; <prog> ::= <exp> ...
;;
;; <exp> ::= (define (<var> <name> ...) <exp> ...)
;;        |  (define <var> <exp>)
;;        |  (set! <var> <exp>)
;;        |  (let ([<var> <exp>] ...) <exp>)
;;        |  (if <exp> <exp> <exp>)
;;        |  (lambda (<name> ...) <exp>)
;;        |  <number>
;;        |  <boolean>
;;        |  <string>
;;        |  <var>
;;        |  <quoted list>
;;        |  <symbol>

;; Output language:

;; <prog> ::= <dec> ... <exp> ...
;;
;; <dec> ::= (define <var> '())
;;
;; <aexp> ::= (lambda (<name> ...) <dec> ... <exp>)
;;         |  <number>
;;         |  <boolean>
;;         |  <string>
;;         |  <var>
;;         |  <symbol>
;;         |  <quoted list>
;;         |  (void)
;;
;; <cexp> ::= (<aexp> <aexp> ...)
;;         |  (if <aexp> <exp> <exp>)
;;         |  (set! <var> <aexp>)
;;
;; <exp> ::= (let ([<var> <cexp>]) <dec> ... <exp>)
;;        |  (let () <dec> ... <exp>)
;;        |  <aexp>
;;        |  <cexp>

(require (rename-in racket/base
                    (read rkt-read)
                    (primitive? rkt-primitive?))
         json)

(provide read read-loop)

;;
;; Helper functions
;;


(define decls '())
(define stack '())
(define (add-decl! dec)
  (set! decls (cons dec decls)))
(define (push-scope!)
  (set! stack (cons decls stack))
  (set! decls '()))
(define (pop-scope!)
  (set! decls (car stack))
  (set! stack (cdr stack)))
(define (clear-scope!)
  (set! decls '()))
(define (clear-stack!)
  (set! stack '()))

;; Normalization code adapted from
;; http://matt.might.net/articles/a-normalization/
;; The code had to be changed to support define and set! as expressions.

(define primitives
  '(+ - * / =))

(define (atomic? exp)
  (match exp
    [`(quote ,_) #t]
    [(? number?) #t]
    [(? boolean?) #t]
    [(? string?) #t]
    ;; TODO: does slip have chars?
    [(? char?) #t]
    [(? symbol?) #t]
    ;;[(or '+ '- '* '/ '=) #t]
    ;; TODO: remove this primitives thing
    [else (or (member exp primitives) #f)]))

(define (normalize-let bindings body k)
  (define (helper bindings)
    (match bindings
      ['()
       ;; TODO: multiple exprs in body
       (normalize-term body)]
      [`([,x ,exp] . ,clause)
       (normalize exp (lambda (aexp)
                        `(let ([,x ,aexp])
                           ,(helper clause))))]))
  (push-scope!)
  (let* ([let (helper bindings)]
         [vars decls])
    (pop-scope!)
    `(let ()
       ,@(for/list ([var vars])
           `(define ,var '()))
       ,let)))

;; TODO: test IFs
;; TODO: output JSON
;; TODO: optimize excessive frames for define/set!
(define (normalize exp k)
  (match exp
    [`(lambda ,params ,body)
     ;; TODO: body with multiple exprs
     (push-scope!)
     (let* ([nt (normalize-term body)]
            [vars decls])
       (pop-scope!)
       (k `(lambda ,params
             ,@(for/list ([var vars])
                 `(define ,var '()))
             ,nt)))]

    [`(let () ,exp)
     (push-scope!)
     (let* ([nt (normalize-term exp)]
            [vars decls])
       (pop-scope!)
       (k `(let ()
             ,@(for/list ([var vars])
                 `(define ,var '()))
             ,nt)))]

    [`(let ,bindings ,body)
     (normalize-let bindings body k)]


    [`(if ,exp1 ,exp2 ,exp3)
     (normalize-name exp1 (lambda (t)
                            (k `(if ,t ,(normalize-term exp2)
                                    ,(normalize-term exp3)))))]

    [`(set! ,v ,exp)
     (normalize-name exp (lambda (t)
                           (k `(set! ,v ,t))))]

    [`(define (,f . ,params) ,body)
     (normalize `(define ,f (lambda ,params ,body)) k)
     #;(error "Function definition in normalize")]

    [`(define ,v ,exp)
     (add-decl! v)
     (normalize-name exp (lambda (t)
                           (k `(set! ,v ,t))))]

    [(? atomic?)
     (k exp)]

    [`(,f . ,e*)
     (normalize-name f (lambda (t)
                         (normalize-name* e* (lambda (t*)
                                               (k `(,t . ,t*))))))]))

(define (normalize-name exp k)
  (normalize exp
             (lambda (aexp)
               (if (atomic? aexp)
                   (k aexp)
                   (let ([t (gensym)])
                     `(let ([,t ,aexp])
                        ,(k t)))))))

(define (normalize-name* exp* k)
  (if (null? exp*)
      (k '())
      (normalize-name (car exp*)
                      (lambda (t)
                        (normalize-name* (cdr exp*)
                                         (lambda (t*)
                                           (k `(,t . ,t*))))))))

(define (normalize-term exp)
  (normalize exp (lambda (k) k)))

(define (normalize-program exps)
  (define (helper exps)
    (match exps
      ['()
       '()]
      [(cons exp rest)
       (cons (normalize-term exp)
             (helper rest))]))
  (clear-scope!)
  (clear-stack!)
  (let ([transformed (helper exps)])
    `(,@(for/list ([var decls])
          `(define ,var '()))
      ,@(if (eq? (car transformed) 'begin)
            (cdr transformed)
            transformed))))

;;
;; JSON serialization
;;

(define (aexp? exp)
  (match exp
    [`(lambda . ,_) #t]
    [`(quote . ,_) #t]
    [(? number?) #t]
    [(? boolean?) #t]
    [(? string?) #t]
    [(? char?) #t]
    [(? symbol?) #t]
    [else #f]))

(define (cexp? exp)
  (match exp
    [`(if . ,_) #t]
    [`(set! . ,_) #t]
    [(list aexps ...) (andmap aexp? aexps)]
    [else #f]))

(define (lambda->json vars body)
  ;; TODO: multiple exprs blabla
  (define (helper vars exps body)
    (match body
      ['()
       (hash 'vars (map symbol->string vars)
             'exps exps)]
      [(cons `(define ,var '()) rest)
       (helper (cons var vars)
               exps
               rest)]
      [(cons exp rest)
       (helper vars
               (cons (exp->json exp) exps)
               rest)]))
  (hash 'type "lambda"
        'vars (map symbol->string vars)
        'body (helper '() '() body)))

(define (list->json list)
  (match list
    ['()
     '()]
    [(cons e rest)
     (cons (match e
             [(list x ...)
              (list->json x)]
             [(? number?)
              (hash 'type "number"
                    'val e)]
             [(? boolean?)
              (hash 'type "bool"
                    'val e)]
             [(? string?)
              (hash 'type "string"
                    'val e)]
             [(? char?)
              (hash 'type "char"
                    'val e)]
             [(? symbol?)
              (hash 'type "symbol"
                    'val (symbol->string e))])
           (list->json rest))]))

(define (aexp->json aexp)
  (match aexp
    [`(lambda ,vars . ,body)
     (lambda->json vars body)]
    [`(quote ,x)
     (if (list? x)
         (hash 'type "quoted-list"
               'val (list->json x))
         (hash 'type "symbol"
               'val x))]
    [(? number?)
     (hash 'type "number"
           'val aexp)]
    [(? boolean?)
     (hash 'type "bool"
           'val aexp)]
    [(? string?)
     (hash 'type "string"
           'val aexp)]
    [(? char?)
     (hash 'type "char"
           'val aexp)]
    [(? symbol?)
     (hash 'type "var"
           'val (symbol->string aexp))]))

(define (cexp->json cexp)
  (match cexp
    [`(if ,aexp ,exp1 ,exp2)
     (hash 'type "if"
           'test (aexp->json aexp)
           'consequent (exp->json exp1)
           'alternative (exp->json exp2))]
    [`(set! ,var ,aexp)
     (hash 'type "set"
           'target (symbol->string var)
           'val (aexp->json aexp))]
    [`(,op . ,aexps)
     (hash 'type "apl"
           'operator (aexp->json op)
           'operands (map aexp->json aexps))]))

(define (exp->json exp)
  (match exp
    [`(let () ,body)
     ;; TODO: implement this
     (error "Vergeet me nietje")]
    [`(let ([,var ,cexp]) ,body)
     (hash 'type "let"
           'var (symbol->string var)
           'val (cexp->json cexp)
           'body (exp->json body))]
    [(? aexp?)
     (aexp->json exp)]
    [(? cexp?)
     (cexp->json exp)]
    [else (error "Malformed expression")]))

;; TODO: check all orders of exp lists
(define (prog->json prog)
  (define (helper vars exps prog)
    (match prog
      ['()
       (hash 'vars (map symbol->string vars)
             'exps exps)]
      [(cons `(define ,var '()) rest)
       (helper (cons var vars) exps rest)]
      [(cons exp rest)
       (helper vars (cons (exp->json exp) exps) rest)]))
  (helper '() '() prog))

;;
;; Read code
;;

(define (read exp #:json [json #f])
  (let ([res (normalize-program exp)])
    (if json
        (prog->json res)
        res)))

(define (read-loop* str)
  (with-handlers ([(lambda _ #t)
                   (lambda (e)
                     (jsexpr->string (hash 'success #f
                                           'content (format "~a" e))))])
    (let* ([s-exp (rkt-read (open-input-string str))]
           [res (read s-exp #:json #t)]
           #;[out (format "~a" res)])
      (jsexpr->string (hash 'success #t
                            'content res)))))

;; Code adapted from Pycket
(define (read-loop)
  (define mod
    (let rd ([s null])
      (define d (read-bytes-line))
      (cond [(or (equal? d #"\0") (eof-object? d))
             (read-loop* (bytes->string/utf-8
                          (apply bytes-append
                                 (add-between (reverse s) #"\n"))))]
            [else
             (rd (cons d s))])))
  ;; Never forget the newline
  (displayln mod)
  (flush-output)
  (read-loop))
