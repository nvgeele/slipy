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
;;         |  <aexp>
;;
;; <exp> ::= (let ([<var> <cexp>]) <dec> ... <exp>)
;;        |  (let () <dec> ... <exp>)
;;        |  <aexp>
;;        |  <cexp>

;; JSON Output specifics:

;; <prog> ::= {vars: [<var>], exps: [<exp>]}

;; <var> ::= <string>

;; <exp> ::= {type: "let", var: <var>, val: <cexp>, body: <exp>}
;;        |  <var-let>
;;        |  <aexp>
;;        |  <cexp>

;; <var-let> ::= {type: "var-let", vars: [<var>], body: <exp>}

;; <cexp> ::= {type: "if", test: <aexp>, consequent: <exp>, alternative: <exp>}
;;         |  {type: "set", target: <var>, val: <aexp>}
;;         |  {type: "apl", operator: <aexp>, operands: [<aexp>]}
;;         |  <aexp>

;; TODO: Fix grammar voor quoted-list
;; <aexp> ::= {type: "lambda", vars: [<vars>], body: <var-let>}
;;         |  {type: "quoted-list", val: [<aexp>]}
;;         |  {type: "symbol", val: <string>}
;;         |  {type: "number", int: <bool>, val: <number>}
;;         |  {type: "bool", val: <bool>}
;;         |  {type: "string", val: <string>}
;;         |  {type: "char", val: <char>}
;;         |  {type: "var", val: <var>}

(require (rename-in racket/base
                    (primitive? rkt-primitive?))
         json)

(provide do-read read-loop slip-expand slip-read)

;; TODO: What about reading cons cells?

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

;; TODO: test (lambda x x)
;; TODO: optimize excessive frames for define/set!
(define (normalize exp k)
  (match exp
    [`(lambda ,params . ,body)
     (push-scope!)
     (let* ([nt (map normalize-term body)]
            [vars decls])
       (pop-scope!)
       (k `(lambda ,params
             ,@(for/list ([var vars])
                 `(define ,var '()))
             ,@nt)))]

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
     ;; TODO: body with multiple exprs
     (normalize-let bindings body k)]


    [`(if ,exp1 ,exp2 ,exp3)
     ;; TODO: Improve this horrible code
     (let-values ([(con vars1)
                   (begin
                     (push-scope!)
                     (let ([e (normalize-term exp2)]
                           [vars decls])
                       (pop-scope!)
                       (values e vars)))]
                  [(alt vars2)
                   (begin
                     (push-scope!)
                     (let ([e (normalize-term exp3)]
                           [vars decls])
                       (pop-scope!)
                       (values e vars)))])
       (normalize-name exp1 (lambda (t)
                              (k `(if ,t
                                      ,(if (= (length vars1) 0)
                                           con
                                           `(let ()
                                              ,@(for/list ([var vars1])
                                                  `(define ,var '()))
                                              ,con))
                                      ,(if (= (length vars2) 0)
                                           alt
                                           `(let ()
                                              ,@(for/list ([var vars2])
                                                  `(define ,var '()))
                                              ,alt)))))))]

    [`(set! ,v ,exp)
     (normalize-name exp (lambda (t)
                           (k `(set! ,v ,t))))]

    [`(define (,f . ,params) . ,body)
     (let ([lambda (normalize `(lambda ,params ,@body)
                              identity)])
       (add-decl! f)
       (k `(set! ,f ,lambda)))]

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
  (hash 'type "lambda"
        'vars (map symbol->string vars)
        'body (var-let->json body)))

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
                    'int (exact? e)
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

(define (var-let->json body)
  ;; TODO: is de body wel correct
  (define (helper exp vars exps)
    (match exp
      [(cons `(define ,var '()) rest)
       (helper rest (cons var vars) exps)]
      [(cons exp '())
       (values (reverse (cons (exp->json exp) exps)) vars)]
      [(cons exp rest)
       (helper rest vars (cons (exp->json exp) exps))]))
  (let-values ([(exps vars) (helper body '() '())])
    (hash 'type "var-let"
          'vars (map symbol->string vars)
          'body exps)))

(define (aexp->json aexp)
  (match aexp
    [`(lambda ,vars . ,body)
     (lambda->json vars body)]
    [`(quote ,x)
     (if (list? x)
         (hash 'type "quoted-list"
               'val (list->json x))
         (hash 'type "symbol"
               'val (symbol->string x)))]
    [(? number?)
     (hash 'type "number"
           'int (exact? aexp)
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
           'val (symbol->string aexp))]
    [else (error (~a "Malformed aexp: " aexp))]))

(define (cexp->json cexp)
  (match cexp
    ;; If lambda is an operator in an application, well, though break
    ;; In SLIP this is also the case.
    ;; TODO: Maybe put lambdas in evaluator as macro/native?
    [(? aexp?)
     (aexp->json cexp)]
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
           'operands (map aexp->json aexps))]
    [else (error (~a "Malformed cexp: " cexp))]))

(define (exp->json exp)
  (match exp
    [`(let () . ,body)
     (var-let->json body)]
    [`(let ([,var ,cexp]) ,body)
     (hash 'type "let"
           'var (symbol->string var)
           'val (cexp->json cexp)
           'body (exp->json body))]
    [(? aexp?)
     (aexp->json exp)]
    [(? cexp?)
     (cexp->json exp)]
    [else (error (~a "Malformed expression: " exp))]))

;; TODO: check all orders of exp lists
(define (prog->json prog)
  (define (helper vars exps prog)
    (match prog
      ['()
       (hash 'vars (map symbol->string vars)
             'exps (reverse exps))]
      [(cons `(define ,var '()) rest)
       (helper (cons var vars) exps rest)]
      [(cons exp rest)
       (helper vars (cons (exp->json exp) exps) rest)]))
  (helper '() '() prog))

;;
;; Read code
;;

(define (slip-read s-exp)
  (if (list? s-exp)
      (hash 'type "quoted-list"
            'val (list->json s-exp))
      #;(aexp->json s-exp)
      (car (list->json (list s-exp)))))

(define (slip-expand exp #:json [json #f])
  (let ([res (normalize-program exp)])
    ;;(pretty-print res)
    (if json
        (prog->json res)
        res)))

(define (read-loop* str)
  (with-handlers ([(lambda _ #t)
                   (lambda (e)
                     (jsexpr->string (hash 'success #f
                                           'content (format "~a" e))))])
    (let* ([s-exp (read (open-input-string str))]
           [res (match s-exp
                  [`(read ,data)
                   (slip-read data)]
                  [`(expand ,data)
                   (slip-expand data #:json #t)]
                  [else
                   (error "Wrong modus")])])
      (jsexpr->string (hash 'success #t
                            'content res)))))

;; Code adapted from Pycket
(define (do-read path)
  (define output
    (let rd ([s null])
      (define d (read-bytes-line))
      (cond [(or (equal? d #"\0") (eof-object? d))
             (read-loop* (bytes->string/utf-8
                          (apply bytes-append
                                 (add-between (reverse s) #"\n"))))]
            [else
             (rd (cons d s))])))
  (call-with-output-file path
    (lambda (out)
      (display output out))
    #:exists 'truncate))

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
