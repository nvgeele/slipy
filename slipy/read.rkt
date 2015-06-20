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
;;        |  (let ([<var> <exp>] ...) <exp> ...)
;;        |  (if <exp> <exp> <exp>)
;;        |  (lambda (<name> ...) <exp>)
;;        |  (begin <exp> ...)
;;        |  <number>
;;        |  <boolean>
;;        |  <string>
;;        |  <var>
;;        |  <quoted list>
;;        |  <symbol>

;; Output language:

;; <prog> ::= <dec> ... <exp> ...
;;
;; <dec> ::= <var>
;;
;; <aexp> ::= (lambda (<name> ...) (<dec>*) <exp>+)
;;         |  <number>
;;         |  <boolean>
;;         |  <string>
;;         |  <var>
;;         |  <symbol>
;;         |  <quoted list>
;;         |  (void)
;;
;; <cexp> ::= (<aexp> <aexp>*)
;;         |  (if <aexp> <exp> <exp>)
;;         |  (set! <var> <aexp>)
;;         |  (begin <exp> ...)
;;         |  <aexp>
;;
;; <exp> ::= (let ([<var> <cexp>]*) (<dec>*) <exp>+)
;;        |  <aexp>
;;        |  <cexp>

;; JSON Output specifics:

;; <prog> ::= {vars: [<var>], exps: [<exp>]}

;; <var> ::= <string>

;; <exp> ::= {type: "let", var: <var>, val: <cexp>, body: [<exp>]}
;;        |  <var-let>
;;        |  <aexp>
;;        |  <cexp>

;; <var-let> ::= {type: "var-let", vars: [<var>], body: [<exp>]}

;; TODO: Fix grammar voor if (?)
;; <cexp> ::= {type: "if", test: <aexp>, consequent: <exp>, alternative: <exp>}
;;         |  {type: "begin", body: [<exp>]}
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

#;(provide do-read read-loop slip-expand slip-read)
(provide (all-defined-out))

;; TODO: merge normalize and json generation
;; TODO: What about reading cons cells?
;; TODO: Change the way let expressions are normalised

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
  (let ([vars decls])
    (set! decls (car stack))
    (set! stack (cdr stack))
    vars))
(define (clear-scope!)
  (set! decls '()))
(define (clear-stack!)
  (set! stack '()))

;; Normalization code adapted from
;; http://matt.might.net/articles/a-normalization/
;; The code had to be changed to support define and set! as expressions.

(define primitives
  '(not + - * / = < > <= >= exact->inexact time display displayln newline void list append cons car cdr length null? read vector make-vector vector-length vector-ref vector-set! void apply eval call/cc sin quotient))

(define (atomic? exp)
  (match exp
    [`(quote ,_) #t]
    [(? number?) #t]
    [(? boolean?) #t]
    [(? string?) #t]
    ;; TODO: does slip have chars?
    [(? char?) #t]
    [(? symbol?) #t]
    ;; TODO: are primitives necessary?
    [else (or (member exp primitives) #f)]))

;; Returns all items that are exclusively in l2
(define (djl l1 l2)
  (let ([s1 (list->set l1)]
        [s2 (list->set l2)])
    (set->list (set-subtract s2 s1))))

(define (contract-let exp)
  (define (inner exp bindings all-vars)
    (match exp
      [`(let (,binding) ,vars ,body)
       (inner body (cons binding bindings) (cons vars all-vars))]
      [`(let (,binding) ,vars . ,body)
       (let ((bindings (cons binding bindings)))
         `(let (,@(reverse bindings))
            ,(djl (map car bindings) (flatten (cons vars all-vars)))
            ,@body))]
      [`(let (,binding . ,rest) ,vars . ,body)
       (inner `(let (,@rest) ,vars ,@body)
              (cons binding bindings)
              all-vars)]
      [else
       `(let (,@(reverse bindings))
          ,(djl (map car bindings) (flatten all-vars))
          ,exp)]))
  (if (and (list? exp) (eq? (car exp) 'let))
      (inner exp '() '())
      exp))

(define (normalize-let bindings body k)
  (define vars (map car bindings))
  (define exps (map cadr bindings))
  (push-scope!)
  (define e
    (contract-let
     (foldr (lambda (var b-exp exp)
              (normalize b-exp
                         (lambda (aexp)
                           `(let ([,var ,aexp]) ()
                              ,exp))))
            '()
            vars
            exps)))
  (match e
    [`(let ,bindings () ())
     (let ([body (map normalize-term body)])
       (k `(let ,bindings
             ,(djl vars (pop-scope!))
             ,@body)))]
    [else
     (pretty-print e)
     (error "no matching")]))

;; TODO: test (lambda x x)
;; TODO: optimise excessive frames for define/set!
(define (normalize exp k)
  (match exp
    [`(begin . ,body)
     (k `(begin ,@(map normalize-term body)))]

    [`(lambda ,params . ,body)
     (push-scope!)
     (let ([nt (map normalize-term body)])
       (k `(lambda ,params
             ,(djl params (pop-scope!))
             ,@nt)))]

    [`(let () . ,exps)
     (push-scope!)
     (let* ([nt (map normalize-term exps)])
       (k `(let ()
             ,(pop-scope!)
             ,@nt)))]

    [`(let ,bindings . ,body)
     (normalize-let bindings body k)]


    [`(if ,exp1 ,exp2 ,exp3)
     ;; TODO: Improve this horrible code
     ;; Note how we assume arguments are eval'd l->r
     (let-values ([(con vars1) (begin
                                 (push-scope!)
                                 (values (normalize-term exp2)
                                         (pop-scope!)))]
                  [(alt vars2) (begin (push-scope!)
                                      (values (normalize-term exp3)
                                              (pop-scope!)))])
       (normalize-name
        exp1
        (lambda (t)
          (k `(if ,t
                  ,(if (= (length vars1) 0)
                       con
                       `(let ()
                          ,vars1
                          ,con))
                  ,(if (= (length vars2) 0)
                       alt
                       `(let ()
                          ,vars2
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
                                               (k `(,t . ,t*))))))]
    [`()
     (k '())]))

(define (normalize-name exp k)
  (normalize exp
             (lambda (aexp)
               (if (atomic? aexp)
                   (k aexp)
                   (let ([t (gensym)])
                     `(let ([,t ,aexp]) ()
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
  (contract-let (normalize exp identity)))

(define (normalize-program exps)
  (clear-scope!)
  (clear-stack!)
  (let ([transformed (map normalize-term exps)])
    (cons decls transformed)))

;;
;; Lexical Addressing
;;

(struct frame (scope bindings previous)
        #:transparent)
(struct ref ())
(struct lex-ref ref (symbol scope offset)
        #:transparent)
(struct nat-ref ref (symbol)
        #:transparent)

(define (make-frame vars previous)
  (frame (if previous
             (+ 1 (frame-scope previous))
             1)
         (for/hash ([v vars]
                    [o (range 0 (length vars))])
           (values v o))
         previous))

(define (get-address frame symbol)
  (define h (frame-bindings frame))
  (define o (hash-ref h symbol #f))
  (cond [o (lex-ref symbol (frame-scope frame) o)]
        [(frame-previous frame)
         (get-address (frame-previous frame) symbol)]
        [(member symbol primitives)
         (nat-ref symbol)]
        [else symbol]))

(define (lexical-address* exp frame)
  (match exp
    [`(let ,bindings ,vars . ,body)
     (let* ([new-frame (make-frame (append (map car bindings) vars) frame)]
            [bindings (for/list ([binding bindings])
                        `(,(car binding)
                          ,(lexical-address* (cadr binding) new-frame)))])
       `(let ,bindings
          ,vars
          ,@(map (lambda (e) (lexical-address* e new-frame)) body)))]
    [`(lambda ,params ,vars . ,body)
     (let* ([new-frame (make-frame (append params vars) frame)])
       `(lambda ,params
          ,vars
          ,@(map (lambda (e) (lexical-address* e new-frame)) body)))]
    [`(if ,test ,con ,alt)
     `(if ,(get-address frame test)
          ,(lexical-address* con frame)
          ,(lexical-address* alt frame))]
    [`(set! ,var ,aexp)
     `(set! ,(lexical-address* var frame)
        ,(lexical-address* aexp frame))]
    [`(begin . ,exps)
     `(begin ,@(map (lambda (e) (lexical-address* e frame)) exps))]
    [`(,rator . ,rands)
     `(,(let ([ref (get-address frame rator)])
          (if (eq? 'quote 'ref)
              'quote
              ref))
       ,@(map (lambda (e) (lexical-address* e frame)) rands))]
    [(? symbol?)
     (get-address frame exp)]
    [else exp]))

(define (lexical-address program)
  (let ([frame (make-frame (car program) #f)])
    (cons (car program)
          (map (lambda (e) (lexical-address* e frame))
               (cdr program)))))

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
    [(? ref?) #t]
    ;; TODO: remove symbol?
    ;; [(? symbol?) #t]
    ['() #t]
    [else #f]))

(define (cexp? exp)
  (match exp
    [`(if . ,_) #t]
    [`(set! . ,_) #t]
    ;; [`(begin . ,_) #t]
    [(list aexps ...) (andmap aexp? aexps)]
    [else #f]))

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

(define (ref->json ref)
  (cond ((nat-ref? ref)
         (hash 'type "nat-ref"
               'symbol (symbol->string (nat-ref-symbol ref))))
        ((lex-ref? ref)
         (hash 'type "lex-ref"
               'scope (lex-ref-scope ref)
               'offset (lex-ref-offset ref)
               'symbol (symbol->string (lex-ref-symbol ref))))
        (else (error (~a "not a ref: " ref)))))

(define (aexp->json aexp)
  (match aexp
    [`(lambda ,params ,vars . ,body)
     (hash 'type "lambda"
           'params (map symbol->string params)
           'vars (map symbol->string vars)
           'body (map exp->json body))]
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
    [(? ref?)
     (ref->json aexp)]
    ;; TODO: Remove symbol?
    ;; [(? symbol?)
    ;;  (error "Symbol not expected here")]
    ['()
     (hash 'type "quoted-list"
           'val '())]
    [else (error (~a "Malformed aexp: " aexp))]))

(define (cexp->json cexp)
  (match cexp
    [(? aexp?)
     (aexp->json cexp)]
    [`(if ,aexp ,exp1 ,exp2)
     (hash 'type "if"
           'test (aexp->json aexp)
           'consequent (exp->json exp1)
           'alternative (exp->json exp2))]
    [`(set! ,var ,aexp)
     (hash 'type "set"
           'target (ref->json var)
           'val (aexp->json aexp))]
    [`(,op . ,aexps)
     (hash 'type "apl"
           'operator (aexp->json op)
           'operands (map aexp->json aexps))]
    [else (error (~a "Malformed cexp: " cexp))]))

(define (exp->json exp)
  (match exp
    [`(let ,bindings ,vars . ,body)
     (hash 'type "let"
           'vars (map (compose symbol->string car) bindings)
           'vals (map (compose exp->json cadr) bindings)
           'decls (map symbol->string vars)
           'body (map exp->json body))]
    [`(begin . ,exprs)
     (hash 'type "begin"
           'body (map exp->json exprs))]
    [(? aexp?)
     (aexp->json exp)]
    [(? cexp?)
     (cexp->json exp)]
    [else (error (~a "Malformed expression: " exp))]))

(define (prog->json prog)
  (hash 'vars (map symbol->string (car prog))
        'exps (map exp->json (cdr prog))))

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
  (let ([res (lexical-address (normalize-program exp))])
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
