#lang racket

(require (rename-in racket/base
                    (read rkt-read)
                    (primitive? rkt-primitive?))
         json)

(define (primitive? exp)
  (member exp '(+ - * /)))

(define (quoted? exp)
  (and (list? exp)
       (eq? (car exp) 'quote)))

(define (var? exp)
  (symbol? exp))

(define (lambda? exp)
  (and (list? exp)
       (eq? (car exp) 'lambda)
       (= (length exp) 3)
       (andmap symbol? (cadr exp))
       (exp? (caddr exp))))

(define (if? exp)
  (and (list? exp)
       (eq? (car exp) 'if)
       (= (length exp) 4)
       (aexp? (cadr exp))
       (exp? (caddr exp))
       (exp? (cadddr exp))))

(define (let-bindings-ok? exp)
  (and (list? exp)
       (= (length exp) 1)
       (list? (car exp))
       (= (length (car exp)) 2)
       (symbol? (caar exp))
       (cexp? (cadar exp))))

(define (let? exp)
  (and (list? exp)
       (eq? (car exp) 'let)
       (= (length exp) 3)
       (let-bindings-ok? (cadr exp))
       (exp? (caddr exp))))

(define (aexp? exp)
  (or (number? exp)
      (string? exp)
      (var? exp)
      (boolean? exp)
      (lambda? exp)
      (quoted? exp)
      (primitive? exp)))

(define (cexp? exp)
  (or (and (list? exp)
           (andmap aexp? exp))
      (if? exp)))

(define (exp? exp)
  (or (cexp? exp)
      (aexp? exp)
      (let? exp)))

(define (transform exp)
  #;(pretty-print exp)
  #;(pretty-print (exp? exp))
  exp)

(define (read str)
  (let ([s-exp (rkt-read (open-input-string str))])
    (transform s-exp)))

(define (read-loop str)
  (with-handlers ([(lambda _ #t)
                   (lambda (e)
                     (jsexpr->string (hash 'success #f
                                           'content (format "~a" e))))])
    (let* ([res (read str)]
           [out (format "~a" res)])
      (jsexpr->string (hash 'success #t
                            'content out)))))

;; Code adapted from Pycket
(define (loop)
  (define mod
    (let rd ([s null])
      (define d (read-bytes-line))
      (cond [(or (equal? d #"\0") (eof-object? d))
             (read-loop (bytes->string/utf-8
                         (apply bytes-append
                                (add-between (reverse s) #"\n"))))]
            [else
             (rd (cons d s))])))
  ;; Never forget the newline
  (displayln mod)
  (flush-output)
  (loop))

#;(read (rkt-read (open-input-string "(let ((a (* 1 1))) (+ a a))")))

(loop)
