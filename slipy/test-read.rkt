#lang racket

(require (rename-in "read.rkt"
                    (read slip-read))
         json)

(define helper (compose (lambda _ (displayln "-----"))
                        (lambda (hash)
                          (pretty-print hash)
                          (pretty-print (jsexpr->string hash)))
                        (lambda (input)
                          (slip-read input #:json #t))
                        read
                        open-input-string))

(helper "((+ 1 (call/cc (lambda (cont) (cont 2)))))")
;;(helper "( ((lambda (x) x) 42) )")
;;(helper "( ((lambda (x) (+ x x)) 42))")
;;(helper "( ((lambda (x y) ((lambda (z) (- x y z)) 10)) 32 24) )")
;;(helper "((let ([b 1]) (define a 1)))")
;;(helper "((let ([a(if #t (+ 1 (+ 2 3)) (- 1 (- 2 3)))]) (+ a a)))")
;;(helper "((define (id x) x) (+ (id 1) (id 2)))")
;;(helper "(begin (lambda (x y) (define z (* x y (+ x y)))))")
#;(helper "((+ 1 2)
  (* 3 3)
(append '(1 2 3) (append '(4 5 6) '(7 8 9))))")
#;(helper "(((define (id x)
      (* x (* x (define y 1))))
    42))")
#;(helper "(begin
  (let ([y (define z (+ 1 (+ 2 3)))]
        [x 1])
    (* x y (+ z (set! z 1))))
  (* x y (+ z 1)))")
#;(helper "(begin
  (+ (define x (* 1 (* 2 3)))
     (define y (+ 1 (+ 2 3)))
     'a
     (car '(1 2 '(3 #t))))
  (+ x y (* x y)))")
#;(helper "(begin
  (let ()
    (+ (define x (* 1 (* 2 3)))
       (define y (+ 1 (+ 2 3)))))
  (+ x y (* x y)))")
#;(helper "(begin ((lambda (n) (+ (define x 1) (define x 2))) 1))")
#;(helper "(begin (let () (+ 1 2 (* 2 2))))")
#;(helper "(begin (+ (define x 1) (define y 2)))")
#;(helper "(begin (+ (define x 1) (define y 2)) (+ x y))")
#;(helper "(begin (define x 1) (define y 2) (+ x y))")

;;(helper "(define (k) (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 6))))))")
;;(helper "(define x (append '(1) (append '(2 3) '(4 5))))")
;;(helper "(append '(1) (append '(2 3) '(4 5)))")
;;(helper "(begin (append '(1) (append '(2 3) '(4 5))))")
;;(helper "(define loop (lambda () (loop)))")
;;(helper "(define (loop n) (define x (* 1 (+ 2 3))) (set! x 2) (loop 0))")
;;(helper "(begin ((define (id x) (* x (* x x))) (* 120 120)))")
