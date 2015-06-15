#lang racket

(require "read.rkt"
         json)

(define helper (compose (lambda _ (displayln "-----"))
                        (lambda (hash)
                          (pretty-print hash)
                          ;;(pretty-print (jsexpr->string hash))
                          (void))
                        (lambda (input)
                          (slip-expand input #:json #t))
                        read
                        open-input-string))

(define helper2 (compose (lambda _ (displayln "-----"))
                         (lambda (hash)
                           (pretty-print hash)
                           (void))
                         (lambda (input)
                           (slip-read input))
                         read
                         open-input-string))

;;
;; "Tests" for slip-read
;;

;;(helper2 "(if #t 1 2)")
;;(helper2 "a")
;;(helper2 "1")
;;(helper2 "'a")
;;(helper2 "'(1 2 3)")

;;
;; "Tests" for slip-expand
;;

#;(contract-let '(let ((a 1))
                 (void)))
#;(contract-let '(let ((a 1))
                 (let ((b 2))
                   (void))))
#;(contract-let '(let ((a 1))
                 (let ((b 2))
                   (let ((c 3))
                     (void)
                     (void)))))

(define id identity)
(define test (compose pretty-print normalize-program))
(define test2 (compose pretty-print lexical-address normalize-program))
(define test3 (compose pretty-print
                       prog->json
                       ;;pretty-print
                       lexical-address
                       normalize-program))

#;(test3 '((begin

  ;;; SUMFP -- Compute sum of integers from 0 to 10000 using floating point

           ;;(begin (define (displayln x) (display x)(newline)) (define iters 500000.0) (define samples 30) (define (loop i sum) (if (= i 0.0) sum (loop (- i 1.0) (+ i sum)))) (define (test) (displayln "Doing a test run") (define start (time)) (loop iters 0.0) (- (time) start)) (define (do-tests n res) (if (= n 0) res (do-tests (- n 1) (cons (test) res)))) (define (mean lst) (/ (apply + lst) samples)) (/ (mean (do-tests samples '())) 1000000))

           (define iters 500000.0)
           (define samples 30)
           (define (loop i sum) (if (= i 0.0) sum (loop (- i 1.0) (+ i sum))))
           (define (test) (display "Doing a test run")(newline) (define start (time)) (loop iters 0.0) (- (time) start))
           (define (do-tests n res) (if (= n 0) res (do-tests (- n 1) (cons (test) res))))
           (define (mean lst) (/ (apply + lst) samples))
           (mean (do-tests samples '())))))
#;(test3
 '((define (mean list)
    (/ (apply + list)
       (length list)))
  (let ([l '(3 4 5)]
        [l (cons 2 l)]
        [l (cons 1 l)]
        [mean2 (lambda (list)
                 (displayln "Calculating mean")
                 (mean (cdr (cdr list))))])
    (mean2 l)))
 )
#;(newline)
#;(contract-let
 '(let ((a 1)) ()
       (let ((b 2)
             (c 3)) ()
             (* a b c))))
;;(newline)
#;(test2
 '((define x 1)
   (let ((a 1))
     (let ((b 2)
           (c 3))
       (* a b c)))
   (+ x x)))
;;(newline)
#;(test
 '((let ((a 1))
     (define b 2)
     (+ a b))))

;;(test '((let ((a (let ((b 2)) b))) a)))

;;(normalize '(if x (define y 1) (define z 2)) id)
;;(normalize '(lambda (x y) (define x 1) y) id)


#;(normalize-let '((a (+ 1 2)) (x (* a (+ a (define x 1))))) '((define y 2)(void)(+ a b)) identity)


;;(helper "((begin (displayln x) (define x 1)))")
;;(helper "((if (+ 1 2) #t #f))")
;;(helper "((+ 1 2 3 (+ 2 3) 4 5))")
;;(helper "((let ((n (let ((y 2)(x 3)) (+ x y)))) (* n n)))")
;;(helper "((let ((x 1) (y (* x x))) y))")
;;(let () 1 2 3 4 (define (loop) 1 2 3 4 5 (loop)) 1 2 (loop) 4 5)
;;(helper "((let ((n (begin 1 2 3))) n))")
;;(let () (define (fac n) (if (= 0 n) 1 (* n (fac (- n 1))))) (fac 10))
;;(let () (define (loop) 1 2 3 4 5 (loop)) 1 2 3 4 5 (loop))
;;(helper "(())")
#;(helper "((let () 1 2 3))")
#;(helper "((let ((n (begin 1 2 3))) n))")
;;(helper "((if #t (define x 1) 2))")
;;(helper "('a)")
;;(helper "((+ 1 'a))")
;;(helper "((define (f return) (return 2) 3) (display (f (lambda (x) x))) (display (call/cc f)))")
;;(helper "((+ 1 (call/cc (lambda (cont) (cont 2)))))")
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
