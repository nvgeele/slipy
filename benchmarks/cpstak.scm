;;; CPSTAK -- A continuation-passing version of the TAK benchmark.
;;; A good test of first class procedures and tail recursion.

(define (cpstak x y z)

  (define (tak x y z k)
    (if (not (< y x))
        (k z)
        (tak (- x 1)
             y
             z
             (lambda (v1)
               (tak (- y 1)
                    z
                    x
                    (lambda (v2)
                      (tak (- z 1)
                           x
                           y
                           (lambda (v3)
                             (tak v1 v2 v3 k)))))))))

  (tak x y z (lambda (a) a)))

(define samples 30)

(define (mean d l)
  (/ (apply + d) l))

(define (stddev data length)
  (let ((m (mean data length)))
    (sqrt
     (/ (apply + (map (lambda (n) (* (- n m) (- n m))) data))
        (- length 1)))))

(define (do-tests n times)
  (if (= n 0)
      (let ((m (mean times samples))
            (s (stddev times samples)))
        (display "Mean: ") (display m) (newline)
        (display "Stddev: ") (display s) (newline))
      (begin (display n)(newline)
             (let ((s (time)))
               (cpstak 18 12 6)
               (do-tests (- n 1) (cons (- (time) s) times))))))

(do-tests samples '())
