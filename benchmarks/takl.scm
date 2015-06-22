;;; TAKL -- The TAKeuchi function using lists as counters.

(define (listn n)
  (if (= n 0)
      '()
      (cons n (listn (- n 1)))))

(define l18 (listn 18))
(define l12 (listn 12))
(define  l6 (listn 6))

(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x) y z)
           (mas (cdr y) z x)
           (mas (cdr z) x y))))

(define (shorterp x y)
  (if (not (null? y))
      (if (null? x)
          #t
          (shorterp (cdr x)
                    (cdr y)))
      #f))

(define samples 20)

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
      (let ((s (time)))
        (mas l18 l12 l6)
        (let ((t (- (time) s)))
          (display t)(newline)
          (do-tests (- n 1) (cons t times))))))

(do-tests samples '())
