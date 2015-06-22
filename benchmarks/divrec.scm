(define (cddr l)
  (cdr (cdr l)))

(define (create-n n)
  (define (g39 n a)
    (if (= n 0) a
        (g39 (- n 1) (cons '() a))))
  (g39 n '()))

(define *ll* (create-n 131072))

(define (recursive-div2 l)
  (if (null? l)
      '()
      (cons (car l) (recursive-div2 (cddr l)))))

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
        (recursive-div2 *ll*)
        (let ((t (- (time) s)))
          (display t)(newline)
          (do-tests (- n 1) (cons t times))))))

(do-tests samples '())
