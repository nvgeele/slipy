;;; SUMFP -- Compute sum of integers from 0 to 10000 using floating point


(define iters 500000.0)

(define (run n)
  (define (loop i sum)
    (if (< i 0.0)
        sum
        (loop (- i 1.0) (+ i sum))))
  (loop n 0.0))

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
        (run iters)
        (let ((t (- (time) s)))
          (display t)(newline)
          (do-tests (- n 1) (cons t times))))))

(do-tests samples '())
