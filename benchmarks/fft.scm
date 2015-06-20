;;; FFT - Fast Fourier Transform, translated from "Numerical Recipes in C"

;; Let's ignore this for now
(define (exact->inexact n) n)

(define (four1 data)
  (let ((n (vector-length data))
        (pi*2 6.28318530717959)) ; to compute the inverse, negate this value

    ; bit-reversal section

    (define (loop1 i j)
      (if (< i n)
          (begin
            (if (< i j)
                (begin
                  (let ((temp (vector-ref data i)))
                    (vector-set! data i (vector-ref data j))
                    (vector-set! data j temp))
                  (let ((temp (vector-ref data (+ i 1))))
                    (vector-set! data (+ i 1) (vector-ref data (+ j 1)))
                    (vector-set! data (+ j 1) temp)))
                (void))
            (define (loop2 m j)
              (if (if (>= m 2) (>= j m) #f)
                  (loop2 (quotient m 2) (- j m))
                  (loop1 (+ i 2) (+ j m))))
            (loop2 (quotient n 2) j))
          (void)))
    (loop1 0 0)

    ; Danielson-Lanczos section

    (define (loop3 mmax)
      (if (< mmax n)
        (let ((theta
                (/ pi*2 (exact->inexact mmax)))
               (wpr
                (let ((x (sin (* 0.5 theta))))
                  (* -2.0 (* x x))))
               (wpi
                (sin theta)))
          (define (loop4 wr wi m)
            (if (< m mmax)
              (begin

                (define (loop5 i)
                  (if (< i n)
                    (let ((j
                           (+ i mmax))
                          (tempr
                           (-
                            (* wr (vector-ref data j))
                            (* wi (vector-ref data (+ j 1)))))
                          (tempi
                           (+
                            (* wr (vector-ref data (+ j 1)))
                            (* wi (vector-ref data j)))))
                      (vector-set! data j
                        (- (vector-ref data i) tempr))
                      (vector-set! data (+ j 1)
                        (- (vector-ref data (+ i 1)) tempi))
                      (vector-set! data i
                        (+ (vector-ref data i) tempr))
                      (vector-set! data (+ i 1)
                        (+ (vector-ref data (+ i 1)) tempi))
                      (loop5 (+ j mmax)));***))
                (loop4 (+ (- (* wr wpr) (* wi wpi)) wr)
                       (+ (+ (* wi wpr) (* wr wpi)) wi)
                       (+ m 2))))
                (loop5 m))
              (void)))
          (loop4 1.0 0.0 0)
          (loop3 (* mmax 2)))
        (void)))
    (loop3 2)))

(define data
  (make-vector (* 4096 4) 0.0))

(define (run data)
  (four1 data)
  (vector-ref data 0))

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
               (run data)
               (do-tests (- n 1) (cons (- (time) s) times))))))

(do-tests samples '())
