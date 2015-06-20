;;; MBROT -- Generation of Mandelbrot set fractal.

(define (count r i step x y)
  (let ((max-count 64)
        (radius^2  16.0))
    (let ((cr (+ r (* x ;;(exact->inexact x)
                      step)))
          (ci (+ i (* y ;;(exact->inexact y)
                      step))))
      (define (loop zr zi c)
        (if (= c max-count)
            c
            (let ((zr^2 (* zr zr))
                  (zi^2 (* zi zi)))
              (if (> (+ zr^2 zi^2) radius^2)
                  c
                  (let ((new-zr (+ (- zr^2 zi^2) cr))
                        (new-zi (+ (* 2.0 (* zr zi)) ci)))
                    (loop new-zr new-zi (+ c 1)))))))
      (loop cr ci 0))))

(define (mbrot matrix r i step n)
  (define (loop1 y)
    (if (>= y 0)
        (begin
          (define (loop2 x)
            (if (>= x 0)
                (begin
                  (vector-set! (vector-ref matrix x) y (count r i step x y))
                  (loop2 (- x 1)))
                (loop1 (- y 1))))
          (loop2 (- n 1)))
        (void)))
  (loop1 (- n 1)))

(define (test n)
  (let ((matrix (make-vector n 0)))
    (define (loop i)
      (if (>= i 0)
          (begin
            (vector-set! matrix i (make-vector n 0))
            (loop (- i 1)))
          (void)))
    (loop (- n 1))
    (mbrot matrix -1.0 -0.5 0.005 n)
    (vector-ref (vector-ref matrix 0) 0)))

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
               (test 75)
               (do-tests (- n 1) (cons (- (time) s) times))))))

(do-tests samples '())
