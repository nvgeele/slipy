(define write display)

(define trace? #f)

(define (nqueens n)

  (define (_1-to n)
    (define (loop i l)
      (if (= i 0)
          l
          (loop (- i 1) (cons i l))))
    (loop n '()))

  (define (ok? row dist placed)
    (if (null? placed)
        #t
        (if (not (= (car placed) (+ row dist)))
            (if (not (= (car placed) (- row dist)))
                (if (ok? row (+ dist 1) (cdr placed))
                    #t
                    #f)
                #f)
            #f)))

  (define (my-try x y z)
    (if (null? x)
        (if (null? y)
            (begin
              (if trace?
                  (begin (write z) (newline))
                  (void)) 1)
            0)
        (+ (if (ok? (car x) 1 z)
               (my-try (append (cdr x) y) '() (cons (car x) z))
               0)
           (my-try (cdr x) (cons (car x) y) z))))

  (my-try (_1-to n) '() '()))

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
        (nqueens 10)
        (let ((t (- (time) s)))
          (display t)(newline)
          (do-tests (- n 1) (cons t times))))))

(do-tests samples '())
