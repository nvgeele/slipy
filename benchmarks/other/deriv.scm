;;; DERIV -- Symbolic derivation.

;;; Returns the wrong answer for quotients.
;;; Fortunately these aren't used in the benchmark.

(define (cadr p)
  (car (cdr p)))

(define (caddr p)
  (car (cdr (cdr p))))

(define (deriv a)
  (if (not (pair? a))
      (if (eq? a 'x) 1 0)
      (if (eq? (car a) '+)
          (cons '+ (map deriv (cdr a)))
          (if (eq? (car a) '-)
              (cons '- (map deriv (cdr a)))
              (if (eq? (car a) '*)
                  (list '*
                        a
                        (cons '+
                              (map (lambda (a) (list '/ (deriv a) a)) (cdr a))))
                  (if (eq? (car a) '*)
                      (list '*
                            a
                            (cons '+
                                  (map (lambda (a) (list '/ (deriv a) a)) (cdr a))))
                      (if (eq? (car a) '/)
                          (list '-
                                (list '/
                                      (deriv (cadr a))
                                      (caddr a))
                                (list '/
                                      (cadr a)
                                      (list '*
                                            (caddr a)
                                            (caddr a)
                                            (deriv (caddr a)))))
                          (error "No derivation method available"))))))))

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
               (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
               (do-tests (- n 1) (cons (- (time) s) times))))))

(do-tests samples '())
