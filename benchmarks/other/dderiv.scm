;;; DDERIV -- Table-driven symbolic derivation.

;;; Returns the wrong answer for quotients.
;;; Fortunately these aren't used in the benchmark.

(define (cadr p)
  (car (cdr p)))

(define (caddr p)
  (car (cdr (cdr p))))

(define (lookup key table)
  (define (loop x)
    (if (null? x)
        #f
        (let ((pair (car x)))
          (if (eq? (car pair) key)
              pair
              (loop (cdr x))))))
  (loop table))

(define properties '())

(define (get key1 key2)
  (let ((x (lookup key1 properties)))
    (if x
      (let ((y (lookup key2 (cdr x))))
        (if y
          (cdr y)
          #f))
      #f)))

(define (put key1 key2 val)
  (let ((x (lookup key1 properties)))
    (if x
      (let ((y (lookup key2 (cdr x))))
        (if y
          (set-cdr! y val)
          (set-cdr! x (cons (cons key2 val) (cdr x)))))
      (set! properties
        (cons (list key1 (cons key2 val)) properties)))))

(define (my+dderiv a)
  (cons '+
        (map dderiv (cdr a))))

(define (my-dderiv a)
  (cons '-
        (map dderiv (cdr a))))

(define (*dderiv a)
  (list '*
         a
         (cons '+
               (map (lambda (a) (list '/ (dderiv a) a)) (cdr a)))))

(define (/dderiv a)
  (list '-
        (list '/
              (dderiv (cadr a))
              (caddr a))
        (list '/
              (cadr a)
              (list '*
                    (caddr a)
                    (caddr a)
                    (dderiv (caddr a))))))

(put '+ 'dderiv my+dderiv)
(put '- 'dderiv my-dderiv)
(put '* 'dderiv *dderiv)
(put '/ 'dderiv /dderiv)

(define (dderiv a)
  (if (not (pair? a))
    (if (eq? a 'x) 1 0)
    (let ((f (get (car a) 'dderiv)))
      (if f
        (f a)
        (error "No derivation method available")))))

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
               (dderiv '(* (+ (* 3 x x) (* a x x) (* b x) 5)
                           (+ (* 3 x x) (* a x x) (* b x) 5)))
               (do-tests (- n 1) (cons (- (time) s) times))))))

(do-tests samples '())
