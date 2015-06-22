;; Changes made:
;; - zero? to = 0
;; - do loops transformed to functions
(define (my-iota n)
  (begin
    (define (g38 n list)
      (if (= n 0)
          list
          (begin
            (g38 (- n 1) (cons (- n 1) list)))))
    (g38 n '())))

(define size 511)
(define classmax 3)
(define typemax 12)

(define *iii* 0)
(define *kount* 0)
(define *d* 8)

(define *piececount* (make-vector (+ classmax 1) 0))
(define *class* (make-vector (+ typemax 1) 0))
(define *piecemax* (make-vector (+ typemax 1) 0))
(define *puzzle* (make-vector (+ size 1) (void)))
(define *p* (make-vector (+ typemax 1) (void)))


(define (fit i j)
  (let ((end (vector-ref *piecemax* i)))
    (begin
      (define (g39 k)
        (if (if (> k end) #t
                (if (vector-ref (vector-ref *p* i) k)
                    (if (vector-ref *puzzle* (+ j k)) #t #f) #f))
            (if (> k end) #t #f)
            (begin (g39 (+ k 1)))))
      (g39 0))))

(define (place i j)
  (let ((end (vector-ref *piecemax* i)))
    (begin
      (define (g40 k)
        (if (> k end)
            (void)
            (begin
              (if (vector-ref (vector-ref *p* i) k)
                  (begin (vector-set! *puzzle* (+ j k) #t) #t)
                  (void))
              (g40 (+ k 1)))))
      (g40 0))


    (vector-set! *piececount*
                 (vector-ref *class* i)
                 (- (vector-ref *piececount* (vector-ref *class* i)) 1))
    (begin
      (define (g41 k)
        (if (if (> k size) #t (not (vector-ref *puzzle* k)))
            (if (> k size) 0 k)
            (begin (g41 (+ k 1)))))
      (g41 j))))

(define (puzzle-remove i j)
  (let ((end (vector-ref *piecemax* i)))
    (begin
      (define (g42 k)
        (if (> k end)
            (void)
            (begin
              (if (vector-ref (vector-ref *p* i) k)
                  (begin (vector-set! *puzzle* (+ j k) #f) #f)
                  (void))
              (g42 (+ k 1)))))
      (g42 0))

    (vector-set! *piececount*
                 (vector-ref *class* i)
                 (+ (vector-ref *piececount* (vector-ref *class* i)) 1))))

(define (trial j)
  (let ((k 0))
    (call-with-current-continuation
     (lambda (return)
       (begin
         (define (g43 i)
           (if (> i typemax)
               (begin (set! *kount* (+ *kount* 1)) #f)
               (begin
                 (if (not (= 0 (vector-ref *piececount* (vector-ref *class* i))))
                     (begin
                       (if (fit i j)
                           (begin
                             (set! k (place i j))
                             (if (if (trial k) #t (= 0 k))
                                 (begin (set! *kount* (+ *kount* 1)) (return #t))
                                 (puzzle-remove i j)))
                           (void)))
                     (void))
                 (g43 (+ i 1)))))
         (g43 0))))))

(define (definePiece iclass ii jj kk)
  (let ((index 0))
    (begin
      (define (g44 i)
        (if (> i ii)
            (void)
            (begin
              (begin
                (define (g45 j)
                  (if (> j jj)
                      (void)
                      (begin
                        (begin
                          (define (g46 k)
                            (if (> k kk)
                                (void)
                                (begin
                                  (set! index (+ i (* *d* (+ j (* *d* k)))))
                                  (vector-set! (vector-ref *p* *iii*) index #t)
                                  (g46 (+ k 1)))))
                          (g46 0))
                        (g45 (+ j 1)))))
                (g45 0))
              (g44 (+ i 1)))))
      (g44 0))
    (vector-set! *class* *iii* iclass)
    (vector-set! *piecemax* *iii* index)
    (if (not (= *iii* typemax))
        (set! *iii* (+ *iii* 1))
        (void))))

(define (start)
  (set! *kount* 0)

  (begin
    (define (g47 m)
      (if (> m size)
          (void)
          (begin
            (vector-set! *puzzle* m #t)
            (g47 (+ m 1)))))
    (g47 0))

  (begin
    (define (g48 i)
      (if (> i 5)
          (void)
          (begin
            (begin
              (define (g49 j)
                (if (> j 5)
                    (void)
                    (begin
                      (begin
                        (define (g50 k)
                          (if (> k 5)
                              (void)
                              (begin
                                (vector-set!
                                 *puzzle*
                                 (+ i (* *d* (+ j (* *d* k))))
                                 #f)
                                (g50 (+ k 1)))))
                        (g50 1))
                      (g49 (+ j 1)))))
              (g49 1))
            (g48 (+ i 1)))))
    (g48 1))


  (begin
    (define (g51 i)
      (if (> i typemax)
          (void)
          (begin
            (begin
              (define (g52 m)
                (if (> m size)
                    (void)
                    (begin
                      (vector-set! (vector-ref *p* i) m #f)
                      (g52 (+ m 1)))))
              (g52 0))
            (g51 (+ i 1)))))
    (g51 0))

  (set! *iii* 0)
  (definePiece 0 3 1 0)
  (definePiece 0 1 0 3)
  (definePiece 0 0 3 1)
  (definePiece 0 1 3 0)
  (definePiece 0 3 0 1)
  (definePiece 0 0 1 3)

  (definePiece 1 2 0 0)
  (definePiece 1 0 2 0)
  (definePiece 1 0 0 2)

  (definePiece 2 1 1 0)
  (definePiece 2 1 0 1)
  (definePiece 2 0 1 1)

  (definePiece 3 1 1 1)

  (vector-set! *piececount* 0 13)
  (vector-set! *piececount* 1 3)
  (vector-set! *piececount* 2 1)
  (vector-set! *piececount* 3 1)
  (let ((m (+ (* *d* (+ *d* 1)) 1))
        (n 0))
    (if (fit 0 m)
        (set! n (place 0 m))
        (begin (newline) (display "Error.")))
    (if (trial n)
        *kount*
        #f)))

(define (fe l)
  (if (null? l)
      (void)
      (begin
        (vector-set! *p* (car l) (make-vector (+ size 1) (void)))
        (fe (cdr l)))))
(fe (my-iota (+ typemax 1)))

(define (run iters)
  (if (= iters 0)
      'done
      (begin
        (displayln "iter")
        (displayln (start))
        (run (- iters 1)))))

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
        (start)
        (let ((t (- (time) s)))
          (display t)(newline)
          (do-tests (- n 1) (cons t times))))))

(do-tests samples '())
