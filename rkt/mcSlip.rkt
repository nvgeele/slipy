(begin
  (define (loop output environment)
    (define rollback environment)

    (define (evaluate expression)

      (define (abort message qualifier)
        (display message)
        (loop qualifier rollback))

      (define (bind-variable variable value)
        (define binding (cons variable value))
        (set! environment (cons binding environment)))

      (define (bind-parameters parameters arguments)
        (if (symbol? parameters)
          (bind-variable parameters arguments)
          (if (pair? parameters)
            (begin
              (define variable (car parameters))
              (define value (car arguments))
              (bind-variable variable value)
              (bind-parameters (cdr parameters) (cdr arguments))))))

      (define (thunkify expression)
        (define frozen-environment environment)
        (define value (evaluate expression))
        (set! environment frozen-environment)
        value)

      (define (evaluate-sequence expressions)
        (define head (car expressions))
        (define tail (cdr expressions))
        (define value (evaluate head))
        (if (null? tail)
          value
          (evaluate-sequence tail)))

      (define (close parameters expressions)
        (define lexical-environment environment)
        (define (closure . arguments)
          (define dynamic-environment environment)
          (set! environment lexical-environment)
          (bind-parameters parameters arguments)
          (define value (evaluate-sequence expressions))
          (set! environment dynamic-environment)
          value)
        closure)

      (define (evaluate-application operator)
        (lambda operands
          (apply (evaluate operator) (map evaluate operands))))

      (define (evaluate-begin . expressions)
        (evaluate-sequence expressions))

      (define (evaluate-define pattern . expressions)
        (if (symbol? pattern)
          (begin
            (define value (evaluate (car expressions)))
            (define binding (cons pattern value))
            (set! environment (cons binding environment))
            value)
          (begin
            (define binding (cons (car pattern) ()))
            (set! environment (cons binding environment))
            (define closure (close (cdr pattern) expressions))
            (set-cdr! binding closure)
            closure)))

      (define (evaluate-eval expression)
        (evaluate (evaluate expression)))

      (define (evaluate-if predicate consequent . alternate)
        (if (evaluate predicate)
          (thunkify consequent)
          (if (null? alternate)
            ()
            (thunkify (car alternate)))))

      (define (evaluate-lambda parameters . expressions)
        (close parameters expressions))

      (define (evaluate-load string)
        (evaluate (evaluate (read (open-input-file string)))))

      (define (evaluate-quote expression)
        expression)

      (define (evaluate-set! variable expression)
        (define value (evaluate expression))
        (define binding (assoc variable environment))
        (if (pair? binding)
          (set-cdr! binding value)
          (abort "inaccessible variable: " variable)))

      (define (evaluate-variable variable)
        (define binding (assoc variable environment))
        (if (pair? binding)
          (cdr binding)
          (eval variable)))

      (if (symbol? expression)
        (evaluate-variable expression)
        (if (pair? expression)
          (begin
            (define operator (car expression))
            (define operands (cdr expression))
            (apply
              (if (equal? operator 'begin) evaluate-begin
                (if (equal? operator 'define) evaluate-define
                  (if (equal? operator 'eval) evaluate-eval
                    (if (equal? operator 'if) evaluate-if
                      (if (equal? operator 'lambda) evaluate-lambda
                        (if (equal? operator 'load) evaluate-load
                          (if (equal? operator 'quote) evaluate-quote
                            (if (equal? operator 'set!) evaluate-set!
                              (evaluate-application operator))))))))) operands))
          expression)))

    (display output)
    (newline)
    (display ">")
    (loop (evaluate (read)) environment))

  (loop "Meta-Circular Slip" ()))