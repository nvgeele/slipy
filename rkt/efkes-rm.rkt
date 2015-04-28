#lang r5rs

;;<expression>  ::= <computation>|<lambda>|<quote>|<variable>|<literal>
;; <computation> ::= <definition>|<assignment>|<sequence>
;;                   <conditional>|<application>|<load>|<eval>
;; <definition>  ::= (define <variable> <expression>)
;; <definition>  ::= (define <pattern> <expression>+)
;; <assignment>  ::= (set! <variable> <expression>)
;; <sequence>    ::= (begin <expression>+)
;; <conditional> ::= (if <expression> <expression> <expression>)
;; <conditional> ::= (if <expression> <expression>)
;; <application> ::= (<expression>+)
;; <eval>        ::= (eval expression)
;; <load>        ::= (load expression)
;; <lambda>      ::= (lambda () <expression>+)
;; <lambda>      ::= (lambda <variable> <expression>+)
;; <lambda>      ::= (lambda (<pattern>) <expression>+)
;; <quote>       ::= (quote expression)
;; <pattern>     ::= (<variable>+)
;; <pattern>     ::= (<variable>+ . <variable>)
;; <variable>    ::= [symbol]
;; <literal>     ::= [number] | [character] | [string] | #t | #f | ()

(begin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define ADR '())     ; address
  (define ARG '())     ; argument list
  (define BND '())     ; binding
  (define ENV '())     ; environment
  (define EXP '())     ; expression
  (define FRM '())     ; frame
  (define GLB '())     ; global frame
  (define IDX '())     ; index
  (define KON '())     ; continuation
  (define LEN '())     ; length
  (define LST '())     ; list
  (define PAR '())     ; parameter list
  (define PAT '())     ; pattern
  (define PRC '())     ; procedure
  (define SEQ '())     ; expression sequence
  (define STK '())     ; stack
  (define STO '())     ; store
  (define TOP '())     ; next available address
  (define VAL '())     ; value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  stacks [ these should be macros ]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; saving
;
  (define (save register)
    (set! STK (cons register STK)))
;
; restoring
;
  (define (restore)
    (define register (car STK))
    (set! STK (cdr STK))
    register)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  error [ this should be a macro ]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; generate error and escape to REP
;
  (define (error message register)
    (display message)
    (set! VAL register)
    (set! ENV '())
    (set! FRM GLB)
    (kontinue/REP))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  clones
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; cloning of pair
;   requires: EXP, KON
;    assigns: EXP, KON
;    invokes: car
;     stacks: EXP, KON
;   unstacks: none
;  transfers: clone
;
  (define (clone-pair)
    (save KON)
    (save EXP)
    (set! EXP (car EXP))
    (set! KON kontinue/pair)
    (clone))
;
; continuation for clone-pair
;
;   requires: VAL
;    assigns: EXP, KON
;    invokes: cdr
;     stacks: VAL
;   unstacks: EXP
;  transfers: clone
;
  (define (kontinue/pair)
    (set! EXP (restore))
    (save VAL)
    (set! EXP (cdr EXP))
    (set! KON kontinue/pair-1)
    (clone))
;
; continuation for clone-pair-1
;
;   requires: VAL
;    assigns: VAL
;    invokes: cons
;     stacks: none
;   unstacks: EXP, KON
;  transfers: KON
;
  (define (kontinue/pair-1)
    (set! EXP (restore))
    (set! KON (restore))
    (set! VAL (cons EXP VAL))
    (KON))                                             ; continue
;
; cloning of string
;   requires: EXP, KON
;    assigns: VAL
;    invokes: string-append
;     stacks: none
;   unstacks: none
;  transfers: KON
;
  (define (clone-string)
    (set! VAL (string-copy EXP))
    (KON))                                             ; continue
;
; cloning of vector
;   requires: EXP, KON
;    invokes: make-vector, vector-length, vector-ref, zero?
;    assigns: EXP, IDX, KON, LEN, VAL
;     stacks: EXP, IDX, KON, VAL
;   unstacks: none
;  transfers: clone, KON
;
  (define (clone-vector)
    (set! LEN (vector-length EXP))
    (cond
      ((zero? LEN)
        (set! VAL #())
        (KON)))                                        ; continue
    (set! VAL (make-vector LEN '()))
    (set! IDX 0)
    (save KON)
    (save EXP)
    (save VAL)
    (save IDX)
    (set! EXP (vector-ref EXP 0))
    (set! KON kontinue/vector)
    (clone))
;
; continuation for clone-vector
;
;   requires: VAL
;    assigns: EXP, IDX, LEN, VAL
;    invokes: vector-length, vector-ref, vector-set!
;     stacks: EXP, IDX, VAL
;   unstacks: EXP, IDX, KON
;  transfers: kontinue/vector, KON
;
  (define (kontinue/vector)
    (set! IDX (restore))
    (set! EXP (restore))
    (vector-set! EXP IDX VAL)
    (set! VAL EXP)
    (set! EXP (restore))
    (set! LEN (vector-length EXP))
    (set! IDX (+ IDX 1))
    (cond
      ((= IDX LEN)
        (set! KON (restore))
        (KON)))                                        ; continue
    (save EXP)
    (save VAL)
    (save IDX)
    (set! EXP (vector-ref EXP IDX))
    (clone))
;
; cloning
;   requires: EXP, KON
;    assigns: VAL
;    invokes: none
;     stacks: none
;   unstacks: none
;  transfers: clone-pair, clone-vector, clone-string, KON
;
  (define (clone)
    (cond
      ((pair? EXP)
        (clone-pair))
      ((string? EXP)
        (clone-string))
      ((vector? EXP)
        (clone-vector)))
    (set! VAL EXP)
    (KON))                                             ; continue

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  natives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define natives
    (list (cons '- -)
          (cons '* *)
          (cons '/ /)
          (cons '+ +)
          (cons '< <)
          (cons '<= <=)
          (cons '= =)
          (cons '> >)
          (cons '>= >=)
          (cons 'abs abs)
          (cons 'acos acos)
          (cons 'append append)
          (cons 'asin asin)
          (cons 'assoc assoc)
          (cons 'atan atan)
          (cons 'boolean? boolean?)
          (cons 'caaaar caaaar)
          (cons 'caaadr caaadr)
          (cons 'caaar caaar)
          (cons 'caadar caadar)
          (cons 'caaddr caaddr)
          (cons 'caadr caadr)
          (cons 'caar caar)
          (cons 'cadaar cadaar)
          (cons 'cadadr cadadr)
          (cons 'cadar cadar)
          (cons 'caddar caddar)
          (cons 'cadddr cadddr)
          (cons 'caddr caddr)
          (cons 'cadr cadr)
          (cons 'car car)
          (cons 'cdaaar cdaaar)
          (cons 'cdaadr cdaadr)
          (cons 'cdaar cdaar)
          (cons 'cdadar cdadar)
          (cons 'cdaddr cdaddr)
          (cons 'cdadr cdadr)
          (cons 'cdar cdar)
          (cons 'cddaar cddaar)
          (cons 'cddadr cddadr)
          (cons 'cddar cddar)
          (cons 'cdddar cdddar)
          (cons 'cddddr cddddr)
          (cons 'cdddr cdddr)
          (cons 'cddr cddr)
          (cons 'cdr cdr)
          (cons 'char->integer char->integer)
          (cons 'char? char?)
          (cons 'char<? char<?)
          (cons 'char<=? char<=?)
          (cons 'char=? char=?)
          (cons 'char>? char>?)
          (cons 'char>=? char>=?)
          (cons 'close-input-port close-input-port)
          (cons 'cons cons)
          (cons 'cos cos)
          (cons 'display display  )
          (cons 'eof-object? eof-object?)
          (cons 'equal? equal?)
          (cons 'exp exp)
          (cons 'expt expt)
          (cons 'input-port? input-port?)
          (cons 'integer->char integer->char)
          (cons 'integer? integer?)
          (cons 'length length)
          (cons 'list list)
          (cons 'list->vector list->vector)
          (cons 'log log)
          (cons 'make-string make-string)
          (cons 'make-vector make-vector)
          (cons 'member member)
          (cons 'negative? negative?)
          (cons 'newline newline  )
          (cons 'not not)
          (cons 'null? null?)
          (cons 'number->string number->string)
          (cons 'number? number?)
          (cons 'open-input-file open-input-file)
          (cons 'output-port? output-port?)
          (cons 'pair? pair?)
          (cons 'procedure? procedure?)
          (cons 'quotient quotient)
          (cons 'read read)
          (cons 'read-char read-char)
          (cons 'real? real?)
          (cons 'remainder remainder)
          (cons 'reverse reverse)
          (cons 'set-car! set-car!)
          (cons 'set-cdr! set-cdr!)
          (cons 'sin sin)
          (cons 'sqrt sqrt)
          (cons 'string->number string->number)
          (cons 'string->symbol string->symbol)
          (cons 'string-append string-append)
          (cons 'string-copy string-copy)
          (cons 'string-length string-length)
          (cons 'string-ref string-ref)
          (cons 'string-set! string-set!)
          (cons 'string? string?)
          (cons 'string<? string<?)
          (cons 'string<=? string<=?)
          (cons 'string=? string=?)
          (cons 'string>? string>?)
          (cons 'string>=? string>=?)
          (cons 'substring substring)
          (cons 'symbol->string symbol->string)
          (cons 'symbol? symbol?)
          (cons 'tan tan)
          (cons 'vector vector)
          (cons 'vector->list vector->list)
          (cons 'vector-length vector-length)
          (cons 'vector-ref vector-ref)
          (cons 'vector-set! vector-set!)
          (cons 'vector? vector?)
          (cons 'zero? zero?)))
;
; initialize natives
;
;   requires: ENV, LST, STO, TOP
;    assigns: BND, ENV, LST, PRC, STO, TOP, VAL
;    invokes: make-native, car, cdr, cons
;     stacks: none
;   unstacks: none
;  transfers: initialize-natives
;
  (define (initialize-natives)
    (cond
      ((null? LST)                                     ; list exhausted
        '())
      (else
        (set! BND (car LST))
        (set! LST (cdr LST))
        (set! PAT (car BND))                           ; extract native name
        (set! PRC (cdr BND))                           ; extract native value
        (set! VAL (make-native PRC))                   ; build procedure
        (set! BND (cons PAT TOP))
        (set! FRM (cons BND FRM))                      ; extend frame
        (set! BND (cons TOP VAL))                      ; extend store
        (set! STO (cons BND STO))
        (set! TOP (+ TOP 1))                           ; fresh address
        (initialize-natives))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  higher-order natives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; apply higher-order native
;
;   requires: ARG
;    assigns: ARG, PRC
;    invokes: car, cadr
;     stacks: none
;   unstacks: none
;  transfers: applicate
;
  (define (apply-higher-order-native)
    (set! PRC (car ARG))
    (set! ARG (cadr ARG))
    (applicate))
;
; map higher-order native
;
;   requires: ARG, KON, PRC
;    assigns: ARG, KON, LST, PRC, VAL
;    invokes: car, cdr, list
;     stacks: KON, LST, PRC
;   unstacks: none
;  transfers: applicate, KON
;
  (define (map-higher-order-native)
    (set! LST (cadr ARG))
    (cond
      ((null? LST)
        (set! VAL '())
        (KON)))                                        ; continue
    (save KON)
    (set! PRC (car ARG))
    (save PRC)
    (set! ARG (list (car LST)))
    (set! LST (cdr LST))
    (save LST)
    (set! LST '())
    (save LST)
    (set! KON kontinue/map)
    (applicate))
;
; continuation for map
;
;   requires: VAL
;    assigns: ARG, KON, LST, PRC, VAL
;    invokes: car, cdr, list, reverse
;     stacks: LST, PRC, VAL
;   unstacks: KON, LST, PRC, VAL
;  transfers: applicate, KON
;
  (define (kontinue/map)
    (set! LST (restore))
    (set! VAL (cons VAL LST))
    (set! LST (restore))
    (set! PRC (restore))
    (cond
      ((null? LST)
       (set! KON (restore))
       (set! VAL (reverse VAL))
       (KON)))                                         ; continue
    (save PRC)
    (set! ARG (list (car LST)))
    (set! LST (cdr LST))
    (save LST)
    (save VAL)
    (applicate))
;
; for-each higher-order native
;
;   requires: ARG, KON
;    assigns: KON, LST, PRC
;    invokes: none
;     stacks: KON, LST, PRC
;   unstacks: none
;  transfers: KON
;
  (define (for-each-higher-order-native)
    (save KON)
    (set! PRC (car ARG))
    (save PRC)
    (set! LST (cadr ARG))
    (save LST)
    (set! KON kontinue/for-each)
    (KON))                                             ; continue
;
; continuation for for-each
;
;   requires: none
;    assigns: ARG, KON, LST, VAL
;    invokes: car, cdr, list
;     stacks: LST, PRC
;   unstacks: KON, LST, PRC
;  transfers: applicate, KON
;
  (define (kontinue/for-each)
    (set! LST (restore))
    (set! PRC (restore))
    (cond
      ((null? LST)
        (set! KON (restore))
        (set! VAL '())
        (KON)))                                        ; continue
    (set! ARG (list (car LST)))
    (set! LST (cdr LST))
    (save PRC)
    (save LST)
    (applicate))
;
; call/cc higher-order native
;
;   requires: ARG, ENV, FRM, KON, STK
;    assigns: ARG, PRC
;    invokes: make-continuation, car, list
;     stacks: none
;   unstacks: none
;  transfers: applicate
;
  (define (call/cc-higher-order-native)
    (set! PRC (car ARG))
    (set! ARG (list (make-continuation KON ENV FRM STK)))
    (applicate))

  (define higher-order-natives
    (list (cons 'apply apply-higher-order-native)
          (cons 'map map-higher-order-native)
          (cons 'for-each for-each-higher-order-native)
          (cons 'call-with-current-continuation call/cc-higher-order-native)))

;
; initialize higher-order natives
;
;
; initialize natives
;
;   requires: ENV, LST, STO, TOP
;    assigns: BND, ENV, LST, PRC, STO, TOP, VAL
;    invokes: make-higher-order-native, car, cdr, cons
;     stacks: none
;   unstacks: none
;  transfers: initialize-higher-order-natives
;
  (define (initialize-higher-order-natives)
    (cond
      ((null? LST)                                     ; list exhausted
        '())
      (else
        (set! BND (car LST))
        (set! LST (cdr LST))
        (set! PAT (car BND))                           ; extract native name
        (set! PRC (cdr BND))                           ; extract native value
        (set! VAL (make-higher-order-native PRC))      ; build procedure
        (set! BND (cons PAT TOP))
        (set! FRM (cons BND FRM))                      ; extend frame
        (set! BND (cons TOP VAL))                      ; extend store
        (set! STO (cons BND STO))
        (set! TOP (+ TOP 1))                           ; fresh address
        (initialize-higher-order-natives))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; evaluator for sequence
;
;   requires: KON, SEQ
;    assigns: EXP, KON, SEQ
;    invokes: car, cdr, null?
;     stacks: KON, SEQ
;   unstacks: none
;  transfers: evaluate
;
  (define (sequence)
    (set! EXP (car SEQ))
    (set! SEQ (cdr SEQ))
    (cond
      ((null? SEQ)                                     ; single expression
        (evaluate)))                                   ; evaluate single expression
    (save KON)
    (save SEQ)
    (set! KON kontinue/sequence)
    (evaluate))                                        ; evaluate first expression
;
; continuation for sequence
;
;   requires: none
;    assigns: EXP, SEQ
;    invokes: car, cdr, null?
;     stacks: SEQ
;   unstacks: KON, SEQ
;  transfers: evaluate, sequence
;
  (define (kontinue/sequence)
    (set! SEQ (restore))
    (set! EXP (car SEQ))
    (set! SEQ (cdr SEQ))
    (cond
      ((null? SEQ)                                     ; last expression
        (set! KON (restore))
        (evaluate)))                                   ; evaluate last expression
    (save SEQ)
    (evaluate))                                        ; evaluate next expression

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; make procedure
;
;   requires: none
;    assigns: none
;    invokes: none
;     stacks: none
;   unstacks: none
;  transfers: none
;
  (define (make-procedure par seq env)
    (define (procedure)
      (save KON)
      (save ENV)
      (save FRM)
      (set! PAR par)
      (set! SEQ seq)
      (set! ENV env)
      (set! FRM '())
      (bind))
    procedure)
;
; parameter binding
;
;   requires: ARG, FRM, PAR, SEQ, STO, TOP
;    assigns: ARG, BND, EXP, FRM, KON, PAR, PAT, STO, TOP, VAL
;    invokes: cons, car, cdr, null?, pair?
;     stacks: none
;   unstacks: none
;  transfers: bind, error, sequence
;
  (define (bind)
    (cond
      ((null? PAR)
        (cond
          ((null? ARG)
            (set! KON kontinue/bind)
            (sequence)))
        (error "excess arguments: " ARG))
      ((pair? PAR)
        (cond
          ((null? ARG)
            (error "excess parameters: " PAR)))
        (set! PAT (car PAR))
        (cond
          ((symbol? PAT)
            (set! BND (assoc PAT FRM))                  ; look up variable in local frame
            (cond
              (BND                                      ; variable found in local frame
                (error "duplicate parameter: " PAT)))
            (set! BND (cons PAT TOP))
            (set! FRM (cons BND FRM))
            (set! VAL (car ARG))
            (set! BND (cons TOP VAL))
            (set! STO (cons BND STO))
            (set! PAR (cdr PAR))
            (set! ARG (cdr ARG))
            (set! TOP (+ TOP 1))                       ; fresh address
            (bind)))
        (error "parameter must be a symbol: " PAT))
      ((symbol? PAR)
        (set! BND (assoc PAR FRM))                     ; look up variable in local frame
        (cond
          (BND                                         ; variable found in local frame
            (error "duplicate parameter: " PAR)))
        (set! BND (cons PAR TOP))
        (set! FRM (cons BND FRM))
        (set! BND (cons TOP ARG))
        (set! STO (cons BND STO))
        (set! TOP (+ TOP 1))                           ; fresh address
        (set! KON kontinue/bind)
        (sequence)))
    (error "parameter(s) required: " PAR))
;
; continuation for bind
;
;   requires: none
;    assigns: none
;    invokes: none
;     stacks: none
;   unstacks: ENV, FRM, KON
;  transfers: KON
;
  (define (kontinue/bind)
    (set! FRM (restore))
    (set! ENV (restore))
    (set! KON (restore))
    (KON))                                             ; continue
;
; make native
;
;   requires: none
;    assigns: none
;    invokes: none
;     stacks: none
;   unstacks: none
;  transfers: none
;
  (define (make-native prc)
    (define (native)
      (set! VAL (apply prc ARG))
      (KON))                                           ; continue
    native)
;
; make higher-order-native procedure
;
;   requires: none
;    assigns: none
;    invokes: none
;     stacks: none
;   unstacks: none
;  transfers: none
;
  (define (make-higher-order-native prc)
    prc)
;
; make continuation
;
;   requires: none
;    assigns: none
;    invokes: none
;     stacks: none
;   unstacks: none
;  transfers: none
;
  (define (make-continuation kon env frm stk)
    (define (continuation)
      (set! VAL (car ARG))
      (set! KON kon)
      (set! ENV env)
      (set! FRM frm)
      (set! STK stk)
      (KON))                                           ; continue
    continuation)
;
; applicate
;
;   requires: PRC
;    assigns: none
;    invokes: procedure?
;     stacks: none
;   unstacks: none
;  transfers: KON
;
  (define (applicate)
    (cond
      ((procedure? PRC)
        (PRC)))
    (error "procedure expected: " PRC))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; evaluator for application
;
;   requires: KON, LST
;    assigns: KON
;    invokes: none
;     stacks: KON, LST
;   unstacks: none
;  transfers: evaluate
;
  (define (evaluate-application)
    (save KON)
    (save LST)
    (set! KON kontinue/application)
    (evaluate))                                        ; evaluate operator expression
;
; continuation for application
;
;   requires: VAL
;    assigns: ARG, EXP, KON, LST, PRC
;    invokes: car, cdr, null?
;     stacks: ARG, LST, VAL
;   unstacks: KON, LST
;  transfers: applicate, evaluate
;
  (define (kontinue/application)
    (set! LST (restore))
    (set! ARG '())                                     ; initialize argument list
    (cond
      ((null? LST)                                     ; no operands
        (set! PRC VAL)
        (set! KON (restore))
        (applicate)))                                  ; apply procedure
    (save VAL)
    (save ARG)
    (set! EXP (car LST))                               ; extract first operand
    (set! LST (cdr LST))                               ; remove first operand from list
    (cond
      ((null? LST)                                     ; no operands
        (set! KON kontinue/application-2)
        (evaluate)))                                   ; evaluate last operand
    (save LST)
    (set! KON kontinue/application-1)
    (evaluate))                                        ; evaluate first operand
;
; continuation for application-1
;
;   requires: VAL
;    assigns: ARG, EXP, KON, LST
;    invokes: car, cdr, cons, null?
;     stacks: ARG, LST
;   unstacks: ARG, LST, KON, PRC
;  transfers: evaluate
;
  (define (kontinue/application-1)
    (set! LST (restore))
    (set! ARG (restore))
    (set! ARG (cons VAL ARG))
    (save ARG)
    (set! EXP (car LST))                               ; extract next operand
    (set! LST (cdr LST))                               ; remove next operand from list
    (cond                                              ; operand list exhausted
      ((null? LST)
        (set! KON kontinue/application-2)
        (evaluate)))                                   ; evaluate last operand
    (save LST)
    (set! KON kontinue/application-1)
    (evaluate))                                        ; evaluate next operand
;
; continuation for application-2
;
;   requires: VAL
;    assigns: ARG
;    invokes: cons, reverse
;     stacks: none
;   unstacks: ARG, KON, PRC
;  transfers: applicate
;
  (define (kontinue/application-2)
    (set! ARG (restore))
    (set! ARG (cons VAL ARG))
    (set! ARG (reverse ARG))                           ; reverse argument list
    (set! PRC (restore))
    (set! KON (restore))
    (applicate))                                       ; apply procedure

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; evaluator for begin
;
;   requires: LST
;    assigns: SEQ
;    invokes: none
;     stacks: none
;   unstacks: none
;  transfers: error, sequence
;
  (define (evaluate-begin)
    (cond
      ((>= LEN 1)
        (set! SEQ LST)
        (sequence)))
    (error "begin requires at least 1 argument: " LST))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; evaluator for define
;
;   requires: ENV, FRM, KON, LEN, LST, STO, TOP
;    assigns: BND, ENV, FRM, EXP, KON, LST, PAR, PAT, PRC, SEQ, STO, TOP, VAL
;    invokes: make-procedure, car, cadr, cdr, cons, list, pair?, symbol?, +
;     stacks: KON, PAT
;   unstacks: none
;  transfers: error, evaluate, KON
;
  (define (evaluate-define)
    (cond
      ((>= LEN 2)
        (set! PAT (car LST))                           ; extract pattern from list
        (cond
          ((symbol? PAT)                               ; pattern is a variable
            (cond
              ((= LEN 2)
                (save KON)
                (save PAT)
                (set! EXP (cadr LST))                  ; extract expression from list
                (set! KON kontinue/define)
                (evaluate)))                           ; evaluate variable initialization
            (error "define requires exactly 2 arguments: " LST))
          ((pair? PAT)                                 ; pattern is a variable
            (set! PAR (cdr PAT))                       ; extract parameters from pattern
            (set! PAT (car PAT))                       ; extract name from pattern
            (cond
              ((symbol? PAT)                           ; pattern is a variable
                (set! SEQ (cdr LST))                   ; extract procedure body from list
                (set! BND (assoc PAT FRM))             ; look up variable in local frame
                (cond
                  (BND                                 ; variable found in local frame
                    (set! ENV (cons FRM ENV))          ; grow environment
                    (set! VAL (make-procedure PAR SEQ ENV))
                    (set! ENV (cdr ENV))               ; shrink environment
                    (set! ADR (cdr BND))
                    (set! BND (cons ADR VAL))          ; extend store
                    (set! STO (cons BND STO))
                    (KON)))                            ; continue
                (set! BND (cons PAT TOP))              ; extend frame
                (set! FRM (cons BND FRM))
                (set! ENV (cons FRM ENV))              ; grow environment
                (set! VAL (make-procedure PAR SEQ ENV))
                (set! ENV (cdr ENV))                   ; shrink environment
                (set! BND (cons TOP VAL))              ; extend store
                (set! STO (cons BND STO))
                (set! TOP (+ TOP 1))                   ; fresh address
                (KON)))                                ; continue
            (error "function name required: " PAT)))
      (error "invalid define pattern: " PAT))
    (error "define requires at least 2 arguments: " LST)))
;
; continuation for define
;
;   requires: FRM, STO, TOP, VAL
;    assigns: BND, FRM, STO, TOP
;    invokes: cons
;     stacks: none
;   unstacks: KON, PAT
;  transfers: KON
;
  (define (kontinue/define)
    (set! PAT (restore))
    (set! KON (restore))
    (set! BND (assoc PAT FRM))                         ; look up variable in local frame
    (cond
      (BND                                             ; variable found in local frame
        (set! ADR (cdr BND))
        (set! BND (cons ADR VAL))                      ; extend store
        (set! STO (cons BND STO))
        (KON)))                                        ; continue
    (set! BND (cons PAT TOP))                          ; extend frame
    (set! FRM (cons BND FRM))
    (set! BND (cons TOP VAL))                          ; extend store
    (set! STO (cons BND STO))
    (set! TOP (+ TOP 1))                               ; fresh address
    (KON))                                             ; continue

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; evaluator for eval
;
;   requires: KON, LEN, LST
;    assigns: EXP, KON
;    invokes: car
;     stacks: KON
;   unstacks: none
;  transfers: error, evaluate
;
  (define (evaluate-eval)
    (cond
      ((= LEN 1)
        (set! EXP (car LST))                           ; extract expression from list
        (save KON)
        (set! KON kontinue/eval)
        (evaluate)))                                   ; evaluate expression
    (error "eval requires exactly 1 argument: " LST))
;
; continuation for eval
;
;   requires: VAL
;    assigns: EXP
;    invokes: none
;     stacks: none
;   unstacks: KON
;  transfers: evaluate
;
  (define (kontinue/eval)
    (set! EXP VAL)                                     ; view VAL as an expression
    (set! KON (restore))
    (evaluate))                                        ; evaluate expression

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; evaluator for if
;
;   requires: KON, LEN, LST
;    assigns: EXP, KON
;    invokes: car
;     stacks: KON, LST
;   unstacks: none
;  transfers: error, evaluate
;
  (define (evaluate-if)
    (cond
      ((= LEN 2)                                       ; single-branch if
        (set! EXP (car LST))                           ; extract predicate from list
        (save KON)
        (save LST)
        (set! KON kontinue/if)
        (evaluate))                                    ; evaluate predicate
      ((= LEN 3)                                       ; two-branch if
        (set! EXP (car LST))                           ; extract predicate from list
        (save KON)
        (save LST)
        (set! KON kontinue/if-1)
        (evaluate)))                                   ; evaluate predicate
    (error "if requires 2 or 3 arguments: " LST))
;
; continuation for if
;
;   requires: ENV, FRM, VAL
;    assigns: EXP, FRM, KON, VAL
;    invokes: cadr, cons
;     stacks: ENV, FRM
;   unstacks: KON, LST
;  transfers: evaluate, KON
;
  (define (kontinue/if)
    (set! LST (restore))
    (cond
      (VAL                                             ; predicate is true
        (save ENV)                                     ; enter nested scope
        (save FRM)
        (set! ENV (cons FRM ENV))                      ; grow environment
        (set! FRM '())                                 ; clear frame
        (set! EXP (cadr LST))                          ; extract consequent from list
        (set! KON kontinue/if-2)
        (evaluate)))                                   ; evaluate consequent expression
    (set! VAL '())                                     ; set value to ()
    (set! KON (restore))
    (KON))                                             ; continue
;
; continuation for if-1
;
;   requires: ENV, FRM, VAL
;    assigns: EXP, FRM, KON, LST, VAL
;    invokes: cadr, caddr, cons
;     stacks: ENV, FRM
;   unstacks: KON, LST
;  transfers: evaluate
;
  (define (kontinue/if-1)
    (set! LST (restore))
    (save ENV)                                         ; enter nested scope
    (save FRM)
    (set! ENV (cons FRM ENV))                          ; grow environment
    (set! FRM '())                                     ; clear frame
    (set! KON kontinue/if-2)
    (cond
      (VAL                                             ; predicate is true
        (set! EXP (cadr LST))                          ; extract consequent from list
        (evaluate)))                                   ; evaluate consequent expression
    (set! EXP (caddr LST))                             ; extract tail-end from list
    (evaluate))                                        ; evaluate alternative expression
;
; continuation for if-2
;
;   requires: none
;    assigns: none
;    invokes: none
;     stacks: none
;   unstacks: ENV, KON
;  transfers: KON
;
  (define (kontinue/if-2)
    (set! FRM (restore))                               ; exit nested scope
    (set! ENV (restore))
    (set! KON (restore))
    (KON))                                             ; continue

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; evaluator for lambda
;
;   requires: ENV, FRM, KON, LEN, LST
;    assigns: PAR, SEQ, VAL
;    invokes: make-procedure, car, cdr, list
;     stacks: none
;   unstacks: none
;  transfers: KON
;
  (define (evaluate-lambda)
    (cond
      ((>= LEN 2)
        (set! PAR (car LST))                           ; extract parameters from pattern
        (set! SEQ (cdr LST))                           ; extract procedure body from list
        (set! ENV (cons FRM ENV))                      ; grow environment
        (set! VAL (make-procedure PAR SEQ ENV))
        (set! ENV (cdr ENV))                           ; shrink environment
        (KON)))                                        ; continue
    (error "lambda requires at least 2 arguments: " LST))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; evaluator for load
;
;   requires: KON, LEN, LST
;    assigns: EXP, KON, VAL
;    invokes: car, open-input-file, close-input-port, read
;     stacks: KON
;   unstacks: none
;  transfers: evaluate
;
  (define (evaluate-load)
    (cond
      ((= LEN 1)
        (set! VAL (car LST))                           ; extract expression from list
        (set! VAL (open-input-file VAL))               ; open file
        (set! EXP (read VAL))                          ; parse content of file
        (close-input-port VAL)                         ; close file
        (save KON)
        (set! KON kontinue/load)
        (evaluate)))                                   ; evaluate expression
     (error "load requires exactly 1 argument: " LST))
;
; continuation for load
;
;   requires: VAL
;    assigns: EXP
;    invokes: none
;     stacks: none
;   unstacks: KON
;  transfers: evaluate
;
  (define (kontinue/load)
    (set! EXP VAL)                                     ; view value as an expression
    (set! KON (restore))
    (evaluate))                                        ; evaluate expression

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; evaluator for quote
;
;   requires: LEN, LST
;    assigns: EXP
;    invokes: car
;     stacks: none
;   unstacks: none
;  transfers: clone
;
  (define (evaluate-quote)
    (cond
      ((= LEN 1)
        (set! EXP (car LST))                           ; extract expression from pattern
        (clone)))
     (error "quote requires exactly 1 argument: " LST))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; evaluator for set!
;
;   requires: ENV, FRM, KON, LEN, LST
;    assigns: BND, EXP, KON, PAT
;    invokes: assoc, car, cadr, cdr, null?
;     stacks: ADR, ENV, FRM, KON
;   unstacks: none
;  transfers: error, evaluate
;
  (define (evaluate-set!)
    (cond
      ((= LEN 2)
        (set! PAT (car LST))                           ; look up variable in local frame
        (set! BND (assoc PAT FRM))
        (cond
          (BND                                         ; variable is found in local frame
            (save KON)
            (set! ADR (cdr BND))                       ; look up address in store
            (save ADR)
            (set! EXP (cadr LST))                      ; extract expression from list
            (set! KON kontinue/set!)
            (evaluate)))                               ; evaluate assignment expression
        (cond
          ((null? ENV)                                 ; frame is global
           (error "set! cannot find local variable: " PAT)))
        (save ENV)                                     ; save environment and frame
        (save FRM)
        (assign)))                                     ; proceed with non-local frames
     (error "set! requires exactly 2 arguments: " LST))
;
; continuation for set!
;
;   requires: STO, VAL
;    assigns: BND, STO
;    invokes: cons
;     stacks: none
;   unstacks: ADR, KON
;  transfers: KON
;
  (define (kontinue/set!)
    (set! ADR (restore))                               ; retrieve address
    (set! KON (restore))
    (set! BND (cons ADR VAL))                          ; extend store
    (set! STO (cons BND STO))
    (KON))                                             ; continue
;
; variable assignment
;
;   requires: ENV, KON, LST, PAT, STO
;    assigns: ADR, BND, ENV, EXP, FRM, VAL
;    invokes: assoc, car, cadr, cdr, null?
;     stacks: ADR, KON
;   unstacks: ENV, FRM
;  transfers: error, assign, evaluate
;
  (define (assign)
    (set! FRM (car ENV))                               ; pop frame
    (set! BND (assoc PAT FRM))                         ; look up variable in popped frame
    (cond
      (BND                                             ; variable found in popped frame
        (set! FRM (restore))                           ; restore environment and frame
        (set! ENV (restore))
        (save KON)
        (set! ADR (cdr BND))                           ; look up address in store
        (save ADR)
        (set! EXP (cadr LST))                          ; extract expression from list
        (set! KON kontinue/set!)
        (evaluate)))                                   ; evaluate assignment expression
    (set! ENV (cdr ENV))
    (cond
      ((null? ENV)
       (error "set! cannot find non-local variable: " PAT)))
    (assign))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; evaluator for compound expressions
;
;   requires: EXP, KON
;    assigns: EXP, LEN, LST
;    invokes: car, cdr, equal?
;     stacks: none
;   unstacks: none
;  transfers: evaluate-{ begin | define | if | lambda | quote | set! | application }
;
  (define (evaluate-compound)
    (set! LST (cdr EXP))
    (set! EXP (car EXP))
    (set! LEN (length LST))
    (cond
      ((equal? EXP 'begin)                             ; begin form
        (evaluate-begin))
      ((equal? EXP 'define)                            ; define form
        (evaluate-define))
      ((equal? EXP 'eval)                              ; eval form
        (evaluate-eval))
      ((equal? EXP 'if)                                ; if form
        (evaluate-if))
      ((equal? EXP 'lambda)                            ; lambda form
        (evaluate-lambda))
      ((equal? EXP 'load)                              ; load form
        (evaluate-load))
      ((equal? EXP 'quote)                             ; quote form
        (evaluate-quote))
      ((equal? EXP 'set!)                              ; set! form
        (evaluate-set!)))
    (evaluate-application))                            ; application

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; evaluator for literal
;
;   requires: none
;    assigns: none
;    invokes: none
;     stacks: none
;   unstacks: none
;  transfers: clone
;
  (define (evaluate-literal)
    (clone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; evaluator for variable
;
;   requires: ENV, EXP, FRM, KON, STO
;    assigns: ADR, BND, VAL
;    invokes: assoc, cdr, null?
;     stacks: ENV, FRM
;   unstacks: none
;  transfers: error, lookup, KON
;
  (define (evaluate-variable)
    (set! BND (assoc EXP FRM))                         ; look up variable in local frame
    (cond
      (BND                                             ; variable found in local frame
        (set! ADR (cdr BND))
        (set! BND (assoc ADR STO))                     ; look up address in store
        (set! VAL (cdr BND))                           ; retrieve value
        (KON)))                                        ; continue
    (cond
      ((null? ENV)                                     ; frame is global
       (error "cannot find local variable: " EXP)))
    (save ENV)                                         ; save environment and frame
    (save FRM)
    (lookup))                                          ; proceed with non-local frames
;
; variable lookup
;
;   requires: ENV, EXP, FRM, KON, STO
;    assigns: ADR, BND, ENV, FRM, VAL
;    invokes: assoc, car, cdr, null?
;     stacks: none
;   unstacks: ENV, FRM
;  transfers: error, lookup, KON
;
  (define (lookup)
    (set! FRM (car ENV))                               ; pop frame
    (set! BND (assoc EXP FRM))                         ; look up variable in popped frame
    (cond
      (BND                                             ; variable found in popped frame
        (set! FRM (restore))                           ; restore environment and frame
        (set! ENV (restore))
        (set! ADR (cdr BND))
        (set! BND (assoc ADR STO))                     ; look up address in store
        (set! VAL (cdr BND))                           ; retrieve value
        (KON)))                                        ; continue
    (set! ENV (cdr ENV))
    (cond
      ((null? ENV)                                     ; frame is global
       (error "cannot find non-local variable: " EXP)))
    (lookup))                                          ; iterate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; evaluator for expression
;
;   requires: EXP
;    assigns: none
;    invokes: symbol?, pair?
;     stacks: none
;   unstacks: none
;  transfers: evaluate-{ variable | compound |  literal }
;
  (define (evaluate)
    (cond
      ((pair? EXP)                                     ; compound expression
        (evaluate-compound))
      ((symbol? EXP)                                   ; variable reference
        (evaluate-variable)))
    (evaluate-literal))                                ; literal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; continuation for REP
;
;   requires: VAL
;    assigns: EXP
;    invokes: display, newline, read
;     stacks: none
;   unstacks: none
;  transfers: evaluate
;
  (define (kontinue/REP)
    (display VAL)                                      ; print out result
    (newline)
    (display ">>>")
    (set! EXP (read))                                  ; parse input
    (set! GLB FRM)                                     ; save global frame
    (evaluate))                                        ; evaluate parsed input

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; startup
;
  (set! TOP 0)                                         ; clear store
  (set! STO '())
  (set! ENV '())                                       ; clear environment
  (set! FRM '())                                       ; clear frame
  (set! STK '())                                       ; clear stack
  (set! LST natives)                                   ; define natives
  (initialize-natives)
  (set! LST higher-order-natives)                      ; define higher-order natives
  (initialize-higher-order-natives)
  (set! VAL "efkesSlip")
  (set! KON kontinue/REP)                              ; start read-eval-print
  (kontinue/REP))
