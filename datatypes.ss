

;; Parsed expression datatypes
(define (literal? x) (or (symbol? x) (number? x) (boolean? x) (list? x) (string? x) (vector? x)))
(define (lambda-arg? x) (or (symbol? x) (list? x) (pair? x)))
(define (valid-syms? x) (or ((list-of symbol?) x) (symbol? x) (pair? x)))
(define (cell? x) (and (pair? x) (= 1 (length x))))

(define (cell value) (list value))
(define (cell-ref cell) (car cell))
(define (cell-set! cell value) (set-car! cell value))
(define (cell? obj) (and (list? obj) (= 1 (length obj))))


(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms valid-syms?)
   (vals (list-of scheme-value?))
   (env cell?)]
  [recursively-extended-env-record
    (proc-names (list-of symbol?))
    (args (list-of valid-syms?))
    (bodies (list-of list?))
    (env cell?)])

(define-datatype expression expression?
  [var-exp (id symbol?)]
  [lit-exp (id literal?)]
  [lambda-exp
    (id lambda-arg?)
    (body list?)]
  [let-exp
    (args (list-of symbol?))
    (args-exp list?)
    (body list?)]
  [letrec-exp
    (proc-names (list-of symbol?))
    (args (list-of valid-syms?))
    (bodies (list-of list?))
    (letrec-body (list-of list?))]
  [app-exp
    (rator expression?)
    (rand list?)]
  [if-else-exp
    (test-exp expression?)
    (true-exp expression?)
    (else-exp expression?)]
  [if-exp
    (test-exp expression?)
    (true-exp expression?)]
  [set-exp
    (var expression?)
    (exp expression?)]
  [define-exp
    (var expression?)
    (exp expression?)]
  [while-exp
    (test expression?)
    (body list?)]
  [named-let-exp
    (name symbol?)
    (args list?)
    (args-exp list?)
    (body list?)])

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
    (vars valid-syms?)
    (bodies (list-of expression?))
    (env cell?)
  ])

(define-datatype continuation continuation?
  [test-k
    (true-exp expression?)
    (else-exp expression?)
    (env environment?)
    (k continuation?)
  ]
  [rator-k
    (rands (list-of expression?))
    (env environment?)
    (k continuation?)
  ]
  [rands-k
    (proc-val proc-val?)
    (k continuation?)])

;; environment type definitions
(define scheme-value?
  (lambda (x) #t))
