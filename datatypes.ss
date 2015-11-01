
;; Parsed expression datatypes
(define (literal? x) (or (symbol? x) (number? x) (boolean? x) (list? x) (string? x) (vector? x)))
(define (lambda-arg? x) (or (symbol? x) (list? x) (pair? x)))

(define-datatype expression expression?
  [var-exp (id symbol?)]
  [lit-exp (id literal?)]
  [lambda-exp
    (id lambda-arg?)
    (body list?)]
  [let-exp
    (args list?)
    (args-exp list?)
    (body list?)]
  [letrec-exp
    (proc-names (list-of symbol?))
    (args (list-of (list-of symbol?)))
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
    (var var-exp?)
    (exp expression?)]
  [while-exp
    (test expression?)
    (body list?)]
)

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
    (vars (list-of symbol?))
    (bodies (list-of expression?))
    (env environment?)
  ]
)

;; environment type definitions
(define scheme-value?
  (lambda (x) #t))

(define (valid-syms? x) (or ((list-of symbol?) x) (symbol? x) (pair? x)))

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms valid-syms?)
   (vals (list-of scheme-value?))
   (env environment?)]
  [recursively-extended-env-record
    (proc-names (list-of symbol?))
    (args (list-of (list-of symbol?)))
    (bodies (list-of list?))
    (env environment?)])
