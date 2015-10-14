
;; Parsed expression datatypes

(define-datatype expression expression?
  [var-exp (id symbol?)]
  [lit-exp (id literal?)]
  [lambda-exp
    (id lambda-arg?)
    (body list?)
  ]
  [let-exp
    (id list?)
    (body list?)
  ]
  [let*-exp
    (id list?)
    (body list?)
  ]
  [letrec-exp
    (id list?)
    (body list?)
  ]
  [app-exp
    (rator expression?)
    (rand list?)
  ]
  [if-exp
    (test-exp expression?)
    (true-exp expression?)
    (else-exp expression?)
  ]
  [set-exp
    (var var-exp?)
    (exp expression?)
  ]
)


; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)])




;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))
