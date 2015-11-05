; (define (apply-k k val)
;   (cases k continuation
;     [rator-k (rands env k) (eval-rands rands env (rands-k val k))]
;     [rands-k (proc-val k) (apply-proc proc-val val k)]
;     [test-k (true-exp else-exp env k) ]
;   )
; )

; top-level-eval evaluates a form in the global environment
(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

; eval-exp is the main component of the interpreter

(define eval-exp
  ;(lambda (exp env k)
   (lambda (exp env)
    (cases expression exp
      ;[lit-exp (datum) (appky-k k datum)]
      [lit-exp (datum) datum]
      [var-exp (id)
				(apply-env env id); look up its value.
           ;k ; procedure to call if id is in the environment
           ;(lambda (x) x)
           ;(lambda () (eopl:error 'apply-env ; procedure to call if id not in env
		          ;"variable not found in environment: ~s"
			   ;id
         ;)
        ;))
      ]
      [if-else-exp (test-exp true-exp else-exp) (if (eval-exp test-exp env) (eval-exp true-exp env) (eval-exp else-exp env))]
      [if-exp (test-exp true-exp) (if (eval-exp test-exp env) (eval-exp true-exp env))]
      [app-exp (rator rands)
        (let ;([proc-value (eval-exp rator env (rator-k rands env k))]
             ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args env))]
      [let-exp (args args-exp body)
        (let ([new-env (extend-env args (eval-rands args-exp env) env)])
          (eval-bodies body new-env)
        )]
      [letrec-exp (proc-names args bodies letrec-body)
        (let ([new-env (extend-env-recursively proc-names args bodies env)])
          (eval-bodies letrec-body new-env)
        )]
      ;[lambda-exp (id body) (apply-k k (closure id body env))]
      [lambda-exp (id body) 
          (closure id body env)]
      [while-exp (test body)
        ;(let [(test-results (eval-exp test env))]
          (if (eval-exp test env)
            (begin
              (eval-bodies body env)
              (eval-exp exp env)
            )
          ;)
        )]
      [set-exp (var exp) (set-ref! (apply-env-ref (deref env) (eval-exp var env)) (eval-exp exp env))]
      [define-exp (var exp)
        (let* (
          [var-to-define (eval-exp var env)]
          [exp-to-define (eval-exp exp env)]
          [apply-env-ref-result (apply-env-ref (deref init-env) var-to-define)])
          (if (deref apply-env-ref-result)
            (set-ref! apply-env-ref-result exp-to-define)
              (alter-init-env var-to-define exp-to-define)
          ))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]
    )))

(define (eval-bodies bodies env)
  (let loop ([bodies bodies])
    (if (null? (cdr bodies))
      (eval-exp (car bodies) env)
      (begin (eval-exp (car bodies) env) (loop (cdr bodies)))
    )
  )
)

; evaluate the list of operands, putting results into a list
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
(define apply-proc
  (lambda (proc-value args env)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args (length args) env)]
      [closure (vars bodies env)
        (let ([new-env (extend-env vars args env)])
          (eval-bodies bodies new-env))
      ]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s"
                    proc-value)])))

(define *prim-proc-names*
  '(+ - *  / add1 sub1 zero? cons = not < <= > >=
    car caar caaar cdr cddr cdddr cadr cdar caddr cadar caadr cdaar cdadr cdadr
    list null? assaq eq? equal? atom? length list->vector list? pair? procedure?
    vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr!
    vector-set! display newline void map apply iota eqv? member quotient list-tail quit
    append))

(define init-env         ; for now, our initial global environment only contains
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc
          *prim-proc-names*)
     (empty-env)))

(define reset-global-env
  (lambda ()
    (set! init-env         
      (extend-env         
       *prim-proc-names* 
       (map prim-proc
            *prim-proc-names*)
       (empty-env)))
  )
)

; Usually an interpreter must define each
; built-in procedure individually.  We are "cheating" a little bit.

(define (ls-of-pairs? lop)
  (cond
    ;base case
    [(null? lop) #t]
    ;checks if the parameter is actually a list
    [(not (list? lop)) #f]
    [(not (list? (car lop))) #f]
    ;know we have a list, checks if it a pair
    [else
      (if (and (not (null? (cdr (car lop)))) (null? (cddr (car lop))))
        (ls-of-pairs? (cdr lop))
        #f
      )
    ]
  )
)

(define apply-prim-proc
  (lambda (prim-proc args arg-count env)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(quotient) (apply quotient args)]
      [(add1)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (+ (1st args) 1)]
        )
      ]
      [(sub1)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (- (1st args) 1)]
        )
      ]
      [(zero?)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [(not (number? (1st args))) (eopl:error prim-proc "argument is not a number")]
          [else (= (1st args) 0)]
        )
      ]
      [(cons) (cons (1st args) (2nd args))]
      [(not)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (if (1st args) #f #t)]
        )
      ]
      [(=) (= (1st args) (2nd args))]
      [(<)
        (cond
          [(zero? arg-count) (eopl:error prim-proc "incorrect argument count")]
          [else (apply < args)]
        )
      ]
      [(<=)
        (cond
          [(zero? arg-count) (eopl:error prim-proc "incorrect argument count")]
          [else (apply <= args)]
        )
      ]
      [(>)
        (cond
          [(zero? arg-count) (eopl:error prim-proc "incorrect argument count")]
          [else (apply > args)]
        )]
      [(>=)
        (cond
          [(zero? arg-count) (eopl:error prim-proc "incorrect argument count")]
          [else (apply >= args)]
        )]
      [(car)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (car (1st args))]
        )]
      [(caar)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (caar (1st args))]
        )]
      [(caaar)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (caaar (1st args))]
        )
      ]
      [(cdr)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (cdr (1st args))]
        )]
      [(cddr)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (cddr (1st args))]
        )]
      [(cdddr)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (cdddr (1st args))]
        )]
      [(cadr)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (cadr (1st args))]
        )]
      [(cdar)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (cdar (1st args))]
        )]
      [(caddr)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (caddr (1st args))]
        )]
      [(cdadr)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (cdadr (1st args))]
        )]
      [(cddar)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (cddar (1st args))]
        )]
      [(cdaar)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (cdaar (1st args))]
        )]
      [(cadar)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (cadar (1st args))]
        )
      ]
      [(caadr)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (caadr (1st args))]
        )
      ]
      [(list) (apply list args)]
      [(null?)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (null? (1st args))]
        )]
      [(assaq)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [(not (symbol? (1st args))) (eopl:error prim-proc "argument is not a list")]
          [(not (ls-of-pairs? (2nd args))) (eopl:error prim-proc "argument is not a list")]
          [else (assaq (1st args) (2nd args))]
        )
      ]
      [(eq?)
        (cond
          [(not (= arg-count 2)) (eopl:error prim-proc "incorrect argument count")]
          [else (eq? (1st args) (2nd args))]
        )
      ]
      [(equal?)
        (cond
          [(not (= arg-count 2)) (eopl:error prim-proc "incorrect argument count")]
          [else (equal? (1st args) (2nd args))]
        )
      ]
      [(eqv?)
        (cond
          [(not (= arg-count 2)) (eopl:error prim-proc "incorrect argument count")]
          [else (eqv? (1st args) (2nd args))]
        )
      ]
      [(atom?)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (atom? (1st args))]
        )
      ]
      [(length)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [(not (list? (1st args))) (eopl:error prim-proc "argument is not a list")]
          [else (length (1st args))]
        )
      ]
      [(list->vector)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [(not (list? (1st args))) (eopl:error prim-proc "argument is not a list")]
          [else (list->vector (1st args))]
        )
      ]
      [(list?)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (list? (1st args))]
        )
      ]
      [(pair?)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (pair? (1st args))]
        )
      ]
      [(procedure?)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (proc-val? (1st args))]
        )
      ]
      [(vector->list)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [(not (vector? (1st args))) (eopl:error prim-proc "argument is not a vector")]
          [else (vector->list (1st args))]
        )
      ]
      [(vector) (apply vector args)]
      [(make-vector)
        (cond
          [(not (= arg-count 2)) (eopl:error prim-proc "incorrect argument count")]
          [(not (number? (1st args))) (eopl:error prim-proc "argument is not a number")]
          [else (make-vector (1st args) (2nd args))]
        )
      ]
      [(vector-ref)
        (cond
          [(not (= arg-count 2)) (eopl:error prim-proc "incorrect argument count")]
          [(not (vector? (1st args))) (eopl:error prim-proc "argument is not a vector")]
          [(not (number? (2nd args))) (eopl:error prim-proc "argument is not a number")]
          [else (vector-ref (1st args) (2nd args))]
        )
      ]
      [(vector?)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (vector? (1st args))]
        )
      ]
      [(number?)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (number? (1st args))]
        )
      ]
      [(symbol?)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (symbol? (1st args))]
        )
      ]
      [(set-car!)
        (cond
          [(not (= arg-count 2)) (eopl:error prim-proc "incorrect argument count")]
          [(not (list? (1st args))) (eopl:error prim-proc "argument is not a list")]
          [else (set-car! (1st args) (2nd args))]
        )
      ]
      [(set-cdr!)
        (cond
          [(not (= arg-count 2)) (eopl:error prim-proc "incorrect argument count")]
          [(not (list? (1st args))) (eopl:error prim-proc "argument is not a list")]
          [else (set-cdr! (1st args) (2nd args))]
        )
      ]
      [(vector-set!)
        (cond
          [(not (= arg-count 3)) (eopl:error prim-proc "incorrect argument count")]
          [(not (vector? (1st args))) (eopl:error prim-proc "argument is not a vector")]
          [(not (number? (2nd args))) (eopl:error prim-proc "argument is not a number")]
          [(< (2nd args) 0) (eopl:error prim-proc "position is below zero")]
          [(> (2nd args) (vector-length (1st args))) (eopl:error prim-proc "position is larger than vector-length")]
          [else (vector-set! (1st args) (2nd args) (3rd args))]
        )
      ]
      [(display)
        (cond
          [(not (= arg-count 1)) (eopl:error prim-proc "incorrect argument count")]
          [else (display (1st args))]
        )
      ]
      [(newline) (newline)]
      [(void)
        (cond
          [(not (zero? arg-count)) (eopl:error prim-proc "incorrect argument count")]
          [else (void)]
        )
      ]
      [(map)
        (cond
          [(not (proc-val? (1st args))) (eopl:error prim-proc "argument-1 is not a procedure")]
          [(not ((list-of list?) (cdr args))) (eopl:error prim-proc "argument-2 is not lists")]
          [else
            (let loop ([proc (1st args)][ls (cdr args)])
              (if (andmap null? ls)
                '()
                (cons (apply-proc proc (map car ls) env) (loop proc (map cdr ls)))
              )
            )
          ]
        )
      ]
      [(apply)
        (cond
          [(not (proc-val? (1st args))) (eopl:error prim-proc "argument-2 is not a procedure")]
          [(not (list? (2nd args))) (eopl:error prim-proc "argument-2 is not lists")]
          [else (apply (lambda (x) (apply-proc (1st args) x env)) (cdr args))]
        )
      ]
      [(iota)
        (cond
          [(not (zero? arg-count)) (eopl:error prim-proc "incorrect argument count")]
          [else
            (let loop ([i 0])
              (if (= i (1st args))
                '()
                (cons i (loop (add1 i)))
              )
            )
          ]
        )
      ]
      [(member)
        (cond
          [(not (= arg-count 2)) (eopl:error prim-proc "incorrect argument count")]
          [(not (list? (2nd args))) (eopl:error prim-proc "Argument-2 is not a list")]
          [else (member (1st args) (2nd args))]
        )
      ]
      [(list-tail)
        (cond
          [(not (= arg-count 2)) (eopl:error prim-proc "incorrect argument count")]
          [(not (list? (1st args))) (eopl:error prim-proc "Argument-1 is not a list")]
          [(not (number? (2nd args))) (eopl:error prim-proc "Argument-2 is not a number")]
          [else (list-tail (1st args) (2nd args))]
        )
      ]
      [(quit)
        (cond
          [(not (= arg-count 0)) (eopl:error prim-proc "incorrect argument count")]
          [else '(quit)]
        )
      ]
      [(append)
        (cond
          [(not (= arg-count 2)) (eopl:error prim-proc "incorrect argument count")]
          [(not (list? (1st args))) (eopl:error prim-proc "Argument-1 is not a list")]
          [(not (list? (2nd args))) (eopl:error prim-proc "Argument-2 is not a list")]
          [else (append (1st args) (2nd args))]
        )
      ]
      [else (error 'apply-prim-proc
            "Bad primitive procedure name: ~s"
            prim-op)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
      (cond
        [(equal? '(quit) answer) (newline)]
        [(equal? (void) answer) (newline) (rep)]
        [else (eopl:pretty-print answer) (rep)]
      )
    )
  )
)

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))










