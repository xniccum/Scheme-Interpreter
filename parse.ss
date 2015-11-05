; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)
(define 5th (lambda (x) (4th (cdr x))))

(define (let->application ls) (cons (list 'lambda (map car (cadr ls)) (caddr ls)) (map cadr (cadr ls))))
(define (let*->let lst)
  (let let-loop ([ls (cadr lst)])
    (if (null? ls)
      (caddr lst)
      (list 'let (list (car ls)) (let-loop (cdr ls)))
    )
  )
)

(define (parse-exp datum)
  (cond
    [(symbol? datum) (var-exp datum)]
    [(number? datum) (lit-exp datum)]
    [(string? datum) (lit-exp datum)]
    [(boolean? datum) (lit-exp datum)]
    [(vector? datum) (lit-exp datum)]
    [(list? datum)
      (let* ([len (length datum)][key (1st datum)])
        (cond
          [(eqv? key 'quote) (lit-exp (cadr datum))]
          [(eqv? key 'while) (while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
          [(eqv? key 'lambda)
            (cond
              [(< len 3) (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" (list datum))]
              [(not (check-lambda-args (2nd datum))) (eopl:error 'parse-exp "lambda's formal arguments ~s must all be symbols" (list datum))]
              [else (lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
            )]
          ;Named-Let
          [(and (eqv? key 'let) (symbol? (2nd datum)))
            (cond
              [(not (>= len 4)) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" (list 'let datum))]
              [(not (list? (3rd datum))) (eopl:error 'parse-exp "declarations in ~s-expression not a list ~s" (list 'let datum))]
              [(not (andmap (lambda (x) (list? x)) (3rd datum))) (eopl:error 'parse-exp "declaration in ~s-exp is not a proper list ~s" (list 'let datum))]
              [(not (andmap (lambda (x) (= (length x) 2)) (3rd datum))) (eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s" (list 'let datum))]
              [(not (andmap (lambda (x) (symbol? (car x))) (3rd datum))) (eopl:error 'parse-exp "vars in ~s-exp must be symbols ~s" (list 'let datum))]
              [else (named-let-exp (2nd datum) (map (lambda (x) (1st x)) (3rd datum)) (map (lambda (x) (parse-exp (2nd x))) (3rd datum)) (map parse-exp (cdddr datum)))]
            )]
          ;Regular Let
          [(eqv? key 'let)
            (cond
              [(not (>= len 3)) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" (list 'let datum))]
              [(not (list? (2nd datum))) (eopl:error 'parse-exp "declarations in ~s-expression not a list ~s" (list 'let datum))]
              [(not (andmap (lambda (x) (list? x)) (2nd datum))) (eopl:error 'parse-exp "declaration in ~s-exp is not a proper list ~s" (list 'let datum))]
              [(not (andmap (lambda (x) (= (length x) 2)) (2nd datum))) (eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s" (list 'let datum))]
              [(not (andmap (lambda (x) (symbol? (car x))) (2nd datum))) (eopl:error 'parse-exp "vars in ~s-exp must be symbols ~s" (list 'let datum))]
              [else (let-exp (map (lambda (x) (1st x)) (2nd datum)) (map (lambda (x) (parse-exp (2nd x))) (2nd datum)) (map parse-exp (cddr datum)))]
            )]
          [(eqv? key 'let*)
            (cond
              [(not (>= len 3)) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" (list datum))]
              [(not (list? (2nd datum))) (eopl:error 'parse-exp "declarations in ~s-expression not a list ~s" (list datum))]
              [(not (andmap (lambda (x) (list? x)) (2nd datum))) (eopl:error 'parse-exp "declaration in ~s-exp is not a proper list ~s" (list 'let* datum))]
              [(not (andmap (lambda (x) (= (length x) 2)) (2nd datum))) (eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s" (list 'let* datum))]
              [(not (andmap (lambda (x) (symbol? (car x))) (2nd datum))) (eopl:error 'parse-exp "vars in ~s-exp must be symbols ~s" (list 'let* datum))]
              [else (parse-exp (let*->let datum))]
            )]
          [(eqv? key 'letrec)
            (cond
              [(not (>= len 3)) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" (list '(letrec datum)))]
              [(not (list? (2nd datum))) (eopl:error 'parse-exp "declarations in ~s-expression not a list ~s" (list 'letrec datum))]
              [(not (andmap (lambda (x) (list? x)) (2nd datum))) (eopl:error 'parse-exp  "declaration in ~s-exp is not a proper list ~s" (list 'letrec datum))]
              [(not (andmap (lambda (x) (symbol? (car x))) (2nd datum))) (eopl:error 'parse-exp "vars in ~s-exp must be symbols ~s" (list 'letrec datum))]
              [else (letrec-exp
                (map 1st (2nd datum))
                (map (lambda (x) (2nd (2nd x))) (2nd datum))
                (map (lambda (x) (map parse-exp (cddr (2nd x)))) (2nd datum))
                (map parse-exp (cddr datum)))]
            )]
          [(eqv? key 'if)
            (cond
              [(< len 3) (eopl:error 'parse-exp "if-expression ~s does not have test, then, and optionally else" (list datum))]
              [(> len 4) (eopl:error 'parse-exp "Invalid syntax ~s" (list datum))]
              [(= len 3) (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
              [else (if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))]
            )]
          [(eqv? key 'set!)
            (cond
              [(not (= len 3)) (eopl:error 'parse-exp "set! expression ~s does not have (only) variable and expression" datum)]
              [(not (symbol? (2nd datum))) (eopl:error 'parse-exp "set! expression ~s does not have (only) variable as first argument" datum)]
              [else (set-exp (lit-exp (2nd datum)) (parse-exp (3rd datum)))]
            )]
          [(eqv? key 'define)
            (cond
              [(not (= len 3)) (eopl:error 'parse-exp "define expression ~s does not have (only) variable and expression" datum)]
              [(not (symbol? (2nd datum))) (eopl:error 'parse-exp "define expression ~s does not have (only) variable as first argument" datum)]
              [else (define-exp (lit-exp (2nd datum)) (parse-exp (3rd datum)))]
            )
          ]
          [else (app-exp (parse-exp key) (map parse-exp (cdr datum)))]
        )
      )
    ]
    [else (eopl:error 'parse-exp "expression ~s is not a proper list" datum)]
  )
)

(define (unparse-exp e)
  (cases expression e
    [lambda-exp (id body) (cons 'lambda (cons id (map unparse-exp body)))]
    [let-exp (args args-exp body) (cons 'let (cons (map (lambda (x y) (list x (unparse-exp y))) args args-exp) (map unparse-exp body)))]
    [letrec-exp (proc-names args  bodies letrec-body)
      (cons 'letrec
        (cons (map (lambda (x y) (list x y)) proc-names
          (map (lambda (x y) (cons 'lambda (cons x (map unparse-exp y)))) args bodies))
        (map unparse-exp letrec-body)))]
    [named-let-exp (name args args-exp body) (cons 'let (cons name (cons (map (lambda (x y) (list x (unparse-exp y))) args args-exp) (map unparse-exp body))))]
    [if-else-exp (test-exp true-exp else-exp) (list 'if (unparse-exp test-exp) (unparse-exp true-exp) (unparse-exp else-exp))]
    [if-exp (test-exp true-exp) (list 'if (unparse-exp test-exp) (unparse-exp true-exp))]
    [set-exp (var exp) (list 'set! (unparse-exp var) (unparse-exp exp))]
    [define-exp (var exp) (list 'define (unparse-exp var) (unparse-exp exp))]
    [app-exp (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))]
    [while-exp (test body) (list 'while (unparse-exp test) (map unparse-exp body))]
    [var-exp (id) id]
    [lit-exp (id) (if (null? id) (quote '()) id)]
  )
)

(define check-lambda-args
  (lambda (datum)
    (cond
      [(null? datum) #t]
      [(symbol? datum) #t]
      [(pair? datum)
        (and (check-lambda-args (car datum)) (check-lambda-args (cdr datum)))]
      [(andmap symbol? (2nd datum)) #t]
      [else #f]
    )
  )
)
