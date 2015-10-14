; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

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
          [(eqv? key 'quote) (lit-exp datum)]
          [(eqv? key 'lambda)
            (cond
              [(< len 3) (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" (list datum))]
              [(not (or (symbol? (2nd datum)) (andmap symbol? (2nd datum)))) (eopl:error 'parse-exp "lambda's formal arguments ~s must all be symbols" (list datum))]
              [else (lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
            )
          ]
          [(eqv? key 'let)
            (cond
              [(not (>= len 3)) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" (list 'let datum))]
              [(not (list? (2nd datum))) (eopl:error 'parse-exp "declarations in ~s-expression not a list ~s" (list 'let datum))]
              [(not (andmap (lambda (x) (list? x)) (2nd datum))) (eopl:error 'parse-exp "declaration in ~s-exp is not a proper list ~s" (list 'let datum))]
              [(not (andmap (lambda (x) (= (length x) 2)) (2nd datum))) (eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s" (list 'let datum))]
              [(not (andmap (lambda (x) (symbol? (car x))) (2nd datum))) (eopl:error 'parse-exp "vars in ~s-exp must be symbols ~s" (list 'let datum))]
              [else (let-exp (map parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
            )
          ]
          [(eqv? key 'let*)
            (cond
              [(not (>= len 3)) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" (list datum))]
              [(not (list? (2nd datum))) (eopl:error 'parse-exp "declarations in ~s-expression not a list ~s" (list datum))]
              [(not (andmap (lambda (x) (list? x)) (2nd datum))) (eopl:error 'parse-exp "declaration in ~s-exp is not a proper list ~s" (list 'let* datum))]
              [(not (andmap (lambda (x) (= (length x) 2)) (2nd datum))) (eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s" (list 'let* datum))]
              [(not (andmap (lambda (x) (symbol? (car x))) (2nd datum))) (eopl:error 'parse-exp "vars in ~s-exp must be symbols ~s" (list 'let* datum))]
              [else (let*-exp (map parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
            )
          ]
          [(eqv? key 'letrec)
            (cond
              [(not (>= len 3)) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" (list '(letrec datum)))]
              [(not (list? (2nd datum))) (eopl:error 'parse-exp "declarations in ~s-expression not a list ~s" (list 'letrec datum))]
              [(not (andmap (lambda (x) (list? x)) (2nd datum))) (eopl:error 'parse-exp  "declaration in ~s-exp is not a proper list ~s" (list 'letrec datum))]
              [(not (andmap (lambda (x) (= (length x) 2)) (2nd datum))) (eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s" (list 'letrec datum))]
              [(not (andmap (lambda (x) (symbol? (car x))) (2nd datum))) (eopl:error 'parse-exp "vars in ~s-exp must be symbols ~s" (list 'letrec datum))]
              [else (letrec-exp (map parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
            )
          ]
          [(eqv? key 'if)
            (cond
               [(not (= len 4)) (eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" (list datum))]
               [else (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))]
            )
          ]
          [(eqv? key 'set!)
            (cond
              [(not (= len 3)) (eopl:error 'parse-exp "set! expression ~s does not have (only) variable and expression" datum)]
              [else (set-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
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
    [let-exp (id body) (cons 'let (cons (map unparse-exp id) (map unparse-exp body)))]
    [let*-exp (id body) (cons 'let* (cons (map unparse-exp id) (map unparse-exp body)))]
    [letrec-exp (id body) (cons 'letrec (cons (map unparse-exp id) (map unparse-exp body)))]
    [if-exp (test-exp true-exp else-exp) (list 'if (unparse-exp test-exp) (unparse-exp true-exp) (unparse-exp else-exp))]
    [set-exp (var exp) (list 'set! (unparse-exp var) (unparse-exp exp))]
    [app-exp (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))]
    [var-exp (id) id]
    [lit-exp (id) id]
  )
)










