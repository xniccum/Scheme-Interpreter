(define (syntax-expand exp)
  (cases expression exp
    [lambda-exp (id body) (lambda-exp id (map syntax-expand body))]
    [let-exp (args args-exp body) (let-exp args (map syntax-expand args-exp) (map syntax-expand body))]
    [letrec-exp (proc-names args bodies letrec-body) (letrec-exp proc-names args (map (lambda (x) (map syntax-expand x)) bodies) (map syntax-expand letrec-body))]
    [if-else-exp (test-exp true-exp else-exp) (if-else-exp (syntax-expand test-exp) (syntax-expand true-exp) (syntax-expand else-exp))]
    [if-exp (test-exp true-exp) (if-exp (syntax-expand test-exp) (syntax-expand true-exp))]
    [set-exp (var exp) (set-exp var (syntax-expand exp))]
    [app-exp (rator rand) (handle-app-exp exp)]
    [while-exp (count body) exp]
    [var-exp (id) exp]
    [lit-exp (id) exp]
  )
)

(define handle-app-exp
  (lambda (exp)
    (cond
      [(eqv? (2nd (2nd exp)) 'cond) (syntax-expand (parse-exp (cond->if (cdr (unparse-exp exp)))))]
      [(eqv? (2nd (2nd exp)) 'case) (syntax-expand (parse-exp (case->if (cdr (unparse-exp exp)))))]
      [(eqv? (2nd (2nd exp)) 'and) (syntax-expand (parse-exp (and->if (cdr (unparse-exp exp)))))]
      [(eqv? (2nd (2nd exp)) 'or) (syntax-expand (parse-exp (or->if (cdr (unparse-exp exp)))))]
      [(eqv? (2nd (2nd exp)) 'begin) (syntax-expand (parse-exp (begin->let (cdr (unparse-exp exp)))))]
      [else exp]
    )
  )
)

(define (cond->if exp)
  (cond
    [(null? exp) '(if #f 1)]
    [(eqv? (caar exp) 'else) (cadar exp)]
    [else (list 'if (caar exp) (cadar exp) (cond->if (cdr exp)))]
  )
)

(define (case->if exp)
  (define key (car exp))
  (letrec ([helper
    (lambda (exp)
      (cond
        [(null? exp) '(if #f 1)]
        [(eqv? (caar exp) 'else) (if (symbol? (cadar exp)) (list 'quote (cadar exp)) (cadar exp))]
        [else (list 'if (list 'member key (cons 'list (caar exp))) (if (symbol? (cadar exp)) (list 'quote (cadar exp)) (cadar exp)) (helper (cdr exp)))]
      )
    )
    ]) (helper (cdr exp)))
)

(define (or->if exp)
  (cond
    [(null? exp) '#f]
    [else (list 'if (car exp) (car exp) (or->if (cdr exp)))]
  )
)

(define (and->if exp)
  (cond
    [(null? exp) '#t]
    [(null? (cdr exp)) (list 'if (car exp) (car exp) '#f)]
    [else (list 'if (car exp) (and->if (cdr exp)) '#f)]
  )
)

(define begin->let
  (lambda (exp)
    (cond
      [(null? (cdr exp)) (car exp)]
      [else (list 'let (list (list 'i (car exp))) (begin->let (cdr exp)))]
    )
  )
)
