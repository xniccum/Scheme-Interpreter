; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (cell (empty-env-record))))

(define (pair->list ls)
  (if (symbol? (cdr ls))
    (list (car ls) (cdr ls))
    (cons (car ls) (pair->list (cdr ls)))
  )
)
(define make-proper-vals
  (lambda (vals len)
    (if (<= len 1)
      (list vals)
      (cons (car vals) (make-proper-vals (cdr vals) (- len 1)))
    )
  )
)

(define extend-env
  (lambda (syms vals env)
    (cond
      [(list? syms) (cell (extended-env-record syms (map cell vals) env))]
      [(symbol? syms) (cell (extended-env-record (list syms) (cell (list vals)) env))]
      [else (cell (extended-env-record (pair->list syms) (map cell (make-proper-vals vals (length (pair->list syms)))) env))]
    )
  )
)

(define (extend-env-recursively proc-names args bodies old-env)
  (cell (recursively-extended-env-record proc-names args bodies old-env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define (apply-env-ref env var)
  (cases environment env
    [empty-env-record () (cell #f)]
    [extended-env-record (syms vals env)
      (let ((pos (list-find-position var syms)))
        (if (number? pos)
          (list-ref vals pos)
          (apply-env-ref (deref env) var)
        )
      )
    ]
    [recursively-extended-env-record (procnames args bodies old-env)
      (let ([pos (list-find-position var procnames)])
        (if (number? pos)
          (cell (closure (list-ref args pos) (list-ref bodies pos) (cell env)))
          (apply-env-ref (deref old-env) var)
        )
      )
    ]
  )
)
(define (apply-env env var) (deref (apply-env-ref (deref env) var)))

(define (deref ref) (cell-ref ref))
(define (set-ref! ref val) (cell-set! ref val))

(define alter-init-env
  (lambda (sym exp)
    (set-car! init-env (deref (extend-env (cons sym (cadr (deref init-env))) (cons exp (map deref (caddr (deref init-env)))) (cadddr (deref init-env)))))
  )
)

