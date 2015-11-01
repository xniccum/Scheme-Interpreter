; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

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
      [(list? syms) (extended-env-record syms vals env)]
      [(symbol? syms) (extended-env-record (list syms) (list vals) env)]
      [else (extended-env-record (pair->list syms) (make-proper-vals vals (length (pair->list syms))) env)]
    )
  )
)

(define (extend-env-recursively proc-names args bodies old-env)
  (recursively-extended-env-record proc-names args bodies old-env))

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

(define (apply-env env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      [empty-env-record () (fail)]
      [extended-env-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
            (succeed (list-ref vals pos))
            (apply-env env sym succeed fail)
          )
        )
      ]
      [recursively-extended-env-record (procnames args bodies old-env)
        (let ([pos (list-find-position sym procnames)])
          (if (number? pos)
            (closure (list-ref args pos) (list-ref bodies pos) env)
            (apply-env 
              old-env 
              sym 
              (lambda (x) x) 
              (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
                "variable not found in environment: ~s" sym))
              )
          )
        )
      ]
    )
  )

