Let's apply substitution to evaluate:

(extend-env '() '() (empty-env))
(extend-env '() '() ())
((() ()) . ())
((() ()))

;Let's check that this value is correct after implementing
;the interface:

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (syms vals env)
    (cons (list syms vals) env)))

(define apply-env
  (lambda (env sym)
    (if (null? env)
      (eopl:error 'apply-env "No binding for ~s" sym)
      (let ((syms (car (car env)))
            (vals (cadr (car env)))
            (env (cdr env)))
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
            (list-ref vals pos)
            (apply-env env sym)))))))

(define rib-find-position list-find-position)

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;> (extend-env '() '() (empty-env))
;((() ()))
