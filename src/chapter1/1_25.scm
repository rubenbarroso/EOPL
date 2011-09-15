;A (quote <datum>) expression does not bind any variable since the <datum>
;are treated as literals.

(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'quote)      ;modified
       #t)
      ((eqv? (car exp) 'lambda)
       (and (not (memq var (cadr exp)))
            (occurs-free? var (caddr exp))))
      ((eqv? (car exp) 'let)
       (occurs-free? var (let->lambda exp)))
      (else (or (occurs-free? var (car exp))
                (occurs-free? var (cadr exp)))))))

(define occurs-bound?
  (lambda (var exp)
    (cond
      ((symbol? exp) #f)
      ((eqv? (car exp) 'quote)      ;modified
       #f)
      ((eqv? (car exp) 'lambda)
       (or (occurs-bound? var (caddr exp))
           (and (memq var (cadr exp))
                (occurs-free? var (caddr exp)))))
      ((eqv? (car exp) 'let)
       (occurs-bound? var (let->lambda exp)))
      (else (or (occurs-bound? var (car exp))
                (occurs-bound? var (cadr exp)))))))
