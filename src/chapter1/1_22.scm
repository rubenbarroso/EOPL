;A variable x occurs free in a lambda calculus expression E if and only if
;
;1. E is a variable reference and E is the same as x; or
;
;2. E is of the form (lambda (y1 y2 ... yn) E'), where n>0 and x is different
;   from every yi (i=1..n), and x occurs free in E'; or
;
;3. E is of the form (E1 E2) and x occurs free in E1 or E2

(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'lambda)
       (and (not (memq var (cadr exp)))    ;modified
            (occurs-free? var (caddr exp))))
      (else (or (occurs-free? var (car exp))
                (occurs-free? var (cadr exp)))))))

;A quick verification:
> (occurs-free? 'x '(lambda (y x) x))
#f
> (occurs-free? 'x '(lambda (y z) x))
#t

;A variable x occurs bound in a lambda calculus expression E if and only if
;
;1. E is of the form (lambda (y1 y2 ... yn) E'), where n>0 and x occurs
;   bound in E' or x is equal to any yi (i=1..n) and yi occurs free in E'; or
;
;2. E is of the form (E1 E2) and x occurs bound in E1 or E2

(define occurs-bound?
  (lambda (var exp)
    (cond
      ((symbol? exp) #f)
      ((eqv? (car exp) 'lambda)
       (or (occurs-bound? var (caddr exp))
           (and (memq var (cadr exp))       ;modified
                (occurs-free? var (caddr exp)))))
      (else (or (occurs-bound? var (car exp))
                (occurs-bound? var (cadr exp)))))))

;A quick verification
> (occurs-bound? 'x '(lambda (x y) x))
#t
> (occurs-bound? 'x '(lambda (z y) x))
#f
