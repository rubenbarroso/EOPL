;We start by modifying the grammar for the lambda calculus:
;
;<expression> ::= <identifier>
;             ::= (lambda (<identifier> ... <identifier>) <expression>)
;             ::= (<expression> <expression>)
;             ::= (if <expression> <expression> <expression>)
;
;A variable x occurs free in a lambda calculus expression E if and only if
;
;1. E is a variable reference and E is the same as x; or
;
;2. E is of the form (lambda (y1 y2 ... yn) E'), where n>0 and x is different
;   from every yi (i=1..n), and x occurs free in E'; or
;
;3. E is of the form (E1 E2) and x occurs free in E1 or E2; or
;
;4. E is of the form (if E1 E2 E3) and x occurs free in E1 or E2 or E3

(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'lambda)
       (and (not (memq var (cadr exp)))
            (occurs-free? var (caddr exp))))
      ((eqv? (car exp) 'if)
       (or (occurs-free? var (cadr exp))
           (occurs-free? var (caddr exp))
           (occurs-free? var (cadddr exp))))
      (else (or (occurs-free? var (car exp))
                (occurs-free? var (cadr exp)))))))

;Quick test:
> (occurs-free? 'y '(if x (lambda (y) y) x))
#f
> (occurs-free? 'z '(if x (lambda (y) z) x))
#t


;A variable x occurs bound in a lambda calculus expression E if and only if
;
;1. E is of the form (lambda (y1 y2 ... yn) E'), where n>0 and x occurs
;   bound in E' or x is equal to any yi (i=1..n) and yi occurs free in E'; or
;
;2. E is of the form (E1 E2) and x occurs bound in E1 or E2; or
;
;3. E is of the form (if E1 E2 E3) and x occurs bound in E1 or E2 or E3

(define occurs-bound?
  (lambda (var exp)
    (cond
      ((symbol? exp) #f)
      ((eqv? (car exp) 'lambda)
       (or (occurs-bound? var (caddr exp))
           (and (memq var (cadr exp))
                (occurs-free? var (caddr exp)))))
      ((eqv? (car exp) 'if)
       (or (occurs-bound? var (cadr exp))
           (occurs-bound? var (caddr exp))
           (occurs-bound? var (cadddr exp))))
      (else (or (occurs-bound? var (car exp))
                (occurs-bound? var (cadr exp)))))))

;Quick test:
> (occurs-bound? 'y '(if x (lambda (y) y) x))
#t
> (occurs-bound? 'z '(if x (lambda (y) z) x))
#f
