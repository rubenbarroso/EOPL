From SICP: "Let* is similar to let, except that the bindings of the let variables
            are performed sequentially from left to right, and each binding is
            made in an environment in which all of the preceding bindings are
            visible"

Therefore, we are again dealing with a derived expression, that can be transformed
into another one that is covered by the formal definition. In the case of let*,
we can transfor it into a equivalent set of nested expressions:

(let* ((<var1> <exp1>) ... (<varn> <expn>))
  <body>)

will turn into:

(let ((<var1> <exp1>))
  ...
  (let ... (<varn> <expn>))
  <body>)

Let us see an more concrete example:

(let* ((a 3) (b (+ a 2)))
  (* a b))

is transformed into:

(let ((a 3))
  (let ((b (+ a 2)))
    (* a b)))


;The following procedures are taken from my answer to SICP's exercise 4.7:

(define (let*-exps exp) (cadr exp))

(define (let*-body exp) (caddr exp))

(define (make-let let-exps body)
  (list 'let let-exps body))

(define (let*->nested-lets exp)
  (expand-let* (let*-exps exp) (let*-body exp)))

(define (expand-let* let-exps let-body)
  (if (null? let-exps)
      let-body
      (make-let (list (car let-exps))
                (expand-let* (cdr let-exps) let-body))))

;Quick test - formatted output
> (let*->nested-lets '(let* ((a 3) (b (+ a 2))) (* a b)))
(let ((a 3))
  (let ((b (+ a 2)))
    (* a b)))

;occurs-free? and occurs-bound? use let*->nested-lets
(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'lambda)
       (and (not (memq var (cadr exp)))
            (occurs-free? var (caddr exp))))
      ((eqv? (car exp) 'let)
       (occurs-free? var (let->lambda exp)))
      ((eqv? (car exp) 'let*)
       (occurs-free? var (let*->nested-lets exp)))
      (else (or (occurs-free? var (car exp))
                (occurs-free? var (cadr exp)))))))

> (occurs-free? 'a '(let* ((a c) (d e)) (- a)))
#f
> (occurs-free? 'c '(let* ((a c) (d e)) (- a)))
#t
> (occurs-free? - '(let* ((a c) (d e)) (- a)))
#f


(define occurs-bound?
  (lambda (var exp)
    (cond
      ((symbol? exp) #f)
      ((eqv? (car exp) 'lambda)
       (or (occurs-bound? var (caddr exp))
           (and (memq var (cadr exp))
                (occurs-free? var (caddr exp)))))
      ((eqv? (car exp) 'let)
       (occurs-bound? var (let->lambda exp)))
      ((eqv? (car exp) 'let*)
       (occurs-bound? var (let*->nested-lets exp)))
      (else (or (occurs-bound? var (car exp))
                (occurs-bound? var (cadr exp)))))))

> (occurs-bound? 'a '(let* ((a c) (d e)) (- a)))
#t
> (occurs-bound? 'c '(let* ((a c) (d e)) (- a)))
#f
> (occurs-bound? - '(let* ((a c) (d e)) (- a)))
#f
