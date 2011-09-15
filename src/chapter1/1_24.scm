We start by modifying the grammar for the lambda calculus to include let:

<expression> ::= <identifier>
             ::= (lambda (<identifier> ... <identifier>) <expression>)
             ::= (<expression> <expression>)
             ::= (if <expression> <expression> <expression>)
             ::= (let ((<identifier> <expression) ...) <expression>)

We remember from SICP's 4th chapter that let expressions are derived expressions:

(let ((<var1> <exp1>) ... (<varn> <expn>))
  <body>)

is equivalent to:

((lambda (<var1> ... <varn>)
   <body>)
 <exp1> ... <expn>)

We need to curry this expression because <exp1> ... <expn> cannot be derived
syntactically, since we only allow lambdas with one argument. Let's see this
with a simpler expression involving two arguments:

((lambda (<var1> <var2>)
   <body>)
 <exp1> <exp2>)

is curried to:

((lambda (<var1>)
   ((lambda (<var2>)
      <body>)
    <exp2>))
 <exp1>)

As an specific example, take a look at the divide procedure, pretty silly
since it is a primitive procedure already, but it serves our purposes of
illustrating this transformation:

(define div
  (lambda (a b) (/ a b)))

(define div
  (lambda (a)
    (lambda (b) (/ a b))))

   (div 4 2)
=> ((div 4) 2)
=> ((lambda (b) (/ 4 b)) 2)
=> (/ 4 2)
=> 2

Another example with the application of a function that negates a number:

> ((lambda (a b) (a b)) - 2)
-2

The curried version:
> ((lambda (a) ((lambda (b) (a b)) 2)) -)
-2

We can easily derive syntactically the curried expression so that we prove
it is an <expression>:

   <expression>
=> (<expression> <expression>)
=> (<expression> <identifier>)
=> (<expression> -)
=> ((lambda (<identifier>) <expression>) -)
=> ((lambda (a) <expression>) -)
=> ((lambda (a) (<expression> <expression>)) -)
=> ((lambda (a) (<expression> <identifier>)) -)
=> ((lambda (a) (<expression> 2)) -)
=> ((lambda (a) ((lambda (<identifier>) <expression>) 2)) -)
=> ((lambda (a) ((lambda (b) <expression>) 2)) -)
=> ((lambda (a) ((lambda (b) (<expression> <expression>)) 2)) -)
=> ((lambda (a) ((lambda (b) (<identifier> <expression>)) 2)) -)
=> ((lambda (a) ((lambda (b) (a <expression>)) 2)) -)
=> ((lambda (a) ((lambda (b) (a <identifier>)) 2)) -)
=> ((lambda (a) ((lambda (b) (a b)) 2)) -)


;At this moment, we can rest assured that we do not need to extend the
;formal definition in order to support let expressions, since they are
;just derived expressions.


;The following procedure let->lambda transforms a let expression
(let ((<var1> <exp1>) ... (<varn> <expn>))
  <body>)

;into the equivalent lambda expression:
((lambda (<var1> ... <varn>)
   <body>)
 <exp1> ... <expn>)

(define let->lambda
  (lambda (exp)
    (append (list (list 'lambda
                        (map car (cadr exp))
                        (caddr exp)))
            (map cadr (cadr exp)))))

;Test
> (let->lambda '(let ((a c)) (- a)))
((lambda (a) (- a)) c)

;occurs-free? makes use of let->lambda
(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'lambda)
       (and (not (memq var (cadr exp)))
            (occurs-free? var (caddr exp))))
      ((eqv? (car exp) 'let)
       (occurs-free? var (let->lambda exp)))
      (else (or (occurs-free? var (car exp))
                (occurs-free? var (cadr exp)))))))

> (occurs-free? 'a '(let ((a c)) (- a)))
#f
> (occurs-free? 'c '(let ((a c)) (- a)))
#t
> (occurs-free? - '(let ((a c)) (- a)))
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
      (else (or (occurs-bound? var (car exp))
                (occurs-bound? var (cadr exp)))))))

> (occurs-bound? 'a '(let ((a c)) (- a)))
#t
> (occurs-bound? 'c '(let ((a c)) (- a)))
#f
> (occurs-bound? - '(let ((a c)) (- a)))
#f
