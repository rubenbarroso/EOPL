(load "/Users/ruben/Dropbox/EOPL/src/interps/3-8need.scm")

;Random numbers support
(require-library 'sisc/libs/srfi/srfi-27)
(import srfi-27)

;This is explained very clearly in SICP, creating a procedural
;implementation of pairs:
;
;(define (cons x y)
;  (define (dispatch m)
;    (cond ((= m 0) x)
;          ((= m 1) y)
;          (else (error "Argument not 0 or 1 -- CONS" m))))
;  dispatch)
;
;(define (car z) (z 0))
;
;(define (cdr z) (z 1))

;We are actually asked to implement delayed lists. This is also explained
;in SICP when we deal with streams.

;We introduce the new primitives into the grammar
(define the-grammar
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp)
    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)
   (expression
      ("let" (arbno  identifier "=" expression) "in" expression)
      let-exp)
    (expression
      ("proc" "(" (separated-list identifier ",") ")" expression)
      proc-exp)
    (expression
      ("(" expression (arbno expression) ")")
      app-exp)
    (expression ("set" identifier "=" expression) varassign-exp)
    (expression
      ("begin" expression (arbno ";" expression) "end")
      begin-exp)
    (expression
      ("letrec"
        (arbno identifier "(" (separated-list identifier ",") ")"
          "=" expression)
        "in" expression)
      letrec-exp)

    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("zero?") zero-test-prim)

    (primitive ("conz") conz-prim)
    (primitive ("caz") caz-prim)
    (primitive ("cdz") cdz-prim)
    (primitive ("random") random-prim)

    ))

;When primapp-exp, we do not automatically evaluate the operands.
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (varassign-exp (id rhs-exp)
        (begin
          (setref!
            (apply-env-ref env id)
            (eval-expression rhs-exp env))
          1))
      (primapp-exp (prim rands)
          (apply-primitive prim rands env))
      (if-exp (test-exp true-exp false-exp)
        (if (true-value? (eval-expression test-exp env))
          (eval-expression true-exp env)
          (eval-expression false-exp env)))
      (proc-exp (ids body) (closure ids body env))
      (begin-exp (exp1 exps)
        (let loop ((acc (eval-expression exp1 env))
                   (exps exps))
          (if (null? exps) acc
            (loop (eval-expression (car exps) env) (cdr exps)))))
      (let-exp (ids rands body)
        (let ((args (eval-let-exp-rands rands env)))
          (eval-expression body (extend-env ids args env))))
      (app-exp (rator rands)
        (let ((proc (eval-expression rator env))
              (args (eval-rands rands env)))
          (if (procval? proc)
            (apply-procval proc args)
            (eopl:error 'eval-expression
              "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
        (eval-expression letrec-body
          (extend-env-recursively proc-names idss bodies env)))
      )))

;In http://www.cs.aau.dk/~normark/prog3-03/html/notes/eval-order_themes-delay-stream-section.html#eval-order_delay-force_title_1
;we can find the following explanation:
;
;"The thing to notice is the semantic idea behind the implementation of delay.
; The expression (delay expr) is equivalent to the expression (lambda () expr).
; The first expression is supposed to replace the other expression at program
; source level. The value of the lambda expression is a closure, cf.
; Section 8.11, which captures free names in its context together with the
; syntactic form of expr. As it appears from the definition of the function
; force in Program 21.1 the promise returned by the delay form is redeemed
; by calling the parameter less function object. It is easy to see that this
; carries out the evaluation of expr.
;
; Be sure to observe that force can be implemented by a function, whereas delay
; cannot. The reason is, of course, that we cannot allow a functional
; implementation of delay to evaluate the parameter of delay. The whole point
; of delay is to avoid such evaluation. This rules out an implementation of
; delay as a function. The force primitive, on the other hand, can be
; implemented by a function, because it works on the value of a lambda
; expression."

;These are the implementations of lazy delay and force given in SICP:
;
;(define delay
;  (lambda (exp)
;    (memo-proc (lambda () exp))))
;
;(define force
;  (lambda (delayed-object)
;    (eopl:printf "forcing: ~s ~%" delayed-object)
;    (delayed-object)))
;
;(define memo-proc
;  (lambda (proc)
;    (let ((already-run? #f)
;          (result #f))
;      (lambda ()
;        (if (not already-run?)
;            (begin
;              (set! result (proc))
;              (set! already-run? #t)
;              (eopl:printf "new result: ~s ~%" result)
;              result)
;            (begin
;              (eopl:printf "cached result: ~s ~%" result)
;              result))))))

;We will use the implementations of delay and force given by the Scheme
;implementation.

(define apply-primitive
  (lambda (prim rands env)
    (cases primitive prim
      (add-prim ()
                (let ((args (eval-primapp-exp-rands rands env)))
                  (+ (car args) (cadr args))))
      (subtract-prim ()
                     (let ((args (eval-primapp-exp-rands rands env)))
                       (- (car args) (cadr args))))
      (mult-prim ()
                 (let ((args (eval-primapp-exp-rands rands env)))
                   (* (car args) (cadr args))))
      (incr-prim ()
                 (let ((args (eval-primapp-exp-rands rands env)))
                   (+ (car args) 1)))
      (decr-prim ()
                 (let ((args (eval-primapp-exp-rands rands env)))
                   (- (car args) 1)))
      (zero-test-prim ()
                      (let ((args (eval-primapp-exp-rands rands env)))
                        (if (zero? (car args)) 1 0)))

      (conz-prim ()
                 (cons (delay (car (eval-primapp-exp-rands rands env)))
                       (delay (cadr (eval-primapp-exp-rands rands env)))))
      (caz-prim ()
                (let ((args (eval-primapp-exp-rands rands env)))
                  (force (caar args))))
      (cdz-prim ()
                (let ((args (eval-primapp-exp-rands rands env)))
                  (force (cdar args))))
      (random-prim ()
                   (let ((args (eval-primapp-exp-rands rands env)))
                     (eopl:printf "calling random integer ~%")
                     (random-integer (car args))))
      )))

;We now also accept pairs
(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (pair? x))))

;Tests
;
;With call-by-need, the program always returns 1:
;
;> (run
;      "let lz = conz(random(10),0)
;       in let u = caz(lz)
;          in zero?(-(caz(lz),u))")
;1
;> (run
;      "let lz = conz(random(10),0)
;       in let u = caz(lz)
;          in zero?(-(caz(lz),u))")
;1
;> (run
;      "let lz = conz(random(10),0)
;       in let u = caz(lz)
;          in zero?(-(caz(lz),u))")
;1
;
;...
