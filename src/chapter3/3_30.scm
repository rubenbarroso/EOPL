(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/2-3-3.scm") ;environments
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")

(define scanner-spec-3-13
  '((white-sp
      (whitespace) skip)
    (comment
      ("%" (arbno (not #\newline))) skip)
    (identifier
      (letter (arbno (or letter digit "?"))) symbol)
    (number
      (digit (arbno digit)) number)))

(define grammar-3-13
  '((program
      (expression)
      a-program)
    (expression
      (number)
      lit-exp)
    (expression
      (identifier)
      var-exp)
    (expression
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp)
    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)
    (expression
      ("let" (arbno identifier "=" expression) "in" expression)
      let-exp)
    (expression
      ("proc" "(" (separated-list identifier ",") ")" expression)
      proc-exp)
    (expression
      ("(" expression (arbno expression) ")")
      app-exp)
    (primitive
      ("+")
      add-prim)
    (primitive
      ("-")
      substract-prim)
    (primitive
      ("*")
      mult-prim)
    (primitive
      ("add1")
      incr-prim)
    (primitive
      ("sub1")
      decr-prim)
    (primitive
      ("equal?")
      equal-prim)
    (primitive
      ("zero?")
      zero-prim)
    (primitive
      ("greater?")
      greater-prim)
    (primitive
      ("less?")
      less-prim)))

(define scan&parse
  (sllgen:make-string-parser
    scanner-spec-3-13
    grammar-3-13))

(sllgen:make-define-datatypes scanner-spec-3-13 grammar-3-13)

(define run
  (lambda (string)
    (eval-program
      (scan&parse string))))

;helpers
(define true-value?
  (lambda (x)
    (not (zero? x))))

; the interpreter
(define eval-program
  (lambda (pgm)
    (cases program pgm
           (a-program (body)
                      (eval-expression body (init-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
           (lit-exp (datum) datum)
           (var-exp (id) (apply-env env id))
           (primapp-exp (prim rands)
                        (let ((args (eval-rands rands env)))
                          (apply-primitive prim args)))
           (if-exp (test-exp true-exp false-exp)
                   (if (true-value? (eval-expression test-exp env))
                       (eval-expression true-exp env)
                       (eval-expression false-exp env)))
           (let-exp (ids rands body)
                    (let ((args (eval-rands rands env)))
                      (eval-expression body (extend-env ids args env))))
           (proc-exp (ids body) (procedure ids body))
           (app-exp (rator rands)
                    (let ((proc (eval-expression rator env))
                          (args (eval-rands rands env)))
                      (proc args env))))))

;defined-language procedures
(define procedure
  (lambda (ids body)
    (lambda (args env)
      (eval-expression body (extend-env ids args env)))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
           (add-prim () (+ (car args) (cadr args)))
           (substract-prim () (- (car args) (cadr args)))
           (mult-prim () (* (car args) (cadr args)))
           (incr-prim () (+ (car args) 1))
           (decr-prim () (- (car args) 1))
           (equal-prim () (if (= (car args) (cadr args)) 1 0))
           (zero-prim () (if (zero? (car args)) 1 0))
           (greater-prim () (if (> (car args) (cadr args)) 1 0))
           (less-prim () (if (< (car args) (cadr args)) 1 0)))))

(define init-env
  (lambda ()
    (extend-env
      '(i v x)
      '(1 5 10)
      (empty-env))))

;With the lexical-scoped interpreter of figure 3.7, the value of a in the
;procedure body is bound to 3, not 5:
;> (run
;    "let a = 3
;     in let p = proc (x) +(x,a)
;            a = 5
;        in *(a, (p 2))")
;25

;With dynamic scoping:
;> (run
;    "let a = 3
;     in let p = proc (x) +(x,a)
;            a = 5
;        in *(a, (p 2))")
;35

;The defined-language procedures do not contain any free variables. This is
;a characteristic of closures.