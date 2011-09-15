(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/2-3-2.scm") ;environments
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

;Lambda-calculus to the rescue. We can use a procedural implementation of
;the boolean values:

(define true
  (lambda ()
    (lambda (a b) a)))

(define false
  (lambda ()
    (lambda (a b) b)))

;if will need only to do (see eval-expression):
;(if c a b) (c a b)

;We no longer need true-value?

;the interpreter
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
           (if-exp (test-exp true-exp false-exp)
                   ((eval-expression test-exp env)
                    (eval-expression true-exp env)
                    (eval-expression false-exp env)))
           (primapp-exp (prim rands)
                        (let ((args (eval-rands rands env)))
                          (apply-primitive prim args))))))

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
           (equal-prim () (boolean-value?
                            (= (car args) (cadr args))))
           (zero-prim () (boolean-value?
                           (zero? (car args))))
           (greater-prim () (boolean-value?
                              (> (car args) (cadr args))))
           (less-prim () (boolean-value?
                           (< (car args) (cadr args)))))))

(define boolean-value?
  (lambda (test)
    (if test (true) (false))))

(define init-env
  (lambda ()
    (extend-env
      '(i v x)
      '(1 5 10)
      (empty-env))))


;Tests
;> (run "if greater?(2,3) then 5 else 6")
;6
;> (run "if greater?(4,2) then 5 else 6")
;5
;> (run "if equal?(4,2) then add1(4) else +(1,4)")
;5
