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
      ("if" bool-exp "then" expression "else" expression)
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
    (bool-exp
      ("equal?" "(" (separated-list expression ",") ")")
      equal-pred)
    (bool-exp
      ("zero?" "(" expression ")")
      zero-pred)
    (bool-exp
      ("greater?" "(" (separated-list expression ",") ")")
      greater-pred)
    (bool-exp
      ("less?" "(" (separated-list expression ",") ")")
      less-pred)))

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

;As we saw in SICP, cond can be implemented as a nest of if expressions, without
;the need to implement a new form. Here will implement it as a form, as
;required by the exercise.

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
           (if-exp (test-exp true-exp false-exp)
                   (if (true-value? (eval-bool-exp test-exp env))
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

(define eval-bool-exp
  (lambda (exp env)
    (cases bool-exp exp
           (equal-pred (args)
                       (if (= (eval-expression (car args) env)
                              (eval-expression (cadr args) env))
                           1 0))
           (zero-pred (arg)
                      (if (zero? (eval-expression arg env))
                          1 0))
           (greater-pred (args)
                         (if (> (eval-expression (car args) env)
                                (eval-expression (cadr args) env))
                             1 0))
           (less-pred (args)
                      (if (< (eval-expression (car args) env)
                             (eval-expression (cadr args) env))
                          1 0)))))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
           (add-prim () (+ (car args) (cadr args)))
           (substract-prim () (- (car args) (cadr args)))
           (mult-prim () (* (car args) (cadr args)))
           (incr-prim () (+ (car args) 1))
           (decr-prim () (- (car args) 1)))))

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

;The predicated of exercise 3.11 end up pattern-matched in eval-bool-exp.