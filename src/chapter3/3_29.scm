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
      ("traceproc" "(" (separated-list identifier ",") ")" expression)
      traceproc-exp)
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
           (proc-exp (ids body) (closure ids body env #f))
           (traceproc-exp (ids body) (closure ids body env #t))
           (app-exp (rator rands)
                    (let ((proc-name (extract-proc-name rator))
                          (proc (eval-expression rator env))
                          (args (eval-rands rands env)))
                      (if (procval? proc)
                          (apply-procval proc-name proc args)
                          (eopl:error 'eval-expression
                                      "Attempt to apply a non-procedure ~s" proc)))))))

(define extract-proc-name
  (lambda (rator)
    (cases expression rator
           (lit-exp (datum)
                    (eopl:error 'extract-proc-name
                                "Only var-exp variant supported"))
           (var-exp (id) id)
           (primapp-exp (prim rands)
                        (eopl:error 'extract-proc-name
                                    "Only var-exp variant supported"))
           (if-exp (test-exp true-exp false-exp)
                   (eopl:error 'extract-proc-name
                               "Only var-exp variant supported"))
           (let-exp (ids rands body)
                    (eopl:error 'extract-proc-name
                                "Only var-exp variant supported"))
           (proc-exp (ids body)
                     (eopl:error 'extract-proc-name
                                 "Only var-exp variant supported"))
           (traceproc-exp (ids body)
                          (eopl:error 'extract-proc-name
                                      "Only var-exp variant supported"))
           (app-exp (rator rands)
                    (eopl:error 'extract-proc-name
                                "Only var-exp variant supported")))))

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

(define-datatype procval procval?
                 (closure
                   (ids (list-of symbol?))
                   (body expression?)
                   (env environment?)
                   (trace-on boolean?)))

(define apply-procval
  (lambda (proc-name proc args)
    (cases procval proc
           (closure (ids body env trace-on)
                    (if (= (length ids) (length args))
                      (begin
                        (if trace-on
                              (begin
                                (display "Entering ")
                                (display proc-name)
                                (newline)))
                        (let ((val (eval-expression
                                     body
                                     (extend-env ids args env))))
                          (if trace-on
                              (begin
                                (display "Exiting ")
                                (display proc-name)
                                (newline)))
                          val))
                        (eopl:error 'apply-procval
                                    "Incorrect number of arguments to procedure. Expected: ~s, Actual: ~s"
                                    (length ids) (length args)))))))

(define init-env
  (lambda ()
    (extend-env
      '(i v x)
      '(1 5 10)
      (empty-env))))

;Let's check we didn't break the vanilla proc:

;> (run
;    "let makemult = proc (maker, x)
;                      if x
;                      then +(4,(maker maker -(x,1)))
;                      else 0
;     in let times4 = proc (x) (makemult makemult x)
;        in (times4 3)")
;12

;Now, times4 using traceproc:

;> (run
;    "let makemult = traceproc (maker, x)
;                      if x
;                      then +(4,(maker maker -(x,1)))
;                      else 0
;     in let times4 = traceproc (x) (makemult makemult x)
;        in (times4 3)")
;Entering times4
;Entering makemult
;Entering maker
;Entering maker
;Entering maker
;Exiting maker
;Exiting maker
;Exiting maker
;Exiting makemult
;Exiting times4
;12