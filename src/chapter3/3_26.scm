(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")

;Flat environments representation from exercise 2.23, adapted to this one
(define-datatype simple-rib simple-rib?
                 (simple-rib-record
                   (syms (list-of symbol?))
                   (vals (list-of scheme-value?))))

(define scheme-value? (lambda (v) #t))

(define empty-simple-rib
  (lambda ()
    (simple-rib-record '() '())))

(define extend-simple-rib
  (lambda (syms vals rib)
    (cases simple-rib rib
           (simple-rib-record
             (syms1 vals1)
             (simple-rib-record
               (append syms syms1)
               (append vals vals1))))))

(define lookup-simple-rib
  (lambda (pos rib)
    (cases simple-rib rib
           (simple-rib-record
             (syms vals)
             (list-ref vals pos)))))

(define empty-env
  (lambda () (empty-simple-rib)))

(define extend-env
  (lambda (syms vals env)
    (extend-simple-rib syms vals env)))

(define apply-env
  (lambda (env pos)
    (if (= pos -1)
        (eopl:error 'apply-env "Error accessing free variable"))
    (lookup-simple-rib pos env)))

;Tests:
;> (apply-env
;    (extend-env
;      '(a b)
;      '(3 43)
;      (extend-env
;        '(c d)
;        '(4 21)
;        (extend-env
;          '(e)
;          '(31)
;          (empty-env))))
;    0)
;31
;> (apply-env
;    (extend-env
;      '(a b)
;      '(3 43)
;      (extend-env
;        '(c d)
;        '(4 21)
;        (extend-env
;          '(e)
;          '(31)
;          (empty-env))))
;    4)
;43

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
      ("lexvar" number)
      lexvar-exp)
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
      (lexical-address-calc
        (scan&parse string)))))

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
           (var-exp (id) (eopl:error
                           'eval-expression
                           "var-exp should not appear in the instrumented interpreter"))
           (lexvar-exp (pos) (apply-env env pos))
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
           (proc-exp (ids body) (closure ids body env))
           (app-exp (rator rands)
                    (let ((proc (eval-expression rator env))
                          (args (eval-rands rands env)))
                      (if (procval? proc)
                          (apply-procval proc args)
                          (eopl:error 'eval-expression
                                      "Attempt to apply a non-procedure ~s" proc)))))))

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
                   (env simple-rib?)))

(define apply-procval
  (lambda (proc args)
    (cases procval proc
           (closure (ids body env)
                    (if (= (length ids) (length args))
                        (eval-expression body
                                         (extend-env ids args env))
                        (eopl:error 'apply-procval
                                    "Incorrect number of arguments to procedure. Expected: ~s, Actual: ~s"
                                    (length ids) (length args)))))))


(define init-env
  (lambda ()
    (extend-env
      '(i v x)
      '(1 5 10)
      (empty-env))))

;Helper procedures from exercise 1.31

(define make-lexical-address
  (lambda (v p)
    (list v p)))

(define get-v
  (lambda (address)
    (car address)))

(define get-p
  (lambda (address)
    (cadr address)))

(define get-lexical-address
  (lambda (exp addresses)
    (define iter
      (lambda (lst)
        (cond ((null? lst) (make-lexical-address exp -1))
              ((eqv? exp (get-v (car lst))) (car lst))
              (else (get-lexical-address exp (cdr lst))))))
    (iter addresses)))

(define index-of
  (lambda (v declarations)
    (define helper
      (lambda (lst index)
        (cond ((null? lst) 'free)
              ((eqv? (car lst) v) index)
              (else (helper (cdr lst) (+ index 1))))))
    (helper declarations 0)))

(define apply-offset
  (lambda (addresses offset)
    (map (lambda (addr)
           (make-lexical-address (get-v addr) (+ offset (get-p addr))))
         addresses)))

;(apply-offset '((a 0) (b 1)) 2)
;((a 2) (b 3))

(define cross-contour
  (lambda (declarations addresses)
    (append
      (map (lambda (decl)
             (make-lexical-address decl
                                   (index-of decl declarations)))
           declarations)
      (apply-offset addresses (length declarations)))))

(define lexical-address-calc-helper
  (lambda (exp addresses)
    (cases expression exp
           (lit-exp (datum)
                    (lit-exp datum))
           (var-exp (id)
                    (let ((lexical-address (get-lexical-address id addresses)))
                      (lexvar-exp (get-p lexical-address))))
           (lexvar-exp (pos)
                       (lexvar-exp pos))
           (primapp-exp (prim rands)
                        (primapp-exp prim
                                     (map (lambda (rand)
                                            (lexical-address-calc-helper rand addresses))
                                          rands)))
           (if-exp (test-exp true-exp false-exp)
                   (if-exp test-exp true-exp false-exp))
           (let-exp (ids rands body)
                    (let-exp ids
                             (map (lambda (rand)
                                    (lexical-address-calc-helper rand addresses))
                                  rands)
                             (lexical-address-calc-helper
                               body
                               (cross-contour ids addresses))))
           (proc-exp (ids body)
                     (proc-exp ids
                               (lexical-address-calc-helper
                               body
                               (cross-contour ids addresses))))
           (app-exp (rator rands)
                    (app-exp (lexical-address-calc-helper
                               rator
                               addresses)
                             (map (lambda (rand)
                                    (lexical-address-calc-helper rand addresses))
                                  rands))))))

(define lexical-address-calc
  (lambda (pgm)
    (a-program
      (cases program pgm
             (a-program (body)
                        (lexical-address-calc-helper body '()))))))

;> (run
;    "let x = 1
;         y = 2
;      in let a = +(x,2)
;             p = proc (a, b) -(a, b)
;          in (p a +(x,y))")
;0

;> (run "let a = 1 in +(a,1)")
;2

;Since we are using lexical addresses without the variable identifier,
;we lose the ability to report such identifier in the error messages
;> (run "let a = 1 in +(a,j)")
;Error reported by apply-env:
;Error accessing free variable
;
