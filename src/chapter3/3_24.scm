(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")

;environment procedures reflecting the change to support lexical addresses
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vals (list-of scheme-value?))
    (env environment?)))

(define scheme-value? (lambda (v) #t))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define apply-env
  (lambda (env sym depth pos)
    (cases environment env
      (empty-env-record ()
        (if (= pos -1)
            (display "Variable found in its expected lexical address (free): ")
            (display "Variable not found in its expected lexical address: "))
        (display sym)
        (newline)
        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((rib-pos (list-find-position sym syms)))
          (if (number? rib-pos)
            (let ((val (list-ref vals rib-pos)))
              (if (and (= depth 0) (= rib-pos pos))
                  (display "Variable found in its expected lexical address: ")
                  (display "Variable not found in its expected lexical address: "))
              (display sym)
              (newline)
              val)
            (apply-env env sym (- depth 1) pos)))))))

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
      ("lexvar" "(" identifier number number ")")
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
           (lexvar-exp (id depth pos) (apply-env env id depth pos))
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
                   (env environment?)))

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

;We must recall that the following expression:
;(let ((a 1) (b 2))
;  (+ a b))
;
;is equivalent to:
;((lambda (a b) (+ a b)) 1 2)

;Helper procedures from exercise 1.31

(define make-lexical-address
  (lambda (v d p)
    (list v ': d p)))

(define get-v
  (lambda (address)
    (car address)))

(define get-d
  (lambda (address)
    (caddr address)))

(define get-p
  (lambda (address)
    (cadddr address)))

(define increment-depth
  (lambda (address)
    (make-lexical-address (get-v address)
                          (+ 1 (get-d address))
                          (get-p address))))

;we will represent free variables with (v -1 -1)
(define get-lexical-address
  (lambda (exp addresses)
    (define iter
      (lambda (lst)
        (cond ((null? lst) (make-lexical-address exp -1 -1))
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

(define cross-contour
  (lambda (declarations addresses)
    (let ((bound (filter-bound declarations))
          (free (filter-free declarations addresses)))
      (append bound free))))

(define filter-bound
  (lambda (declarations)
    (map (lambda (decl)
           (make-lexical-address decl
                                 0
                                 (index-of decl declarations)))
         declarations)))

(define filter-free
  (lambda (declarations addresses)
    (define iter
      (lambda (lst)
        (cond ((null? lst) '())
              ((not (memq (get-v (car lst)) declarations))
               (cons (increment-depth (car lst))
                     (iter (cdr lst))))
              (else (iter (cdr lst))))))
    (iter addresses)))

;Exercise - Lexical address calculator

(define lexical-address-calc-helper
  (lambda (exp addresses)
    (cases expression exp
           (lit-exp (datum)
                    (lit-exp datum))
           (var-exp (id)
                    (let ((lexical-address (get-lexical-address id addresses)))
                      (lexvar-exp (get-v lexical-address)
                                  (get-d lexical-address)
                                  (get-p lexical-address))))
           (lexvar-exp (id depth pos)
                       (lexvar-exp id depth pos))
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

;> (run "let a = 1 in +(a,1)")
;Variable found in its expected lexical address: a
;2

;> (run "let a = 1 in +(a,b)")
;Variable found in its expected lexical address: a
;Variable found in its expected lexical address (free): b
;Error reported by apply-env:
;No binding for b

;> (run
;    "let x = 1
;         y = 2
;      in let a = +(x,2)
;             p = proc (a, b) -(a, b)
;          in (p a +(x,y))")
;Variable found in its expected lexical address: x
;Variable found in its expected lexical address: a
;Variable found in its expected lexical address: x
;Variable found in its expected lexical address: y
;Variable found in its expected lexical address: p
;Variable found in its expected lexical address: a
;Variable found in its expected lexical address: b
;0
