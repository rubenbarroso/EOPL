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
           (lexvar-exp (id depth pos) (apply-env env id))
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

;> (lexical-address-calc
;            (scan&parse
;              "let x = 1
;                   y = 2
;                in let a = +(x,2)
;                       p = proc (a, b) -(a, b)
;                    in (p a +(x,y))"))
;(a-program
;  (let-exp
;    (x y)
;    ((lit-exp 1) (lit-exp 2))
;    (let-exp
;      (a p)
;      ((primapp-exp
;         (add-prim)
;         ((lexvar-exp x 0 0) (lit-exp 2)))
;       (proc-exp
;         (a b)
;         (primapp-exp
;           (substract-prim)
;           ((lexvar-exp a 0 0) (lexvar-exp b 0 1)))))
;      (app-exp
;        (lexvar-exp p 0 1)
;        ((lexvar-exp a 0 0)
;         (primapp-exp
;           (add-prim)
;           ((lexvar-exp x 1 0) (lexvar-exp y 1 1))))))))