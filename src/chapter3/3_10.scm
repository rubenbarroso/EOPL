(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/2-3-2.scm") ;environments

;the data types
(define-datatype program program?
                 (a-program
                   (exp expression?)))

(define-datatype expression expression?
                 (lit-exp
                   (datum number?))
                 (var-exp
                   (id symbol?))
                 (if-exp
                   (test-exp expression?)
                   (true-exp expression?)
                   (false-exp expression?))
                 (primapp-exp
                   (prim primitive?)
                   (rands (list-of expression?))))

(define-datatype primitive primitive?
                 (add-prim)
                 (substract-prim)
                 (mult-prim)
                 (incr-prim)
                 (decr-prim))

;helper
(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

;parser
(define parse-program
  (lambda (src)
    (define parse-primitive
      (lambda (prim)
        (cond ((eqv? prim '+)
               (add-prim))
              ((eqv? prim '-)
               (substract-prim))
              ((eqv? prim '*)
               (mult-prim))
              ((eqv? prim 'add1)
               (incr-prim))
              ((eqv? prim 'sub1)
               (decr-prim)))))
    (define parse-expression
      (lambda (exp)
        (cond ((number? exp)
               (lit-exp exp))
              ((symbol? exp)
               (var-exp exp))
              ((pair? exp)
               (cond ((eqv? (car exp) 'if)
                      (if-exp (parse-expression (cadr exp))
                              (parse-expression (caddr exp))
                              (parse-expression (cadddr exp))))
                     (else
                       (primapp-exp (parse-primitive (car exp))
                                    (map (lambda (rand)
                                           (parse-expression rand))
                                         (cdr exp)))))))))
    (a-program (parse-expression src))))

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
              (if (true-value? (eval-expression test-exp env))
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
      (add-prim  () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim  () (* (car args) (cadr args)))
      (incr-prim  () (+ (car args) 1))
      (decr-prim  () (- (car args) 1)))))

(define init-env
  (lambda ()
    (extend-env
      '(i v x)
      '(1 5 10)
      (empty-env))))

(define true-value?
  (lambda (x)
    (not (zero? x))))

(define run
  (lambda (x)
    (eval-program (parse-program x))))

;Tests
;> (run '(if 1 2 3))
;2
;> (run '(if (- 3 (+ 1 2)) 2 3))
;3