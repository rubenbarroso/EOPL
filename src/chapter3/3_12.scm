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
                 (primapp-exp
                   (prim primitive?)
                   (rands (list-of expression?))))

(define-datatype primitive primitive?
                 (add-prim)
                 (substract-prim)
                 (mult-prim)
                 (incr-prim)
                 (decr-prim)
                 (list-prim)
                 (cons-prim)
                 (car-prim)
                 (cdr-prim)
                 (null-prim))
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
               (decr-prim))
              ((eqv? prim 'list)
               (list-prim))
              ((eqv? prim 'cons)
               (cons-prim))
              ((eqv? prim 'car)
               (car-prim))
              ((eqv? prim 'cdr)
               (cdr-prim))
              ((eqv? prim 'null?)
               (null-prim)))))
    (define parse-expression
      (lambda (exp)
        (cond ((number? exp)
               (lit-exp exp))
              ((symbol? exp)
               (var-exp exp))
              ((pair? exp)
               (primapp-exp (parse-primitive (car exp))
                            (map (lambda (rand)
                                   (parse-expression rand))
                                 (cdr exp)))))))
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
           (list-prim () args)
           (cons-prim () (cons (car args) (cadr args)))
           (car-prim () (caar args))
           (cdr-prim () (cdar args))
           (null-prim () (if (null? (car args)) 1 0)))))

(define init-env
  (lambda ()
    (extend-env
      '(i v x emptylist)
      '(1 5 10 ())
      (empty-env))))

(define run
  (lambda (x)
    (eval-program (parse-program x))))

;Tests
;> (run '(null? (list 1 2 3)))
;0
;> (run '(null? (cons 4 emptylist)))
;0
;> (run '(null? emptylist))
;1
;> (run '(null? (cdr (cons 4 emptylist))))
;1