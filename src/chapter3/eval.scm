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
                 (print-prim)  ;exercise 3.5
                 (minus-prim)  ;exercise 3.6
                 (list-prim) ;exercise 3.7
                 (cons-prim) ;exercise 3.7
                 (car-prim) ;exercise 3.7
                 (cdr-prim) ;exercise 3.7
                 (setcar-prim)) ;exercise 3.8

;helper
(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

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
      (add-prim  () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim  () (* (car args) (cadr args)))
      (incr-prim  () (+ (car args) 1))
      (decr-prim  () (- (car args) 1))
      (print-prim ()
                  (display (car args))  ;exercise 3.5
                  (newline)
                  1)
      (minus-prim () (- (car args))) ;exercise 3.6
      (list-prim () args) ;exercise 3.7
      (cons-prim () (cons (car args) (cadr args))) ;exercise 3.7
      (car-prim () (caar args)) ;exercise 3.7
      (cdr-prim () (cdar args)) ;exercise 3.7
      (setcar-prim () (set-car! (car args) (cadr args)))))) ;exercise 3.8

(define init-env
  (lambda ()
    (extend-env
      '(i v x emptylist) ;exercise 3.7
      '(1 5 10 ())
      (empty-env))))
