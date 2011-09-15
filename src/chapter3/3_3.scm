(load "/Users/ruben/Dropbox/EOPL/src/chapter3/eval.scm")

(define parse-program
  (lambda (src)
    (define parse-primitive
      (lambda (prim)
        (cond ((eqv? prim '+)
               (cons (add-prim) 2))
              ((eqv? prim '-)
               (cons (substract-prim) 2))
              ((eqv? prim '*)
               (cons (mult-prim) 2))
              ((eqv? prim 'add1)
               (cons (incr-prim) 1))
              ((eqv? prim 'sub1)
               (cons (decr-prim) 2))
              ((eqv? prim 'print) ;exercise 3.5
               (cons (print-prim) 1))
              ((eqv? prim 'minus) ;exercise 3.6
               (cons (minus-prim) 1))
              ((eqv? prim 'list) ;exercise 3.7
               (cons (list-prim) 3)) ;this won't work
              ((eqv? prim 'cons) ;exercise 3.7
               (cons (cons-prim) 2))
              ((eqv? prim 'car) ;exercise 3.7
               (cons (car-prim) 1))
              ((eqv? prim 'cdr) ;exercise 3.7
               (cons (cdr-prim) 1))
              ((eqv? prim 'setcar) ;exercise 3.8
               (cons (setcar-prim) 2)))))
    (define parse-expression
      (lambda (exp)
        (cond ((number? exp)
               (lit-exp exp))
              ((symbol? exp)
               (var-exp exp))
              ((pair? exp)
               (let ((primitive (parse-primitive (car exp))))
                 (if (not (= (cdr primitive)
                             (length (cdr exp))))
                     (eopl:error 'parse-primitive
                                 "Incorrect number of parameters for ~s: ~s"
                                 (car primitive) (length (cdr exp)))
                     (primapp-exp (car primitive)
                                  (map (lambda (rand)
                                         (parse-expression rand))
                                       (cdr exp)))))))))
    (a-program (parse-expression src))))

;> (parse-program '(add1 2))
;(a-program
;  (primapp-exp (incr-prim) ((lit-exp 2))))
;> (parse-program '(+ 2 1))
;(a-program
;  (primapp-exp
;    (add-prim)
;    ((lit-exp 2) (lit-exp 1))))
;> (parse-program '(+ (add1 2) (- 6 4)))
;(a-program
;  (primapp-exp
;    (add-prim)
;    ((primapp-exp (incr-prim) ((lit-exp 2)))
;     (primapp-exp
;       (substract-prim)
;       ((lit-exp 6) (lit-exp 4))))))
