(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/2-2-1.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/2-2-2.scm")

;We will use the following utility procedure to remove duplicates from
;a list:
(define remove-duplicates
  (lambda (lst)
    (cond ((null? lst) '())
          ((memv (car lst) (cdr lst))
           (remove-duplicates (cdr lst)))
          (else (cons (car lst)
                      (remove-duplicates (cdr lst)))))))

;> (remove-duplicates '(x y x y))
;(x y)
;> (remove-duplicates '())
;()
;> (remove-duplicates '(y))
;(y)
;> (remove-duplicates '(y u))
;(y u)

(define free-vars
  (lambda (exp)
    (let ((ast (parse-expression exp)))
      (define free-vars-iter
        (lambda (subexp)
          (cases expression subexp
                 (var-exp (id)
                          (if (occurs-free? id ast)
                              (list id)
                              '()))
                 (lambda-exp (id body)
                             (if (occurs-free? id ast)
                                 (append (list id)
                                         (free-vars-iter body))
                                 (free-vars-iter body)))
                 (app-exp (rator rand)
                          (append (free-vars-iter rator)
                                  (free-vars-iter rand))))))
      (remove-duplicates (free-vars-iter ast)))))

;> (free-vars '(lambda (x) (lambda (y) (x (y z)))))
;(z)

;This implementation of free-vars with abstract syntax is way much clearer
;because the cases form lets us to perform case analysis on the set of
;variants. In exercise 1.19, we had to implement a helper method extract-vars
;that did not know anything about the syntax of the stuff it needed to
;extract the variables from, hence we have a bunch of c*r procedure calls
;that make such method less understandable and more fragile to changes in
;the syntax.

;For bound-vars, we need to define occurs-bound?:

(define occurs-bound?
  (lambda (var exp)
    (cases expression exp
           (var-exp (id) #f)
           (lambda-exp (id body)
                       (or (occurs-bound? var body)
                           (and (eqv? id var)
                                (occurs-free? var body))))
           (app-exp (rator rand)
                    (or (occurs-bound? var rator)
                        (occurs-bound? var rand))))))

(define bound-vars
  (lambda (exp)
    (let ((ast (parse-expression exp)))
      (define bound-vars-iter
        (lambda (subexp)
          (cases expression subexp
                 (var-exp (id)
                          (if (occurs-bound? id ast)
                              (list id)
                              '()))
                 (lambda-exp (id body)
                             (if (occurs-bound? id ast)
                                 (append (list id)
                                         (bound-vars-iter body))
                                 (bound-vars-iter body)))
                 (app-exp (rator rand)
                          (append (bound-vars-iter rator)
                                  (bound-vars-iter rand))))))
      (remove-duplicates (bound-vars-iter ast)))))

;> (bound-vars '(lambda (x) (lambda (y) (x (y z)))))
;(x y)
