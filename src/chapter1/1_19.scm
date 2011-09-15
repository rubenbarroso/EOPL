(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'lambda)
       (and (not (eqv? (caadr exp) var))
            (occurs-free? var (caddr exp))))
      (else (or (occurs-free? var (car exp))
                (occurs-free? var (cadr exp)))))))

(define occurs-bound?
  (lambda (var exp)
    (cond
      ((symbol? exp) #f)
      ((eqv? (car exp) 'lambda)
       (or (occurs-bound? var (caddr exp))
           (and (eqv? (caadr exp) var)
                (occurs-free? var (caddr exp)))))
      (else (or (occurs-bound? var (car exp))
                (occurs-bound? var (cadr exp)))))))

(define extract-vars
  (lambda (exp)
    (define collector
      (lambda (exp vars)
        (cond ((symbol? exp)
               (if (not (memq exp vars))
                   (cons exp vars)))
              ((eqv? (car exp) 'lambda)
               (collector (caddr exp) vars))
              (else (append (collector (car exp) vars)
                            (collector (cadr exp) vars))))))
    (collector exp '())))

;> (extract-vars '(lambda (x) (lambda (y) (x (y z)))))
;(x y z)

;We will use filter-in from exercise 1.15
(define filter-in
  (lambda (pred lst)
    (cond ((null? lst) '())
          ((pred (car lst))
           (cons (car lst) (filter-in pred (cdr lst))))
          (else (filter-in pred (cdr lst))))))

(define free-vars
  (lambda (exp)
    (filter-in (lambda (var)
                 (occurs-free? var exp))
               (extract-vars exp))))

;> (free-vars '(lambda (x) (lambda (y) (x (y z)))))
;(z)

(define bound-vars
  (lambda (exp)
    (filter-in (lambda (var)
                 (occurs-bound? var exp))
               (extract-vars exp))))

;> (bound-vars '(lambda (x) (lambda (y) (x (y z)))))
;(x y)
