;The following procedures are the same as in exercise 1.31

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


;Below are the procedures for exercise 1.32

(define get-variable
  (lambda (exp addresses)
    (cond ((eqv? 'free (cadr exp))
           (car exp))
          ((null? addresses)
           #f)
          ((and (eqv? (cadr exp) (get-d (car addresses)))
                (eqv? (caddr exp) (get-p (car addresses))))
           (get-v (car addresses)))
          (else (get-variable exp (cdr addresses))))))

;> (get-variable '(: 0 1) '((a : 1 0) (b : 0 1) (c : 1 1)))
;b

(define reference?
  (lambda (exp)
    (eqv? ': (car exp))))

;> (reference? '(: 1 0))
;#t

(define un-lexical-address-helper
  (lambda (exp addresses)
    (cond ((reference? exp)
           (get-variable exp addresses))
          ((eqv? (car exp) 'if)
           (let ((condition (un-lexical-address-helper (cadr exp) addresses))
                 (consequent (un-lexical-address-helper (caddr exp) addresses))
                 (alternative (un-lexical-address-helper (cadddr exp) addresses)))
             (if (or (not condition) (not consequent) (not alternative))
                 #f
                 (list 'if condition consequent alternative))))
          ((eqv? (car exp) 'lambda)
           (let ((lambda-body (un-lexical-address-helper (caddr exp)
                                                         (cross-contour (cadr exp) addresses))))
             (if (not lambda-body)
                 #f
                 (list 'lambda
                       (cadr exp)
                       lambda-body))))
          (else (map (lambda (subexp)
                       (un-lexical-address-helper subexp addresses))
                     exp)))))

(define un-lexical-address
  (lambda (exp)
    (un-lexical-address-helper exp '())))

;> (un-lexical-address '(lambda (a)
;                         (lambda (b c)
;                           ((: 1 0) (: 0 0) (: 0 1)))))
;(lambda (a) (lambda (b c) (a b c)))

;> (un-lexical-address '(lambda (a) (lambda (a) (: 0 1))))
;#f
