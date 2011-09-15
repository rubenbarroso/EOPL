(define make-lexical-address
  (lambda (v d p)
    (list v ': d p)))

;(make-lexical-address 'a 1 2)
;> (a : 1 2)

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

;> (increment-depth '(a : 0 0))
;(a : 1 0)

(define get-lexical-address
  (lambda (exp addresses)
    (define iter
      (lambda (lst)
        (cond ((null? lst) (list exp 'free))
              ((eqv? exp (get-v (car lst))) (car lst))
              (else (get-lexical-address exp (cdr lst))))))
    (iter addresses)))

;> (get-lexical-address 'b '((a : 0 0) (b : 0 1)))
;(b : 0 1)
;> (get-lexical-address 'c '((a : 0 0) (b : 0 1)))
;(c free)
;> (get-lexical-address 'a '((a : 0 0) (b : 0 1) (a : 1 1)))
;(a : 0 0)

(define index-of
  (lambda (v declarations)
    (define helper
      (lambda (lst index)
        (cond ((null? lst) 'free)
              ((eqv? (car lst) v) index)
              (else (helper (cdr lst) (+ index 1))))))
    (helper declarations 0)))

;> (index-of 'a '(b c d e f a g))
;5
;> (index-of 'a '(b c d e f h g))
;free

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

;> (filter-bound '(a c))
;((a : 0 0) (c : 0 1))

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

;> (filter-free '(a c) '((a : 0 0) (b : 0 0)))
;((b : 1 0))

(define lexical-address-helper
  (lambda (exp addresses)
    (cond ((symbol? exp)
           (get-lexical-address exp addresses))
          ((eqv? (car exp) 'if)
           (list 'if
                 (lexical-address-helper (cadr exp) addresses)
                 (lexical-address-helper (caddr exp) addresses)
                 (lexical-address-helper (cadddr exp) addresses)))
          ((eqv? (car exp) 'lambda)
           (list 'lambda
                 (cadr exp)
                 (lexical-address-helper (caddr exp)
                                         (cross-contour (cadr exp) addresses))))
          (else (map (lambda (subexp)
                       (lexical-address-helper subexp addresses))
                     exp)))))

(define lexical-address
  (lambda (exp)
    (lexical-address-helper exp '())))

;> (lexical-address '(lambda (a b c)
;                     (if (eqv? b c)
;                         ((lambda (c)
;                            (cons a c))
;                          a)
;                         b)))
;
;(lambda (a b c)
;  (if ((eqv? free) (b : 0 1) (c : 0 2))
;    ((lambda (c) ((cons free) (a : 1 0) (c : 0 0)))
;     (a : 0 0))
;    (b : 0 1)))
