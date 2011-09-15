(define repeated-declaration?
  (lambda (new declarations)
    (cond ((null? new) #f)
          ((memq (car new) declarations) #t)
          (else (repeated-declaration? (cdr new) declarations)))))

;> (repeated-declaration? '(a b c) '(d e f g))
;#f
;> (repeated-declaration? '(a b c) '(d e f c))
;#t

(define check-subexpressions
  (lambda (sub-exps declarations)
    (cond ((null? sub-exps) #t)
          ((not (check-declarations-helper (car sub-exps) declarations)) #f)
          (else (check-subexpressions (cdr sub-exps) declarations)))))

(define check-declarations-helper
  (lambda (exp declarations)
    (cond ((symbol? exp) #t)
          ((eqv? (car exp) 'if)
           (let ((condition (check-declarations-helper (cadr exp) declarations))
                 (consequent (check-declarations-helper (caddr exp) declarations))
                 (alternative (check-declarations-helper (cadddr exp) declarations)))
             (and condition consequent alternative)))
          ((eqv? (car exp) 'lambda)
           (if (repeated-declaration? (cadr exp) declarations)
               #f
               (check-declarations-helper (caddr exp) (append (cadr exp) declarations))))
          (else (check-subexpressions exp declarations)))))

(define check-declarations
  (lambda (exp)
    (let ((ok (check-declarations-helper exp '())))
      (if (not ok)
          (error "Already declared variables")
          ok))))

;> (check-declarations '(lambda (a) (lambda (a) a)))
;Error: Already declared variables

;> (check-declarations '(lambda (a) (lambda (b) a)))
;#t

;Certainly the correct way to report the error is stating which is the
;offending declaration, maybe by bubbling up the lexical address of the
;variable declaration that was already declared in an outer level.
