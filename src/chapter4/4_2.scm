(define check-equal-type!
  (lambda (t1 t2 exp)
    (define type-equal?
      (lambda (t1 t2)
        (cases type t1
               (atomic-type (name1)
                            (cases type t2
                                   (atomic-type (name2) (eq? name1 name2))
                                   (proc-type (arg-types result-type) #f)))
               (proc-type (arg-types1 result-type1)
                          (cases type t2
                                 (atomic-type (name) #f)
                                 (proc-type (arg-types2 result-type2)
                                            (and* (lambda (x y) (type-equal? x y))
                                                  (append arg-types1 (list result-type1))
                                                  (append arg-types2 (list result-type2)))))))))
    (if (not (type-equal? t1 t2))
        (eopl:error 'check-equal-type!
                    "Types didn't match: ~s != ~s in ~%~s"
                    (type-to-external-form t1)
                    (type-to-external-form t2)
                    exp))))

;helper procedure
(define and*
  (lambda (test list1 list2)
    (cond ((null? list1) #t)
          ((not (test (car list1) (car list2))) #f)
          (else (and* test (cdr list1) (cdr list2))))))

;> (and* (lambda (x y) (> x y))
;        '(1 2 3 4 5)
;        '(2 3 4 5 6))
;#f
;
;> (and* (lambda (x y) (>= x y))
;        '(2 4 4 7 5)
;        '(2 3 4 5 5))
;#t
