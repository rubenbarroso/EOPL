(define parse
  (lambda (spec)
    (cond ((null? spec) '())
          ((pair? (car spec))
           (cons (parse (car spec))
                 (parse (cdr spec))))
          ((eq? (car spec) 'int)
           (cons 'int-type
                 (parse (cdr spec))))
          ((eq? (car spec) 'bool)
           (cons 'bool-type
                 (parse (cdr spec))))
          ((eq? (car spec) '*)
           (parse (cdr spec)))
          ((eq? (car spec) '->)
           (parse (cdr spec)))
          (else eopl:error
                'parse
                "Wrong type element: ~s" (car spec)))))

(define primitive-type-parser
  (lambda (spec-lst)
    (cons (reverse (parse (cddr (reverse spec-lst))))
          (parse (list (car (reverse spec-lst)))))))

;> (primitive-type-parser '(int * int -> int))
;((int-type int-type) int-type)

(define prim-proc-type
  (lambda (spec-lst)
    (let ((type-spec (primitive-type-parser spec-lst)))
      (proc-type (car (type-spec)) (cadr (type-spec))))))

(define type-of-primitive
  (lambda (prim)
    (cases primitive prim
           (add-prim ()
                     (prim-proc-type '(int * int - > int)))
           (subtract-prim ()
                          (prim-proc-type '(int -> int)))
           (mult-prim ()
                      (prim-proc-type '(int * int - > int)))
           (incr-prim ()
                      (prim-proc-type '(int -> int)))
           (decr-prim ()
                      (prim-proc-type '(int -> int)))
           (zero-test-prim ()
                           (prim-proc-type '(int -> bool)))
           )))
