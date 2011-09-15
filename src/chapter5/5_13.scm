(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/5-3.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/5-4-4.scm")

;We simply need to include the number of the parameters of the method
;in the lookup-method method

(define lookup-method
  (lambda (m-name m-ids-length methods)
    (cond
      ((null? methods) #f)
      ((and (eqv? m-name (method->method-name (car methods)))
            (= m-ids-length (length (method->ids (car methods)))))
       (car methods))
      (else (lookup-method m-name m-ids-length (cdr methods))))))

;And change the callers accordingly

(define merge-methods
  (lambda (super-methods methods)
    (cond
      ((null? super-methods) methods)
      (else
        (let ((overriding-method
                (lookup-method
                  (method->method-name (car super-methods))
                  (length (method->ids (car super-methods)))
                  methods)))
          (if (method? overriding-method)
            (cons overriding-method
              (merge-methods (cdr super-methods)
                (remove-method overriding-method methods)))
            (cons (car super-methods)
              (merge-methods (cdr super-methods)
                 methods))))))))

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (let ((method (lookup-method
                    m-name
                    (length args)
                    (class-name->methods host-name))))
      (if (method? method)
        (apply-method method host-name self args)
        (eopl:error 'find-method-and-apply
          "No method for name ~s" m-name)))))

;Test
;> (run
;    "class test extends object
;       method initialize () 1
;       method a () 3
;       method a (x) +(x,5)
;     let t = new test()
;     in +(send t a(), send t a(5))")
;13
