(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/2-2-1.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/2-2-2.scm")

(define all-ids
  (lambda (exp)
    (define all-ids-iter
      (lambda (exp ids)
        (cases expression exp
               (var-exp (id)
                        (if (memv id ids)
                            ids
                            (cons id ids)))
               (lambda-exp (id body)
                           (if (memv id ids)
                               (all-ids-iter body ids)
                               (all-ids-iter body (cons id ids))))
               (app-exp (rator rand)
                        (all-ids-iter rator (all-ids-iter rand ids))))))
    (all-ids-iter exp '())))

;> (all-ids (app-exp (var-exp 'w0) (var-exp 'w0)))
;(w0)
;> (all-ids (app-exp
;              (lambda-exp 'w2
;                          (app-exp (var-exp 'w1) (var-exp 'w0)))
;              (var-exp 'w3)))
;(w1 w0 w2 w3)

(define fresh-id
  (lambda (exp s)
    (let ((syms (all-ids exp)))
      (letrec
              ((loop (lambda (n)
                       (let ((sym (string->symbol
                                    (string-append s
                                                   (number->string n)))))
                         (if (memv sym syms) (loop (+ n 1)) sym)))))
        (loop 0)))))

;> (fresh-id
;    (app-exp
;      (lambda-exp 'w2
;                  (app-exp (var-exp 'w1) (var-exp 'w0)))
;      (var-exp 'w3))
;    "w")
;w4
