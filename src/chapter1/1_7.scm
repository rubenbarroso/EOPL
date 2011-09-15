;(load "interps/r5rs.scm") <-- an issue with the scheme IDE does not allow to load external definitions. I can't use eopl:error

(define nth-elt
  (lambda (lst n)
    (letrec ((process-first
               (lambda (rest i)
                 (if (zero? i) (car rest)
                     (process-rest (cdr rest) (- i 1)))))
             (process-rest
               (lambda (rest i)
                 (if (null? rest)
                     (begin
                       (display lst)
                       (display " does not have an element ")
                       (display n)
                       (newline))
                     (process-first rest i)))))
      (process-rest lst n))))

;> (nth-elt '(a b c) 3)
;(a b c) does not have an element 3