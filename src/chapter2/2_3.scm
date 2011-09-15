(define vector-of
  (lambda (pred)
    (define vector-iter
      (lambda (the-vector pred)
        (define iter-helper
          (lambda (v i)
            (cond ((= i (vector-length v)) #t)
                  ((not (pred (vector-ref v i))) #f)
                  (else (iter-helper v (+ i 1))))))
        (iter-helper the-vector 0)))
    (lambda (val)
      (and (vector? val)
           (vector-iter val pred)))))

;> ((vector-of number?) '#(3 1 4 1 5 9))
;#t
;> ((vector-of number?) '#(3 1 4 1 5 b))
;#f
;> ((vector-of number?) '#())
;#t

;Now, we could do this:

(define-datatype vector vector?
                 (a-vector
                   (data (vector-of symbol-exp?))))
