(define list-of-numbers?
  (lambda (lst)
    (cond ((null? lst) #t)
          ((not (pair? lst)) #f)
          (else
            (and
              (number? (car lst))
              (list-of-numbers? (cdr lst)))))))

;> (list-of-numbers? '(1 4))
;#t
;> (list-of-numbers? '(1 a))
;#f
;> (list-of-numbers? '(1 (a)))
;#f
;> (list-of-numbers? 5)
;#f
;> (list-of-numbers? '(3))
;#t
