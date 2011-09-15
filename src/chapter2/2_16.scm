(define list-find-last-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym))
                los
                0)))

(define list-index
  (lambda (pred ls index)
    (cond ((null? ls) #f)
          ((pred (car ls))
           (if (memv (car ls) (cdr ls))
               (list-index pred (cdr ls) (+ index 1))
               index))
          (else (list-index pred (cdr ls) (+ index 1))))))

;> (list-find-last-position
;    'a
;    '(c a a b g a))
;5
;> (list-find-last-position
;    'a
;    '(c a a))
;2
;> (list-find-last-position
;    'a
;    '(c a a g))
;2
;> (list-find-last-position
;    'a
;    '(a a a))
;2
;> (list-find-last-position
;    'a
;    '())
;#f

;Both list-find-position and list-find-last-position can be used interchangeably
;when the element searched can appear at most once (i.e. 0..1). This would mean
;that we would be dealing with non-extendable environments.