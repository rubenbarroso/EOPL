;This version of remove-first:

(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (remove-first s (cdr los))))))

;would return the list of symbols after the first occurrence of s, or the empty list if s is not in los.

;Let's check it:

;> (remove-first 'a '(f g a j k m))
;(j k m)
;> (remove-first 'a '())
;()
;> (remove-first 'a '(a b c d e))
;(b c d e)
;> (remove-first 'a '(a b c d e a b))
;(b c d e a b)
;> (remove-first 'p '(a l k))
;()