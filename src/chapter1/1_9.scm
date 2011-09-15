;This version of remove:

(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (remove s (cdr los))
            (remove s (cdr los))))))

;is transformed into:

(define remove2
  (lambda (s los)
    (if (null? los)
        '()
        (remove2 s (cdr los)))))

;which returns always the empty list. Let's check it:

;> (remove2 'a '(a b c))
;()
;> (remove2 'a '())
;()
;> (remove2 'a '(b c d e))
;()
