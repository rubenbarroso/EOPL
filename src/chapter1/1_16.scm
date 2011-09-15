;1
(define up
  (lambda (lst)
    (cond ((null? lst)
           '())
          ((pair? (car lst))
           (append (car lst) (up (cdr lst))))
          (else (cons (car lst) (up (cdr lst)))))))

;> (up '((1 2) (3 4)))
;(1 2 3 4)
;> (up '((x (y)) z))
;(x (y) z)

;Let's check that (down (up lst)) is not lst:

;> (down (up '((x (y)) z)))
;((x) ((y)) (z))

;This is because we lose information when up-ing, since the non-list
;elements are left as-is, and then down does not know if they were
;lists or not.

;2
;This is a *-function (see Little Schemer)
(define swapper
  (lambda (s1 s2 slist)
    (cond ((null? slist)
           '())
          ((pair? (car slist))
           (cons (swapper s1 s2 (car slist))
                 (swapper s1 s2 (cdr slist))))
          ((eq? s1 (car slist))
           (cons s2
                 (swapper s1 s2 (cdr slist))))
          ((eq? s2 (car slist))
           (cons s1
                 (swapper s1 s2 (cdr slist))))
          (else (cons (car slist)
                      (swapper s1 s2 (cdr slist)))))))

;> (swapper 'a 'd '(a b c d))
;(d b c a)
;> (swapper 'a 'd '(a d () c d))
;(d a () c a)
;> (swapper 'x 'y '((x) y (z (x))))
;((y) x (z (y)))

;3
(define count-occurrences
  (lambda (s slist)
    (cond ((null? slist) 0)
          ((pair? (car slist))
           (+ (count-occurrences s (car slist))
              (count-occurrences s (cdr slist))))
          ((eqv? s (car slist))
           (+ 1 (count-occurrences s (cdr slist))))
          (else (count-occurrences s (cdr slist))))))

;> (count-occurrences 'x '((f x) y (((x z) x))))
;3
;> (count-occurrences 'x '((f x) y (((x z) () x))))
;3
;> (count-occurrences 'w '((f x) y (((x z) z))))
;0

;4
(define flatten
  (lambda (slist)
    (cond ((null? slist) '())
          ((null? (car slist)) (flatten (cdr slist)))
          ((pair? (car slist))
           (append (flatten (car slist))
                   (flatten (cdr slist))))
          (else (cons (car slist)
                      (flatten (cdr slist)))))))

;> (flatten '((a) () (b ()) () (c)))
;(a b c)
;> (flatten '(a b c))
;(a b c)
;> (flatten '((a) () (b ()) () (c)))
;(a b c)
;> (flatten '((a b) c (((d)) e)))
;(a b c d e)
;> (flatten '(a b (() (c))))
;(a b c)

;5
(define merge
  (lambda (lon1 lon2)
    (cond ((null? lon1) lon2)
          ((null? lon2) lon1)
          ((<= (car lon1) (car lon2))
           (cons (car lon1)
                 (merge (cdr lon1) lon2)))
          (else (cons (car lon2)
                      (merge lon1 (cdr lon2)))))))

;> (merge '(1 4) '(1 2 8))
;(1 1 2 4 8)
;> (merge '(35 62 81 90 91) '(3 83 85 90))
;(3 35 62 81 83 85 90 90 91)
