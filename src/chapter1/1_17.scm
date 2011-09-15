;1
;There has to be a simpler way, who knows ...
(define path
  (lambda (n bst)
    (letrec ((finder
               (lambda (tree the-path)
                 (cond ((null? tree) '())
                       ((= (car tree) n) the-path)
                       (else (let ((found-left (finder (cadr tree)
                                                       (append the-path '(left)))))
                               (if (not (null? found-left))
                                   found-left
                                   (finder (caddr tree)
                                           (append the-path '(right))))))))))
      (finder bst '()))))

;> (path 17 '(14 (7 () (12 () ()))
;           (26 (20 (17 () ())
;                ())
;            (31 () ()))))
;(right left left)
;> (path 19 '(14 (7 () (12 () ()))
;           (26 (20 (17 () ())
;                ())
;            (31 () ()))))
;()
;> (path 21 '(21 () ()))
;()

;2
;Let's try to reuse the famous Haskell implementation of
;Quicksort:
;
; qsort []     = []
; qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++ qsort (filter (>= x) xs)
;
; Thanks Hoare!

;filter-in from exercise 1.15

(define sort
  (lambda (lon)
    (define less-than
      (lambda (n) (lambda (x) (< x n))))
    (define greater-or-equal-than
      (lambda (n) (lambda (x) (>= x n))))
    (if (null? lon)
        '()
        (append (sort (filter-in (less-than (car lon))
                                 (cdr lon)))
                (list (car lon))
                (sort (filter-in (greater-or-equal-than (car lon))
                                 (cdr lon)))))))

;(sort '(8 2 5 2 3))
;(2 2 3 5 8)

;I must confess that I feel a jealous sad comparing the conciseness of
;the Haskell implementation versus the Scheme one.

;3
;Analogous to the previous one
(define sort
  (lambda (predicate lon)
    (define pred-n
      (lambda (pred n) (lambda (x) (pred x n))))
    (define not-pred-n
      (lambda (pred n) (lambda (x) (not (pred x n)))))
    (if (null? lon)
        '()
        (append (sort predicate
                      (filter-in (pred-n predicate (car lon))
                                 (cdr lon)))
                (list (car lon))
                (sort predicate
                      (filter-in (not-pred-n predicate (car lon))
                                 (cdr lon)))))))

;> (sort < '(8 2 5 2 3))
;(2 2 3 5 8)
;> (sort > '(8 2 5 2 3))
;(8 5 3 2 2)