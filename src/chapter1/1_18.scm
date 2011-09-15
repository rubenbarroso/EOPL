;1
(define compose
  (lambda (p1 p2)
    (lambda (x) (p1 (p2 x)))))

;> ((compose car cdr) '(a b c d))
;b

;2
;We will apply the Tenth Commandment of the Little Schemer to this
;procedure and use a collector.

(define collect
  (lambda (s slist errvalue col)
    (cond ((null? slist) (col errvalue))
          ((pair? (car slist))
           (let ((e (collect s (car slist) errvalue
                             (lambda (seen)
                               (col `(compose ,seen car))))))
             (if (eqv? e errvalue)
                 (collect s (cdr slist) errvalue
                          (lambda (seen)
                            (col `(compose ,seen cdr))))
                 e)))
          (else (if (eqv? s (car slist))
                    (col 'car)
                    (collect s (cdr slist) errvalue
                             (lambda (seen)
                               (if (eqv? seen errvalue)
                                   errvalue
                                   (col `(compose ,seen cdr))))))))))

(define car&cdr
  (lambda (s slist errvalue)
    (define identity
      (lambda (x) x))
    (collect s slist errvalue identity)))

;> (car&cdr 'a '(a b c) 'fail)
;car
;> (car&cdr 'c '(a b c) 'fail)
;(compose (compose car cdr) cdr)
;> (car&cdr 'dog '(cat lion (fish dog ()) pig) 'fail)
;(compose
;  (compose (compose (compose car cdr) car) cdr)
;  cdr)
;> (car&cdr 'dog '((fish dog ()) dog) 'fail)
;(compose (compose car cdr) car)
;> (car&cdr 'dog '((dog) fish) 'fail)
;(compose car car)
;> (car&cdr 'dog '(whale (cat) ((mouse ((dog) snake) fish) cow) (dog fish)) 'fail)
;(compose
;  (compose
;    (compose
;      (compose
;        (compose (compose (compose car car) car) cdr)
;        car)
;      car)
;    cdr)
;  cdr)
;> (car&cdr 'a '() 'fail)
;fail

;3
;Same as above, but without using compose
;This means that, say we have:
;
;> (car&cdr 'c '(a b c) 'fail)
;(compose (compose car cdr) cdr)
;
;With car&cdr2, the output would be:
;
;> (car&cdr2 'c '(a b c) 'fail)
;(car (cdr (cdr)))

;The helper procedure insert-depth append the symbol s to the deepest
;nested element in slist:

;> (insert-depth 'car '(car (car (cdr))))
;(car (car (cdr (car))))
;> (insert-depth 'cdr '(car (cdr (cdr (car)))))
;(car (cdr (cdr (car (cdr)))))

(define insert-depth
  (lambda (s slist)
    (cond ((not (pair? slist))
           (list slist (list s)))
          ((null? slist)
           (list s))
          ((null? (cdr slist))
           (insert-depth s (car slist)))
          ((not (null? (cdr slist)))
           (list (car slist)
                 (insert-depth s (cdr slist)))))))

;A collector procedure for the use of car&cdr2
(define collect2
  (lambda (s slist errvalue col)
    (cond ((null? slist) (col errvalue))
          ((pair? (car slist))
           (let ((e (collect2 s (car slist) errvalue
                              (lambda (seen)
                                (col (insert-depth 'car seen))))))
             (if (eqv? e errvalue)
                 (collect2 s (cdr slist) errvalue
                           (lambda (seen)
                             (col (insert-depth 'cdr seen))))
                 e)))
          (else (if (eqv? s (car slist))
                    (col '(car))
                    (collect2 s (cdr slist) errvalue
                              (lambda (seen)
                                (if (eqv? seen errvalue)
                                    errvalue
                                    (col (insert-depth 'cdr seen))))))))))

(define car&cdr2
  (lambda (s slist errvalue)
    (define identity
      (lambda (x) x))
    (collect2 s slist errvalue identity)))

;> (car&cdr2 'a '(a b c) 'fail)
;(car)
;> (car&cdr2 'c '(a b c) 'fail)
;(car (cdr (cdr)))
;> (car&cdr2 'dog '(cat lion (fish dog ()) pig) 'fail)
;(car (cdr (car (cdr (cdr)))))
;> (car&cdr2 'a '(b c) 'fail)
;fail
;> (car&cdr2 'pig '(cat lion (fish dog ()) pig) 'fail)
;(car (cdr (cdr (cdr))))
;> (car&cdr2 'whale '(cat lion (fish dog ()) pig) 'fail)
;fail
;> (car&cdr2 'whale '() 'fail)
;fail
