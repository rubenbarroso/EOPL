;1
(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons x (duple (- n 1) x)))))

;> (duple 2 3)
;(3 3)
;> (duple 4 '(ho ho))
;((ho ho) (ho ho) (ho ho) (ho ho))
;> (duple 0 '(blah))
;()

;2
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (cadar lst) (caar lst))
              (invert (cdr lst))))))

;we could have used map also

;> (invert '((a 1) (a 2) (b 1) (b 2)))
;((1 a) (2 a) (1 b) (2 b))
;> (invert '())
;()

;3
(define filter-in
  (lambda (pred lst)
    (cond ((null? lst) '())
          ((pred (car lst))
           (cons (car lst) (filter-in pred (cdr lst))))
          (else (filter-in pred (cdr lst))))))

;> (filter-in number? '(a 2 (1 3) b 7))
;(2 7)
;> (filter-in symbol? '(a (b c) 17 foo))
;(a foo)

;4
(define every?
  (lambda (pred lst)
    (cond ((null? lst) #t)
          ((not (pred (car lst))) #f)
          (else (every? pred (cdr lst))))))

;> (every? number? '(a b c 3 e))
;#f
;> (every? number? '(1 2 3 5 4))
;#t

;5
;exists? is equivalent to asking if it is not the case that all the elements in lst do not satisfy pred
(define exists?
  (lambda (pred lst)
    (not (every? (lambda (item)
                   (not (pred item)))
                 lst))))

;> (exists? number? '(a b c 3 e))
;#t
;> (exists? number? '(a b c d e))
;#f

;6
(define vector-index
  (lambda (pred v)
    (letrec ((helper
               (lambda (index)
                 (cond ((= index (vector-length v))
                        #f)
                       ((pred (vector-ref v index))
                        index)
                       (else (helper (+ index 1)))))))
      (helper 0))))

;> (vector-index (lambda (x) (eqv? x 'c)) '#(a b c d))
;2
;> (vector-ref '#(a b c)
;    (vector-index (lambda (x) (eqv? x 'b)) '#(a b c)))
;b
;> (vector-index (lambda (x) (eqv? x 'c)) '#(a b d))
;#f
;> (vector-index (lambda (x) (eqv? x 'c)) '#())
;#f

;7
;This is equivalent to insert-right
(define list-set
  (lambda (lst n x)
    (cond ((null? lst) '())
          ((= n 0) (cons x lst))
          (else (cons (car lst)
                      (list-set (cdr lst) (- n 1) x))))))

;> (list-set '(a b c d) 2 '(1 2))
;(a b (1 2) c d)
;> (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)
;(1 5 10)

;8
(define product
  (lambda (los1 los2)
    (if (null? los1)
        '()
        (append (map (lambda (s) (list (car los1) s))
                     los2)
                (product (cdr los1) los2)))))

;> (product '(a b c) '(x y))
;((a x) (a y) (b x) (b y) (c x) (c y))

;9
(define down
  (lambda (lst)
    (map (lambda (x) (list x)) lst)))

;> (down '(1 2 3))
;((1) (2) (3))
;> (down '((a) (fine) (idea)))
;(((a)) ((fine)) ((idea)))
;> (down '(a (more (complicated)) object))
;((a) ((more (complicated))) (object))


;10
;I tried this version first, but SISC reported that
;vector is immutable, hence I can't use vector-set!
(define vector-append-list
  (lambda (v lst)
    (letrec ((helper
               (lambda (rest)
                 (if (null? rest)
                     v
                     (begin
                       (vector-set! v
                                    (- (vector-length v) 1)
                                    (car rest))
                       (helper (cdr rest)))))))
      (helper lst))))

;Let's try again by allocating a new vector and setting its
;elements one by one:
(define vector-append-list
  (lambda (v lst)
    (define set-vector-elements!
      (lambda (v vector)
        (letrec ((helper
                   (lambda (index)
                     (if (= index (vector-length v))
                         #t
                         (begin
                           (vector-set! vector
                                        index
                                        (vector-ref v index))
                           (helper (+ index 1)))))))
          (helper 0))))
    (define set-list-elements!
      (lambda (lst vector)
        (letrec ((helper
                   (lambda (index rest)
                     (if (= index (vector-length vector))
                         #t
                         (begin
                           (vector-set! vector
                                        index
                                        (car rest))
                           (helper (+ index 1) (cdr rest)))))))
          (helper (vector-length v) lst))))
    (let ((vector (make-vector (+ (vector-length v)
                                  (length lst)))))
      (set-vector-elements! v vector)
      (set-list-elements! lst vector)
      vector)))

;> (vector-append-list '#(1 2 3) '(4 5))
;#5(1 2 3 4 5)
