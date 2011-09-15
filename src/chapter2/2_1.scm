;Let us encapsulate the operations of the interface into a message-accepting
;object so that we can represent the nonnegative integers with the given
;base.

(define positives
  (lambda (base)
    (define zero '())
    (define iszero? null?)
    (define succ
      (lambda (n)
        (if (iszero? n)
            (cons 1 zero)
            (if (= (car n) (- base 1))
                (cons 0 (succ (cdr n)))
                (cons (+ 1 (car n)) (cdr n))))))
    (define pred
      (lambda (n)
        (letrec ((loop
                   (lambda (c)
                     (if (equal? (succ c) n)
                         c
                         (loop (succ c))))))
          (loop zero))))
    (lambda (m) ;dispatch
                (cond ((eqv? m 'zero) zero)
                      ((eqv? m 'iszero?) iszero?)
                      ((eqv? m 'succ) succ)
                      ((eqv? m 'pred) pred)
                      (else (error "Unknown method" m))))))


;We now publish the operations using base 10 to test the implementation
(define zero
  ((positives 10) 'zero))

;> zero
;()

(define iszero?
  ((positives 10) 'iszero?))

;> (iszero? (succ zero))
;#f
;> (iszero? zero)
;#t

(define succ
  ((positives 10) 'succ))

;> (succ (succ zero))
;(2)
;> (succ (succ (succ (succ (succ zero)))))
;(5)
;> (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ zero))))))))))
;(0 1)

(define pred
  ((positives 10) 'pred))

;> (pred (succ zero))
;()
;> (pred (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ zero)))))))))))
;(9)

(define plus
  (lambda (x y)
    (if (iszero? x)
        y
        (succ (plus (pred x) y)))))

;> (plus zero (succ zero))
;(1)
;> (plus (succ zero) (succ zero))
;(2)
;> (plus zero zero)
;()

(define mul
  (lambda (x y)
    (cond ((iszero? x) zero)
          ((iszero? y) zero)
          ((iszero? (pred x)) y)
          (else (plus y (mul (pred x) y))))))

;> (mul (succ zero) (succ zero))
;(1)
;> (mul (succ (succ zero)) (succ (succ zero)))
;(4)
;> (mul (succ zero) zero)
;()
;> (define nine (succ (succ (succ (succ (succ (succ (succ (succ (succ zero))))))))))
;> (mul nine nine)
;(1 8)

(define fact
  (lambda (n)
    (cond ((iszero? n) (succ zero))
          ((iszero? (pred n)) (succ zero))
          (else (mul n (fact (pred n)))))))

;> (fact zero)
;(1)
;> (fact (succ zero))
;(1)
;> (fact (succ (succ zero)))
;(2)
;> (fact (succ (succ (succ zero))))
;(6)

;> (define four (succ (succ (succ (succ zero)))))
;> (time (fact four))
;((4 2) (0 ms))

;> (define five (succ (succ (succ (succ (succ zero))))))
;> (time (fact five))
;((0 2 1) (3 ms))

;> (define six (succ (succ (succ (succ (succ (succ zero)))))))
;> (time (fact six))
;((0 2 7) (82 ms))

;The following takes a lot more time to execute:
;> (define seven (succ (succ (succ (succ (succ (succ (succ zero))))))))
;> (time (fact seven))
;((0 4 0 5) (3412 ms))

;The following takes way more time (minutes):
;(define eight (succ (succ (succ (succ (succ (succ (succ (succ zero)))))))))
;> (time (fact eight))
;((0 2 3 0 4) (197938 ms))

;Calculating the factorial of 9 takes an eternity (~4 hours):
;> (define nine (succ (succ (succ (succ (succ (succ (succ (succ (succ zero))))))))))
;> (time (fact nine))
;((0 8 8 2 6 3) (14707704 ms))

;The performance of such a pred procedure is horrible. Let's try to implement
;a new procedure that is linear in the number of digits:

(define pred
  (lambda (n)
    (define p
      (lambda (c carry)
        (cond ((null? c) '())
              ((and (= (car c) 0) (= carry 1))
               (cons (- base10 1) (p (cdr c) 1)))
              ((and (= (car c) 1) (= carry 1) (null? (cdr c)))
               '())
              (else (cons (- (car c) carry) (cdr c))))))
    (p n 1)))

;> (pred zero)
;()
;> (pred (succ zero))
;()
;> (pred (succ (succ zero)))
;(1)
;> (pred '(1 1 1))
;(0 1 1)
;> (pred '(0 1 1))
;(9 0 1)
;> (pred '(0 0 1))
;(9 9)
;> (pred '(1 0 1))
;(0 9)

;Now, if this implementation behaves linearly with respect to the input as
;expected, we should see a tangible improvement over the previous executions:

;> (define four (succ (succ (succ (succ zero)))))
;> (time (fact four))
;((4 2) (0 ms))
;> (define five (succ (succ (succ (succ (succ zero))))))
;> (time (fact five))
;((0 2 1) (0 ms))
;> (define six (succ (succ (succ (succ (succ (succ zero)))))))
;> (time (fact six))
;((0 2 7) (2 ms))
;> (define seven (succ (succ (succ (succ (succ (succ (succ zero))))))))
;> (time (fact seven))
;((0 4 0 5) (12 ms))
;> (define eight (succ (succ (succ (succ (succ (succ (succ (succ zero)))))))))
;> (time (fact eight))
;((0 2 3 0 4) (97 ms))
;> (define nine (succ (succ (succ (succ (succ (succ (succ (succ (succ zero))))))))))
;> (time (fact nine))
;((0 8 8 2 6 3) (964 ms))
;> (define ten (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ zero)))))))))))
;> (time (fact ten))
;((0 0 8 8 2 6 3) (13927 ms))

;We just saw the power of specifying data via interfaces. While implementing
;pred, our main purpose was to get a correct implementation of the interface
;operation. Upon achieving this, we came across a poorly perfomant procedure
;that would not serve us. The data abstraction technique let us to
;implement the operation anew without having to change the clients: plus,
;mul and fact in our case. This is possible because the those clients were
;representation-independent.
