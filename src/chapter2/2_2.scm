;We will analyze the implementations from a client's viewpoint, plus in this case:

(define plus
  (lambda (x y)
    (if (iszero? x)
        y
        (succ (plus (pred x) y)))))

;Unary reprentation
(define zero '())
(define iszero? null?)
(define succ
  (lambda (n) (cons #t n)))
(define pred cdr)

;The unary representation is the simplest of our implementation since it just maintains a list of #t's
;representing the positive integers. All four operation implementations have constant access time.
;The downside is the space required, lineal in the number represented. This becomes unsustainable for
;large numbers.


;Scheme number representation
(define zero 0)
(define iszero? zero?)
(define succ
  (lambda (n) (+ n 1)))
(define pref
  (lambda (n) (- n 1)))

;Here we are just delegating the implementation to that of Scheme. We are actually introducing a new
;abstraction layer that may become helpful in the case we need to change the representation of the
;data specification without affecting the clients (e.g. for performance purposes).
;The space required to represent a number is likely optimized on the underlying Scheme internal
;representation, especially for big numbers.
;It seems that, since pred delegates to Scheme's internal representation, (pred zero) returns -1 and
;this would invalidate the implementation. But as stated in the text, the specifications says
;nothing about (pred zero) and any behavior is acceptable.

;Bignum representation
;The implementation of the operations can be found in exercise 2.1., along with related comments.
;As explained in the text: "The most efficient representation is often a lot more difficult to
;implement, so we may wish to develop a simple implementation first and only change to a more
;efficient representation if it proves critical to the overall performance of a system". We verified
;this when we implemented bigints, as it turned out that it took a lot more time to develop a
;working implementation, albeit more efficient.

;Church numerals
;In exercise 2.6 of SICP, we implement a representation of the nonnegative integers using only
;lambda expressions:

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))

;As we can see, we just need the concept of lambda expressions to represent the positives.



