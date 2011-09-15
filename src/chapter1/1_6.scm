;if nth-elt is passed a symbol and 0, then (car lst) will complain about lst not being a pair. If n>0, then it
;is cdr who complains about lst not being a pair. Let's check it out:

(define nth-elt
  (lambda (lst n)
    (if (null? lst)
        (error "lst too short")
        (if (zero? n)
            (car lst)
            (nth-elt (cdr lst) (- n 1))))))

;> (nth-elt 'symbol 2)
;Error in cdr: expected type pair, got 'symbol'.
;file:/Users/ruben/IdeaProjects/EOPL/src/1_6.scm:9:22: <from call to cdr>
;[previous entry repeated twice]

;> (nth-elt 'symbol 0)
;Error in car: expected type pair, got 'symbol'.
;file:/Users/ruben/IdeaProjects/EOPL/src/1_6.scm:8:13: <from call to car>


;With list-length, cdr complains:

(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (list-length (cdr lst))))))

;> (list-length 'symbol)
;Error in cdr: expected type pair, got 'symbol'.
;console:5:25: <from call to cdr>
;[previous entry repeated twice]
;console:5:12: <from call to list-length>


;Robust versions of both

(define nth-elt-robust
  (lambda (lst n)
    (cond ((null? lst) (error "lst too short"))
          ((not (pair? lst)) (error "lst not a pair"))
          ((zero? n) (car lst))
          (else (nth-elt-robust (cdr lst) (- n 1))))))

;> (nth-elt-robust '(a b) 0)
;a

;> (nth-elt-robust 'symbol 0)
;Error: lst not a pair

;> (nth-elt-robust '(a b) 2)
;Error: lst too short

(define list-length
  (lambda (lst)
    (cond ((null? lst) 0)
          ((not (pair? lst)) (error "lst not a pair"))
          (else (+ 1 (list-length (cdr lst)))))))

;> (list-length '())
;0
;> (list-length '(a (b) c))
;3
;> (list-length 'symbol)
;Error: lst not a pair
