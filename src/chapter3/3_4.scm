(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/2-3-2.scm") ;environments
(load "/Users/ruben/Dropbox/EOPL/src/chapter3/3_3.scm")
(load "/Users/ruben/Dropbox/EOPL/src/chapter3/eval.scm")

(define run
  (lambda (x)
    (eval-program (parse-program x))))

;> (run '(add1 3))
;4
;> (run '5)
;5
;> (run '(+ 2 1))
;3
;> (run '(+ (add1 2) (- 6 4)))
;5

(define read-eval-print
  (lambda ()
    (begin
      (display "--> ")
      (write (eval-program (parse-program (read))))
      (newline)
      (read-eval-print))))

;TODO