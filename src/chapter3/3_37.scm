(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")

(define-datatype reference reference?
  (a-ref
    (position integer?)
    (vec vector?)))

(define newrefs
  (lambda (vals)
    (let ((vec (list->vector vals)))
      (map (lambda (pos val) (a-ref pos vec))
           (iota (length vals)) vals))))

;> (newrefs '(7 42 9 61))
;((a-ref 0 #4(7 42 9 61))
; (a-ref 1 #4(7 42 9 61))
; (a-ref 2 #4(7 42 9 61))
; (a-ref 3 #4(7 42 9 61)))

;newrefs allows us to share the internal vector between the references
