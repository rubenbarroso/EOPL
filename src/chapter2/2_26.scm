(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")

(define-datatype reference reference?
                 (a-ref
                   (position integer?)
                   (vec vector?)))

(define cell
  (lambda ()
    (a-ref 0 (vector 'undefined))))

(define cell?
  (lambda (c)
    (and (reference? c)
         (cases reference c
                (a-ref (position vec)
                       (and (= position 0)
                            (= (vector-length vec) 1)))))))

(define contents
  (lambda (c)
    (cases reference c
           (a-ref (position vec)
                  (vector-ref vec 0)))))

(define setcell
  (lambda (c x)
    (cases reference c
           (a-ref (position vec)
                  (vector-set! vec position x)))))

;> (cell? c1)
;#t
;> (cell? (a-ref 0 (make-vector 2)))
;#f
;> (contents c1)
;undefined
;> (setcell c1 "hey!")
;> (contents c1)
;"hey!"

;A cell object
(define cell
  (lambda ()
    (let ((c (a-ref 0 (vector 'undefined))))
      (let ((cell?
              (lambda ()
                (and (reference? c)
                     (cases reference c
                            (a-ref (position vec)
                                   (and (= position 0)
                                        (= (vector-length vec) 1)))))))
            (contents
              (lambda ()
                (cases reference c
                       (a-ref (position vec)
                              (vector-ref vec 0)))))
            (setcell
              (lambda (x)
                (cases reference c
                       (a-ref (position vec)
                              (vector-set! vec position x))))))
        (vector cell? contents setcell)))))

(define cell-cell?-operation
  (lambda (c) (vector-ref c 0)))

(define cell-contents-operation
  (lambda (c) (vector-ref c 1)))

(define cell-setcell-operation
  (lambda (c) (vector-ref c 2)))

;A client using the cell object
;> (let* ((c1 (cell))
;         (contents1 (cell-contents-operation c1))
;         (setcell1 (cell-setcell-operation c1)))
;    (begin
;      (setcell1 25)
;      (contents1)))
;25
