(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/2-2-1.scm")

(define tag-tree
  (lambda (tree)
    (cases bintree tree
           (leaf-node (datum) '())
           (interior-node (key left right)
                          (let ((sum (leaf-sum tree)))
                            (append (list (list key sum))
                                    (tag-tree left)
                                    (tag-tree right)))))))

;> (define tree-a
;    (interior-node 'a (leaf-node 2) (leaf-node 3)))
;> (define tree-b
;    (interior-node 'b (leaf-node -1) tree-a))
;> (define tree-c
;    (interior-node 'c tree-b (leaf-node 1)))

;> (tag-tree tree-a)
;((a 5))
;> (tag-tree tree-b)
;((b 4) (a 5))
;> (tag-tree tree-c)
;((c 5) (b 4) (a 5))

(define max-node
  (lambda (leaf-nums)
    (define max-node-helper
      (lambda (lst max)
        (cond ((null? lst) max)
              ((> (cadar lst) (cadr max))
               (max-node-helper (cdr lst) (car lst)))
              (else (max-node-helper (cdr lst) max)))))
    (max-node-helper leaf-nums (car leaf-nums))))

;> (max-node '((c 5) (b 4) (a 5)))
;(c 5)
;> (max-node '((c 5) (b 4) (a 9) (d -5)))
;(a 9)

(define max-interior
  (lambda (tree)
    (car (max-node (tag-tree tree)))))

;> (max-interior tree-b)
;a
;> (max-interior tree-c)
;c
