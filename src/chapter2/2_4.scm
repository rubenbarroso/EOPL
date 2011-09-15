(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/2-2-1.scm")

(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
           (leaf-node (datum) (list 'leaf-node datum))
           (interior-node (key left right)
                          (list 'interior-node
                                key
                                (bintree-to-list left)
                                (bintree-to-list right))))))

;Tests
;> (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))
;(interior-node a (leaf-node 3) (leaf-node 4))

;> (bintree-to-list (interior-node 'a (leaf-node 3) (interior-node 'b (leaf-node 6) (leaf-node 9))))
;(interior-node
;  a
;  (leaf-node 3)
;  (interior-node b (leaf-node 6) (leaf-node 9)))

;> (leaf-sum (interior-node 'a (leaf-node 3) (interior-node 'b (leaf-node 6) (leaf-node 9))))
;18
