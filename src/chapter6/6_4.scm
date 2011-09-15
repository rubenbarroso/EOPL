(load "/Users/ruben/Dropbox/EOPL/src/interps/r5rs.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/define-datatype.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/sllgen.scm")
(load "/Users/ruben/Dropbox/EOPL/src/interps/6-top.scm")

;See changes to /Users/ruben/Dropbox/EOPL/src/interps/6-interp.scm
;Methods marked with ;''

;Test
;> (run
;    "abstract class test extends object
;       abstractmethod int initialize ()
;     new test()")
;Error reported by new-object:
;Abstract class test cannot be instantiated
