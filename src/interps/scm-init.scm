;;; $Id: scm-init.scm,v 1.2 2001/08/21 03:29:33 leavens Exp leavens $
;;; EOPL2e compatibility file for SCM (tested with SCM version 5d3)

;;; save the nicer SCM behavior
(define scm-pretty-print pretty-print)

;;; The following make SCM R5RS compliant
(require 'values)
(require 'macro)
(require 'eval)
(set! *R4RS-macro* #t)

(load "r5rs.scm")
 
;;; use the nicer chez behavior for these
(set! sllgen:pretty-print scm-pretty-print)
(set! eopl:pretty-print scm-pretty-print)
(set! define-datatype:pretty-print scm-pretty-print)

(load "sllgen.scm")
(load "define-datatype.scm")
(load "test-harness.scm")
(load "test-suite.scm")
