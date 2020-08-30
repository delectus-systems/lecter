;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          test-utils.scm
;;;; Project:       Delectus
;;;; Purpose:       utilities to faciliate testing
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (test:report name . msg-args)
  (format "~%  ~s: ~a"
          name (string-join (map str msg-args) " ")))

(define (test:expect name expr 
                #!key
                (success (test:report name "Succeeded"))
                (failure (test:report name "FAILED!")))
  (if expr
      (display success)
      (display failure)))

(define (test:msg msg)
  (newline)
  (display msg)
  (newline))

