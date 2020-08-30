;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.scm
;;;; Project:       Delectus
;;;; Purpose:       function utilities
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (identity x) x)

(define (partial fn . args)
  (lambda more-args
    (apply fn `(,@args ,@more-args))))

(define (complement fn)
  (lambda args
    (not (apply fn args))))

(define (always c)
  (lambda args c))
