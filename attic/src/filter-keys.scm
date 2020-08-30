;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          filter-keys.scm
;;;; Project:       Delectus
;;;; Purpose:       filter keys that refer to values
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (filter-vector-indexes vec indexes test)
  (filter (lambda (i)(test (vector-ref vec i)))
          indexes))
