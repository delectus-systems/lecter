;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sort-keys.scm
;;;; Project:       Delectus
;;;; Purpose:       sort keys that refer to values
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (sort-vector-indexes vec indexes comparator)
  (sort indexes 
        (lambda (i1 i2)
          (comparator (vector-ref vec i1)
                      (vector-ref vec i2)))))
