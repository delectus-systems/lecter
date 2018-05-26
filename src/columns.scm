;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          data.scm
;;;; Project:       Delectus
;;;; Purpose:       columns and column sequences
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; columns
;;; ----------------------------------------------------------------------

(define-type column
  id: 788DB6EF-0829-41EA-BF23-F0B977978672
  constructor: %make-column
  (label column:label column:set-label!)
  (deleted? column:deleted? column:set-deleted!))

(define (column:make label #!key (deleted #f))
  (if (string? label)
      (%make-column label deleted)
      (error "Invalid column label" label)))

;;; (define $c (column:make "Name"))
;;; $c

;;; ----------------------------------------------------------------------
;;; column-sequences
;;; ----------------------------------------------------------------------

(define-type column-sequence
  id: 2D65FC83-52F9-4A24-BF8E-99A8CA106583
  constructor: %make-column-sequence
  (columns column-sequence:columns column-sequence:set-columns!))

(define (column-sequence:make labels)
  (if (duplicates? string-ci=? labels)
      (error "Duplicate column labels" labels)
      (%make-column-sequence (list->vector (map column:make labels)))))

(define (column-sequence:element seq index)
  (vector-ref (column-sequence:columns seq) index))

(define (column-sequence:labels seq)
  (vector-map column:label (column-sequence:columns seq)))

(define (column-sequence:position seq val)
  (vector-position (lambda (elt v)(string-ci=? (column:label elt) v))
                   (column-sequence:columns seq) val))

(define (column-sequence:length seq)
  (vector-length (column-sequence:columns seq)))

(define (column-sequence:add-element! seq val)
  (let ((val-index (column-sequence:position seq val)))
    (if val-index
        (error "Column exists" val)
        (column-sequence:set-columns! seq (vector-add-last (column-sequence:columns seq) (column:make val))))))

