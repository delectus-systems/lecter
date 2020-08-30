;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          data.scm
;;;; Project:       Delectus
;;;; Purpose:       rows of entries
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; rows
;;; ----------------------------------------------------------------------

(define-type row
  id: 3520C851-B065-48DA-80B9-358367AF8A3E
  constructor: %make-row
  (entries row:entries row:set-entries!)
  (deleted? row:deleted? row:set-deleted!)
  (finished? row:finished? row:set-finished!))

(define (row:make vals)
  (%make-row (list->vector (map entry:make vals)) #f #f))

(define (row:make-with-row-entries r indexes)
  (let loop ((is indexes)
             (result '()))
    (if (null? is)
        (%make-row (list->vector (reverse result)) 
                   (row:deleted? r)
                   (row:finished? r))
        (loop (cdr is)
              (cons (vector-ref (row:entries r) (car is))
                    result)))))

(define (row:element seq index)
  (entry:value (vector-ref (row:entries seq) index)))

(define (row:element-as-number seq index)
  (%entry-number-value (vector-ref (row:entries seq) index)))

(define (row:element-for-numeric-sort row col-index)
  (or (row:element-as-number row col-index)
      $max-sort-fixnum))

(define (row:element-for-string-sort row col-index)
  (let ((s (row:element row col-index)))
    (if (or (not s)
            (not (string? s))
            (zero? (string-length s))
            (string-every? char-whitespace? s))
        $max-sort-string
        s)))

(define (row:position seq val)
  (vector-position (lambda (elt v)(string-ci=? (entry:value elt) v))
                   (row:entries seq) val))

(define (row:set-element! seq index val)
  (entry:set-value! (vector-ref (row:entries seq) index) val))

(define (row:add-element! seq val)
  (row:set-entries! seq (vector-add-last (row:entries seq) (entry:make val))))

(define (row:match-text? row text)
  (vector-some? (lambda (e)
                  (let ((v (entry:value e)))
                    (and (string? v)(string-contains-ci? v text))))
                (row:entries row))) 

(define (row:count-elements row)
  (vector-length (row:entries row)))

