;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tables.scm
;;;; Project:       Delectus
;;;; Purpose:       tables of data entries
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; tables
;;; ----------------------------------------------------------------------

(define-type delectus-table
  id: D604C012-3A9D-4D7B-BE70-4ACB9260659F
  constructor: %make-delectus-table
  (column-sequence table:column-sequence table:set-column-sequence!)
  (rows table:rows table:set-rows!))

(define (%table-parse-columns column-descriptions)
  (column-sequence:make column-descriptions))

(define (%table-parse-rows row-descriptions)
  (list->vector (map row:make row-descriptions)))

;;; return a list of columns
(define (%table-select-columns tbl pred)
  (filter pred (vector->list (column-sequence:columns (table:column-sequence tbl)))))

;;; return a list of rows
(define (%table-select-rows tbl pred)
  (filter pred (vector->list (table:rows tbl))))

;;; return a list of entries for the column
(define (%table-column-entries tbl column-label)
  (let ((rowcount (table:count-rows tbl))
        (colindex (table:column-index tbl column-label)))
    (let loop ((i 0)
               (result '()))
      (if (< i rowcount)
          (loop (+ i 1)(cons (vector-ref (row:entries (vector-ref (table:rows tbl)
                                                                  i))
                                         colindex)
                             result))
          (reverse result)))))

(define (table:make #!key (columns '()) (rows '()))
  (let ((col-sequence (%table-parse-columns columns))
        (row-vector (%table-parse-rows rows)))
    (%make-delectus-table col-sequence row-vector)))

(define (table:compact! tbl)
  (let* ((new-cols (%table-select-columns tbl (complement column:deleted?)))
         (live-column-labels (map column:label new-cols))
         (live-column-indexes (map (partial column-sequence:position (table:column-sequence tbl))
                                   live-column-labels))
         (live-rows (%table-select-rows tbl (complement row:deleted?)))
         (new-rows (map (lambda (r)(row:make-with-row-entries r live-column-indexes))
                        live-rows)))
    (table:set-column-sequence! tbl (column-sequence:make live-column-labels))
    (table:set-rows! tbl (list->vector new-rows))
    tbl))

(define (table:column-labels tbl)
  (vector->list (column-sequence:labels (table:column-sequence tbl))))

(define (table:count-columns tbl)
  (column-sequence:length (table:column-sequence tbl)))

(define (table:count-deleted-columns tbl)
  (let ((count 0))
    (vector-for-each (lambda (c)(if (column:deleted? c)(set! count (+ count 1))))
                     (column-sequence:columns (table:column-sequence tbl)))
    count))

(define (table:count-rows tbl)
  (vector-length (table:rows tbl)))

(define (table:count-deleted-rows tbl)
  (let ((count 0))
    (vector-for-each (lambda (r)(if (row:deleted? r)(set! count (+ count 1))))
                     (table:rows tbl))
    count))

(define (table:column-index tbl column-label)
  (column-sequence:position (table:column-sequence tbl) column-label))

(define (table:column-at tbl column-label)
  (let ((index (table:column-index tbl column-label)))
    (if index
        (column-sequence:element (table:column-sequence tbl)
                                 index)
        #f)))

(define (table:column-at-index tbl index)
  (column:label (column-sequence:element (table:column-sequence tbl) index)))

(define (table:row-at tbl row-index)
  (vector-ref (table:rows tbl) row-index))

(define (table:column-values tbl column-label)
  (map entry:value (%table-column-entries tbl column-label)))

(define (table:column-values-as-numbers tbl column-label)
  (map %entry-number-value (%table-column-entries tbl column-label)))

(define (table:numeric-column? tbl column-label)
  (if (every? number? (table:column-values-as-numbers tbl column-label))
      #t
      #f))

(define (table:add-row! tbl)
  (table:set-rows! tbl
                   (vector-add-last (table:rows tbl)
                                    (row:make (repeat (table:count-columns tbl) #f))))
  tbl)

(define (table:add-column! tbl column-label)
  (column-sequence:add-element! (table:column-sequence tbl) column-label)
  (vector-for-each (lambda (row)(row:add-element! row #f))
            (table:rows tbl))
  tbl)

(define (table:rename-column! tbl old-label new-label)
  (let ((col (table:column-at tbl old-label)))
    (column:set-label! col new-label))
  tbl)

(define (table:mark-column-deleted! tbl column-label deleted?)
  (column:set-deleted! (table:column-at tbl column-label)
                       deleted?))

(define (table:mark-row-deleted! tbl row-index deleted?)
  (row:set-deleted! (table:row-at tbl row-index)
                       deleted?))

(define (table:value-at tbl column-label row-index)
  (let ((col-index (table:column-index tbl column-label)))
    (if col-index
        (row:element (table:row-at tbl row-index)
                     col-index)
        #f)))

(define (table:has-deleted? tbl)
  (or (vector-some? column:deleted?
                    (column-sequence:columns (table:column-sequence tbl)))
      (vector-some? row:deleted?
                    (table:rows tbl))))

