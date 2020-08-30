;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          views.scm
;;;; Project:       Delectus
;;;; Purpose:       computed views of tables
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; view objects
;;; ---------------------------------------------------------------------

(define-type view
  id: 8A68D921-4553-4F94-BC60-9D6624A123AD
  constructor: %make-view
  (table-id view:table-id)
  (include-deleted? view:include-deleted?)
  (filter-text view:filter-text)
  (sort-column view:sort-column)
  (sort-order view:sort-order)
  (column-labels view:column-labels)
  (row-indexes view:row-indexes))

(define (view:equal? v1 v2)
  (and (= (view:table-id v1)
          (view:table-id v2))
       (eqv? (view:include-deleted? v1)
             (view:include-deleted? v2))
       (or (and (not (view:filter-text v1))
                (not (view:filter-text v2)))
           (and (view:filter-text v1)
                (view:filter-text v2)
                (string-ci=? (view:filter-text v1)
                             (view:filter-text v2))))
       (or (and (not (view:sort-column v1))
                (not (view:sort-column v2)))
           (and (view:sort-column v1)
                (view:sort-column v2)
                (string-ci=? (view:sort-column v1)
                             (view:sort-column v2))))
       (or (and (not (view:sort-order v1))
                (not (view:sort-order v2)))
           (and (view:sort-column v1)
                (view:sort-column v2)
                (= (view:sort-order v1)
                   (view:sort-order v2))))))

(define (%view-select-live-columns table include-deleted?)
  (map column:label
       (filter (if include-deleted?
                   (always #t)
                   (complement column:deleted?))
               (vector->list (column-sequence:columns (table:column-sequence table))))))

(define (%view-select-live-row-indexes table include-deleted?)
  (filter-vector-indexes (table:rows table)
                         (range 0 (table:count-rows table)) 
                         (if include-deleted?
                             (always #t)
                             (complement row:deleted?))))

(define (%view-filter-row-indexes table row-indexes filter-text)
  (filter-vector-indexes (table:rows table)
                         row-indexes 
                         (if filter-text
                             (lambda (row)(row:match-text? row filter-text))
                             (always #t))))

(define (%view-row-numeric-comparator table sort-column-index sort-order)
  (let ((comp (cond
               ((eqv? sort-order $SORT_ASCENDING) <)
               ((eqv? sort-order $SORT_DESCENDING) >)
               (else (error "Invalid sort order" sort-order)))))
    (lambda (row1 row2)
      (comp (row:element-for-numeric-sort row1 sort-column-index)
            (row:element-for-numeric-sort row2 sort-column-index)))))

(define (%view-row-lexicographic-comparator table sort-column-index sort-order)
  (let ((comp (cond
               ((eqv? sort-order $SORT_ASCENDING) string-ci<?)
               ((eqv? sort-order $SORT_DESCENDING) string-ci>?)
               (else (error "Invalid sort order" sort-order)))))
    (lambda (row1 row2)
      (comp (row:element-for-string-sort row1 sort-column-index)
            (row:element-for-string-sort row2 sort-column-index)))))

(define (%view-sort-row-indexes table row-indexes sort-column sort-order)
  (if (and sort-column sort-order)
      (let ((col-index (table:column-index table sort-column)))
        (if (table:numeric-column? table sort-column)
            ;; numeric sort
            (sort-vector-indexes (table:rows table)
                                 row-indexes
                                 (%view-row-numeric-comparator table col-index sort-order))
            ;; lexicographic sort
            (sort-vector-indexes (table:rows table)
                                 row-indexes
                                 (%view-row-lexicographic-comparator table col-index sort-order))))
      row-indexes))

(define (view:create table-id #!key 
                     (include-deleted #f)
                     (filter-text #f)
                     (sort-column #f)
                     (sort-order #f))
  (let ((table (reg:find-table table-id)))
    (if table
        (let* ((live-columns (%view-select-live-columns table include-deleted))
               (live-row-indexes (%view-select-live-row-indexes table include-deleted))
               (filtered-row-indexes (%view-filter-row-indexes table live-row-indexes filter-text))
               (sorted-row-indexes (%view-sort-row-indexes table filtered-row-indexes sort-column sort-order)))
          (%make-view table-id include-deleted filter-text sort-column sort-order
                      live-columns (list->vector sorted-row-indexes)))
        (error "No such table" table-id))))

(define (view:count-rows v)
  (vector-length (view:row-indexes v)))

(define (view:count-columns v)
  (length (view:column-labels v)))

